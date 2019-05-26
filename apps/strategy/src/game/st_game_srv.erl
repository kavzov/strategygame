-module(st_game_srv).
-behaviour(gen_server).

-export([start_link/2, plr_cmd/3, stop_by_player_interrupt/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("st_game.hrl").

-define(SEP, <<" ">>).
-define(ENDSTR, <<"\r\n">>).

-define(WIDTH, get_battle_field_width()).
-define(HEIGHT, get_battle_field_height()).
-define(INIT_POS, [1, ?WIDTH * ?HEIGHT]).
-define(HSTEP, 1).
-define(VSTEP, ?WIDTH).
-define(PL1, <<"A">>).
-define(PL2, <<"B">>).
-define(PLS, [?PL1, ?PL2]).
-define(EMP, <<" ">>).
-define(OCC, <<"X">>).
-define(DIRS, [up, right, down, left]).
-define(YOURMOVE, <<"Your move">>).
-define(WAITMOVE, <<"Wait for the opponent move...", ?ENDSTR/binary>>).

-record(state, {
    board :: list(),
    players :: map(),
    que  :: list()
}).


start_link(Player1, Player2) ->
    gen_server:start_link(?MODULE, [Player1, Player2], []).

plr_cmd(GameSrv, PlrSock, Cmd) ->
    case maps:to_list(st_game_maker:get_game(GameSrv)) of
        [_Game] -> 
            gen_server:call(GameSrv, {cmd, PlrSock, Cmd});
        [] ->
            gen_tcp:send(PlrSock, <<"Have a little patience, please.\r\nWaiting for an opponent...\r\n">>)
    end.

send_reply(PlayerSock, Message) ->
    gen_tcp:send(PlayerSock, Message).

send_reply_to_players(Reply) ->
    maps:map(
        fun(PlayerSock, PlayerMsg) ->
            send_reply(PlayerSock, PlayerMsg)
        end, Reply).

send_info(PlayerSrv, Info) ->
    PlayerSrv ! Info.

send_info_to_players(Players, Info) ->
    maps:map(
        fun(_Sock, Player) -> send_info(maps:get(srv, Player), Info) end,
        Players
    ).

stop_by_player_interrupt(GameSrv, PlayerSock) ->
    gen_server:call(GameSrv, {stop_by_player_interrupt, PlayerSock, GameSrv}).

stop_game_player(PlayerSrv) ->
    send_info(PlayerSrv, 'EXIT_BATTLE').

stop_game(WinnerSock, LoserSock, Players) ->
    send_info_to_players(Players, 'EXIT_BATTLE'),
    st_game_maker:del_game(self()),
    WinnerId = maps:get(id, maps:get(WinnerSock, Players)),
    LoserId = maps:get(id, maps:get(LoserSock, Players)),
    db_end_game(WinnerId, LoserId),
    ok.

%% gen_server API
init([P1_Sock, P2_Sock]) ->
    {P1_ID, P1_Name, _P1_Rating, P1_Srv, _} = st_player_storage:get_player(P1_Sock),
    {P2_ID, P2_Name, _P2_Rating, P2_Srv, _} = st_player_storage:get_player(P2_Sock),
    Player_1 = #{id => P1_ID, name => P1_Name, srv => P1_Srv, ch => ?PL1, moves => []},
    Player_2 = #{id => P2_ID, name => P2_Name, srv => P2_Srv, ch => ?PL2, moves => []},

    Players = #{P1_Sock => Player_1, P2_Sock => Player_2},
    Que = [P1_Sock, P2_Sock],

    BoardData = new_board(?WIDTH, ?HEIGHT),
    Board = get_board(BoardData),

    State = #state{board = BoardData, players = Players, que = Que},

    YourMoveMsg = get_your_move_msg(?PL1, P1_Name),
    Controls = get_battle_controls(),
    Reply = #{P1_Sock => <<?ENDSTR/binary, "Welcome to the battle, ", P1_Name/binary, "!", ?ENDSTR/binary, "Your playing symbol - ", ?PL1/binary, ".", Controls/binary, Board/binary, YourMoveMsg/binary>>, P2_Sock => <<?ENDSTR/binary, "Welcome to the battle, ", P2_Name/binary, "!", ?ENDSTR/binary, "Your playing symbol - ", ?PL2/binary, ".", Controls/binary, Board/binary, ?WAITMOVE/binary>>},

    % st_game_maker:add_game(self(), Players),
    % st_game_maker:del_player_from_waitlist(P1_Sock),
    % st_game_maker:del_player_from_waitlist(P2_Sock),
    lager:info("Players ~p and ~p started new game", [P1_Name, P2_Name]),

    send_info_to_players(Players, {'START_BATTLE', self()}),
    send_reply_to_players(Reply),
    % TODO send messages to all bet players
    lager:info("Game started"),

    {ok, State}.


handle_call({cmd, PlrSock, Cmd}, _From, #state{board = BoardData, players = Players, que = Que} = State) ->
    Player = maps:get(PlrSock, Players),
    PlrCh = maps:get(ch, Player),
    case (PlrSock =:= first(Que)) orelse (Cmd =:= <<"giveup">>) of
        true ->
            CurrPos = string:str(BoardData, [PlrCh]),
            case move(BoardData, PlrCh, PlrSock, CurrPos, Cmd) of
                {ok, NewBoardData, NewPos} ->
                    NewQue = swap_que(Que),
                    % Update player moves
                    PlrMoves = maps:get(moves, Player) ++ [NewPos],
                    NewPlr = maps:update(moves, PlrMoves, Player),
                    NewPlayers = maps:update(PlrSock, NewPlr, Players),
                    % New State
                    NewState = State#state{board = NewBoardData, players = NewPlayers, que = NewQue},
                    Board = get_board(NewBoardData),
                    % Check for leader or winner
                    NewQuePL = lists:map(fun(Sock) -> {maps:get(ch, maps:get(Sock, Players)), Sock} end, NewQue),
                    case get_leader(NewBoardData, NewPlayers, NewQuePL) of
                        {prewinner, PreWinnerSock} ->
                            PreWinnerName = maps:get(name, maps:get(PreWinnerSock, Players)),
                            PreWinnerCh = maps:get(ch, maps:get(PreWinnerSock, Players)),
                            PreLoserSock = get_opponent(PreWinnerSock, Que),
                            PreLoserName = maps:get(name, maps:get(PreLoserSock, Players)),
                            PreLoserCh = maps:get(ch, maps:get(PreLoserSock, Players)),
                            PreWinnerMoveMsg = whose_move(PreWinnerSock, PreWinnerCh, PreWinnerName, NewQue),
                            PreloserMoveMsg = whose_move(PreLoserSock, PreLoserCh, PreLoserName, NewQue),
                            Reply = #{
                                PreWinnerSock => <<Board/binary, "You are in a win-win situation", ?ENDSTR/binary, PreWinnerMoveMsg/binary>>,
                                get_opponent(PreWinnerSock, Que) => <<Board/binary, "You are in a no-win situation", ?ENDSTR/binary, PreloserMoveMsg/binary>>
                            },
                            send_reply_to_players(Reply),
                            % send messages to all bet players
                            {reply, ok, NewState};
                        {winner, WinnerSock} ->
                            WinnerName = maps:get(name, maps:get(WinnerSock, Players)),
                            LoserSock = get_opponent(WinnerSock, Que),
                            LoserName = maps:get(name, maps:get(LoserSock, Players)),
                            ServerMsg = server_msg(),
                            Reply = #{WinnerSock => <<Board/binary, "You won the game, ", WinnerName/binary, "!", ?ENDSTR/binary, ServerMsg/binary>>, LoserSock => <<Board/binary, "You lost, ", LoserName/binary, ".", ?ENDSTR/binary, ServerMsg/binary>>},
                            send_reply_to_players(Reply),
                            stop_game(WinnerSock, LoserSock, Players),
                            lager:info("~p won the game.", [WinnerName]),
                            {stop, normal, ok, State};
                        error ->
                            MovingPlrSock = lists:nth(1, NewQue),
                            MovingPlrName = maps:get(name, maps:get(MovingPlrSock, Players)),
                            MovingPlrCh = maps:get(ch, maps:get(MovingPlrSock, Players)),
                            YourMoveMsg = get_your_move_msg(MovingPlrCh, MovingPlrName),
                            Reply = #{MovingPlrSock => <<Board/binary, YourMoveMsg/binary>>, lists:nth(2, NewQue) => <<Board/binary, ?WAITMOVE/binary>>},
                            send_reply_to_players(Reply),
                            {reply, ok, NewState}
                    end;
                {giveup, GaveupSock} ->
                    WinnerSock = get_opponent(GaveupSock, Que),
                    WinnerName = maps:get(name, maps:get(WinnerSock, Players)),
                    GaveupName = maps:get(name, maps:get(GaveupSock, Players)),
                    ServerMsg = server_msg(),
                    Reply = #{GaveupSock => <<"You lost, ", GaveupName/binary, ".", ?ENDSTR/binary, ServerMsg/binary>>, WinnerSock => <<"Your opponent ", GaveupName/binary, " has gave up.", ?ENDSTR/binary, "You won the game, ", WinnerName/binary, "!", ?ENDSTR/binary, ServerMsg/binary>>},
                    send_reply_to_players(Reply),
                    stop_game(WinnerSock, GaveupSock, Players),
                    lager:info("~p gave up. ~p won the game.", [GaveupName, WinnerName]),
                    {stop, normal, ok, State};
                {error, Reason} ->
                    PlrName = maps:get(name, Player),
                    YourMoveMsg = get_your_move_msg(PlrCh, PlrName),
                    gen_tcp:send(PlrSock, <<"ERROR: ", Reason/binary, ?ENDSTR/binary, YourMoveMsg/binary>>),
                    {reply, ok, State}
            end;
        false ->
            gen_tcp:send(PlrSock, <<?WAITMOVE/binary>>),
            {reply, ok, State}
    end;

handle_call({stop_by_player_interrupt, PlayerSock, GameSrv}, _From, #state{players = Players, que = Que} = State) ->
    OpntSock = get_opponent(PlayerSock, Que),
    OpntSrv = maps:get(srv, maps:get(OpntSock, Players)),
    stop_game_player(OpntSrv),
    send_reply(OpntSock, <<"The game stopped by a reason on the opponent side\r\n">>),
    st_game_maker:del_game(GameSrv),
    {stop, normal, ok, State};


handle_call({check_turn, PlrCh}, _From, #state{que = Que} = State) ->
    {reply, PlrCh =:= lists:nth(1, Que), State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal functions
move(BoardData, PlrCh, PlrSock, CurrPos, CmdBin) ->
    Up = up, Right = right, Down = down, Left = left,
    Cmds = #{<<"up">> => Up, <<"w">> => Up, <<"right">> => Right, <<"d">> => Right, <<"down">> => Down, <<"s">> => Down, <<"left">> => Left, <<"a">> => Left, <<"giveup">> => giveup},
    case maps:find(CmdBin, Cmds) of
        {ok, Cmd} ->
            if Cmd =:= giveup -> {giveup, PlrSock};
            true -> check_move(Cmd, BoardData, PlrCh, CurrPos)
            end;
        error   -> {error, <<"unknown command">>}
    end.


check_move(up, BoardData, PlrCh, CurrPos) ->
    NewPos = CurrPos - ?VSTEP,
    if (NewPos < 1) ->
        {error, <<"border">>};
    true ->
        case lists:nth(NewPos, BoardData) of
            ?EMP -> {ok, get_new_board(up, BoardData, PlrCh, CurrPos), NewPos};
            _ ->    {error, <<"occupied place">>}
        end
    end;

check_move(right, BoardData, PlrCh, CurrPos) ->
    Width= ?WIDTH,
    if (CurrPos rem Width =:= 0) ->
        {error,  <<"border">>};
    true ->
        NewPos = CurrPos + ?HSTEP,
        case lists:nth(NewPos, BoardData) of
            ?EMP -> {ok, get_new_board(right, BoardData, PlrCh, CurrPos), NewPos};
            _    -> {error, <<"occupied place">>}
        end
    end;

check_move(down, BoardData, PlrCh, CurrPos) ->
    Width= ?WIDTH, Height = ?HEIGHT,
    NewPos = CurrPos + ?VSTEP,
    if (NewPos > Height * Width) ->
        {error,  <<"border">>};
    true ->
        case lists:nth(NewPos, BoardData) of
            ?EMP -> {ok, get_new_board(down, BoardData, PlrCh, CurrPos), NewPos};
            _ ->    {error, <<"occupied place">>}
        end
    end;

check_move(left, BoardData, PlrCh, CurrPos) ->
    Width= ?WIDTH,
    NewPos = CurrPos - ?HSTEP,
    if (CurrPos rem Width =:= 1) ->
        {error,  <<"border">>};
    true ->
        case lists:nth(NewPos, BoardData) of
            ?EMP -> {ok, get_new_board(left, BoardData, PlrCh, CurrPos), NewPos};
            _ ->    {error, <<"occupied place">>}
        end
    end.


get_leader(BoardData, Players, QuePL) ->
    ChQue  = lists:map(fun({Ch, _Sock}) -> Ch end, QuePL),
    case get_winner(BoardData, ChQue) of
        {winner, PlayerCh} -> {winner, proplists:get_value(PlayerCh, QuePL)};
        error ->
            % check for prewinner
            [Plr1Ch, Plr2Ch] = ChQue,
            Plr1Moves = maps:get(moves, maps:get(proplists:get_value(Plr1Ch, QuePL), Players)),
            Plr2Moves = maps:get(moves, maps:get(proplists:get_value(Plr2Ch, QuePL), Players)),
            Plr1RestMoves = lists:sort(get_rest_moves(BoardData, Plr1Ch)),
            Plr2RestMoves = lists:sort(get_rest_moves(BoardData, Plr2Ch)),
            CommonRestMoves = lists:sort(sets:to_list(sets:intersection(sets:from_list(Plr1RestMoves), sets:from_list(Plr2RestMoves)))),

            if (length(Plr1Moves) =:= length(Plr2Moves)) andalso ((CommonRestMoves =:= []) orelse ((Plr1RestMoves =:= CommonRestMoves) orelse (Plr2RestMoves =:= CommonRestMoves))) ->
                if  (length(Plr1RestMoves) > length(Plr2RestMoves)) -> {prewinner, proplists:get_value(Plr1Ch, QuePL)};
                    (length(Plr1RestMoves) < length(Plr2RestMoves)) -> {prewinner, proplists:get_value(Plr2Ch, QuePL)};
                true -> error
                end;
            true -> error
            end
    end.

get_winner(_Board, []) -> error;
get_winner(Board, [Player | Rest]) ->
    case is_stalemate(Board, Player) of
        true  -> {winner, get_opponent(Player)};
        false -> get_winner(Board, Rest)
    end.


is_stalemate(Board, Player) ->
    Pos = string:str(Board, [Player]),
    is_stalemate(Board, Player, Pos, ?DIRS).
is_stalemate(_State, _Player, _Pos, []) -> true;
is_stalemate(State, Player, Pos, [Dir | Rest]) ->
    case check_move(Dir, State, Player, Pos) of
        {ok, _State, _NewPos} -> false;
        {error, _Reason} -> is_stalemate(State, Player, Pos, Rest)
    end.


get_rest_moves(Board, Player) ->
    Pos = string:str(Board, [Player]),
    get_rest_moves(Board, Player, Pos, ?DIRS, [], []).

get_rest_moves(_Board, _Player, _Pos, [], [], Acc) -> Acc;
get_rest_moves(Board, Player, _Pos, [], [{PosRe, RestDirs} | RestPos], Acc) ->
    get_rest_moves(Board, Player, PosRe, RestDirs, RestPos, Acc);
get_rest_moves(Board, Player, Pos, [Dir | RestDirs], AccPos, Acc) ->
    case check_move(Dir, Board, Player, Pos) of
        {ok, _Board, NewPos} ->
            case lists:member(NewPos, Acc) of
                true -> get_rest_moves(Board, Player, Pos, RestDirs, AccPos, Acc);
                false -> get_rest_moves(Board, Player, NewPos, ?DIRS, [{Pos, RestDirs} | AccPos], [NewPos | Acc])
            end;
        {error, _Reason} ->
            get_rest_moves(Board, Player, Pos, RestDirs, AccPos, Acc)
    end.


get_new_board(up, Board, Player, Pos) ->
    lists:sublist(Board, Pos-?VSTEP-1) ++ [Player] ++ lists:sublist(Board, Pos-?VSTEP+1, ?WIDTH-1) ++ [?OCC] ++ lists:nthtail(Pos, Board);

get_new_board(right, Board, Player, Pos) ->
    lists:sublist(Board, Pos-1) ++ [?OCC] ++ [Player] ++ lists:nthtail(Pos+1, Board);

get_new_board(down, Board, Player, Pos) ->
    lists:sublist(Board, Pos-1) ++ [?OCC] ++ lists:sublist(Board, Pos+1, ?WIDTH-1) ++ [Player] ++ lists:nthtail(Pos+?VSTEP, Board);

get_new_board(left, Board, Player, Pos) ->
    lists:sublist(Board, Pos-2) ++ [Player] ++ [?OCC] ++ lists:nthtail(Pos, Board).


%% -- Board -- %%
new_board(Width, Height) ->
    [?PL1] ++ [?EMP || _ <- lists:seq(1, Width * Height - 2)] ++ [?PL2].

get_board(State) ->
    CR = <<"+">>,
    HL = <<"---">>,
    VL = <<"|">>,
    SP = <<" ">>,

    HLINE = lists:foldl(
        fun(LinePart, LINE) ->
            <<LINE/binary, LinePart/binary>>
        end,
        CR,
        [<<HL/binary, CR/binary>> || _ <- lists:seq(1, ?WIDTH)]
    ),
    lists:foldl(
        fun(L, BOARD) ->
            ValStr = lists:foldl(
                fun(CH, STR) ->
                    <<STR/binary, SP/binary, CH/binary, SP/binary, VL/binary>>
                end, VL, L
            ),
            <<BOARD/binary, ValStr/binary, ?ENDSTR/binary, HLINE/binary, ?ENDSTR/binary>>
        end,
        <<HLINE/binary, ?ENDSTR/binary>>,
        % split state list into HEIGHT lists
        [lists:sublist(State, N * ?WIDTH + 1, ?WIDTH) || N <- lists:seq(0, ?HEIGHT - 1)]
    ).
%% -- Board -- %%


%% -- Utils -- %%
first(List) -> [First | _] = List, First.

swap_que(List) -> lists:nthtail(1, List) ++ [first(List)].

get_opponent(PlayerCh) ->
    case PlayerCh of
        ?PL1 -> ?PL2;
        ?PL2 -> ?PL1
    end.

get_opponent(PlrSock, [Plr1Sock, Plr2Sock]) ->
    if PlrSock =:= Plr1Sock -> Plr2Sock;
    true -> Plr1Sock
end.


get_your_move_msg(Ch, Name) ->
    <<?YOURMOVE/binary, ", ", Name/binary, " (", Ch/binary, "): ">>.

whose_move(PlrSock, PlrCh, PlrName, Que) ->
    case PlrSock =:= lists:nth(1, Que) of
        true  -> get_your_move_msg(PlrCh, PlrName);
        false -> ?WAITMOVE
    end.

get_battle_field_width() ->
    {ok, Width} = application:get_env(strategy, battle_field_width),
    Width.

get_battle_field_height() ->
    {ok, Height} = application:get_env(strategy, battle_field_width),
    Height.

get_battle_controls() ->
    Up = <<226,134,145>>, Right = <<226,134,146>>, Down = <<226,134,147>>, Left = <<226,134,144>>,
    <<"
+---+-------+---+
| ", Up/binary, " | up    | w |
| ", Right/binary, " | right | d |
| ", Down/binary, " | down  | s |
| ", Left/binary, " | left  | a |
+---+-------+---+
\r\n">>.


db_end_game(WinnerId, LoserId) ->
    case epgsql:connect("localhost", "strategy", "strategy", #{database => "strategy", port => 15432, timeout => 5000}) of
        {ok, C} ->
            {ok, _} = epgsql:equery(C, "
                                UPDATE players SET
                                played_battles = played_battles + 1,
                                winrate = round(100 * battles_won::numeric / played_battles::numeric) 
                                WHERE id IN ($1, $2);
                                ", [WinnerId, LoserId]),
            {ok, _} = epgsql:equery(C, "
                                UPDATE players SET
                                battles_won = battles_won + 1
                                WHERE id = $1;
                                ", [WinnerId]),
			ok = epgsql:close(C);
        {error, Reason} -> lager:error("NO CONNECTION TO DB:~n~p", [Reason])
    end.
