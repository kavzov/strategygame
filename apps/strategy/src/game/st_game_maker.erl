-module(st_game_maker).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,

            calc_coef/2, init_coef/1, rand_real/2,

         connect/2, add_new_game/2, get_games/0, get_games/1, add_game/2, get_game/1, get_game_by_srv/1, get_game_id/1, play_game/2, get_all_games/0, del_game/1, del_player_from_waitlist/1
]).

% -record(state, {}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_game(GameSrv, Player) ->
    gen_server:call(?MODULE, {add_game, GameSrv, Player}).

add_player_to_waitlist(Socket) ->
    gen_server:call(?MODULE, {add_player_to_waitlist, Socket}).

del_player_from_waitlist(Socket) ->
    gen_server:call(?MODULE, {del_player_from_waitlist, Socket}).

get_game(GameId) ->
    gen_server:call(?MODULE, {get_game, GameId}).

get_game_by_srv(GameSrv) ->
    gen_server:call(?MODULE, {get_game_by_srv, GameSrv}).

get_game_id(GameSrv) ->
    gen_server:call(?MODULE, {get_game_id, GameSrv}).

get_all_games() ->
    gen_server:call(?MODULE, get_all_games).

del_game(GameSrv) ->
    gen_server:cast(?MODULE, {del_game, GameSrv}).

get_opponent(Rating) ->
    gen_server:call(?MODULE, {get_opponent, Rating}).

connect(PlayerSock, _PlayerSrv) ->
    case get_opponent(PlayerSock) of
        {ok, OpntSock} ->
            {ok, GameSrv} = supervisor:start_child(st_game_sup, [OpntSock, PlayerSock]),
            erlang:monitor(process, GameSrv),   % ?
            start_game;
        not_found ->
            add_player_to_waitlist(PlayerSock),
            gen_tcp:send(PlayerSock, <<"Waiting for opponent...\r\n">>),
            no_opponent
    end.


% New BET version
add_new_game(PlayerSrv, [Width, Height]) ->
    {state, PlayerSock, {player, Id, Name, Wallet, Battles, Won, Rating, Position}, _Mode, GameSrv} = st_player_srv:get_player_info(PlayerSrv),
    gen_server:call(?MODULE, {add_new_game, Width, Height, Id, Name, Wallet, Battles, Won, Rating, Position, PlayerSock, PlayerSrv, GameSrv}).

play_game(GameId, PlayerSrv) ->
    {state, PlayerSock, {player, Id, Name, Wallet, Battles, Won, Rating, Position}, _Mode, GameSrv} = st_player_srv:get_player_info(PlayerSrv),
    % GET OPNT SOCKET
    gen_server:call(?MODULE, {play_game, GameId, Id, Name, Wallet, Battles, Won, Rating, Position, PlayerSock, PlayerSrv, GameSrv}).

get_games() ->
    gen_server:call(?MODULE, get_games).

get_games(Status) ->
    gen_server:call(?MODULE, {get_games, Status}).


%%% gen_server API

init([]) ->
    lager:info("~p init", [?MODULE]),
    State = #{},
    {ok, State}.

handle_call({add_new_game, Width, Height, Id, Name, Wallet, Battles, Won, Rating, Position, PlayerSock, PlayerSrv, GameSrv}, _From, State) ->
    Player = #{name => Name, wallet => Wallet, battles => Battles, won => Won, rating => Rating, position => Position, srv => PlayerSrv, socket => PlayerSock, coef => 1.0},
    Game = #{status => wait, players => #{Id => Player}, srv => GameSrv, size => [Width, Height]},
    gen_tcp:send(PlayerSock, <<"Wait for an opponent...\r\n">>),
    {reply, ok, maps:put(get_id(maps:keys(State)), Game, State)};

handle_call({play_game, GameId, PlayerId, PlayerName, Wallet, Battles, Won, Rating, Position, PlayerSock, PlayerSrv, _GameSrv}, _From, State) ->
    case maps:find(GameId, State) of
        {ok, Game} ->
            GamePlayers = maps:get(players, Game),
            [Player_1_0] = maps:values(GamePlayers),
            [Player_1_0_ID] = maps:keys(GamePlayers),
            Pl_1_Sock = maps:get(socket, Player_1_0), Pl_2_Sock = PlayerSock,
            Pl_1_Rt = maps:get(rating, Player_1_0), Pl_2_Rt = Rating,
            {Pl_1_Coef, Pl_2_Coef} = calc_coef(Pl_1_Rt, Pl_2_Rt),
            Player_1 = maps:update(coef, Pl_1_Coef, Player_1_0),
            GamePlayers1 = maps:update(Player_1_0_ID, Player_1, GamePlayers),
            Player_2 = #{name => PlayerName, wallet => Wallet, battles => Battles, won => Won, rating => Rating, position => Position, srv => PlayerSrv, socket => PlayerSock, coef => Pl_2_Coef},
            GamePlayers2 = maps:put(PlayerId, Player_2, GamePlayers1),
            {ok, GameSrv} = supervisor:start_child(st_game_sup, [Pl_1_Sock, Pl_2_Sock, maps:get(size, Game)]),
            erlang:monitor(process, GameSrv),
            Game1 = maps:update(status, battle, Game),
            Game2 = maps:update(players, GamePlayers2, Game1),
            Game3 = maps:update(srv, GameSrv, Game2),
            {reply, start_game, maps:update(GameId, Game3, State)};
        error ->
            {reply, game_not_found, State}
    end;

handle_call({get_game, GameId}, _From, State) ->
    {reply, maps:get(GameId, State), State};

handle_call({get_game_by_srv, GameSrv}, _From, State) ->
    Res = maps:filter(fun(_GameId, Game) ->
        maps:get(srv, Game) =:= GameSrv end, State),
    {reply, Res, State};

handle_call({get_game_id, GameSrv}, _From, State) ->
    Res = maps:filter(fun(_GameId, Game) ->
        maps:get(srv, Game) =:= GameSrv end, State),
    [GameId] = maps:keys(Res),
    {reply, GameId, State};

handle_call(get_games, _From, State) ->
    {reply, State, State};

handle_call({get_games, Status}, _From, State) ->
    Reply = maps:filter(
        fun(_Id, Game) ->
            case maps:get(status, Game) of
                Status -> true;
                _ -> false
            end
        end,
        State
    ),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({del_game, GameSrv}, State) ->
    Game = maps:filter(fun(_Id, G) ->
        maps:get(srv, G) =:= GameSrv end, State),
    [GameId] = maps:keys(Game),
    {noreply, maps:remove(GameId, State)};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Info}, State) ->
    lager:info("Game down ~p ~p", [Pid, Info]),
    {noreply, State};

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


% Utlis
get_id([]) -> 1;
get_id(Ids) ->
    IdMax = lists:max(Ids),
    IdsCand = lists:seq(1, IdMax),
    get_id([], IdMax, Ids, IdsCand).
get_id([], IdMax, _Ids, []) -> IdMax + 1;
get_id([], IdMax, Ids, [Id | Rest]) ->
    case lists:member(Id, Ids) of
        false -> Id;
        true  -> get_id([], IdMax, Ids, Rest)
    end.

calc_coef(R1, R2) ->
    if (R1 > R2) ->
        Div = R1 / R2,
        if (Div > 1) ->
            Coef_1 = init_coef(R1) + rand_real(0, 0.01),
            Coef_2 = init_coef(R2) * rand_real(1, Div),
            {fcoef(Coef_1), fcoef(Coef_2)};
        true ->
            Coef_1 = init_coef(R1) + rand_real(0.1, 0.2),
            Coef_2 = init_coef(R2) + rand_real(0.2, 0.5),
            {fcoef(Coef_1), fcoef(Coef_2)}
        end;
        (R1 =:= R2) -> 
            Coef_1 = init_coef(R1) + rand_real(0.01, 0.1),
            Coef_2 = init_coef(R2) + rand_real(0.01, 0.1),
            {fcoef(Coef_1), fcoef(Coef_2)};
        (R1 < R2) ->
            Div = R2 / R1,
            if (Div > 1) ->
                Coef_2 = init_coef(R2) + rand_real(0.1, 0.2),
                Coef_1 = init_coef(R1) * Div * rand_real(1, Div),
                {fcoef(Coef_1), fcoef(Coef_2)};
            true ->
                Coef_2 = init_coef(R2) + rand_real(0, 0.01),
                Coef_1 = init_coef(R1) * rand_real(1, Div),
                {fcoef(Coef_1), fcoef(Coef_2)}
            end
    end.


add_coef(V) ->
    AddCoef = math:log(V),
    if AddCoef < 1 -> V;
    true -> AddCoef
end.

init_coef(Rating) ->
    2 - Rating/100.

rand_real() -> rand:uniform_real().

rand_real_begin(Begin, End) when Begin < End->
    Rand = Begin + rand_real(),
    if Rand < End -> Rand;
    true -> rand_real_begin(Begin, End)
end.

rand_real_end(Begin, End) when Begin < End ->
    Rand = End - rand_real(),
    if Rand > Begin -> Rand;
    true -> rand_real_end(Begin, End)
    end.

rand_real_2(Begin, End) ->
    if End-Begin < 1 -> rand_real_begin(Begin, End);
    true -> Begin + rand:uniform(round(End-Begin))-1 + rand:uniform_real()
    end.

rand_real(Begin, End) ->
    Rands = [fun rand_real_begin/2, fun rand_real_end/2, fun rand_real_2/2],
    F = lists:nth(rand:uniform(length(Rands)), Rands),
    F(Begin, End).

fcoef(Coef) ->
    list_to_float(float_to_list(Coef, [{decimals, 4}])).

set_bet_coef(Rating) ->
    list_to_float(float_to_list(1.9 - Rating/100, [{decimals, 2}])).