-module(st_game_maker).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
         connect/2, add_new_game/2, get_games/1, add_game/2, get_game/1, get_game_id/1, play_game/2, get_all_games/0, del_game/1, del_player_from_waitlist/1
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

get_game(GameSrv) ->
    gen_server:call(?MODULE, {get_game, GameSrv}).

get_game_id(GameSrv) ->
    gen_server:call(?MODULE, {get_game, GameSrv}).

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
    {state, PlayerSock, {player, Id, Name, Rating}, _Mode, GameSrv} = st_player_srv:get_player_info(PlayerSrv),
    gen_server:call(?MODULE, {add_new_game, Width, Height, Id, Name, Rating, PlayerSock, PlayerSrv, GameSrv}).

play_game(GameId, PlayerSrv) ->
    {state, PlayerSock, {player, Id, Name, Rating}, _Mode, GameSrv} = st_player_srv:get_player_info(PlayerSrv),
    % GET OPNT SOCKET
    gen_server:call(?MODULE, {play_game, GameId, Id, Name, Rating, PlayerSock, PlayerSrv, GameSrv}).

get_games(Status) ->
    gen_server:call(?MODULE, {get_games, Status}).


%%% gen_server API

init([]) ->
    lager:info("~p init", [?MODULE]),
    State = #{},
    {ok, State}.

handle_call({add_new_game, Width, Height, Id, Name, Rating, PlayerSock, PlayerSrv, GameSrv}, _From, State) ->
    Player = #{name => Name, rating => Rating, srv => PlayerSrv, socket => PlayerSock, coef => set_bet_coef(Rating)},
    Game = #{status => wait, players => #{Id => Player}, srv => GameSrv, size => [Width, Height]},
    gen_tcp:send(PlayerSock, <<"Wait for an opponent...\r\n">>),
    {reply, ok, maps:put(get_id(), Game, State)};

handle_call({play_game, GameId, PlayerId, PlayerName, Rating, PlayerSock, PlayerSrv, _GameSrv}, _From, State) ->
    case maps:find(GameId, State) of
        {ok, Game} ->
            GamePlayers = maps:get(players, Game),
            [Player_1] = maps:values(GamePlayers),
            Pl_1_Sock = maps:get(socket, Player_1), Pl_2_Sock = PlayerSock,
            Player = #{name => PlayerName, rating => Rating, srv => PlayerSrv, socket => PlayerSock, coef => set_bet_coef(Rating)},
            NewGamePlayers = maps:put(PlayerId, Player, GamePlayers),
            {ok, GameSrv} = supervisor:start_child(st_game_sup, [Pl_1_Sock, Pl_2_Sock, maps:get(size, Game)]),
            erlang:monitor(process, GameSrv),
            Game1 = maps:update(status, battle, Game),
            Game2 = maps:update(players, NewGamePlayers, Game1),
            Game3 = maps:update(srv, GameSrv, Game2),
            {reply, start_game, maps:update(GameId, Game3, State)};
        error ->
            {reply, game_not_found, State}
    end;

handle_call({get_game, GameSrv}, _From, State) ->
    Res = maps:filter(fun(_GameId, Game) ->
        maps:get(srv, Game) =:= GameSrv end, State),
    {reply, Res, State};

handle_call({get_game_id, GameSrv}, _From, State) ->
    Res = maps:filter(fun(_GameId, Game) ->
        maps:get(srv, Game) =:= GameSrv end, State),
    [GameId] = maps:keys(Res),
    {reply, GameId, State};

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

% handle_call({add_game, GameSrv, Players}, _From, #state{games = Games} = State) ->
%     % erlang:monitor(process, PlayerSrv),
%     {reply, ok, State#state{games = maps:put(GameSrv, Players, Games)}};

% handle_call({add_player_to_waitlist, Socket}, _From, #state{waitlist = WaitList} = State) ->
%     {reply, ok, State#state{waitlist = lists:append(WaitList, [Socket])}};

% handle_call({del_player_from_waitlist, Socket}, _From, #state{waitlist = WaitList} = State) ->
%     {reply, ok, State#state{waitlist = lists:delete(Socket, WaitList)}};

% handle_call({get_game, GameSrv}, _From, #state{games = Games} = State) ->
%     {reply, maps:find(GameSrv, Games), State};

% handle_call(get_all_games, _From, #state{games = Games} = State) ->
%     {reply, maps:to_list(Games), State};

% handle_call({get_opponent, _Raiting}, _From, #state{waitlist = WaitList} = State) ->
%     % Match making logic -- maps:filter -> map with matching Player
%     case WaitList of
%         [] -> {reply, not_found, State};
%         [OpntSock] -> {reply, {ok, OpntSock}, State}
%     end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({del_game, GameSrv}, State) ->
    Game = maps:filter(fun(_Id, G) ->
        maps:get(srv, G) =:= GameSrv end, State),
    [GameId] = maps:keys(Game),
    {noreply, maps:remove(GameId, State)};

% handle_cast({del_game, GameSrv}, #state{games = Games} = State) ->
%     {noreply, State#state{games = maps:remove(GameSrv, Games)}};

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
get_id() ->
    integer_to_binary(rand:uniform(100)).

set_bet_coef(Rating) ->
    list_to_float(float_to_list(1.9 - Rating/100, [{decimals, 2}])).