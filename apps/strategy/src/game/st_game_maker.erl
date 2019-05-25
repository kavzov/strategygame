-module(st_game_maker).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
         connect/2, add_game/2, get_game/1, get_all_games/0, del_game/1, del_player_from_waitlist/1
]).

-record(state, {
    games :: map(),
    waitlist :: list()
}).


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


%%% gen_server API

init([]) ->
    lager:info("~p init", [?MODULE]),
    State = #state{
        games = #{},
        waitlist = []
    },
    {ok, State}.

handle_call({add_game, GameSrv, Players}, _From, #state{games = Games} = State) ->
    % erlang:monitor(process, PlayerSrv),
    {reply, ok, State#state{games = maps:put(GameSrv, Players, Games)}};

handle_call({add_player_to_waitlist, Socket}, _From, #state{waitlist = WaitList} = State) ->
    {reply, ok, State#state{waitlist = lists:append(WaitList, [Socket])}};

handle_call({del_player_from_waitlist, Socket}, _From, #state{waitlist = WaitList} = State) ->
    {reply, ok, State#state{waitlist = lists:delete(Socket, WaitList)}};

handle_call({get_game, GameSrv}, _From, #state{games = Games} = State) ->
    {reply, maps:find(GameSrv, Games), State};

handle_call(get_all_games, _From, #state{games = Games} = State) ->
    {reply, maps:to_list(Games), State};

handle_call({get_opponent, _Raiting}, _From, #state{waitlist = WaitList} = State) ->
    % Match making logic -- maps:filter -> map with matching Player
    case WaitList of
        [] -> {reply, not_found, State};
        [OpntSock] -> {reply, {ok, OpntSock}, State}
    end;

handle_call(_Request, _From, #state{} = State) ->
    {reply, ok, State}.


handle_cast({del_game, GameSrv}, #state{games = Games} = State) ->
    {noreply, State#state{games = maps:remove(GameSrv, Games)}};

handle_cast(_Request, #state{} = State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Info}, State) ->
    ets:delete(?MODULE, Pid),
    lager:info("Player down ~p ~p", [Pid, Info]),
    {noreply, State};

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.