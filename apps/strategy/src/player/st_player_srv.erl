-module(st_player_srv).
-behavior(gen_server).

-export([start_link/1, auth/4, get_player_info/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("st_player.hrl").

-record(state, {
    socket :: pid(),
    player :: #player{},
    mode :: atom(),
    gamesrv :: boolean() | pid()
}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

auth(PlayerSrv, Id, Name, Rating) ->
    gen_server:call(PlayerSrv, {auth, Id, Name, Rating, PlayerSrv}).

get_player_info(PlayerSrv) ->
    gen_server:call(PlayerSrv, get_player_info).

stop(PlayerSrv) ->
    gen_server:call(PlayerSrv, stop).

%%% gen_server API

init(Socket) ->
    Player = #player{id = <<>>, name = <<>>, rating = <<>>},
    State = #state{
        socket = Socket,
        player = Player,
        mode = initial,   % [initial, server, battle]
        gamesrv = false
    },
    st_player_storage:add_player(Player, self(), Socket),
    lager:info("player created ~p", [State]),
    {ok, State}.

handle_call({auth, Id, Name, Rating, PlayerSrv}, _From, State = #state{socket = PlayerSock, player=Player}) ->
    Player2 = Player#player{id = Id, name = Name, rating = Rating},
    st_player_storage:update_player(Player2, PlayerSrv, PlayerSock),
    State2 = State#state{player = Player2, mode = server},
    {reply, ok, State2};

handle_call(get_player_info, _From, State) ->
    {reply, State, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, #player{} = State) ->
    {reply, ok, State}.

handle_cast(_Request, #player{} = State) ->
    {noreply, State}.

handle_info('SET_BATTLE_MODE', State) ->
    {noreply, State#state{mode = battle}};

handle_info('SET_BET', State) ->
    {noreply, State#state{mode = bet}};

handle_info({'START_BATTLE', GameSrv}, State) ->
    {noreply, State#state{gamesrv = GameSrv}};

handle_info('EXIT_BATTLE', State) ->
    {noreply, State#state{mode = server, gamesrv = false}};

handle_info({'DOWN', _Ref, process, Pid, Info}, State) ->
    lager:info("Player has down ~p ~p", [Pid, Info]),
    {noreply, State};

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.