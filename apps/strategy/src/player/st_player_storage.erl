-module(st_player_storage).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, add_player/3, update_player/3, del_player/1,
        all_players/0, get_player/1, get_player_by_srv/1
]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("st_player.hrl").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

add_player(Player, PlayerSrv, PlayerSock) ->
    % Temp real players emulation
    % PlayersTmp = [
    %     {tmp_player, <<"123">>, <<"Bill">>, <<"42">>},
    %     {tmp_player, <<"15">>, <<"Helen">>, <<"22">>}
    % ],
    % TmpPlayer = case length(ets:match(?MODULE, {'_', '_', '_', '_', '_'})) of
    %     Len -> lists:nth(Len+1, PlayersTmp)
    % end,
    gen_server:call(?MODULE, {add_player, Player, PlayerSrv, PlayerSock}).

update_player(Player, PlayerSrv, PlayerSock) ->
    gen_server:call(?MODULE, {update_player, Player, PlayerSrv, PlayerSock}).

del_player(Socket) ->
    gen_server:call(?MODULE, {del_player, Socket}).

%% Temp
all_players() ->
    gen_server:call(?MODULE, {all_players}).

get_player(Socket) ->
    gen_server:call(?MODULE, {get_player, Socket}).

get_player_by_srv(PlayerSrv) ->
    gen_server:call(?MODULE, {get_player, {PlayerSrv}}).
%% Temp 


%%% gen_server API

-record(state, {}).

init(no_args) ->
    lager:info("~p init", [?MODULE]),
    ets:new(?MODULE, [named_table, {keypos, 5}]),
    {ok, #state{}}.

handle_call({add_player, {player, Id, Name, Rating}, PlayerSrv, PlayerSock}, _From, State) ->
    ets:insert(?MODULE, {Id, Name, Rating, PlayerSrv, PlayerSock}),
    lager:info("Handle call for add a player to ets. PlayerSrv: ~p, Socket: ~p", [PlayerSrv, PlayerSock]),
    {reply, ok, State};

handle_call({update_player, {player, Id, Name, Rating}, PlayerSrv, PlayerSock}, _From, State) ->
    ets:insert(?MODULE, {Id, Name, Rating, PlayerSrv, PlayerSock}),
    lager:info("Handle call for UPDATE the player ~p in ets. PlayerSrv: ~p, Socket: ~p", [Name, PlayerSrv, PlayerSock]),
    {reply, ok, State};

handle_call({del_player, Socket}, _From, State) ->
    ets:delete(?MODULE, Socket),
    {reply, ok, State};

%% Temp
handle_call({all_players}, _From, State) ->
    Reply = ets:match_object(?MODULE, {'$1', '$2', '$3', '$4', '$5'}),
    {reply, Reply, State};

handle_call({get_player, Socket}, _From, State) ->
    [Reply] = ets:lookup(?MODULE, Socket),
    {reply, Reply, State};

handle_call({get_player_by_srv, PlayerSrv}, _From, State) ->
    [Player] = ets:match_object(?MODULE, {'$1', '$2', '$3', PlayerSrv, '$5'}),
    {reply, Player, State};
%%Temp


handle_call(_Request, _From, #state{} = State) ->
    {reply, ok, State}.

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