-module(st_bet_storage).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_bet/6]).

-record(state, {}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

add_bet(BeterId, GameId, PlayerId, Coef, Amount, Curr) ->
    gen_server:call(?MODULE, {add_bet, BeterId, GameId, PlayerId, Coef, Amount, Curr}).

%%% gen_server API
init(no_args) ->
    lager:info("Bet storage ~p init", [?MODULE]),
    ets:new(?MODULE, [bag, named_table]),
    {ok, #state{}}.

handle_call({add_bet, BeterId, GameId, PlayerId, Coef, Amount, Curr}, _From, State) ->
    ets:insert(?MODULE, {BeterId, GameId, PlayerId, Coef, Amount, Curr}),
    lager:info("Handle call for bet player Id ~p on player Id in the game Id", [BeterId, PlayerId, GameId]),
    {reply, ok, State};

handle_call(_Request, _From, #state{} = State) ->
    {reply, ok, State}.

handle_cast(_Request, #state{} = State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.