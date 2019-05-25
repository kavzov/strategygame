-module(strategy_worker).
-behavior(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    user_id,
    user_name
        }).


%%% module API

start_link(UserID, UserName) ->
    gen_server:start_link(?MODULE, {UserID, UserName}, []).


%%% gen_server API

init({UserID, UserName}) ->
    State = #state{user_id = UserID, user_name = UserName},
    lager:info("init ~p ~p", [self(), State]),
    lager:warning("Lager WARNING"),
    lager:error("Lager Error"),
    io:format("init ~p ~p~n", [self(), State]),
    {ok, State}.


handle_call(_Request, _From, #state{} = State) ->
    {reply, ok, State}.


handle_cast(_Request, #state{} = State) ->
    {reply, ok, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
