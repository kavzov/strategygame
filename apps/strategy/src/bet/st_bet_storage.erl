-module(st_bet_storage).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_bet/5, handle_bets/2]).

-record(state, {}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

add_bet(BeterId, Bet, Coef, GameId, PlayerId) ->
    gen_server:call(?MODULE, {add_bet, BeterId, Bet, Coef, GameId, PlayerId}).

handle_bets(GameId, WinnerId) ->
    gen_server:call(?MODULE, {handle_bets, GameId, WinnerId}).

%%% gen_server API
init(no_args) ->
    lager:info("Bet storage ~p init", [?MODULE]),
    ets:new(?MODULE, [duplicate_bag, named_table]),
    {ok, #state{}}.

handle_call({add_bet, BeterId, Bet, Coef, GameId, PlayerId}, _From, State) ->
    ets:insert(?MODULE, {BeterId, Bet, Coef, GameId, PlayerId}),
    lager:info("Handle call for BET of ~p with coef ~p of player Id ~p on player Id ~p in the game Id ~p", [Bet, Coef, BeterId, PlayerId, GameId]),
    Reply = case db_update("UPDATE players SET wallet=wallet-$1 WHERE id=$2;", [Bet, BeterId]) of
        {ok, _Count} ->
            BetBin = float_to_binary(Bet, [{decimals, 4}]),
            CoefBin = float_to_binary(Coef, [{decimals, 4}]),
            TheWin = Bet * Coef,
            TheWinBin = float_to_binary(TheWin, [{decimals, 4}]),
            Players = maps:get(players, st_game_maker:get_game(GameId)),
            PlayerName = maps:get(name, maps:get(PlayerId, Players)),
            [P1Nm, P2Nm] = maps:fold(fun(_ID, Plr, Acc)-> Acc ++ [maps:get(name, Plr)] end, [], Players),
            lager:info("The bet accepted"),
            <<"Your bet accepted.\r\nBet details:\r\nBattle ", P1Nm/binary, " vs ", P2Nm/binary, ".\r\nYou bet $", BetBin/binary, " to ", PlayerName/binary, " with coefficient of ", CoefBin/binary, ".\r\nYou may win $", TheWinBin/binary, ".\r\nGood luck!\r\n">>;
        {error, not_updated} ->
            lager:warning("The bet not accepted"),
            <<"The bet not accepted\r\n">>;
        {error, Reason} ->
            lager:error("DB ERROR: ~p", [Reason]),
            <<"An error occurrs while making a bet(\r\n">>
        end,
    {reply, Reply, State};

handle_call({handle_bets, GameId, WinnerId}, _From, State) ->
    BetWinners = ets:match(?MODULE, {'$1', '$2', '$3', GameId, WinnerId}),
    lists:foreach(fun([BeterId, Bet, Coef]) ->
        {ok, {_, BeterName, _, _, _, _, _, _, BeterSock}} = st_player_storage:get_player_by_id(BeterId),
        case db_update("UPDATE players SET wallet=wallet+$1 WHERE id=$2;", [Bet*Coef, BeterId]) of
            {ok, _Count} ->
                lager:info("Player ~p (~p) won ~p.", [BeterName, BeterId, Bet*Coef]),
                gen_tcp:send(BeterSock, <<"Congrats! Your bet was won. The win added to your wallet.\r\n> ">>);   % TODO ?PROMT instead of '> '
            {error, not_updated} ->
                lager:warning("Handle bet db wasn't updated"),
                gen_tcp:send(BeterSock, <<"The bet wasn't handled\r\n">>);
            {error, Reason} ->
                lager:error("DB ERROR: ~p", [Reason]),
                <<"An database error occurrs while handling the bet(\r\n">>
            end
        end, BetWinners),
        ets:match_delete(?MODULE, {'$1', '$2', '$3', GameId, WinnerId}),
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


db_connect() ->
    DbName = "strategy",
    {ok, DbHost} = application:get_env(strategy, db_host),
    {ok, DbPort} = application:get_env(strategy, db_port),
    {ok, DbUser} = application:get_env(strategy, db_user),
    {ok, DbPassword} = application:get_env(strategy, db_password),
    {ok, DbTimeout} = application:get_env(strategy, db_timeout),
    epgsql:connect(DbHost, DbUser, DbPassword, #{database => DbName, port => DbPort, timeout => DbTimeout}).

db_update_query(C, Query, Params) ->
    case epgsql:equery(C, Query, Params) of
        {ok, 0} -> epgsql:close(C), {error, not_updated};
        {ok, Count} -> epgsql:close(C), {ok, Count};
        {error, Reason} -> epgsql:close(C), {error, Reason}                    
    end.

db_update(Query, Params) ->
    case db_connect() of
        {ok, C} -> db_update_query(C, Query, Params);
        {error, Reason} -> {error, Reason} 
    end.
