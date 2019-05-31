-module(st_game_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).


-define(INIT_MODE, initial).
-define(SERVER_MODE, server).
-define(BATTLE_MODE, battle).
-define(BET_MODE, bet).
-define(PROMPT, <<"> ">>).

-include("../game/st_game.hrl").

-record(state, {
	transport,
	socket,
	player_srv,
	game_srv,
	mode
}).


start_link(Ref, _Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
	{ok, Pid}.

init(Ref, Transport, _Opts = []) ->
	{ok, Socket} = ranch:handshake(Ref),
	lager:info("New connection."),
	{ok, PlayerSrv} = supervisor:start_child(st_player_sup, [Socket]),
	erlang:monitor(process, PlayerSrv),
    Res = gen_tcp:controlling_process(Socket, PlayerSrv),
	lager:info("Controlling process ~p", [Res]),
	State = #state{
		transport = Transport,
		socket = Socket,
		player_srv = PlayerSrv
	},
	Transport:send(Socket, get_init_msg()),
	loop(State).

-include("../player/st_player.hrl").

loop(#state{socket = Socket, transport = Transport, player_srv = PlayerSrv} = State) ->
	{ok, ClientDisconnectTimeoutMin} = application:get_env(strategy, client_disconnect_timeout),
	ClientDisconnectTimeoutMillisec = ClientDisconnectTimeoutMin * 60 * 1000,
	case Transport:recv(Socket, 0, ClientDisconnectTimeoutMillisec) of
		{ok, IOData} ->
			{state, Socket, #player{id = PlayerId}, Mode, GameSrv} = st_player_srv:get_player_info(PlayerSrv),
			CmdBin = iodata_init_handle(IOData),
			case {CmdBin, Mode} of
				{<<"auth ", Token/binary>>, ?INIT_MODE} ->
					case handle_auth(PlayerSrv, Token) of
						{ok, Reply} ->
							Transport:send(Socket, Reply),
							loop(State);
						{error, Reply} ->
							Transport:send(Socket, Reply),
							% st_player_srv:stop(PlayerSrv),
							% ok = Transport:close(Socket)
							loop(State);
						Reply ->
							lager:warning("REPLY: ~p", [Reply])
					end;
				{<<"exit">>, ?INIT_MODE} ->
					Transport:send(Socket, <<"Bye!\r\n">>),
					st_player_storage:del_player(Socket),
					st_player_srv:stop(PlayerSrv),
					ok = Transport:close(Socket);
				{_Unknown, ?INIT_MODE} ->
					Reply = <<"Unknown command\r\n", ?PROMPT/binary>>,
					Transport:send(Socket, Reply),
					loop(State);

				{<<"list">>, ?SERVER_MODE} ->
					Reply = handle_list(),
					Transport:send(Socket, Reply),
					loop(State);
				{<<"game ", WidthHeight/binary>>, ?SERVER_MODE} ->
					case handle_game(PlayerSrv, WidthHeight) of
						ok -> PlayerSrv ! 'SET_BATTLE_MODE';
						{error, badarg} -> Transport:send(Socket, <<"Wrong battle field dimension parameters.\r\n", ?PROMPT/binary>>);
						{error, badmatch} -> Transport:send(Socket, <<"Wrong count of battle field dimension parameters.\r\n", ?PROMPT/binary>>);
						{error, badsize} -> Transport:send(Socket, <<"Side size is out of range.\r\n", ?PROMPT/binary>>)
					end,
					loop(State);
				{<<"play ", GameId/binary>>, ?SERVER_MODE} ->
					case handle_play(GameId, PlayerSrv) of
						start_game ->
							% PlayerSrv ! {'SET_BATTLE_MODE', GameSrv},
							loop(State);
						game_not_found ->
							Transport:send(Socket, <<"There is no game number ", GameId/binary, "\r\n", ?PROMPT/binary>>),
							loop(State);
						bad_game_id ->
							Transport:send(Socket, <<"Bad game ID\r\n", ?PROMPT/binary>>),
							loop(State)
					end;
				{<<"game">>, ?SERVER_MODE} ->
					handle_game(Socket, PlayerSrv),
					PlayerSrv ! 'SET_BATTLE_MODE',
					loop(State);
				{<<"exit">>, ?SERVER_MODE} ->
					% TODO ? go to initial mode
					Transport:send(Socket, <<"Bye!\r\n">>),
					st_player_storage:del_player(Socket),
					ok = Transport:close(Socket);
				{<<"bet ", Bet/binary>>, ?SERVER_MODE} ->
					Reply = handle_bet(Bet, PlayerId),
					Transport:send(Socket, <<Reply/binary, ?PROMPT/binary>>),
					loop(State);
				{<<"i">>, ?SERVER_MODE} ->
					% TODO case Reply of {ok, .... {error, ...
					Reply = handle_info(PlayerId),
					Transport:send(Socket, Reply),
					loop(State);
				{<<"players">>, ?SERVER_MODE} ->
					Reply = handle_players(),
					Transport:send(Socket, Reply),
					loop(State);
				{<<"help">>, ?SERVER_MODE} ->
					Reply = handle_help(),
					Transport:send(Socket, Reply),
					loop(State);
				{_Unknown, ?SERVER_MODE} ->
					Reply = <<"Unknown command\r\n", ?PROMPT/binary>>,
					Transport:send(Socket, Reply),
					loop(State);

				{Cmd, ?BATTLE_MODE} ->
					if (GameSrv =:= false) -> ok;
					true -> st_game_srv:plr_cmd(GameSrv, Socket, string:lowercase(Cmd))
					end,
					loop(State)
			end;

		{error, Error} ->
			{state, PlayerSock, _Player, Mode, GameSrv} = st_player_srv:get_player_info(PlayerSrv),
			case Mode of
				?BATTLE_MODE -> st_game_srv:stop_by_player_interrupt(GameSrv, PlayerSock);
				_ -> ok
			end,
			st_player_srv:stop(PlayerSrv),
			st_player_storage:del_player(PlayerSock),
			lager:info("Close connection: '~p', stop player", [Error]),
			ok = Transport:close(PlayerSock)
	end.


handle_auth(PlayerSrv, Token0) ->
	Token = re:replace(Token0, <<"^ +">>, <<>>, [{return, binary}]),
	lager:info("Trying to login with token '~p'", [binary_to_list(Token)]),
	case db_select("WITH summary AS (SELECT *, row_number() OVER (ORDER BY rating DESC, name) AS position FROM players) SELECT id, name, wallet::NUMERIC, battles, won, rating, position FROM summary WHERE token=$1;", [Token]) of
		{ok, []} ->
			lager:warning("Authentication failed with Token: ~p", [Token]),
			{ok, <<"Authentication failed\r\n", ?PROMPT/binary>>};
		{ok, [{Id, Name, Wallet, Battles, Won, Rating, Position}]} ->
			case st_player_storage:get_player_by_id(Id) of
				{ok, _Player} ->
					lager:warning("Trying to secondary login of: ~p", [Name]),
					{error, <<"You are already logined\r\n", ?PROMPT/binary>>};
				error ->
					lager:info("Success authentication of ~p", [binary_to_list(Name)]),
					st_player_srv:auth(PlayerSrv, Id, Name, binary_to_float(Wallet), Battles, Won, Rating, Position),
					{ok, get_server_msg(integer_to_binary(Id), Name, Wallet, integer_to_binary(Battles), integer_to_binary(Won), integer_to_binary(Rating), integer_to_binary(Position))}
			end;
		{error, Reason} ->
			lager:error("DB SELECT error: ~p", [Reason]),
			{error, <<"Authentication error\r\n", ?PROMPT/binary>>}
	end.


handle_game(PlayerSrv, WidthHeight) ->
	try
		[Width, Height] = lists:map(
			fun(BinVal) -> side_size(binary_to_integer(BinVal)) end,
			binary:split(WidthHeight, [<<"\r">>, <<"\n">>, <<" ">>], [global, trim_all])
			),
		st_game_maker:add_new_game(PlayerSrv, [Width, Height])
	catch
		throw:badsize -> {error, badsize};
		error:badarg -> {error, badarg};
		error:{badmatch, _} -> {error, badmatch}
	end.

handle_play(GameIdBin, PlayerSrv) ->
	try
		GameId = binary_to_integer(GameIdBin),
		Games = st_game_maker:get_games(wait),
		check_game_id(GameId, Games),
		Game = st_game_maker:get_game(GameId),
		case maps:get(status, Game) of
			battle -> game_not_found;
			_ -> st_game_maker:play_game(GameId, PlayerSrv)
		end
	catch
		error:badarg -> bad_game_id;
		throw:badgameid -> game_not_found
	end.

handle_bet(BetStr, BeterId) ->
	Games = st_game_maker:get_games(battle),
	try
		[GameIdBin, PlayerIdBin, BetBin] = binary:split(BetStr, [<<" ">>, <<"\r">>, <<"\n">>], [global, trim_all]),
		GameId = binary_to_integer(GameIdBin),
		PlayerId = binary_to_integer(PlayerIdBin),
		check_game_id(GameId, Games),
		check_player_id(PlayerId, Games),
		Bet = check_bet(BeterId, BetBin),
		Game = st_game_maker:get_game(GameId),
		Player = maps:get(PlayerId, maps:get(players, Game)),
		Coef = maps:get(coef, Player),
		st_bet_storage:add_bet(BeterId, Bet, Coef, GameId, PlayerId)
	catch
		error:{badmatch, _} -> lager:error("Bad arguments"), {error, badmatch};
		throw:badgameid 	-> lager:error("Bad game ID"), {error, badgameid};
		throw:badplayerid 	-> lager:error("Bad player ID"), {error, badplayerid};
		throw:badbinnum		-> lager:error("Bet not a number"), {error, badbinnum};
		throw:negativebet	-> lager:error("Not positive Bet"), <<"You bet not positive amount\r\n", ?PROMPT/binary>>;
		throw:bigbet		-> lager:error("The player has not enough money to bet"), <<"You don't have enough money\r\n", ?PROMPT/binary>>
	end.

handle_list() ->
	Wait = handle_list(wait),
	Battles = handle_list(battle),
	<<Wait/binary, Battles/binary>>.

handle_list(battle) ->
	Games = st_game_maker:get_games(battle),
	HLine = <<"===================================\r\n">>,
	StartList = <<"\r\nRunning games available for bets:\r\n", HLine/binary>>,
	Reply = maps:fold(
		fun(GameId, Game, BinStr) ->
			GameIdBin = integer_to_binary(GameId),
			[Width, Height] = lists:map(fun(Val) -> integer_to_binary(Val) end, maps:get(size, Game)),
			BoardSize = <<GameIdBin/binary, ". Game ", Width/binary, " x ", Height/binary, "\r\n--------------\r\n">>,
			Players = maps:fold(
				fun(PlrId, Plr, Out) ->
					Id = integer_to_binary(PlrId),
					Name = maps:get(name, Plr),
					Rating = integer_to_binary(maps:get(rating, Plr)),
					Coef = float_to_binary(maps:get(coef, Plr), [{decimals, 2}]),
					<<Out/binary, Id/binary, ". ", Name/binary, ", rating: ", Rating/binary, ", Coef: ", Coef/binary, "\r\n">>
				end,
				<<>>, maps:get(players, Game)
			),
			<<BinStr/binary, BoardSize/binary, Players/binary, HLine/binary>>
		end,
		StartList,
		Games
	),
	case Reply of
		StartList -> <<"\r\nThere is no games for bets.\r\nRefresh the list later.\r\n", ?PROMPT/binary>>;
		_ -> Reply
	end;

handle_list(wait) ->
	Games = st_game_maker:get_games(wait),
	HLine = <<"--------------------\r\n">>,
	StartList = <<"\r\nWaiting players:\r\n====================\r\n">>,
	Reply = maps:fold(
		fun(GameId, Game, BinStr) ->
			GameIdBin = integer_to_binary(GameId),
			[Width, Height] = lists:map(fun(Val) -> integer_to_binary(Val) end, maps:get(size, Game)),
			BoardSize = <<Width/binary, " x ", Height/binary>>,
			[Player] = maps:values(maps:get(players, Game)),
			PlrName = maps:get(name, Player),
			Rating = maps:get(rating, Player),
			PlrRating = integer_to_binary(Rating),
			<<BinStr/binary, "Game ID: ",GameIdBin/binary, "\r\nPlayer: ", PlrName/binary, "\r\nRating: ", PlrRating/binary, "\r\nBoard size: ", BoardSize/binary, "\r\n", HLine/binary>>
		end,
		StartList,
		Games
	),
	case Reply of
		StartList -> <<"\r\nThere is no available players.\r\nYou may start a new game by 'game W H' command.\r\n", ?PROMPT/binary>>;
		_ -> Reply
	end.

handle_info(PlayerId) ->
	case db_select("WITH summary AS (SELECT *, row_number() OVER (ORDER BY rating DESC, name) AS position FROM players) SELECT id, name, wallet::NUMERIC, battles, won, rating, position FROM summary WHERE id=$1;", [PlayerId]) of
		{ok, []} ->
			lager:warning("Player with ID ~p trying to get info. FAILED.", [PlayerId]),
			<<"Get info fail\r\n", ?PROMPT/binary>>;
		{ok, [{Id, Name, Wallet, Battles, Won, Rating, Position}]} ->
			BinId = integer_to_binary(Id),
			BinBattles = integer_to_binary(Battles),
			BinWon = integer_to_binary(Won),
			BinRating = integer_to_binary(Rating),
			BinPosition = integer_to_binary(Position),
			<<"------------------
Name: ", Name/binary, "
ID: ", BinId/binary, "
Wallet: ", Wallet/binary, " $
Battles: ", BinBattles/binary, "
Won: ", BinWon/binary, "
Rating: ", BinRating/binary, "
Position: ", BinPosition/binary, "
------------------
", ?PROMPT/binary>>;
		{error, Reason} ->
			lager:error("DB SELECT error: ~p", [Reason]),
			<<"Error selecting info from database\r\n", ?PROMPT/binary>>
	end.

handle_players() ->
	case db_select("WITH summary AS (SELECT *, row_number() OVER (ORDER BY rating DESC, name) AS position FROM players) SELECT position, name, id, battles, won, rating FROM summary") of
		{ok, Players} ->
			PosCellWidth = 4, NameCellWidth = 15, IdCellWidth = 4, BattlesCellWidth = 9, WonCellWidth = 5, RatingCellWidth = 8,
			HPos = val_to_table(<<"N">>, PosCellWidth),
			HName = val_to_table(<<"Name">>, NameCellWidth),
			HId = val_to_table(<<"Id">>, IdCellWidth),
			HBattles = val_to_table(<<"Battles">>, BattlesCellWidth),
			HWon = val_to_table(<<"Won">>, WonCellWidth),
			HRating = val_to_table(<<"Rating">>, RatingCellWidth),
			HLine = bin_ch_n_times(<<"-">>, PosCellWidth + NameCellWidth + IdCellWidth + BattlesCellWidth + WonCellWidth + RatingCellWidth + 7),
			Reply = lists:foldl(
				fun(P, Res) ->
					{Position, Name, Id, Battles, Won, Rating} = P,
					BinPosition = val_to_table(integer_to_binary(Position), PosCellWidth),
					BinName = val_to_table(Name, NameCellWidth),
					BinId = val_to_table(integer_to_binary(Id), IdCellWidth),
					BinBattles = val_to_table(integer_to_binary(Battles), BattlesCellWidth),
					BinWon = val_to_table(integer_to_binary(Won), WonCellWidth),
					BinRating = val_to_table(integer_to_binary(Rating), RatingCellWidth),
					<<Res/binary, "|", BinPosition/binary, "|", BinName/binary, "|", BinId/binary, "|", BinBattles/binary, "|", BinWon/binary, "|", BinRating/binary, "|\r\n">>
				end,
				<<HLine/binary, "\r\n|", HPos/binary, "|", HName/binary, "|", HId/binary, "|", HBattles/binary, "|", HWon/binary, "|", HRating/binary, "|\r\n", HLine/binary, "\r\n">>,
				Players
			),
			<<Reply/binary, HLine/binary, "\r\n", ?PROMPT/binary>>;
		{error, Reason} ->
			lager:error("DB SELECT error: ~p", [Reason]),
			<<"Error selecting players info from database\r\n", ?PROMPT/binary>>
	end.

handle_help() ->
	ServerMsg = server_msg(),
	<<ServerMsg/binary, ?PROMPT/binary>>.

% Utils
iodata_init_handle(Data) ->
    % remove possible leading spaces
    Data1 = re:replace(Data, <<"^ +">>, <<>>, [{return, binary}]),
    % remove trailing line break
    re:replace(Data1, <<"\r\n$">>, <<>>, [{return, binary}]).

side_size(Size) ->
	{ok, MinFieldWidth} = application:get_env(strategy, min_battle_field_length),
	{ok, MaxFieldWidth} = application:get_env(strategy, max_battle_field_length),
	if (Size < MinFieldWidth) orelse (Size > MaxFieldWidth) -> throw(badsize);
	true -> Size
	end.

check_game_id(GameId, Games) ->
	case lists:member(GameId, maps:keys(Games)) of
		true -> ok;
		false -> throw(badgameid)
	end.

check_player_id(PlayerId, Games) ->
	PlayersIds = maps:fold(
		fun(_GameId, Game, Acc) -> Acc ++ maps:keys(maps:get(players, Game)) end,
		[], Games
	),
	case lists:member(PlayerId, PlayersIds) of
		true -> ok;
		false -> throw(badplayerid)
	end.

check_bet(PlayerId, BetBin) ->
	Bet = bin_to_num(BetBin),
	if Bet =< 0 -> throw(negativebet);
	true ->
		case db_select("SELECT wallet FROM players WHERE id=$1;", [PlayerId]) of
			{ok, []} ->
				{ok, <<"No wallet)\r\n">>};
			{ok, [{BinWallet}]} ->
				Wallet = bin_to_num(BinWallet),
				if Wallet < Bet -> throw(bigbet);
				true -> Bet
				end;
			{error, _Reason} ->
				{error, <<"Error\r\n">>}
		end
	end.

bin_to_num(BinNum) ->
	case re:run(BinNum, <<"^\\d+\\.\\d+$">>) of
		{match, _} -> binary_to_float(BinNum);
		nomatch	   ->
			case re:run(BinNum, <<"^\\d+$">>) of
				{match, _} -> binary_to_integer(BinNum) + 0.0;
				nomatch	   -> throw(badbinnum)
			end
	end.

val_to_table(Val, CellWidth) ->
	SP = <<" ">>,
	ValWidth = byte_size(Val),
	lists:foldl(
		fun(_, Str) -> <<Str/binary, SP/binary>> end,
		<<SP/binary, Val/binary>>,
		lists:seq(1, CellWidth - ValWidth - 1)
	).

bin_ch_n_times(Ch, N) ->
	lists:foldl(
		fun(_, Str) -> <<Str/binary, Ch/binary>> end,
		<<>>, lists:seq(1, N)
	).

get_init_msg() ->
	<<"
Hi! It's Polyana game.\r\nAuthenticate and play or go out)
+===========================+
| Command    | Action       |
+===========================+
| auth Token | authenticate |
+---------------------------+
| exit       | go out       |
+---------------------------+
\r\n", ?PROMPT/binary>>.

get_server_msg(_Id, Name, _Wallet, Battles, Won, Rating, Position) ->
	HLine = <<"\r\n-----------------------------------------------------\r\n">>,
	ServerMsg = server_msg(),
	WaitPlayers = handle_list(wait),
	BetBattles = handle_list(battle),
	<<HLine/binary, "Welcome, ", Name/binary, "!\r\nYou participated in ", Battles/binary, " battles. Won in ", Won/binary, ".\r\nYour rating - ", Rating/binary, ". Position in the championship - ", Position/binary, ".", HLine/binary, WaitPlayers/binary, BetBattles/binary, ServerMsg/binary, ?PROMPT/binary>>.


% TODO pass it (and from st_bet_storage) to include/db.hrl
db_connect() ->
    DbName = "strategy",
    {ok, DbHost} = application:get_env(strategy, db_host),
    {ok, DbPort} = application:get_env(strategy, db_port),
    {ok, DbUser} = application:get_env(strategy, db_user),
    {ok, DbPassword} = application:get_env(strategy, db_password),
    {ok, DbTimeout} = application:get_env(strategy, db_timeout),
    epgsql:connect(DbHost, DbUser, DbPassword, #{database => DbName, port => DbPort, timeout => DbTimeout}).

db_select_query(C, Query, Params) ->
    case epgsql:equery(C, Query, Params) of
        {ok, _Columns, Rows} -> epgsql:close(C), {ok, Rows};
        {error, Reason} -> epgsql:close(C), {error, Reason}                    
    end.

db_select(Query) -> db_select(Query, []).
db_select(Query, Params) ->
    case db_connect() of
        {ok, C} -> db_select_query(C, Query, Params);
        {error, Reason} -> {error, Reason} 
    end.
