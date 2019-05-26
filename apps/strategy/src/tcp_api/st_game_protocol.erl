-module(st_game_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).


-define(INIT_MODE, initial).
-define(SERVER_MODE, server).
-define(BATTLE_MODE, battle).
-define(BET_MODE, bet).

-define(MIN_FIELD_WIDTH, 3).
-define(MAX_FIELD_WIDTH, 10).

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

loop(#state{socket = Socket, transport = Transport, player_srv = PlayerSrv} = State) ->
	{ok, ClientDisconnectTimeoutMin} = application:get_env(strategy, client_disconnect_timeout),
	ClientDisconnectTimeoutMillisec = ClientDisconnectTimeoutMin * 60 * 1000,
	case Transport:recv(Socket, 0, ClientDisconnectTimeoutMillisec) of
		{ok, IOData} ->
			{state, Socket, _Player, Mode, GameSrv} = st_player_srv:get_player_info(PlayerSrv),
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
					Reply = <<"Unknown command\r\n">>,
					Transport:send(Socket, Reply),
					loop(State);

				{<<"list">>, ?SERVER_MODE} ->
					Reply = handle_list(wait),
					Transport:send(Socket, Reply),
					loop(State);
				{<<"game ", WidthHeight/binary>>, ?SERVER_MODE} ->
					case handle_game(PlayerSrv, WidthHeight) of
						ok -> PlayerSrv ! 'SET_BATTLE_MODE';
						{error, badarg} -> Transport:send(Socket, <<"Wrong battle field dimension paramenters.\r\n">>);
						{error, badmatch} -> Transport:send(Socket, <<"Wrong count of battle field dimension paramenters.\r\n">>);
						{error, badsize} -> Transport:send(Socket, <<"Side size is out of range.\r\n">>)
					end,
					loop(State);
				{<<"play ", GameId/binary>>, ?SERVER_MODE} ->
					case handle_play(GameId, PlayerSrv) of
						start_game ->
							PlayerSrv ! 'SET_BATTLE_MODE',
							loop(State);
						game_not_found ->
							Transport:send(Socket, <<"There is no game number ", GameId/binary>>),
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
				{<<"bet ", CurrencyId/binary>>, ?SERVER_MODE} ->
					% TODO handle Currency Id (1 or 2)
					Reply = handle_bet(),
					Transport:send(Socket, Reply),
					PlayerSrv ! 'SET_BET_MODE',
					loop(State);
				{_Unknown, ?SERVER_MODE} ->
					Reply = <<"Unknown command\r\n">>,
					Transport:send(Socket, Reply),
					loop(State);

				{Cmd, ?BATTLE_MODE} ->
					st_game_srv:plr_cmd(GameSrv, Socket, string:lowercase(Cmd)),
					loop(State);

				{<<"list">>, ?BET_MODE} ->
					Reply = handle_list(battle),
					Transport:send(Socket, Reply),
					loop(State);
				{<<"bet ", >>}
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
	lager:info("Trying to login with token '~p'", [Token]),
	% TODO make function for the dbquery
    case epgsql:connect("localhost", "strategy", "strategy", #{database => "strategy", port => 15432, timeout => 5000}) of
        {ok, C} ->
            case epgsql:equery(C, "SELECT id, nickname, winrate FROM players WHERE token=$1;", [Token]) of
                {ok, _, []} ->
					ok = epgsql:close(C),
					{ok, <<"Authentication failed\r\n">>};
                {ok, _, [{Id, Name, Rating}]} ->
					ok = epgsql:close(C),
					st_player_srv:auth(PlayerSrv, Id, Name, Rating),
					{ok, get_server_msg(Name, integer_to_binary(Rating))};
                {error, _Reason} ->
					ok = epgsql:close(C),
					{error, <<"Authentication error\r\n">>}
            end;
        {error, Reason} -> lager:error("NO CONNECTION TO DB~n~p", [Reason])
    end.

handle_game(PlayerSrv, WidthHeight) ->
	try
		[Width, Height] = lists:map(
			fun(BinVal) -> side_size(binary_to_integer(BinVal)) end,
			binary:split(WidthHeight, [<<"\r">>, <<"\n">>, <<" ">>], [global, trim_all])
			),
		application:set_env([{strategy, [{battle_field_width, Width}, {battle_field_height, Height}]}]),
		st_game_maker:add_new_game(PlayerSrv,[Width, Height])
	catch
		throw:badsize -> {error, badsize};
		error:badarg -> {error, badarg};
		error:{badmatch, _} -> {error, badmatch}
	end.

handle_play(GameId, PlayerSrv) ->
	st_game_maker:play_game(GameId, PlayerSrv).


handle_bet() ->
	Games = st_game_maker:get_games(battle),
	HLine = <<"===================================\r\n">>,
	StartList = <<"\r\nRunning games:\r\n", HLine/binary>>,
	Reply = maps:fold(
		fun(GameId, Game, BinStr) ->
			[Width, Height] = lists:map(fun(Val) -> integer_to_binary(Val) end, maps:get(size, Game)),
			BoardSize = <<GameId/binary, ". Game ", Width/binary, " x ", Height/binary, "\r\n--------------\r\n">>,
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
		StartList -> <<"\r\nThere is no games for bets.\r\nRefresh the list later.\r\n">>;
		_ -> Reply
	end.

% handle_game(Socket, PlayerSrv) ->
% 	st_game_maker:connect(Socket, PlayerSrv).

handle_list(wait) ->
	Games = st_game_maker:get_games(wait),
	HLine = <<"--------------------\r\n">>,
	StartList = <<"\r\nWaiting players:\r\n", HLine/binary>>,
	Reply = maps:fold(
		fun(GameId, Game, BinStr) ->
			[Width, Height] = lists:map(fun(Val) -> integer_to_binary(Val) end, maps:get(size, Game)),
			BoardSize = <<Width/binary, " x ", Height/binary>>,
			[Player] = maps:values(maps:get(players, Game)),
			PlrName = maps:get(name, Player),
			Rating = maps:get(rating, Player),
			PlrRating = integer_to_binary(Rating),
			<<BinStr/binary, "Game ID: ",GameId/binary, "\r\nPlayer: ", PlrName/binary, "\r\nRating: ", PlrRating/binary, "\r\nBoard size: ", BoardSize/binary, "\r\n", HLine/binary>>
		end,
		StartList,
		Games
	),
	case Reply of
		StartList -> <<"\r\nThere is no available players.\r\nYou may start a new game by 'game W H' command.\r\n">>;
		_ -> Reply
	end.


iodata_init_handle(Data) ->
    % remove possible leading spacesauth 6BC4D6927DE71BA05CAA5CBF915D3FF2667BB2C847854B0676C46AACE50EC120

    Data1 = re:replace(Data, <<"^ +">>, <<>>, [{return, binary}]),
    % remove trailing line break
    re:replace(Data1, <<"\r\n$">>, <<>>, [{return, binary}]).

side_size(Size) ->
	if (Size < ?MIN_FIELD_WIDTH) orelse (Size > ?MAX_FIELD_WIDTH) -> throw(badsize);
	true -> Size
	end.



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
\r\n">>.

get_server_msg(Name, Rating) ->
	ServerMsg = server_msg(),
	WaitPlayers = handle_list(wait),
	<<"Welcome, ", Name/binary, "!\r\nYour rating - ", Rating/binary, ".\r\n", WaitPlayers/binary, "\r\n", ServerMsg/binary>>.
