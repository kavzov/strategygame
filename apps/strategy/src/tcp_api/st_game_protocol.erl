-module(st_game_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).

-define(INIT_MODE, initial).
-define(SERVER_MODE, server).
-define(BATTLE_MODE, battle).

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

				{<<"players">>, ?SERVER_MODE} ->
					Reply = handle_get_players(),
					Transport:send(Socket, Reply),
					loop(State);
				{<<"game">>, ?SERVER_MODE} ->
					handle_game(Socket, PlayerSrv),
					PlayerSrv ! 'SET_BATTLE_MODE',
					loop(State);
				{<<"exit">>, ?SERVER_MODE} ->
					% TODO ? go to initial mode
					Transport:send(Socket, <<"Bye!\r\n">>),
					st_player_storage:del_player(Socket),
					ok = Transport:close(Socket);
				{_Unknown, ?SERVER_MODE} ->
					Reply = <<"Unknown command\r\n">>,
					Transport:send(Socket, Reply),
					loop(State);

				{Cmd, ?BATTLE_MODE} ->
					st_game_srv:plr_cmd(GameSrv, Socket, string:lowercase(Cmd)),
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


handle_game(Socket, PlayerSrv) ->
	st_game_maker:connect(Socket, PlayerSrv).

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

handle_get_players() -> <<"[Vasya, Petya]\n">>.


iodata_init_handle(Data) ->
    % remove possible leading spaces
    Data1 = re:replace(Data, <<"^ +">>, <<>>, [{return, binary}]),
    % remove trailing line break
    re:replace(Data1, <<"\r\n$">>, <<>>, [{return, binary}]).

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
	<<"
Welcome, ", Name/binary, "!\r\nYour rating - ", Rating/binary, ".
+=====================================================+
| Command  | Action                                   |
+=====================================================+
| list     | view list of games waiting for opponents |
+-----------------------------------------------------+
| play N   | play game number N (from the wait list)  |
+-----------------------------------------------------+
| game W H | start new game with dimension W x H      |
|          | W - width, H - height of battle field    |
+-----------------------------------------------------+
| exit     | exit                                     |
+-----------------------------------------------------+
\r\n">>.
