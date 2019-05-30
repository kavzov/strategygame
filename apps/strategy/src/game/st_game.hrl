server_msg() ->
	{ok, MinInt} = application:get_env(strategy, min_battle_field_length),
	{ok, MaxInt} = application:get_env(strategy, max_battle_field_length),
    [Min, Max] = lists:map(fun(Val) -> integer_to_binary(Val) end, [MinInt, MaxInt]),
	<<"
+======================================================+
| Command   | Action                                   |
+======================================================+
| list      | update list of players and games         |
+------------------------------------------------------+
| info      | current player info (rating, money, ...) |
+------------------------------------------------------+
| table     | players table with achievements          |
+------------------------------------------------------+
| play ID   | play game number ID (if there is)        |
+------------------------------------------------------+
| game W H  | start new game with dimension W x H      |
|           | W - width, H - height (min: ", Min/binary, ", max: ", Max/binary, ")  |
+------------------------------------------------------+
| bet G P $ | make a bet: GameID, PlayerID, $um        |
+------------------------------------------------------+
| exit      | leave the server                         |
+------------------------------------------------------+
\r\n">>.
