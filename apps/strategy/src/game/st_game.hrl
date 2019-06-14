server_msg() ->
	{ok, MinInt} = application:get_env(strategy, min_battle_field_length),
	{ok, MaxInt} = application:get_env(strategy, max_battle_field_length),
    [Min, Max] = lists:map(fun(Val) -> integer_to_binary(Val) end, [MinInt, MaxInt]),
	<<"
+======================================================+
| Command   | Action                                   |
+======================================================+
| me        | current player info (rating, money, ...) |
+------------------------------------------------------+
| games     | waiting players and active games         |
+------------------------------------------------------+
| players   | players table with achievements          |
+------------------------------------------------------+
| game W H  | start new game with dimension W x H      |
|           | W - width, H - height (min: ", Min/binary, ", max: ", Max/binary, ")  |
+------------------------------------------------------+
| play ID   | play game number ID (if there is)        |
+------------------------------------------------------+
| bet G P $ | make a bet: GameID, PlayerID, $um        |
+------------------------------------------------------+
| exit      | leave the server                         |
+------------------------------------------------------+
\r\n">>.