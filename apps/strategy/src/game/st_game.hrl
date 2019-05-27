server_msg() ->
	{ok, MinInt} = application:get_env(strategy, min_battle_field_length),
	{ok, MaxInt} = application:get_env(strategy, max_battle_field_length),
    [Min, Max] = lists:map(fun(Val) -> integer_to_binary(Val) end, [MinInt, MaxInt]),
	<<"
+=====================================================+
| Command  | Action                                   |
+=====================================================+
| list     | update list waiting players              |
+-----------------------------------------------------+
| play ID  | play game number ID (if there is)        |
+-----------------------------------------------------+
| game W H | start new game with dimension W x H      |
|          | W - width, H - height (min: ", Min/binary, ", max: ", Max/binary, ")  |
+-----------------------------------------------------+
| bet      | make a bet                               |
+-----------------------------------------------------+
| exit     | leave the server                         |
+-----------------------------------------------------+
\r\n">>.
