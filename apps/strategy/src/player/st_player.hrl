-record(player, {
    id :: pos_integer(),
    name :: binary(),
    wallet :: binary(),
    battles :: pos_integer(),
    won :: integer(),
    rating :: pos_integer(),
    position :: pos_integer()
}).
