%%%-------------------------------------------------------------------
%% @doc strategy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(strategy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications =
        [
            #{
                id => st_player_storage,
                start => {st_player_storage, start_link, []},
                restart => permanent,
                shutdown => 2000,
                type => worker,
                modules => [st_player_storage]
            },
            #{
                id => st_game_maker,
                start => {st_game_maker, start_link, []},
                restart => permanent,
                shutdown => 2000,
                type => worker,
                modules => [st_game_maker]
            },
            #{
                id => st_player_sup,
                start => {st_player_sup, start_link, []},
                restart => permanent,
                shutdown => 2000,
                type => supervisor,
                modules => [st_player_sup]
            },
            #{
                id => st_game_sup,
                start => {st_game_sup, start_link, []},
                restart => permanent,
                shutdown => 2000,
                type => supervisor,
                modules => [st_game_sup]
            }
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.

%%====================================================================
%% Internal functions
%%====================================================================
