%%%-------------------------------------------------------------------
%% @doc nuk game
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game).

%% API
-export([new/4]).
-export([get_name/1]).
-export([get_module/1]).
-export([get_min_players/1]).
-export([get_max_players/1]).

%% Types
-export_type([game/0]).

-opaque game() :: #{name => string(),
                    module => atom(),
                    min_players => integer(),
                    max_players => integer()}.

%%====================================================================
%% API
%%====================================================================

-spec new(GameName :: string(),
          Module :: atom(),
          MinPlayers :: integer(),
          MaxPlayers :: integer) -> game().
new(GameName, Module, MinPlayers, MaxPlayers) ->
    #{name => GameName,
      module => Module,
      min_players => MinPlayers,
      max_players => MaxPlayers}.

-spec get_name(Game :: game()) -> string().
get_name(#{name := Name}) ->
    Name.

-spec get_module(Game :: game()) -> atom().
get_module(#{module := Module}) ->
    Module.

-spec get_min_players(Game :: game()) -> integer().
get_min_players(#{min_players := MinPlayers}) ->
    MinPlayers.

-spec get_max_players(Game :: game()) -> integer().
get_max_players(#{max_players := MaxPlayers}) ->
    MaxPlayers.
