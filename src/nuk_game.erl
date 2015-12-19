%%%-------------------------------------------------------------------
%% @doc `nuk_game' module
%%
%% This module is used to operate on the {@link nuk_game:game()} data type.
%% This data type is used during game registration in nuk - i.e. when calling
%% {@link nuk_games:register/1}, {@link nuk_games:get/1},
%% {@link nuk_games:list/0}.
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
%% Data type used to register games in nuk.  Use functions in this module to
%% operate on this data type.

%%====================================================================
%% API
%%====================================================================

%% @doc Create a new {@link nuk_game:game()} data type
%%
%% `GameName' is a string used to identify the game during registration.
%% `Module' is the name of the module implementing the
%% {@link nuk_game_engine} behavior. `MinPlayers' and `MaxPlayers' are integers
%% used for game registration. They are used by nuk to validate correct
%% number of players before starting a game.
%% @end
-spec new(GameName :: string(),
          Module :: atom(),
          MinPlayers :: integer(),
          MaxPlayers :: integer) -> game().
new(GameName, Module, MinPlayers, MaxPlayers) ->
    #{name => GameName,
      module => Module,
      min_players => MinPlayers,
      max_players => MaxPlayers}.

%% @doc Get game name
%%
%% Extract game name from {@link nuk_game:game()} data type.
%% @end
-spec get_name(Game :: game()) -> string().
get_name(#{name := Name}) ->
    Name.

%% @doc Get module name
%%
%% Extract module name from {@link nuk_game:game()} data type.
-spec get_module(Game :: game()) -> atom().
get_module(#{module := Module}) ->
    Module.

%% @doc Get minimum number of players
%%
%% Extract minimum number of players from {@link nuk_game:game()} data type.
%% @end
-spec get_min_players(Game :: game()) -> integer().
get_min_players(#{min_players := MinPlayers}) ->
    MinPlayers.

%% @doc Get maximum number of players
%%
%% Extract maximum number of players from {@link nuk_game:game()} data type.
%% @end
-spec get_max_players(Game :: game()) -> integer().
get_max_players(#{max_players := MaxPlayers}) ->
    MaxPlayers.
