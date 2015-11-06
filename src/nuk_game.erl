%%%-------------------------------------------------------------------
%% @doc nuk game
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game).

%% API
-export([new/2, get_name/1, get_module/1]).
-export_type([game/0]).

-opaque game() :: #{name => string(), module => atom()}.

%%====================================================================
%% API
%%====================================================================

-spec new(GameName :: string(), Module :: atom()) -> game().
new(GameName, Module) ->
    #{name => GameName, module => Module}.

-spec get_name(game()) -> string().
get_name(#{name := Name}) ->
    Name.

-spec get_module(game()) -> atom().
get_module(#{module := Module}) ->
    Module.
