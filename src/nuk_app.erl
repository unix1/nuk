%%%-------------------------------------------------------------------
%% @doc `nuk_app' module
%%
%% This starts the nuk application.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_app).

-behaviour(application).

%% API
-export([get_storage_module/1]).

%% Supervision
-export([start/2, stop/1]).

%%====================================================================
%% Supervision
%%====================================================================

start(_StartType, _StartArgs) ->
    nuk_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% API
%%====================================================================

%% @doc Get storage module
%%
%% Returns the storage module associated with the specified storage type. If
%% none specified, nuk default is returned.
%% @end
-spec get_storage_module(Type :: atom()) -> atom().
get_storage_module(Type) ->
    case application:get_env(Type) of
        undefined ->
            get_storage_module_default(Type);
        {ok, Module} when is_atom(Module) ->
            Module
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Get default storage module
%% @private
%%
%% Returns the default storage module defined for specified type of storage.
%% @end
-spec get_storage_module_default(Type :: atom()) -> atom().
get_storage_module_default(users) -> nuk_user_store_server;
get_storage_module_default(user_sessions) -> nuk_user_session_store_server;
get_storage_module_default(games) -> nuk_game_store_server;
get_storage_module_default(game_sessions) -> nuk_game_session_store_server.
