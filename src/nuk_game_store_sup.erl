%%%-------------------------------------------------------------------
%% @doc `nuk_game_store_sup' module
%%
%% This supervisor is started by {@link nuk_sup} top level supervisor. It
%% supervises {@link nuk_game_store_server}.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_store_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args},
        permanent, 5000, Type, [Module]}).

%%====================================================================
%% Supervision
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 0, 1}, children()} }.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Get children specs
%% @private
%%
%% A convenience function to return all children specs.
%% @end
children() ->
    UserStore = ?CHILD(nuk_game_store_server, nuk_game_store_server, [], worker),
    [UserStore].
