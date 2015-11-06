%%%-------------------------------------------------------------------
%% @doc nuk game server
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/0, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([create/2, start/2, finish/1]).

%%====================================================================
%% Supervision
%%====================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, 0}.

%%====================================================================
%% API
%%====================================================================

%% create a new game session
%% TODO move to init/1 - there's no need for a separate function
-spec create(UserSessionId :: string(), GameName :: string()) ->
    {ok, GameSessionId :: string()} |
    {error, invalid_user_session, Extra :: string()} |
    {error, invalid_game_name, Extra :: string()}.
create(UserSessionId, GameName) ->
    {ok, Pid} = supervisor:start_child(nuk_game_sup, []),
    gen_server:call(Pid, {initialize, UserSessionId, GameName}).

%% start a game.
-spec start(Pid :: pid(), UserSessionId :: string()) ->
    ok |
    {error, invalid_game_session, Extra :: string()} |
    {error, invalid_user_session, Extra :: string()} |
    {error, game_engine_error, Extra :: string()}.
start(Pid, UserSessionId) ->
    ok = gen_server:call(Pid, {start, UserSessionId}).

%% end a game
%% TODO figure out finish
-spec finish(Pid :: pid()) -> ok.
finish(Pid) ->
    ok = gen_server:call(Pid, {finish}).

%%====================================================================
%% Behavior callbacks
%%====================================================================

handle_call({initialize, _UserSessionId, _GameName}, _From, State) ->
    %% TODO look up game engine based on name
    %% TODO invoke game engine
    {reply, ok, State};
handle_call({start, _UserSessionId}, _From, State) ->
    %% TODO invoke game engine
    {reply, ok, State};
handle_call({finish}, _From, State) ->
    %% TODO invoke game engine
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.
