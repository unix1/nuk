%%%-------------------------------------------------------------------
%% @doc nuk user server
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/0, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([login/3]).

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

login(Pid, Username, Password) ->
    {ok, SessionId} = gen_server:call(Pid, {login, Username, Password}),
    SessionId.

%%====================================================================
%% Behavior callbacks
%%====================================================================

handle_call({login, _Username, _Password}, _From, State) ->
    SessionId = list_to_binary(pid_to_list(self())),
    {reply, {ok, SessionId}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.
