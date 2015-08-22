%%%-------------------------------------------------------------------
%% @doc nuk user server
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_server).

-behaviour(gen_server).

-export([start_link/0, init/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([login/3]).

%%%%% Supervision functions %%%%%

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, 0}.

%%%%% User functions %%%%%

%% login.
login(Pid, Username, Password) ->
    {ok, SessionId} = gen_server:call(Pid, {login, Username, Password}),
    SessionId.

%%%%% Server callbacks %%%%%
handle_call({login, _Username, _Password}, _From, State) ->
    SessionId = list_to_binary(pid_to_list(self())),
    {reply, {ok, SessionId}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.
