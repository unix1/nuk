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
    {ok, #{session => nuk_user_session:new()}}.

%%====================================================================
%% API
%%====================================================================

-spec login(Pid :: pid(), Username :: string(), Password :: string()) ->
    {ok, string()} |
    {error, atom(), string()}.
login(Pid, Username, Password) ->
    gen_server:call(Pid, {login, Username, Password}).

%%====================================================================
%% Behavior callbacks
%%====================================================================

handle_call({login, Username, Password}, _From, #{session := Session} = State) ->
    case nuk_user_store_server:validate(Username, Password) of
        {ok, User} ->
            SessionNew = nuk_user_session:set_user(Session, User),
            StateNew = State#{session := SessionNew},
            {reply, {ok, pid_to_list(self())}, StateNew};
        {error, Reason, Extra} ->
            {stop, normal, {error, Reason, Extra}, State}
    end.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.
