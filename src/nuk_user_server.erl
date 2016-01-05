%%%-------------------------------------------------------------------
%% @doc `nuk_user_server' module
%%
%% When a user logs in, a new process is spawned that keeps the session of the
%% user. This is a `gen_server' that keeps the session state and provides
%% interface to its manipulation.
%%
%% For public API to accessing this functionality use the {@link nuk_users} and
%% {@link nuk_user_sessions} modules. Do not call the functions of this module
%% directly.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/0, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([login/2, logout/1, get_session/1]).

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

%% @doc Log in a user
%%
%% Attempts to log a user in given the username and password. Upon successful
%% login a new process is spawned and the string session identifier returned.
%% @end
-spec login(Username :: string(), Password :: string()) ->
    {ok, string()} |
    {error, wrong_password | user_not_found, string()}.
login(Username, Password) ->
    {ok, Pid} = supervisor:start_child(nuk_user_sup, []),
    gen_server:call(Pid, {login, Username, Password}).

%% @doc Log out a user
%%
%% Logs a user session out - i.e. stops the process that was keeping the logged
%% in user state.
%% @end
-spec logout(Pid :: pid()) -> ok.
logout(Pid) ->
    gen_server:call(Pid, {logout}).

%% @doc Get logged in user session
%%
%% Gets the {@link nuk_user_session:session()} data type for the given logged
%% in user session. Use {@link nuk_user_session} module to operate on the
%% returned data type.
%% @end
-spec get_session(Pid :: pid()) -> nuk_user_session:session().
get_session(Pid) ->
    gen_server:call(Pid, {get_session}).

%%====================================================================
%% Behavior callbacks
%%====================================================================

handle_call({login, Username, Password}, _From, #{session := Session} = State) ->
    case nuk_user_store_server:validate(Username, Password) of
        {ok, User} ->
            SessionNew = nuk_user_session:set_user(Session, User),
            StateNew = State#{session := SessionNew},
            %% TODO move this hack to user session storage
            {reply, {ok, pid_to_list(self())}, StateNew};
        {error, Reason, Extra} ->
            {stop, normal, {error, Reason, Extra}, State}
    end;
handle_call({logout}, _From, State) ->
    {stop, normal, ok, State};
handle_call({get_session}, _From, #{session := Session} = State) ->
    {reply, Session, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.
