%%%-------------------------------------------------------------------
%% @doc `nuk_user_store_server' module
%%
%% This is an implementation of {@link nuk_user_storage} behavior. It is meant
%% for testing and proof of concept purposes only.
%%
%% This is a `gen_server' that's started by the {@link nuk_user_store_sup}
%% supervisor. It provides storage interface to registered users. For public
%% API the {@link nuk_users} module should be used which, in turn, will use the
%% appropriate storage backend.
%% @end
%%%-------------------------------------------------------------------


-module(nuk_user_store_server).

-behaviour(gen_server).
-behaviour(nuk_user_storage).

%% Supervision
-export([start_link/0, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([delete/1, get/1, list/0, put/1, validate/2]).

%%===================================================================
%% Supervision
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{data => #{}}}.

%%====================================================================
%% API
%%====================================================================

%% @doc Delete a user
%%
%% Deletes a user by username from the user data storage.
%% @end
-spec delete(Username :: string()) -> ok.
delete(Username) ->
    ok = gen_server:call(?MODULE, {delete, Username}).

%% @doc Get a user
%%
%% Retrieves a user by username from the user data storage.
%% @end
-spec get(Username :: string()) ->
    {ok, nuk_user:user()} |
    {error, user_not_found, string()}.
get(Username) ->
    gen_server:call(?MODULE, {get, Username}).

%% @doc List all users
%%
%% Lists all registered users in the user data storage.
%% @end
-spec list() -> [nuk_user:user()].
list() ->
    gen_server:call(?MODULE, {list}).

%% @doc Create or replace a user
%%
%% If a user by the username is already registered, replaces that registration;
%% otherwise creates a new registered user.
%% @end
-spec put(User :: nuk_user:user()) -> ok.
put(User) ->
    ok = gen_server:call(?MODULE, {put, User}).

%% @doc Validate user credentials
%%
%% Given a username and a password validate that the credentials are correct.
%% @end
-spec validate(Username :: string(), Password :: string()) ->
    {ok, nuk_user:user()} |
    {error, wrong_password | user_not_found, string()}.
validate(Username, Password) ->
    gen_server:call(?MODULE, {validate, Username, Password}).

%%====================================================================
%% Behavior callbacks
%%====================================================================

handle_call({delete, Username}, _From, #{data := Data} = State) ->
    NewData = delete_user(Username, Data),
    NewState = State#{data := NewData},
    {reply, ok, NewState};

handle_call({get, Username}, _From, #{data := Data} = State) ->
    {reply, lookup_user(Username, Data), State};

handle_call({list}, _From, #{data := Data} = State) ->
    {reply, list_users(Data), State};

handle_call({put, #{username := Username} = User}, _From, #{data := Data} = State) ->
    NewState = State#{data := Data#{Username => User}},
    {reply, ok, NewState};

handle_call({validate, Username, Password}, _From, #{data := Data} = State) ->
    {reply, validate_user_with_password(Username, Password, Data), State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Delete a user from user data storage
%% @private
%%
%% Deletes a user by username from the internal map.
%% @end
delete_user(Username, Data) ->
    maps:remove(Username, Data).

%% @doc List all users
%% @private
%%
%% Returns all users stored inside the map.
%% @end
list_users(Data) ->
    maps:values(Data).

%% @doc Look up a user
%% @private
%%
%% Search for a user by username in the map.
%% @end
lookup_user(Username, Data) ->
    try maps:get(Username, Data) of
        User -> {ok, User}
    catch
        error:{badkey, Username} -> {error, user_not_found, Username}
    end.

%% @doc Check password of the user
%% @private
%%
%% Checks password of the user.
%% @end
check_password(User, EnteredPassword) ->
    nuk_user:check_password(User, EnteredPassword).

%% @doc Check username and password validity
%% @private
%%
%% Validate that the given username and password combination is correct.
%% @end
validate_user_with_password(Username, Password, Data) ->
    case lookup_user(Username, Data) of
        {ok, User} ->
            case check_password(User, Password) of
                true -> {ok, User};
                _ -> {error, wrong_password, "Wrong password"}
            end;
        {error, Reason, Extra} -> {error, Reason, Extra}
    end.
