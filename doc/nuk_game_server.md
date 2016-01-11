

# Module nuk_game_server #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_game_server` module.

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="description"></a>

## Description ##

When a nuk game session is created, a new process is spawned that keeps the
general nuk state and the arbitrary game engine state. It is also
responsible for processing significant events during the lifetime of the
game, triggering appropriate [`nuk_game_engine`](nuk_game_engine.md) callbacks, and
processing their results. This is the `gen_server` module that accomplishes
the above.

For public API to accessing this functionality use the [`nuk_games`](nuk_games.md)
module. Do not call the functions of this module directly.
<a name="types"></a>

## Data Types ##




### <a name="type-state">state()</a> ###


<pre><code>
state() = #{session =&gt; <a href="nuk_game_session.md#type-session">nuk_game_session:session()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#create-3">create/3</a></td><td>Create a new game session.</td></tr><tr><td valign="top"><a href="#get_session-1">get_session/1</a></td><td>Get a snapshot of game session.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td>Join a user to the game session.</td></tr><tr><td valign="top"><a href="#leave-2">leave/2</a></td><td>Remove a user from the game session.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>Start a game.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#turn-3">turn/3</a></td><td>Process player's turn.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVersion, State, Extra) -> any()`

<a name="create-3"></a>

### create/3 ###

<pre><code>
create(User::<a href="nuk_user.md#type-user">nuk_user:user()</a>, GameName::string(), Options::list()) -&gt; {ok, GameSessionId::string()} | {error, invalid_game_name, Extra::string()} | {error, invalid_options, Extra::string()}
</code></pre>
<br />

Create a new game session

Given a user, name of the game and list of options, creates a new game
session. This function does 2 things:
- starts a new `nuk_game_server` child via [`nuk_game_sup`](nuk_game_sup.md)
- sends itself an `initialize` message to invoke the game engine to
obtain the initial game state

Calling this function triggers the [`nuk_game_engine:initialize/2`](nuk_game_engine.md#initialize-2)
callback.

For public API [`nuk_games:create/2`](nuk_games.md#create-2) or [`nuk_games:create/3`](nuk_games.md#create-3) must
be used.

<a name="get_session-1"></a>

### get_session/1 ###

<pre><code>
get_session(Pid::pid()) -&gt; <a href="nuk_game_session.md#type-session">nuk_game_session:session()</a>
</code></pre>
<br />

Get a snapshot of game session

This is a function powering the implementation of
[`nuk_games:get_game_session/1`](nuk_games.md#get_game_session-1). It returns the current snapshot of
the general nuk game session state and arbitrary game engine state.

For public API [`nuk_games:get_game_session/1`](nuk_games.md#get_game_session-1) must be used.

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(X1, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Msg, State) -> any()`

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(GameName::[GameName::string()]) -&gt; {ok, State::<a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="join-2"></a>

### join/2 ###

<pre><code>
join(Pid::pid(), User::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; ok | {error, user_already_joined, Extra::string()} | {error, max_users_reached, Extra::string()}
</code></pre>
<br />

Join a user to the game session

This is a function powering the implementation of [`nuk_games:join/2`](nuk_games.md#join-2).
It adds the given user to the current game session after validating that
- user hasn't already joined the game
- maximum number of users allowed by the game wouldn't be exceeded

Calling this function triggers the [`nuk_game_engine:player_join/3`](nuk_game_engine.md#player_join-3)
callback.

For public API [`nuk_games:join/2`](nuk_games.md#join-2) must be used.

<a name="leave-2"></a>

### leave/2 ###

<pre><code>
leave(Pid::pid(), User::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; ok | {error, user_not_in_game, Extra::string()}
</code></pre>
<br />

Remove a user from the game session

This is a function powering the implementation of [`nuk_games:leave/2`](nuk_games.md#leave-2).
It removes a given user from the current game session after validating that
user is in the current game session.

Calling this function triggers the [`nuk_game_engine:player_leave/3`](nuk_game_engine.md#player_leave-3)
callback.

For public API [`nuk_games:leave/2`](nuk_games.md#leave-2) must be used.

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(Pid::pid(), User::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; ok | {error, user_not_in_game, Extra::string()}
</code></pre>
<br />

Start a game

This is a function powering the implementation of [`nuk_games:start/2`](nuk_games.md#start-2).
It starts the current game session after validating that the user requesting
the action is in the current game session.

Calling this function triggers the [`nuk_game_engine:start/2`](nuk_game_engine.md#start-2) callback.

For public API [`nuk_games:start/2`](nuk_games.md#start-2) must be used.

<a name="start_link-1"></a>

### start_link/1 ###

`start_link(GameName) -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="turn-3"></a>

### turn/3 ###

<pre><code>
turn(Pid::pid(), User::<a href="nuk_user.md#type-user">nuk_user:user()</a>, Turn::term()) -&gt; ok | {error, user_not_in_game, Extra::string()} | {error, bad_turn_order, Extra::string()} | {error, invalid_turn, Extra::string()}
</code></pre>
<br />

Process player's turn

This is a function powering the implementation of [`nuk_games:turn/3`](nuk_games.md#turn-3).
It takes and processes a turn for a given player after verifying that the
given player may make a turn at current stage of the game.

Calling this function triggers the [`nuk_game_engine:turn/4`](nuk_game_engine.md#turn-4) callback.
The game engine may return the `invalid_turn` error if the turn data is
not acceptable.

For public API [`nuk_games:turn/3`](nuk_games.md#turn-3) must be used.

