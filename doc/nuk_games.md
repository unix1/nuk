

# Module nuk_games #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_games` module.

<a name="description"></a>

## Description ##

This module should be used as an API to interacting with all game related
actions. There are 2 types of actions: (1) game registration, and (2) game
flow.

The game registration functions are:[`register/2`](#register-2),
[`unregister/1`](#unregister-1), [`get/1`](#get-1), [`list/0`](#list-0).

The game flow functions are: [`create/2`](#create-2), [`create/3`](#create-3),
[`join/2`](#join-2), [`leave/2`](#leave-2), [`start/2`](#start-2),
[`get_game_session/2`](#get_game_session-2), [`turn/3`](#turn-3).<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create-2">create/2</a></td><td>Create a new game session with default options.</td></tr><tr><td valign="top"><a href="#create-3">create/3</a></td><td>Create a new game with options.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Get a game by its name.</td></tr><tr><td valign="top"><a href="#get_game_session-2">get_game_session/2</a></td><td>Get game session containing nuk and game engine states.</td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td>Join a player to a game session.</td></tr><tr><td valign="top"><a href="#leave-2">leave/2</a></td><td>Remove a player from a game session.</td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td>List all registered games.</td></tr><tr><td valign="top"><a href="#register-1">register/1</a></td><td>Register a game engine.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>Start a game.</td></tr><tr><td valign="top"><a href="#turn-3">turn/3</a></td><td>Make a player turn.</td></tr><tr><td valign="top"><a href="#unregister-1">unregister/1</a></td><td>Unregister a game engine.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="create-2"></a>

### create/2 ###

<pre><code>
create(UserSessionId::string(), GameName::string()) -&gt; {ok, GameSessionId::string()} | {error, invalid_user_session, Extra::string()} | {error, invalid_game_name, Extra::string()}
</code></pre>
<br />

Equivalent to [`create(UserSessionId, GameName, [])`](#create-3).

Create a new game session with default options

<a name="create-3"></a>

### create/3 ###

<pre><code>
create(UserSessionId::string(), GameName::string(), Options::[tuple()]) -&gt; {ok, GameSessionId::string()} | {error, invalid_user_session, Extra::string()} | {error, invalid_game_name, Extra::string()}
</code></pre>
<br />

Create a new game with options

Using a logged in user session, create a new game by supplying its
registered name. This does not start a game, but merely creates a new
session allowing other players to join. Game session must be created before
it can be started.

Calling this function triggers the [`nuk_game_engine:initialize/2`](nuk_game_engine.md#initialize-2)
callback.

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(GameName::string()) -&gt; {ok, <a href="nuk_game.md#type-game">nuk_game:game()</a>} | {error, game_not_found, Extra::string()}
</code></pre>
<br />

Get a game by its name

This can be used to look up any registered game metadata for a specific game
engine. Given a game name, get a [`nuk_game:game()`](nuk_game.md#type-game) data type. Then use
[`nuk_game`](nuk_game.md) module functions to extract needed information.

<a name="get_game_session-2"></a>

### get_game_session/2 ###

<pre><code>
get_game_session(GameSessionId::string(), UserSessionId::string()) -&gt; {ok, <a href="nuk_game_session.md#type-session">nuk_game_session:session()</a>} | {error, game_session_not_found, Extra::string()} | {error, user_session_not_found, Extra::string()} | {error, user_not_in_game, Extra::string()}
</code></pre>
<br />

Get game session containing nuk and game engine states

Returns the current snapshot of the game session state. The
[`nuk_game_session`](nuk_game_session.md) module functions should be used to extract needed
data from the returned [`nuk_game_session:session()`](nuk_game_session.md#type-session) data type. This is
useful for players to get a new game session state during the game.

Note that [`nuk_game_session:get_game_state/1`](nuk_game_session.md#get_game_state-1) can be used to extract
[`nuk_game_engine_state`](nuk_game_engine_state.md) set by the specific [`nuk_game_engine`](nuk_game_engine.md).

<a name="join-2"></a>

### join/2 ###

<pre><code>
join(GameSessionId::string(), UserSessionId::string()) -&gt; ok | {error, game_session_not_found, Extra::string()} | {error, user_session_not_found, Extra::string()} | {error, user_already_joined, Extra::string()} | {error, max_users_reached, Extra::string()}
</code></pre>
<br />

Join a player to a game session

Joins a given logged in user session to an existing game session. Game
session must be created first, see [`create/2`](#create-2) and [`create/3`](#create-3).

Calling this function triggers the [`nuk_game_engine:player_join/3`](nuk_game_engine.md#player_join-3)
callback.

<a name="leave-2"></a>

### leave/2 ###

<pre><code>
leave(GameSessionId::string(), UserSessionId::string()) -&gt; ok | {error, game_session_not_found, Extra::string()} | {error, user_session_not_found, Extra::string()} | {error, user_not_in_game, Extra::string()} | {error, game_already_started, Extra::string()}
</code></pre>
<br />

Remove a player from a game session

This does the opposite of [`join/2`](#join-2) - it allows a player to leave an
existing game session that the player has already joined.

Calling this function triggers the [`nuk_game_engine:player_leave/3`](nuk_game_engine.md#player_leave-3)
callback.

<a name="list-0"></a>

### list/0 ###

<pre><code>
list() -&gt; [<a href="nuk_game.md#type-game">nuk_game:game()</a>]
</code></pre>
<br />

List all registered games

Produces a list of all registered games in the system. The return is a list
of [`nuk_game:game()`](nuk_game.md#type-game) data types. Use [`nuk_game`](nuk_game.md) module
functions to extract needed information from each game element.

<a name="register-1"></a>

### register/1 ###

<pre><code>
register(Game::<a href="nuk_game.md#type-game">nuk_game:game()</a>) -&gt; ok
</code></pre>
<br />

Register a game engine

This registers a game engine with nuk. After creating a new game engine
by implementing the [`nuk_game_engine`](nuk_game_engine.md) behavior, create a
[`nuk_game:game()`](nuk_game.md#type-game) data type and use it here to register that game.
A game engine must be registered before it can be played.

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(GameSessionId::string(), UserSessionId::string()) -&gt; ok | {error, game_session_not_found, Extra::string()} | {error, user_session_not_found, Extra::string()} | {error, min_users_not_met, Extra::string()} | {error, user_not_in_game, Extra::string()}
</code></pre>
<br />

Start a game

This starts an existing game session. In general, at this point all players
wishing to participate should have already joined the game via
[`join/2`](#join-2).

Calling this function triggers the [`nuk_game_engine:start/2`](nuk_game_engine.md#start-2) callback.

<a name="turn-3"></a>

### turn/3 ###

<pre><code>
turn(GameSessionId::string(), UserSessionId::string(), Turn::term()) -&gt; ok | {error, game_session_not_found, Extra::string()} | {error, user_session_not_found, Extra::string()} | {error, user_not_in_game, Extra::string()} | {error, bad_turn_order, Extra::string()} | {error, invalid_turn, Extra::string()}
</code></pre>
<br />

Make a player turn

This function should be used when it's time for a specific player to make a
turn. The `Turn` argument is an arbitrary term that is expected by the game
engine. It is not validated by nuk and is passed to the game engine
directly.

Calling this function triggers the [`nuk_game_engine:turn/4`](nuk_game_engine.md#turn-4) callback.

<a name="unregister-1"></a>

### unregister/1 ###

<pre><code>
unregister(GameName::string()) -&gt; ok
</code></pre>
<br />

Unregister a game engine

Unregistering has the opposite effect of registering via [`register/1`](#register-1).
It makes nuk forget about the registered game.

