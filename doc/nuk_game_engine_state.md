

# Module nuk_game_engine_state #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_game_engine_state` module.

<a name="description"></a>

## Description ##
This module is used to operate on [`nuk_game_engine_state:state()`](nuk_game_engine_state.md#type-state) data
type. This data type is used when retrieving game engine's state from
[`nuk_game_session:get_nuk_state/1`](nuk_game_session.md#get_nuk_state-1).
<a name="types"></a>

## Data Types ##




### <a name="type-state">state()</a> ###


__abstract datatype__: `state()`

 Data type containing game engine's game state. This is part of
[`nuk_game_session:session()`](nuk_game_session.md#type-session) data type. Functions in this module can
be used to operate on the following data:
- `private`: part of the state that stays private to the game engine; it is
never shared with any players
- `public`: part of the state that is public - it is shared with all players
- `players`: a map of players usernames to the data that will be shared to
those specific players only
All above data is of type `term()` - i.e. it's up to the game engine

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_all-1">get_all/1</a></td><td>Get all components of engine state.</td></tr><tr><td valign="top"><a href="#get_player-2">get_player/2</a></td><td>Get player specific state.</td></tr><tr><td valign="top"><a href="#get_players-1">get_players/1</a></td><td>Get a map of all player states.</td></tr><tr><td valign="top"><a href="#get_private-1">get_private/1</a></td><td>Get private state.</td></tr><tr><td valign="top"><a href="#get_public-1">get_public/1</a></td><td>Get public state.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Create new a new <a href="#type-state"><code>state()</code></a> data type.</td></tr><tr><td valign="top"><a href="#put_player-3">put_player/3</a></td><td>Put a state for a new or existing player.</td></tr><tr><td valign="top"><a href="#remove_player-2">remove_player/2</a></td><td>Remove player from players state.</td></tr><tr><td valign="top"><a href="#set_all-4">set_all/4</a></td><td>Set all components of the engine state.</td></tr><tr><td valign="top"><a href="#set_player-3">set_player/3</a></td><td>Set a state for an existing player.</td></tr><tr><td valign="top"><a href="#set_players-2">set_players/2</a></td><td>Set all players state.</td></tr><tr><td valign="top"><a href="#set_private-2">set_private/2</a></td><td>Set private state.</td></tr><tr><td valign="top"><a href="#set_public-2">set_public/2</a></td><td>Set public state.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_all-1"></a>

### get_all/1 ###

<pre><code>
get_all(State::<a href="#type-state">state()</a>) -&gt; list()
</code></pre>
<br />

Get all components of engine state

Returns a list containing all - private, public and players - states.

<a name="get_player-2"></a>

### get_player/2 ###

<pre><code>
get_player(State::<a href="#type-state">state()</a>, Username::string()) -&gt; term()
</code></pre>
<br />

Get player specific state

Gets state specific to the given player.

<a name="get_players-1"></a>

### get_players/1 ###

<pre><code>
get_players(State::<a href="#type-state">state()</a>) -&gt; #{}
</code></pre>
<br />

Get a map of all player states

Gets a map of player states with player usernames as keys.

<a name="get_private-1"></a>

### get_private/1 ###

<pre><code>
get_private(State::<a href="#type-state">state()</a>) -&gt; term()
</code></pre>
<br />

Get private state

Gets game engine's private state.

<a name="get_public-1"></a>

### get_public/1 ###

<pre><code>
get_public(State::<a href="#type-state">state()</a>) -&gt; term()
</code></pre>
<br />

Get public state

Gets game engine's public state.

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Private::term(), Public::term(), Players::#{}) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Create new a new [`state()`](#type-state) data type

Creates a new state with specified values.

<a name="put_player-3"></a>

### put_player/3 ###

<pre><code>
put_player(State::<a href="#type-state">state()</a>, Username::string(), Player::term()) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Put a state for a new or existing player

Sets a state for a specific player; if the Username doesn't exist, it is
added; if it exists its data is overwritten.

<a name="remove_player-2"></a>

### remove_player/2 ###

<pre><code>
remove_player(State::<a href="#type-state">state()</a>, Username::string()) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Remove player from players state

Completely removes a player from the map of player states

<a name="set_all-4"></a>

### set_all/4 ###

<pre><code>
set_all(State::<a href="#type-state">state()</a>, Private::term(), Public::term(), Players::#{}) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Set all components of the engine state

Sets all - private, public and players - states

<a name="set_player-3"></a>

### set_player/3 ###

<pre><code>
set_player(State::<a href="#type-state">state()</a>, Username::string(), Player::term()) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Set a state for an existing player

Sets a state for a specific existing player.

<a name="set_players-2"></a>

### set_players/2 ###

<pre><code>
set_players(State::<a href="#type-state">state()</a>, Players::#{}) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Set all players state

Sets a map of states for all players.

<a name="set_private-2"></a>

### set_private/2 ###

<pre><code>
set_private(State::<a href="#type-state">state()</a>, Private::term()) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Set private state

Sets game engine private session state.

<a name="set_public-2"></a>

### set_public/2 ###

<pre><code>
set_public(State::<a href="#type-state">state()</a>, Public::term()) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Set public state

Sets game engine public session state.

