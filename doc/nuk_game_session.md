

# Module nuk_game_session #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

nuk game session.

<a name="types"></a>

## Data Types ##




### <a name="type-nuk_state">nuk_state()</a> ###


__abstract datatype__: `nuk_state()`




### <a name="type-session">session()</a> ###


__abstract datatype__: `session()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_player-2">add_player/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_game-1">get_game/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_game_state-1">get_game_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_players-1">get_players/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_players_count-1">get_players_count/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_players_turn-1">get_players_turn/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_status-1">get_status/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_turn_number-1">get_turn_number/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_winners_losers-1">get_winners_losers/1</a></td><td></td></tr><tr><td valign="top"><a href="#has_player-2">has_player/2</a></td><td></td></tr><tr><td valign="top"><a href="#increment_turn_number-1">increment_turn_number/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_players_turn-2">is_players_turn/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#remove_player-2">remove_player/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_game_state-2">set_game_state/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_players-2">set_players/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_players_turn-2">set_players_turn/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_status-2">set_status/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_turn_number-2">set_turn_number/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_winners_losers-3">set_winners_losers/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_player-2"></a>

### add_player/2 ###

<pre><code>
add_player(Session::<a href="#type-session">session()</a>, Player::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

<a name="get_game-1"></a>

### get_game/1 ###

<pre><code>
get_game(Session::<a href="#type-session">session()</a>) -&gt; <a href="nuk_game.md#type-game">nuk_game:game()</a>
</code></pre>
<br />

<a name="get_game_state-1"></a>

### get_game_state/1 ###

<pre><code>
get_game_state(Session::<a href="#type-session">session()</a>) -&gt; term()
</code></pre>
<br />

<a name="get_players-1"></a>

### get_players/1 ###

<pre><code>
get_players(Session::<a href="#type-session">session()</a>) -&gt; [<a href="nuk_user.md#type-user">nuk_user:user()</a>]
</code></pre>
<br />

<a name="get_players_count-1"></a>

### get_players_count/1 ###

<pre><code>
get_players_count(Session::<a href="#type-session">session()</a>) -&gt; integer()
</code></pre>
<br />

<a name="get_players_turn-1"></a>

### get_players_turn/1 ###

<pre><code>
get_players_turn(Session::<a href="#type-session">session()</a>) -&gt; [<a href="nuk_user.md#type-user">nuk_user:user()</a>]
</code></pre>
<br />

<a name="get_status-1"></a>

### get_status/1 ###

<pre><code>
get_status(Session::<a href="#type-session">session()</a>) -&gt; atom()
</code></pre>
<br />

<a name="get_turn_number-1"></a>

### get_turn_number/1 ###

<pre><code>
get_turn_number(Session::<a href="#type-session">session()</a>) -&gt; integer()
</code></pre>
<br />

<a name="get_winners_losers-1"></a>

### get_winners_losers/1 ###

<pre><code>
get_winners_losers(Session::<a href="#type-session">session()</a>) -&gt; {Winners::[<a href="nuk_user.md#type-user">nuk_user:user()</a>], Losers::[<a href="nuk_user.md#type-user">nuk_user:user()</a>]}
</code></pre>
<br />

<a name="has_player-2"></a>

### has_player/2 ###

<pre><code>
has_player(Session::<a href="#type-session">session()</a>, Player::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; true | false
</code></pre>
<br />

<a name="increment_turn_number-1"></a>

### increment_turn_number/1 ###

<pre><code>
increment_turn_number(Session::<a href="#type-session">session()</a>) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

<a name="is_players_turn-2"></a>

### is_players_turn/2 ###

<pre><code>
is_players_turn(Session::<a href="#type-session">session()</a>, Player::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; true | false
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Game::<a href="nuk_game.md#type-game">nuk_game:game()</a>) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

<a name="remove_player-2"></a>

### remove_player/2 ###

<pre><code>
remove_player(Session::<a href="#type-session">session()</a>, Player::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

<a name="set_game_state-2"></a>

### set_game_state/2 ###

<pre><code>
set_game_state(Session::<a href="#type-session">session()</a>, GameState::term()) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

<a name="set_players-2"></a>

### set_players/2 ###

<pre><code>
set_players(Session::<a href="#type-session">session()</a>, Players::[<a href="nuk_user.md#type-user">nuk_user:user()</a>]) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

<a name="set_players_turn-2"></a>

### set_players_turn/2 ###

<pre><code>
set_players_turn(Session::<a href="#type-session">session()</a>, Players::[<a href="nuk_user.md#type-user">nuk_user:user()</a>]) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

<a name="set_status-2"></a>

### set_status/2 ###

<pre><code>
set_status(Session::<a href="#type-session">session()</a>, Status::atom()) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

<a name="set_turn_number-2"></a>

### set_turn_number/2 ###

<pre><code>
set_turn_number(Session::<a href="#type-session">session()</a>, TurnNumber::integer()) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

<a name="set_winners_losers-3"></a>

### set_winners_losers/3 ###

<pre><code>
set_winners_losers(Session::<a href="#type-session">session()</a>, Winners::[<a href="nuk_user.md#type-user">nuk_user:user()</a>], Losers::[<a href="nuk_user.md#type-user">nuk_user:user()</a>]) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

