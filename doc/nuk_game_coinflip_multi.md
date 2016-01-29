

# Module nuk_game_coinflip_multi #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_game_coinflip_multi` module.

__Behaviours:__ [`nuk_game_engine`](nuk_game_engine.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#finish-2">finish/2</a></td><td></td></tr><tr><td valign="top"><a href="#initialize-2">initialize/2</a></td><td></td></tr><tr><td valign="top"><a href="#player_join-3">player_join/3</a></td><td></td></tr><tr><td valign="top"><a href="#player_leave-3">player_leave/3</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#turn-4">turn/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="finish-2"></a>

### finish/2 ###

`finish(State, NukState) -> any()`

<a name="initialize-2"></a>

### initialize/2 ###

<pre><code>
initialize(User::<a href="nuk_user.md#type-user">nuk_user:user()</a>, OptionsOverride::list()) -&gt; {error, invalid_options, string()} | {ok, <a href="nuk_game_engine_state.md#type-state">nuk_game_engine_state:state()</a>}
</code></pre>
<br />

<a name="player_join-3"></a>

### player_join/3 ###

`player_join(User, State, NukState) -> any()`

<a name="player_leave-3"></a>

### player_leave/3 ###

`player_leave(User, State, NukState) -> any()`

<a name="start-2"></a>

### start/2 ###

`start(State, NukState) -> any()`

<a name="turn-4"></a>

### turn/4 ###

`turn(User, Turn, State, NukState) -> any()`

