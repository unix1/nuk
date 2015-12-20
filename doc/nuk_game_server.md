

# Module nuk_game_server #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

nuk game server.

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#create-3">create/3</a></td><td></td></tr><tr><td valign="top"><a href="#finish-1">finish/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_session-1">get_session/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td></td></tr><tr><td valign="top"><a href="#leave-2">leave/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#turn-3">turn/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVersion, State, Extra) -> any()`

<a name="create-3"></a>

### create/3 ###

<pre><code>
create(User::<a href="nuk_user.md#type-user">nuk_user:user()</a>, GameName::string(), Options::list()) -&gt; {ok, GameSessionId::string()} | {error, invalid_game_name, Extra::string()}
</code></pre>
<br />

<a name="finish-1"></a>

### finish/1 ###

<pre><code>
finish(Pid::pid()) -&gt; ok
</code></pre>
<br />

<a name="get_session-1"></a>

### get_session/1 ###

<pre><code>
get_session(Pid::pid()) -&gt; <a href="nuk_game_session.md#type-session">nuk_game_session:session()</a>
</code></pre>
<br />

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

`init(X1) -> any()`

<a name="join-2"></a>

### join/2 ###

<pre><code>
join(Pid::pid(), User::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; ok | {error, user_already_joined, Extra::string()} | {error, max_users_reached, Extra::string()}
</code></pre>
<br />

<a name="leave-2"></a>

### leave/2 ###

<pre><code>
leave(Pid::pid(), User::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; ok | {error, user_not_in_game, Extra::string()}
</code></pre>
<br />

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(Pid::pid(), User::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; ok | {error, user_not_in_game, Extra::string()}
</code></pre>
<br />

<a name="start_link-1"></a>

### start_link/1 ###

`start_link(GameName) -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="turn-3"></a>

### turn/3 ###

<pre><code>
turn(Pid::pid(), User::<a href="nuk_user.md#type-user">nuk_user:user()</a>, Turn::term()) -&gt; ok | {error, bad_turn_order, Extra::string()} | {error, invalid_turn, Extra::string()}
</code></pre>
<br />

