

# Module nuk_user_server #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_user_server` module.

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="description"></a>

## Description ##

When a user logs in, a new process is spawned that keeps the session of the
user. This is a `gen_server` that keeps the session state and provides
interface to its manipulation.

For public API to accessing this functionality use the [`nuk_users`](nuk_users.md) and
[`nuk_user_sessions`](nuk_user_sessions.md) modules. Do not call the functions of this module
directly.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_session-1">get_session/1</a></td><td>Get logged in user session.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#login-3">login/3</a></td><td>Log in a user.</td></tr><tr><td valign="top"><a href="#logout-1">logout/1</a></td><td>Log out a user.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVersion, State, Extra) -> any()`

<a name="get_session-1"></a>

### get_session/1 ###

<pre><code>
get_session(Pid::pid()) -&gt; <a href="nuk_user_session.md#type-session">nuk_user_session:session()</a>
</code></pre>
<br />

Get logged in user session

Gets the [`nuk_user_session:session()`](nuk_user_session.md#type-session) data type for the given logged
in user session. Use [`nuk_user_session`](nuk_user_session.md) module to operate on the
returned data type.

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

<a name="login-3"></a>

### login/3 ###

<pre><code>
login(Username::string(), Password::string(), StorageModule::atom()) -&gt; {ok, string()} | {error, wrong_password | user_not_found, string()}
</code></pre>
<br />

Log in a user

Attempts to log a user in given the username and password. Upon successful
login a new process is spawned and the string session identifier returned.

<a name="logout-1"></a>

### logout/1 ###

<pre><code>
logout(Pid::pid()) -&gt; ok
</code></pre>
<br />

Log out a user

Logs a user session out - i.e. stops the process that was keeping the logged
in user state.

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

