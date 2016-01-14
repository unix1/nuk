

# Module nuk_user_sessions #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_user_sessions` module.

<a name="description"></a>

## Description ##

This module should be used as an API for mapping user session identifiers
to process IDs:
- given a user process `pid()` create a new unique session identifier
- translate a given unique session identifier to the user session `pid()`

It also provides several convenience functions for getting and extracting
data from user sessions.

The backend implementation of this is swappable. See
[`nuk_user_session_storage`](nuk_user_session_storage.md) behavior and
[`nuk_user_session_store_server`](nuk_user_session_store_server.md) default implementation.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Delete a session.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Get session.</td></tr><tr><td valign="top"><a href="#get_pid-1">get_pid/1</a></td><td>Get a process ID.</td></tr><tr><td valign="top"><a href="#get_user-1">get_user/1</a></td><td>Get user.</td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td>List all sessions.</td></tr><tr><td valign="top"><a href="#logout-1">logout/1</a></td><td>Log out a user session.</td></tr><tr><td valign="top"><a href="#put-1">put/1</a></td><td>Create a new session.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(SessionId::string()) -&gt; ok
</code></pre>
<br />

Delete a session

Delete the session associated with the given session identifier.

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(SessionId::string()) -&gt; {ok, <a href="nuk_user_session.md#type-session">nuk_user_session:session()</a>} | {error, user_session_not_found, Extra::string()}
</code></pre>
<br />

Get session

Given a previously created session identifier, retrieve a process ID. Then
call the process to retrive its session [`nuk_user_session:session()`](nuk_user_session.md#type-session)
data type.

<a name="get_pid-1"></a>

### get_pid/1 ###

<pre><code>
get_pid(SessionId::string()) -&gt; {ok, pid()} | {error, user_session_not_found, Extra::string()}
</code></pre>
<br />

Get a process ID

Given a previously created session identifier, retrieve a process ID.

<a name="get_user-1"></a>

### get_user/1 ###

<pre><code>
get_user(SessionId::string()) -&gt; {ok, <a href="nuk_user.md#type-user">nuk_user:user()</a>} | {error, user_session_not_found, Extra::string()}
</code></pre>
<br />

Get user

Gets user that is associated with this session identifier.

<a name="list-0"></a>

### list/0 ###

<pre><code>
list() -&gt; [<a href="nuk_user_session.md#type-session">nuk_user_session:session()</a>]
</code></pre>
<br />

List all sessions

Returns a list of all user sessions. Used in tests.

<a name="logout-1"></a>

### logout/1 ###

<pre><code>
logout(SessionId::string()) -&gt; ok
</code></pre>
<br />

Log out a user session

Logs out the given user session. Note that the [`delete/1`](#delete-1) happens
after the [`nuk_user_server`](nuk_user_server.md) terminates successfully.

<a name="put-1"></a>

### put/1 ###

<pre><code>
put(Pid::pid()) -&gt; SessionId::string()
</code></pre>
<br />

Create a new session

Given a process ID, create a new unique session identifier.

