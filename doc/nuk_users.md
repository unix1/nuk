

# Module nuk_users #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_users` module.

<a name="description"></a>

## Description ##
This module should be used as an API to interacting with all user related
actions.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Delete an existing user.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Get a user.</td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td>List all users.</td></tr><tr><td valign="top"><a href="#login-2">login/2</a></td><td>Login a user.</td></tr><tr><td valign="top"><a href="#put-1">put/1</a></td><td>Create new or update an existing user.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(Username::string()) -&gt; ok
</code></pre>
<br />

Delete an existing user

This deletes an existing user from user storage.

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(Username::string()) -&gt; {ok, <a href="nuk_user.md#type-user">nuk_user:user()</a>} | {error, user_not_found, string()}
</code></pre>
<br />

Get a user

Retrieves a user from user data storage given a username. The return is a
[`nuk_user:user()`](nuk_user.md#type-user) data type. Use [`nuk_user`](nuk_user.md) module to extract
needed information.

<a name="list-0"></a>

### list/0 ###

<pre><code>
list() -&gt; [<a href="nuk_user.md#type-user">nuk_user:user()</a>]
</code></pre>
<br />

List all users

Get all users from user storage. The return is a list of
[`nuk_user:user()`](nuk_user.md#type-user) data types. Use [`nuk_user`](nuk_user.md) modue to extract
needed information.

<a name="login-2"></a>

### login/2 ###

<pre><code>
login(Username::string(), Password::string()) -&gt; {ok, SessionId::string()} | {error, ErrorCode::atom(), Extra::string()}
</code></pre>
<br />

Login a user

Given a username and a password attempt to login a user. If login is
successful, a new user session identifier is returned; otherwise, an error
is returned.

<a name="put-1"></a>

### put/1 ###

<pre><code>
put(User::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; ok
</code></pre>
<br />

Create new or update an existing user

Performs a put operation. If a user with the given username exists it will
overwrite with the new data. If a new username is given, a new user will be
created. Create a [`nuk_user:user()`](nuk_user.md#type-user) data type and use it here.

