

# Module nuk_user_store_server #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_user_store_server` module.

__Behaviours:__ [`gen_server`](gen_server.md), [`nuk_user_storage`](nuk_user_storage.md).

<a name="description"></a>

## Description ##

This is an implementation of [`nuk_user_storage`](nuk_user_storage.md) behavior. It is meant
for testing and proof of concept purposes only.

This is a `gen_server` that's started by the [`nuk_user_store_sup`](nuk_user_store_sup.md)
supervisor. It provides storage interface to registered users. For public
API the [`nuk_users`](nuk_users.md) module should be used which, in turn, will use the
appropriate storage backend.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Delete a user.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Get a user.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td>List all users.</td></tr><tr><td valign="top"><a href="#put-1">put/1</a></td><td>Create or replace a user.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#validate-2">validate/2</a></td><td>Validate user credentials.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVersion, State, Extra) -> any()`

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(Username::string()) -&gt; ok
</code></pre>
<br />

Delete a user

Deletes a user by username from the user data storage.

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(Username::string()) -&gt; {ok, <a href="nuk_user.md#type-user">nuk_user:user()</a>} | {error, user_not_found, string()}
</code></pre>
<br />

Get a user

Retrieves a user by username from the user data storage.

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

<a name="list-0"></a>

### list/0 ###

<pre><code>
list() -&gt; [<a href="nuk_user.md#type-user">nuk_user:user()</a>]
</code></pre>
<br />

List all users

Lists all registered users in the user data storage.

<a name="put-1"></a>

### put/1 ###

<pre><code>
put(User::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; ok
</code></pre>
<br />

Create or replace a user

If a user by the username is already registered, replaces that registration;
otherwise creates a new registered user.

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="validate-2"></a>

### validate/2 ###

<pre><code>
validate(Username::string(), Password::string()) -&gt; {ok, <a href="nuk_user.md#type-user">nuk_user:user()</a>} | {error, wrong_password | user_not_found, string()}
</code></pre>
<br />

Validate user credentials

Given a username and a password validate that the credentials are correct.

