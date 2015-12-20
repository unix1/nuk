

# Module nuk_user #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_user` module.

<a name="description"></a>

## Description ##
This module is used to operate on [`nuk_user:user()`](nuk_user.md#type-user) data type. This
data type is used when dealing with users in nuk, for example, when calling
[`nuk_users:put/1`](nuk_users.md#put-1) and [`nuk_users:get/1`](nuk_users.md#get-1).
<a name="types"></a>

## Data Types ##




### <a name="type-user">user()</a> ###


__abstract datatype__: `user()`

 Data type used to operate on users in nuk. Use functions in this module to
operate on this data type.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#check_password-2">check_password/2</a></td><td>Verify user's password.</td></tr><tr><td valign="top"><a href="#get_username-1">get_username/1</a></td><td>Get username.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Create a new <a href="nuk_user.md#type-user"><code>nuk_user:user()</code></a> data type.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="check_password-2"></a>

### check_password/2 ###

<pre><code>
check_password(X1::<a href="#type-user">user()</a>, EnteredPassword::string()) -&gt; boolean()
</code></pre>
<br />

Verify user's password

This function is only here for testing and proof of concept purposes. In
production scenarios where users are stored in external
[`nuk_user_storage`](nuk_user_storage.md) implementations the authentication should be
performed by that system, and this function should not be used.

<a name="get_username-1"></a>

### get_username/1 ###

<pre><code>
get_username(X1::<a href="#type-user">user()</a>) -&gt; string()
</code></pre>
<br />

Get username

Extract username from [`nuk_user:user()`](nuk_user.md#type-user) data type.

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Username::string(), Password::string()) -&gt; <a href="#type-user">user()</a>
</code></pre>
<br />

Create a new [`nuk_user:user()`](nuk_user.md#type-user) data type

`Username` is a string that uniquely identifies a user in the user storage.
`Password` is a string used to validate user for login. Note that the
password is here for proof of concept and testing purposes only. It does not
provide hashing or secure password storage. When using
[`nuk_user_storage`](nuk_user_storage.md) implementations for the hashed password is likely
stored there, so this password shouldn't be used in that case.

