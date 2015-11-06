-module(nuk_user_storage).

-callback delete(Username :: string()) ->
    'ok'.

-callback get(Username :: string()) ->
    {ok, nuk_user:user()} |
    {error, ErrorCode :: user_not_found, ErrorText :: string()}.

-callback put(User :: nuk_user:user()) ->
    'ok'.

-callback validate(Username :: string(), Password :: string()) ->
    {ok, User :: nuk_user:user()} |
    {error, ErrorCode :: wrong_password | user_not_found, ErrorText :: string()}.

-callback list() ->
    [nuk_user:user()].
