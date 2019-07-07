module Mastodon.Login exposing (loginTask)

{-| Support for creating an `App` and logging in to a server.

@docs loginTask

-}

import Browser.Navigation as Navigation
import Mastodon.Entity as Entity exposing (Account, App, Authorization, Entity(..))
import Mastodon.Request as Request
    exposing
        ( AccountsReq(..)
        , AppsReq(..)
        , Error(..)
        , Request(..)
        )
import Task exposing (Task)


{-| There should probably be a login version that takes scopes.
-}
scopes : List String
scopes =
    [ "write", "read", "follow", "push" ]


{-| Get a token and, if possible, the logged in `Account` from the server.

Returns a `Task` to either fetch an `Account` for a known authorization token,
or to redirect to the authentication server to create a code with which
to mint the token.

Arguments are:

    loginTask server applicationUri maybeAuthorization

If `maybeAuthorization` is not `Nothing`, will attempt to get an
`Account` using `.authorization` from there. If that fails, will
attempt to mint a new token, using `authorization.clientId` and
`authorization.clientSecret`. If that fails, or if
`maybeAuthorization` is `Nothing`, will create a new `App`, and
redirect to do authentication. When the application is restarted, with
a code and state in the fragment, call `getTokenAndAccount` to
continue use those to mint a new token, and to use that to get an
`Account`.

Usually, you will get an `Authorization` from persistent
`localStorage`, pass that here, and successfully receive the `Account`
back. No redirects necessary. But the redirects must happen at least
once, and then whenever the authorization token expires. This is step
8 below. Only if that fails will this do step 3, or, if there was no
persisted client id and secret, steps 1 through 3.

Currently, permission for all scopes is requested.

The full login procedure is as follows:

1.  Send "POST /api/v1/apps" to the Mastodon server, via a `PostApp` request.

2.  Receive the returned `client_id` and `client_secret`

3.  Redirect to `<server_url>/oauth/authorize?client_id=<client_id>&redirect_uri=<client_uri>&response_type=code&scope=<scopes>&state=<state>`

4.  The user enters login authorization information to the server web site.

5.  The server web site redirects to `<redirect_uri>?code=<code>&state=<state>`

6.  The `<state>` encodes the server uri, <client\_id>, <client\_secret>, and <scopes>.
    Use that to POST to `<server_url>/oauth/token`:

        POST /oauth/token HTTP/1.1
        Host: <server_url>
        Authorization: Basic `(Base64.encode (<client_id> ++ ":" ++ <client_secret>))`
        Content-Type: application/x-www-form-urlencoded

        grant_type=authorization_code&code=<code>&redirect_uri=<client_uri>

7.  Receive back token information, JSON-encoded:

        { "access_token":"cptGSO8ff7xKbBXlTTxH15bnxQS5b9erVUWi_n0_EGc",
          "token_type":"Bearer",
          "scope":"read",
          "created_at":1561845912
        }

8.  Use the `token_type` and `access_token` to authenticate a request
    for the user's `Account`.

-}
loginTask : String -> String -> Maybe Authorization -> FetchAccountOrRedirect msg
loginTask server applicationUri maybeAuthorization =
    case maybeAuthorization of
        Just { authorization } ->
            let
                request =
                    AccountsRequest GetVerifyCredentials

                rawRequest =
                    Request.requestToRawRequest []
                        { server = server, authorization = Just authorization }
                        request

                task =
                    Request.rawRequestToTask rawRequest
                        |> Task.andThen
                            (\response ->
                                case response.entity of
                                    AccountEntity account ->
                                        Task.succeed account

                                    _ ->
                                        Task.fail <| BadUrl "Shouldn't happen"
                            )
            in
            FetchAccount task

        Nothing ->
            let
                postApp =
                    PostApp
                        { client_name = server
                        , redirect_uris = applicationUri
                        , scopes = scopes
                        , website = Nothing
                        }

                rawRequest =
                    Request.requestToRawRequest []
                        { server = server, authorization = Nothing }
                        (AppsRequest postApp)

                redirectTask =
                    Request.rawRequestToTask rawRequest
                        |> Task.andThen
                            (\response ->
                                case response.entity of
                                    AppEntity app ->
                                        appToAuthorizeUrl server app
                                            |> Navigation.load
                                            |> Task.succeed

                                    _ ->
                                        Task.fail (BadUrl "Shouldn't happen")
                            )
            in
            Redirect redirectTask


appToAuthorizeUrl : String -> App -> String
appToAuthorizeUrl server app =
    ""


{-| The result type of the `loginTask` function.

It's either a task to fetch the logged-in user's `Account`
or a task to redirect to the authorization server to get a code.

-}
type FetchAccountOrRedirect msg
    = FetchAccount (Task Error Account)
    | Redirect (Task Error (Cmd msg))
