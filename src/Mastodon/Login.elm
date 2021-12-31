module Mastodon.Login exposing
    ( FetchAccountOrRedirect(..), loginTask, getTokenTask
    , appToAuthorizeUrl
    )

{-| Support for creating an `App` and logging in to a server.

See <https://docs.joinmastodon.org/client/authorized/>

@docs FetchAccountOrRedirect, loginTask, getTokenTask


# Internal functions.

@docs appToAuthorizeUrl

-}

import Base64
import Browser.Navigation as Navigation
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Mastodon.EncodeDecode as ED
import Mastodon.Entity as Entity exposing (Account, App, Authorization, Entity(..))
import Mastodon.Request as Request
    exposing
        ( AccountsReq(..)
        , AppsReq(..)
        , Error(..)
        , Request(..)
        )
import Task exposing (Task)
import Url.Builder as Builder


{-| There should probably be a login version that takes scopes.
-}
scopes : List String
scopes =
    [ "write", "read", "follow", "push" ]


{-| Get a token and, if possible, the logged in `Account` from the server.

The `Maybe String` arg is a login token, called `maybeToken` below, if
it is not Nothing.

Returns a `Task` to either fetch an `Account` for a known authorization token,
or to redirect to the authentication server to create a code with which
to mint the token.

If `maybeToken` is not `Nothing`, will attempt to get an `Account`
using that token. If that fails, will return an error. If `maybeToken`
is `Nothing`, will create a new `App`, and redirect to do
authentication. When the application is restarted, with a `code` in
the URL query, call `getTokenTask` to use that code and the saved
`App` instance to mint a new token, and to use that to get an
`Account`.

Usually, you will get a `Token` from persistent
`localStorage`, pass that here, and successfully receive the `Account`
back. No redirects necessary. But the redirects must happen at least
once, and then whenever the authorization token expires. This is step
8 below. Only if that fails will this do step 3, or, if there was no
persisted client id and secret, steps 1 through 3.

Currently, permission for all scopes is requested. Maybe the scopes should
be passed as a parameter.

The full login procedure is as follows:

1.  Send "POST /api/v1/apps" to the Mastodon server, via a `PostApp` request,

2.  Receive the returned App, including `client_id` and `client_secret`.
    Save it in localStorage, keyed with the Mastodon server's host name
    (from `applicationUri`).

3.  Redirect to `<server_url>/oauth/authorize?client_id=<client_id>&redirect_uri=<applicationUri>&response_type=code&scope=<scopes>`

4.  The user enters login authorization information to the server web site.

5.  The server web site redirects to `<applicationUri>?code=<code>`

6.  Lookup the saved `App` instance using the Mastodon server's host name
    (which your top-level application must persist somewhere).
    Use that to POST to `<server_url>/oauth/token`:

        POST /oauth/token HTTP/1.1
        Host: <server_url>
        Authorization: Basic `(Base64.encode (<client_id> ++ ":" ++ <client_secret>))`
        Content-Type: application/x-www-form-urlencoded
        grant_type=authorization_code&code=<code>&redirect_uri=<applicationUri>&client_id=<client_id>&client_secret=<client_secret>

    The `Authorization` header isn't needed for new Mastodon API servers,
    but that's how it was done for old ones, so I'm hoping this will give
    some backward compatibility.

7.  Receive back token information, JSON-encoded:

        { "access_token":"cptLSO8ff7zKbBXlTTyH15bnxQS5b9erVUWi_n0_EGd",
          "token_type":"Bearer",
          "scope":"write,read,follow,push",
          "created_at":1561845912
        }

8.  Use the `token_type` and `access_token` to authenticate a request
    for the user's `Account`.

-}
loginTask : { client_name : String, server : String, applicationUri : String } -> Maybe String -> FetchAccountOrRedirect msg
loginTask { client_name, server, applicationUri } maybeToken =
    case maybeToken of
        Just token ->
            let
                request =
                    AccountsRequest GetVerifyCredentials

                rawRequest =
                    Request.requestToRawRequest []
                        { server = server, token = Just token }
                        request

                task =
                    Request.rawRequestToTask rawRequest
                        |> Task.mapError (\err -> ( server, err ))
                        |> Task.andThen
                            (\response ->
                                case response.entity of
                                    AccountEntity account ->
                                        Task.succeed ( server, token, account )

                                    _ ->
                                        Task.fail
                                            ( server
                                            , BadUrl "Shouldn't happen"
                                            )
                            )
            in
            FetchAccount task

        Nothing ->
            let
                postApp =
                    PostApp
                        { client_name = client_name
                        , redirect_uris = applicationUri
                        , scopes = scopes
                        , website = Nothing
                        }

                rawRequest =
                    Request.requestToRawRequest []
                        { server = server, token = Nothing }
                        (AppsRequest postApp)

                redirectTask =
                    Request.rawRequestToTask rawRequest
                        |> Task.mapError (\err -> ( server, err ))
                        |> Task.andThen
                            (\response ->
                                case response.entity of
                                    AppEntity app ->
                                        ( server
                                        , app
                                        , appToAuthorizeUrl server app
                                            |> Navigation.load
                                        )
                                            |> Task.succeed

                                    _ ->
                                        Task.fail
                                            ( server
                                            , BadUrl "Shouldn't happen"
                                            )
                            )
            in
            Redirect redirectTask


{-| The result type of the `loginTask` function.

It's either a task to fetch the logged-in user's `Mastodon.Entity.Account`
or a task to redirect to the authorization server to get a code.

The two `String`s in `FetchAccount` are the server name and token.
The `String` in `Redirect` is the server name.

-}
type FetchAccountOrRedirect msg
    = FetchAccount (Task ( String, Error ) ( String, String, Account ))
    | Redirect (Task ( String, Error ) ( String, App, Cmd msg ))


{-| Compute URL to redirect to for authentication.

Args are:

    appToAuthorizeUrl server app

Returns:

    https://<server>/oauth/authorize
       ?client_id=<app.client_id>
       &redirect_uri=<app.redirect_uri>
       &response_type=code
       &scope=<all scopes>

You will call this explicitly only when a token expires, and you need
to mint a new one from a previously created `App`:

    appToAuthorizeUrl server app
        |> Browser.Navigation.load

-}
appToAuthorizeUrl : String -> App -> String
appToAuthorizeUrl server app =
    Builder.crossOrigin
        ("https://" ++ server)
        [ "oauth", "authorize" ]
        [ Builder.string "client_id" app.client_id
        , Builder.string "redirect_uri" app.redirect_uri
        , Builder.string "response_type" "code"
        , Builder.string "scope" <| String.join " " scopes
        ]


{-| Continue after being restarted with a `code` and `state` in the URL query.

This continues from step 6 in the full login procedure description,
which is included with the documentation for `loginTask` above.

Your application will usually persist the `Authorization`, so you can use it
the next time the user starts the application, as a parameter to `loginTask`.

The `String` in the `Task` is the server name, e.g. "mastodon.social".

-}
getTokenTask : { code : String, server : String, app : App } -> Task ( String, Error ) ( String, Authorization, Account )
getTokenTask { code, server, app } =
    let
        { client_id, client_secret, redirect_uri } =
            app
    in
    Http.task
        { method = "POST"
        , headers =
            [ Http.header "Authorization" <|
                "Basic "
                    ++ (Base64.encode <| client_id ++ ":" ++ client_secret)
            ]
        , url =
            Builder.crossOrigin ("https://" ++ server)
                [ "oauth", "token" ]
                []
        , body =
            Http.stringBody "application/x-www-form-urlencoded" <|
                ([ Builder.string "grant_type" "authorization_code"
                 , Builder.string "client_id" client_id
                 , Builder.string "client_secret" client_secret
                 , Builder.string "redirect_uri" redirect_uri
                 , Builder.string "code" code
                 ]
                    |> Builder.toQuery
                    |> String.dropLeft 1
                )
        , resolver =
            Http.stringResolver <|
                receiveAuthorization client_id client_secret
        , timeout = Nothing
        }
        |> Task.mapError (\err -> ( server, err ))
        |> Task.andThen
            (\authorization ->
                AccountsRequest GetVerifyCredentials
                    |> Request.requestToRawRequest []
                        { server = server
                        , token = Just authorization.token
                        }
                    |> Request.rawRequestToTask
                    |> Task.mapError (\err -> ( server, err ))
                    |> Task.andThen
                        (\response ->
                            case response.entity of
                                AccountEntity account ->
                                    Task.succeed
                                        ( server, authorization, account )

                                _ ->
                                    Task.fail
                                        ( server
                                        , BadUrl "Wrong entity type."
                                        )
                        )
            )


type alias RawToken =
    { access_token : String
    , token_type : String
    , scope : String
    , created_at : Int
    }


rawTokenDecoder : Decoder RawToken
rawTokenDecoder =
    JD.map4 RawToken
        (JD.field "access_token" JD.string)
        (JD.field "token_type" JD.string)
        (JD.field "scope" JD.string)
        (JD.field "created_at" JD.int)


receiveAuthorization : String -> String -> Http.Response String -> Result Error Entity.Authorization
receiveAuthorization clientId clientSecret response =
    case response of
        Http.BadUrl_ s ->
            Err <| BadUrl s

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err <| BadStatus metadata body

        Http.GoodStatus_ metadata body ->
            case JD.decodeString rawTokenDecoder body of
                Err err ->
                    Err <| BadBody metadata err body

                Ok rawToken ->
                    Ok
                        { clientId = clientId
                        , clientSecret = clientSecret
                        , token =
                            rawToken.token_type
                                ++ " "
                                ++ rawToken.access_token
                        }
