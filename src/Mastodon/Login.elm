module Mastodon.Login exposing
    ( FetchAccountOrRedirect(..), loginTask, getTokenTask
    , encodeStateString, decodeStateString, appToAuthorizeUrl
    )

{-| Support for creating an `App` and logging in to a server.

@docs FetchAccountOrRedirect, loginTask, getTokenTask


# Internal functions.

@docs encodeStateString, decodeStateString, appToAuthorizeUrl

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

Returns a `Task` to either fetch an `Account` for a known authorization token,
or to redirect to the authentication server to create a code with which
to mint the token.

If `maybeAuthorization` is not `Nothing`, will attempt to get an
`Account` using `.authorization` from there. If that fails, will
attempt to mint a new token, using `authorization.clientId` and
`authorization.clientSecret`. If that fails, or if
`maybeAuthorization` is `Nothing`, will create a new `App`, and
redirect to do authentication. When the application is restarted, with
a `code` and `state` in the URL query, call `getTokenTask` to
use those to mint a new token, and to use that to get an `Account`.

Usually, you will get an `Authorization` from persistent
`localStorage`, pass that here, and successfully receive the `Account`
back. No redirects necessary. But the redirects must happen at least
once, and then whenever the authorization token expires. This is step
8 below. Only if that fails will this do step 3, or, if there was no
persisted client id and secret, steps 1 through 3.

Currently, permission for all scopes is requested. Maybe the scopes should
be passed as a parameter.

The full login procedure is as follows:

1.  Send "POST /api/v1/apps" to the Mastodon server, via a `PostApp` request.

2.  Receive the returned `client_id` and `client_secret`

3.  Redirect to `<server_url>/oauth/authorize?client_id=<client_id>&redirect_uri=<client_uri>&response_type=code&scope=<scopes>&state=<state>`

4.  The user enters login authorization information to the server web site.

5.  The server web site redirects to `<redirect_uri>?code=<code>&state=<state>`

6.  The `<state>` encodes the server uri, `<client_id>`, `<client_secret>`, and `<scopes>`.
    Use that to POST to `<server_url>/oauth/token`:

        POST /oauth/token HTTP/1.1
        Host: <server_url>
        Authorization: Basic `(Base64.encode (<client_id> ++ ":" ++ <client_secret>))`
        Content-Type: application/x-www-form-urlencoded

        grant_type=authorization_code&code=<code>&redirect_uri=<client_uri>

7.  Receive back token information, JSON-encoded:

        { "access_token":"cptLSO8ff7zKbBXlTTyH15bnxQS5b9erVUWi_n0_EGd",
          "token_type":"Bearer",
          "scope":"write,read,follow,push"
          "created_at":1561845912
        }

8.  Use the `token_type` and `access_token` to authenticate a request
    for the user's `Account`.

-}
loginTask : { client_name : String, server : String, applicationUri : String } -> Maybe Authorization -> FetchAccountOrRedirect msg
loginTask { client_name, server, applicationUri } maybeAuthorization =
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
                                        Task.succeed ( server, account )

                                    _ ->
                                        Task.fail <| BadUrl "Shouldn't happen"
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
                        { server = server, authorization = Nothing }
                        (AppsRequest postApp)

                redirectTask =
                    Request.rawRequestToTask rawRequest
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
                                        Task.fail (BadUrl "Shouldn't happen")
                            )
            in
            Redirect redirectTask


{-| The result type of the `loginTask` function.

It's either a task to fetch the logged-in user's `Mastodon.Entity.Account`
or a task to redirect to the authorization server to get a code.

The `String` parts of the successful results are the server domain.

-}
type FetchAccountOrRedirect msg
    = FetchAccount (Task Error ( String, Account ))
    | Redirect (Task Error ( String, App, Cmd msg ))


{-| Compute URL to redirect to for authentication.

Args are:

    appToAuthorizeUrl server app

Returns:

    https://<server>/oauth/authorize
       ?client_id=<app.client_id>
       &redirect_uri=<app.redirect_uri>
       &response_type=code
       &scope=<all scopes>
       &state=<encodeStateString server app>

You will rarely, if ever, call this explicitly It is exposed for debugging.

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
        , Builder.string "state" <| encodeStateString server app
        ]


{-| Convert a server domain and a `Mastodon.Entity.App` into a `state` string.

You will rarely, if ever, call this explicitly. It is exposed for debugging.

-}
encodeStateString : String -> App -> String
encodeStateString server app =
    JE.object
        [ ( "server", JE.string server )
        , ( "app", ED.encodeApp app )
        ]
        |> JE.encode 0
        |> Base64.encode


{-| Decode the string created by `encodeStateString`.

You will rarely, if ever, call this explicitly. It is exposed for debugging.

-}
decodeStateString : String -> Result String ( String, App )
decodeStateString string =
    case Base64.decode string of
        Err err ->
            Err err

        Ok s ->
            case JD.decodeString stateStringDecoder s of
                Err err ->
                    Err <| JD.errorToString err

                Ok res ->
                    Ok res


stateStringDecoder : Decoder ( String, App )
stateStringDecoder =
    JD.map2 (\server app -> ( server, app ))
        (JD.field "server" JD.string)
        (JD.field "app" ED.appDecoder)


{-| Continue after being restarted with a `code` and `state` in the URL query.

This continues from step 6 in the full login procedure description,
which is included with the documentation for `loginTask` above.

Your application will usually persist the `Authorization`, so you can use it
the next time the user starts the application, as a parameter to `loginTask`.

-}
getTokenTask : { code : String, state : String } -> Task Error ( Authorization, Account )
getTokenTask { code, state } =
    case decodeStateString state of
        Err _ ->
            Task.fail <| BadUrl "Cannot decode <state> from authentication server."

        Ok ( server, app ) ->
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
                |> Task.andThen
                    (\authorization ->
                        AccountsRequest GetVerifyCredentials
                            |> Request.requestToRawRequest []
                                { server = server
                                , authorization = Just authorization.authorization
                                }
                            |> Request.rawRequestToTask
                            |> Task.andThen
                                (\response ->
                                    case response.entity of
                                        AccountEntity account ->
                                            Task.succeed ( authorization, account )

                                        _ ->
                                            Task.fail <| BadUrl "Wrong entity type."
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
                    Err <| BadBody metadata (JD.errorToString err)

                Ok rawToken ->
                    Ok
                        { clientId = clientId
                        , clientSecret = clientSecret
                        , authorization =
                            rawToken.token_type
                                ++ " "
                                ++ rawToken.access_token
                        }
