----------------------------------------------------------------------
--
-- Mastodon.elm
-- An Elm client for the Mastodon social networking system.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Mastodon exposing
    ( ServerInfo, Request, Response, Error
    , serverRequest
    , Authorization, Account, login
    )

{-| Talk to the Mastodon API.

See <https://docs.joinmastodon.org/api/guidelines/>


# Types

@docs ServerInfo, Request, Response, Error


# Creating HTTP RESS requests

@docs serverRequest


# Logging in

@docs Authorization, Account, login

-}

import Http
import Mastodon.Entity as Entity
import Mastodon.Request as Request
import OAuthMiddleware exposing (ResponseToken)


{-| Used to create the HTTP URL and fill in its authentication token.

It's the host name for the URL.

Example `server`: "mastodon.social"

A few requests do not require a token. Most do, and will error if you don't include one.

Copy of `Mastodon.Request.ServerInfo`.

-}
type alias ServerInfo =
    { server : String
    , token : Maybe ResponseToken
    }


{-| A request for the REST API.

An alias of `Mastodon.Request.Request`. See that module's documentation for details.

-}
type alias Request =
    Request.Request


{-| A response from the REST API.

An alias of `Mastodon.Request.Response`. See that module's documentation for details.

-}
type alias Response =
    Request.Response


{-| An error from the REST API.

Same as Http.Error, but includes `Http.Metadata` when it's available.

An alias of `Mastodon.Request.Error`. See that module's documentation for details.

-}
type alias Error =
    Request.Error


{-| Create a request for the REST API.

The `id` is whatever will help you to identify the response, if the `Request` and the `Response` don't suffice.

This is a copy of `Mastodon.Request.serverRequest`.

There is a low-level interface for requests in `Mastodon.Request`.

-}
serverRequest : (id -> Result Error Response -> msg) -> List Http.Header -> ServerInfo -> id -> Request -> Cmd msg
serverRequest =
    Request.serverRequest


{-| Authorization Parameters.

Applications will usually save this in `localStorage`, use the saved token
until it expires, then use the client ID and secret to mint a new token.

Copy of `Mastodon.Entity.Authorization`

-}
type alias Authorization =
    { clientId : String
    , clientSecret : String
    , token : String
    }


{-| A Mastodon account.

Alias of `Mastodon.Entity.Account`. See that module's documentation for details.

-}
type alias Account =
    Entity.Account


{-| Get a token and, if possible, the logged in `Account` from the server.

Arguments are:

    login tagger server maybeAuthorization

If `maybeAuthorization` is not `Nothing`, will attempt to get an
`Account` using `authorization.token` from there. If that fails, will
attempt to mint a new token, using `authorization.clientId` and
`authorization.clientSecret`. If that fails, or if
`maybeAuthorization` is `Nothing`, will create a new `App` first.
If it succeeds in getting a token, the tagged `Result` will be `Ok`,
but its `Maybe Account` component may be `Nothing`, if that could not
be fetched with the token.

Usually, you will get an `Authorization` from persistent `localStorage`,
pass that here, and successfully receive the `Authorization` back, along
with the logged-in users's `Account`.

-}
login : (Result Error ( Authorization, Maybe Account ) -> msg) -> String -> Maybe Authorization -> Cmd msg
login tagger server authorization =
    -- TODO
    Cmd.none
