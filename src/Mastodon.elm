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
import Mastodon.Login as Login
import Mastodon.Request as Request


{-| Used to create the HTTP URL and fill in its authentication token.

It's the host name for the URL.

Example `server`: "mastodon.social"

A few requests do not require a token. Most do, and will error if you don't include one.

Copy of `Mastodon.Request.ServerInfo`.

-}
type alias ServerInfo =
    { server : String
    , authorization : Maybe String
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
    , authorization : String
    }


{-| A Mastodon account.

Alias of `Mastodon.Entity.Account`. See that module's documentation for details.

-}
type alias Account =
    Entity.Account


{-| Do everything necessary to login to a server.

The `String` arg is a server url, e.g. "mastodon.social".

See `Login.login` for details.

-}
login : (Result Error ( Authorization, Maybe Account ) -> msg) -> String -> Maybe Authorization -> Cmd msg
login =
    Login.login
