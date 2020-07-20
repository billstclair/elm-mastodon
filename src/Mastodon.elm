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
    )

{-| Talk to the Mastodon API.

This module contains re-exposed versions of the most-used parts of
`Mastodon.Entity` and `Mastodon.Request`.

To authorize access, you need to call the functions in `Mastodon.Login`.

For API documentation, see <https://docs.joinmastodon.org/client/>


# Types

@docs ServerInfo, Request, Response, Error


# Creating an HTTP REST request.

@docs serverRequest

-}

import Http
import Mastodon.Entity as Entity
import Mastodon.Request as Request


{-| Used to create the HTTP URL and fill in its authentication token.

It's the host name for the URL.

Example `server`: "mastodon.social"

A few requests do not require a token. Most do, and will error if you don't include one.

Copy of `Mastodon.Request.ServerInfo`.

-}
type alias ServerInfo =
    { server : String
    , token : Maybe String
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
