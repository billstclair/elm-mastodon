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
    ( Version
    , version, versionToString
    )

{-| Talk to the Mastodon API.

See <https://docs.joinmastodon.org/api/guidelines/>


## Types

@docs Version


## Version

@docs version, versionToString

-}


{-| Represent a software version
-}
type alias Version =
    { major : Int
    , minor : Int
    , patch : Int
    }


{-| Convert a `Version` to a `String`.
-}
versionToString : Version -> String
versionToString v =
    [ v.major, v.minor, v.patch ]
        |> List.map String.fromInt
        |> List.intersperse "."
        |> String.concat


{-| Our version number.
-}
version : Version
version =
    Version 1 0 0
