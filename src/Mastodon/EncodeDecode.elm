----------------------------------------------------------------------
--
-- EncodeDecode.elm
-- JSON Encoders and Decoders
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Mastodon.EncodeDecode exposing
    ( encodeEntity, entityDecoder
    , accountDecoder, encodeAccount
    , sourceDecoder, encodeSource
    )

{-| Encoders and Decoders for JSON that goes over the wire.

@docs encodeEntity, entityDecoder
@docs accountDecoder, encodeAccount
@docs sourceDecoder, encodeSource

-}

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, optional, required)
import Json.Encode as JE exposing (Value)
import Mastodon.Entities as Entities
    exposing
        ( Account
        , Application
        , Attachment
        , AttachmentType(..)
        , Card
        , CardType(..)
        , Context
        , Conversation
        , Emoji
        , Entity(..)
        , Error
        , Field
        , Filter
        , FilterContext(..)
        , Focus
        , History
        , Instance
        , ListEntity
        , Mention
        , Meta
        , MetaInfo(..)
        , Notification
        , NotificationType(..)
        , Poll
        , PollOption
        , PushSubscription
        , Relationship
        , Results
        , ScheduledStatus
        , Source
        , Stats
        , Status
        , StatusParams
        , Tag
        , Token
        , URLs
        , Visibility(..)
        , WrappedAccount(..)
        , WrappedStatus(..)
        )


{-| Encode an `Entity` into a `Value`.
-}
encodeEntity : Entity -> Value
encodeEntity entity =
    case entity of
        AccountEntity account ->
            encodeAccount account

        SourceEntity source ->
            encodeSource source

        _ ->
            JE.string "TODO"


{-| Decode an `Entity`.

You'll usually know which entity you're looking for, and will use
its decoder explicitly. In case you don't...

-}
entityDecoder : Decoder Entity
entityDecoder =
    JD.oneOf
        [ accountDecoder |> JD.map AccountEntity
        , sourceDecoder |> JD.map SourceEntity
        ]


encodeMaybe : (x -> Value) -> Maybe x -> Value
encodeMaybe encoder mx =
    case mx of
        Nothing ->
            JE.null

        Just x ->
            encoder x


unwrapAccount : WrappedAccount -> Account
unwrapAccount (WrappedAccount account) =
    account


unwrapStatus : WrappedStatus -> Status
unwrapStatus (WrappedStatus status) =
    status


{-| Encode an `Account`.
-}
encodeAccount : Account -> Value
encodeAccount account =
    JE.object
        [ ( "id", JE.string account.id )
        , ( "username", JE.string account.username )
        , ( "acct", JE.string account.acct )
        , ( "display_name", JE.string account.display_name )
        , ( "locked", JE.bool account.locked )
        , ( "created_at", JE.string account.created_at )
        , ( "followers_count", JE.int account.followers_count )
        , ( "following_count", JE.int account.following_count )
        , ( "statuses_count", JE.int account.statuses_count )
        , ( "note", JE.string account.note )
        , ( "url", JE.string account.url )
        , ( "avatar", JE.string account.avatar )
        , ( "avatar_static", JE.string account.avatar_static )
        , ( "header", JE.string account.header )
        , ( "header_static", JE.string account.header_static )
        , ( "emojis", JE.list encodeEmoji account.emojis )
        , ( "moved", encodeMaybe (unwrapAccount >> encodeAccount) account.moved )
        , ( "fields", JE.list encodeField account.fields )
        , ( "bot", JE.bool account.bot )
        ]


justSucceed : a -> Decoder (Maybe a)
justSucceed a =
    JD.succeed <| Just a


{-| Decode an `Account`.
-}
accountDecoder : Decoder Account
accountDecoder =
    JD.succeed Account
        |> required "id" JD.string
        |> required "username" JD.string
        |> required "acct" JD.string
        |> required "display_name" JD.string
        |> required "locked" JD.bool
        |> required "created_at" JD.string
        |> required "followers_count" JD.int
        |> required "following_count" JD.int
        |> required "statuses_count" JD.int
        |> required "note" JD.string
        |> required "url" JD.string
        |> required "avatar" JD.string
        |> required "avatar_static" JD.string
        |> required "header" JD.string
        |> required "header_static" JD.string
        |> required "emojis" (JD.list emojiDecoder)
        |> optional "moved"
            (JD.lazy
                (\() ->
                    JD.nullable
                        (accountDecoder |> JD.map WrappedAccount)
                )
            )
            Nothing
        |> optional "fields"
            (JD.list fieldDecoder)
            []
        |> optional "bot" JD.bool False
        |> custom JD.value


{-| Encode an `Emoji`.
-}
encodeEmoji : Emoji -> Value
encodeEmoji emoji =
    JE.object
        [ ( "shortcode", JE.string emoji.shortcode )
        , ( "static_url", JE.string emoji.static_url )
        , ( "url", JE.string emoji.url )
        , ( "visible_in_picker", JE.bool emoji.visible_in_picker )
        ]


{-| Decode an `Emoji`.
-}
emojiDecoder : Decoder Emoji
emojiDecoder =
    JD.succeed Emoji
        |> required "shortcode" JD.string
        |> required "static_url" JD.string
        |> required "url" JD.string
        |> required "visible_in_picker" JD.bool


{-| Encode a `Field`.
-}
encodeField : Field -> Value
encodeField field =
    JE.object
        [ ( "name", JE.string field.name )
        , ( "value", JE.string field.value )
        , ( "verified_at", encodeMaybe JE.string field.verified_at )
        ]


{-| Decode a `Field`.
-}
fieldDecoder : Decoder Field
fieldDecoder =
    JD.succeed Field
        |> required "name" JD.string
        |> required "value" JD.string
        |> optional "verified_at" (JD.nullable JD.string) Nothing


{-| Encode a `Source`.
-}
encodeSource : Source -> Value
encodeSource source =
    JE.object
        [ ( "privacy", encodeMaybe JE.string source.privacy )
        , ( "sensitive", JE.bool source.sensitive )
        , ( "language", encodeMaybe JE.string source.language )
        , ( "note", JE.string source.note )
        , ( "fields", JE.list encodeField source.fields )
        ]


{-| Decode a `Source`.
-}
sourceDecoder : Decoder Source
sourceDecoder =
    JD.succeed Source
        |> optional "privacy" (JD.nullable JD.string) Nothing
        |> optional "sensitive" JD.bool False
        |> optional "language" (JD.nullable JD.string) Nothing
        |> required "note" JD.string
        |> required "fields" (JD.list fieldDecoder)
        |> custom JD.value
