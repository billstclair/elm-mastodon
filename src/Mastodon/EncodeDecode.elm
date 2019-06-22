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


module Mastodon.EncodeDecode exposing (accountDecoder, encodeAccount)

{-| Encoders and Decoders for JSON that goes over the wire.

@docs accountDecoder, encodeAccount

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
                    accountDecoder
                        |> JD.andThen (WrappedAccount >> justSucceed)
                )
            )
            Nothing
        |> optional "fields"
            (JD.list fieldDecoder)
            []
        |> optional "bot" JD.bool False
        |> custom JD.value


encodeEmoji : Emoji -> Value
encodeEmoji emoji =
    JE.null


emojiDecoder : Decoder Emoji
emojiDecoder =
    JD.fail "TODO"


encodeField : Field -> Value
encodeField field =
    JE.null


fieldDecoder : Decoder Field
fieldDecoder =
    JD.fail "TODO"
