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
    , tokenDecoder, encodeToken
    , applicationDecoder, encodeApplication
    )

{-| Encoders and Decoders for JSON that goes over the wire.

@docs encodeEntity, entityDecoder
@docs accountDecoder, encodeAccount
@docs sourceDecoder, encodeSource
@docs tokenDecoder, encodeToken
@docs applicationDecoder, encodeApplication

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
        , ImageMetaFields
        , ImageMetaInfo
        , Instance
        , ListEntity
        , Mention
        , Meta(..)
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
        , VideoMetaFields
        , VideoMetaInfo
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

        TokenEntity source ->
            encodeToken source

        ApplicationEntity application ->
            encodeApplication application

        AttachmentEntity attachment ->
            encodeAttachment attachment

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
        , tokenDecoder |> JD.map TokenEntity
        , applicationDecoder |> JD.map ApplicationEntity
        , attachmentDecoder |> JD.map AttachmentEntity
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


{-| Encode a `Token`.
-}
encodeToken : Token -> Value
encodeToken token =
    JE.object
        [ ( "access_token", JE.string token.access_token )
        , ( "token_type", JE.string token.token_type )
        , ( "scope", JE.string token.scope )
        , ( "created_at", JE.int token.created_at )
        ]


{-| Decode a `Token`.
-}
tokenDecoder : Decoder Token
tokenDecoder =
    JD.succeed Token
        |> required "access_token" JD.string
        |> required "token_type" JD.string
        |> required "scope" JD.string
        |> required "created_at" JD.int
        |> custom JD.value


{-| Encode an `Application`.
-}
encodeApplication : Application -> Value
encodeApplication application =
    JE.object
        [ ( "name", JE.string application.name )
        , ( "website", encodeMaybe JE.string application.website )
        ]


{-| Decode an `Application`.
-}
applicationDecoder : Decoder Application
applicationDecoder =
    JD.succeed Application
        |> required "name" JD.string
        |> optional "website" (JD.nullable JD.string) Nothing
        |> custom JD.value


encodeAttachmentType : AttachmentType -> Value
encodeAttachmentType attachmentType =
    case attachmentType of
        UnknownAttachment ->
            JE.string "UnknownAttachment"

        ImageAttachment ->
            JE.string "ImageAttachment"

        GifvAttachment ->
            JE.string "GifvAttachment"

        VideoAttachment ->
            JE.string "VideoAttachment"


attachmentTypeDecoder : Decoder AttachmentType
attachmentTypeDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "UnknownAttachment" ->
                        JD.succeed UnknownAttachment

                    "ImageAttachment" ->
                        JD.succeed ImageAttachment

                    "GifvAttachment" ->
                        JD.succeed GifvAttachment

                    "VideoAttachment" ->
                        JD.succeed VideoAttachment

                    _ ->
                        JD.fail <| "Unknown AttachmentType: " ++ s
            )


encodeMeta : Meta -> Value
encodeMeta meta =
    case meta of
        ImageMeta fields ->
            encodeImageMetaFields fields

        VideoMeta fields ->
            encodeVideoMetaFields fields


metaDecoder : AttachmentType -> Decoder (Maybe Meta)
metaDecoder attachmentType =
    JD.oneOf
        [ JD.null Nothing
        , case attachmentType of
            UnknownAttachment ->
                JD.succeed Nothing

            ImageAttachment ->
                JD.map (Just << ImageMeta) imageMetaFieldsDecoder

            _ ->
                JD.map (Just << VideoMeta) videoMetaFieldsDecoder
        ]


encodeImageMetaFields : ImageMetaFields -> Value
encodeImageMetaFields fields =
    JE.object
        [ ( "small", encodeMaybe encodeImageMetaInfo fields.small )
        , ( "original", encodeMaybe encodeImageMetaInfo fields.original )
        , ( "focus", encodeMaybe encodeFocus fields.focus )
        ]


imageMetaFieldsDecoder : Decoder ImageMetaFields
imageMetaFieldsDecoder =
    JD.succeed ImageMetaFields
        |> optional "small" (JD.nullable imageMetaInfoDecoder) Nothing
        |> optional "original" (JD.nullable imageMetaInfoDecoder) Nothing
        |> optional "focus" (JD.nullable focusDecoder) Nothing


encodeImageMetaInfo : ImageMetaInfo -> Value
encodeImageMetaInfo { width, height, size, aspect } =
    JE.object
        [ ( "width", encodeMaybe JE.int width )
        , ( "height", encodeMaybe JE.int height )
        , ( "size", encodeMaybe JE.int size )
        , ( "aspect", encodeMaybe JE.float aspect )
        ]


imageMetaInfoDecoder : Decoder ImageMetaInfo
imageMetaInfoDecoder =
    JD.succeed ImageMetaInfo
        |> optional "width" (JD.nullable JD.int) Nothing
        |> optional "height" (JD.nullable JD.int) Nothing
        |> optional "size" (JD.nullable JD.int) Nothing
        |> optional "aspect" (JD.nullable JD.float) Nothing


encodeVideoMetaFields : VideoMetaFields -> Value
encodeVideoMetaFields fields =
    JE.object
        [ ( "small", encodeMaybe encodeVideoMetaInfo fields.small )
        , ( "original", encodeMaybe encodeVideoMetaInfo fields.original )
        , ( "focus", encodeMaybe encodeFocus fields.focus )
        ]


videoMetaFieldsDecoder : Decoder VideoMetaFields
videoMetaFieldsDecoder =
    JD.succeed VideoMetaFields
        |> optional "small" (JD.nullable videoMetaInfoDecoder) Nothing
        |> optional "original" (JD.nullable videoMetaInfoDecoder) Nothing
        |> optional "focus" (JD.nullable focusDecoder) Nothing


encodeVideoMetaInfo : VideoMetaInfo -> Value
encodeVideoMetaInfo { width, height, frame_rate, duration, bitrate } =
    JE.object
        [ ( "width", encodeMaybe JE.int width )
        , ( "height", encodeMaybe JE.int height )
        , ( "frame_rate", encodeMaybe JE.int frame_rate )
        , ( "duration", encodeMaybe JE.float duration )
        , ( "bitrate", encodeMaybe JE.int bitrate )
        ]


videoMetaInfoDecoder : Decoder VideoMetaInfo
videoMetaInfoDecoder =
    JD.succeed VideoMetaInfo
        |> optional "width" (JD.nullable JD.int) Nothing
        |> optional "height" (JD.nullable JD.int) Nothing
        |> optional "frame_rate" (JD.nullable JD.int) Nothing
        |> optional "duration" (JD.nullable JD.float) Nothing
        |> optional "bitrate" (JD.nullable JD.int) Nothing


encodeFocus : Focus -> Value
encodeFocus { x, y } =
    JE.object
        [ ( "x", JE.float x )
        , ( "y", JE.float y )
        ]


focusDecoder : Decoder Focus
focusDecoder =
    JD.succeed Focus
        |> required "x" JD.float
        |> required "y" JD.float


{-| Encode an `Attachment`.
-}
encodeAttachment : Attachment -> Value
encodeAttachment attachment =
    JE.object
        [ ( "id", JE.string attachment.id )
        , ( "type_", encodeAttachmentType attachment.type_ )
        , ( "url", JE.string attachment.url )
        , ( "remote_url", encodeMaybe JE.string attachment.remote_url )
        , ( "preview_url", JE.string attachment.preview_url )
        , ( "text_url", encodeMaybe JE.string attachment.text_url )
        , ( "meta", encodeMaybe encodeMeta attachment.meta )
        , ( "description", JE.string attachment.description )
        ]


{-| Decode an `Attachment`.
-}
attachmentDecoder : Decoder Attachment
attachmentDecoder =
    JD.field "type_" attachmentTypeDecoder
        |> JD.andThen
            (\type_ ->
                JD.succeed Attachment
                    |> required "id" JD.string
                    |> required "type_" (JD.succeed type_)
                    |> required "url" JD.string
                    |> optional "remote_url" (JD.nullable JD.string) Nothing
                    |> required "preview_url" JD.string
                    |> optional "text_url" (JD.nullable JD.string) Nothing
                    |> optional "meta" (metaDecoder type_) Nothing
                    |> required "description" JD.string
                    |> custom JD.value
            )
