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
    ( encodeEntity, entityDecoder, entityValue
    , accountDecoder, encodeAccount
    , fieldDecoder, encodeField
    , appDecoder, encodeApp
    , sourceDecoder, encodeSource
    , tokenDecoder, encodeToken
    , applicationDecoder, encodeApplication
    , attachmentDecoder, encodeAttachment
    , cardDecoder, encodeCard
    , contextDecoder, encodeContext
    , visibilityDecoder, encodeVisibility
    , emojiDecoder, encodeEmoji
    , encodeStatus, statusDecoder
    , encodeError, errorDecoder
    , encodePoll, pollDecoder
    , encodeFilter, filterDecoder
    , encodeFilterContext, filterContextDecoder
    , encodeInstance, instanceDecoder
    , encodeListEntity, listEntityDecoder
    , encodeNotification, notificationDecoder
    , encodeNotificationType, notificationTypeDecoder
    , encodePushSubscription, pushSubscriptionDecoder
    , encodeRelationship, relationshipDecoder
    , encodeResults, resultsDecoder
    , encodeScheduledStatus, scheduledStatusDecoder
    , encodeConversation, conversationDecoder
    , encodeGroup, groupDecoder
    , encodeTag, tagDecoder
    , encodeAuthorization, authorizationDecoder
    , encodeMaybe, privacyToString
    )

{-| Encoders and Decoders for JSON that goes over the wire.

You will rarely use any of these. Encoding and decoding will usually
be done for you by by `Mastodon.Requests.requestToRawRequest`, which
your code will call indirectly via `Mastodon.Requests.serverRequest`.


# For the one type to rule them all

@docs encodeEntity, entityDecoder, entityValue


# For the individual entity types

@docs accountDecoder, encodeAccount
@docs fieldDecoder, encodeField
@docs appDecoder, encodeApp
@docs sourceDecoder, encodeSource
@docs tokenDecoder, encodeToken
@docs applicationDecoder, encodeApplication
@docs attachmentDecoder, encodeAttachment
@docs cardDecoder, encodeCard
@docs contextDecoder, encodeContext
@docs visibilityDecoder, encodeVisibility
@docs emojiDecoder, encodeEmoji
@docs encodeStatus, statusDecoder
@docs encodeError, errorDecoder
@docs encodePoll, pollDecoder
@docs encodeFilter, filterDecoder
@docs encodeFilterContext, filterContextDecoder
@docs encodeInstance, instanceDecoder
@docs encodeListEntity, listEntityDecoder
@docs encodeNotification, notificationDecoder
@docs encodeNotificationType, notificationTypeDecoder
@docs encodePushSubscription, pushSubscriptionDecoder
@docs encodeRelationship, relationshipDecoder
@docs encodeResults, resultsDecoder
@docs encodeScheduledStatus, scheduledStatusDecoder
@docs encodeConversation, conversationDecoder
@docs encodeGroup, groupDecoder
@docs encodeTag, tagDecoder


# Encoder and decoder for the login parameters.

@docs encodeAuthorization, authorizationDecoder


# Utilities

@docs encodeMaybe, privacyToString

-}

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import Mastodon.Entity as Entity
    exposing
        ( Account
        , App
        , Application
        , Attachment
        , AttachmentType(..)
        , Authorization
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
        , Group
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
        , Privacy(..)
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

        AppEntity app ->
            encodeApp app

        AccountListEntity accounts ->
            JE.list encodeAccount accounts

        SourceEntity source ->
            encodeSource source

        TokenEntity token ->
            encodeToken token

        ApplicationEntity application ->
            encodeApplication application

        CardEntity card ->
            encodeCard card

        ContextEntity context ->
            encodeContext context

        EmojiEntity emoji ->
            encodeEmoji emoji

        EmojiListEntity emojis ->
            JE.list encodeEmoji emojis

        StatusEntity status ->
            encodeStatus status

        PollEntity poll ->
            encodePoll poll

        StatusListEntity statuses ->
            JE.list encodeStatus statuses

        FilterEntity filter ->
            encodeFilter filter

        FilterListEntity filters ->
            JE.list encodeFilter filters

        InstanceEntity instance ->
            encodeInstance instance

        ListEntityEntity list ->
            encodeListEntity list

        NotificationEntity notification ->
            encodeNotification notification

        PushSubscriptionEntity pushSubscription ->
            encodePushSubscription pushSubscription

        RelationshipEntity relationship ->
            encodeRelationship relationship

        RelationshipListEntity relationships ->
            JE.list encodeRelationship relationships

        ResultsEntity results ->
            encodeResults results

        ScheduledStatusEntity scheduledStatus ->
            encodeScheduledStatus scheduledStatus

        ConversationEntity conversation ->
            encodeConversation conversation

        ConversationListEntity conversations ->
            JE.list encodeConversation conversations

        GroupEntity conversation ->
            encodeGroup conversation

        GroupListEntity conversation ->
            JE.list encodeGroup conversation

        TagListEntity tags ->
            JE.list encodeTag tags

        StringListEntity stringList ->
            JE.list JE.string stringList

        ValueEntity value ->
            value

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
        , appDecoder |> JD.map AppEntity
        , JD.list accountDecoder |> JD.map AccountListEntity
        , sourceDecoder |> JD.map SourceEntity
        , tokenDecoder |> JD.map TokenEntity
        , applicationDecoder |> JD.map ApplicationEntity
        , cardDecoder |> JD.map CardEntity
        , contextDecoder |> JD.map ContextEntity
        , emojiDecoder |> JD.map EmojiEntity
        , JD.list emojiDecoder |> JD.map EmojiListEntity
        , statusDecoder |> JD.map StatusEntity
        , pollDecoder |> JD.map PollEntity
        , JD.list statusDecoder |> JD.map StatusListEntity
        , filterDecoder |> JD.map FilterEntity
        , JD.list filterDecoder |> JD.map FilterListEntity
        , instanceDecoder |> JD.map InstanceEntity
        , groupDecoder |> JD.map GroupEntity -- Must come before ListEntity
        , JD.list groupDecoder |> JD.map GroupListEntity
        , listEntityDecoder |> JD.map ListEntityEntity
        , notificationDecoder |> JD.map NotificationEntity
        , pushSubscriptionDecoder |> JD.map PushSubscriptionEntity
        , relationshipDecoder |> JD.map RelationshipEntity
        , JD.list relationshipDecoder |> JD.map RelationshipListEntity
        , resultsDecoder |> JD.map ResultsEntity
        , scheduledStatusDecoder |> JD.map ScheduledStatusEntity
        , conversationDecoder |> JD.map ConversationEntity
        , JD.list tagDecoder |> JD.map TagListEntity
        , JD.list JD.string |> JD.map StringListEntity
        ]


{-| Similar to `encodeEntity`, but returns the `v` field, if it is non-null,

giving you the raw data that came over the wire, if this entity DID come over
the wire, and is not one of the few that don't have a `v` field.

-}
entityValue : Entity -> Value
entityValue entity =
    case entity of
        NoEntity ->
            JE.null

        AccountEntity account ->
            if account.v == JE.null then
                encodeAccount account

            else
                account.v

        AppEntity app ->
            if app.v == JE.null then
                encodeApp app

            else
                app.v

        AccountListEntity accounts ->
            JE.list entityValue (List.map AccountEntity accounts)

        SourceEntity source ->
            if source.v == JE.null then
                encodeSource source

            else
                source.v

        TokenEntity token ->
            if token.v == JE.null then
                encodeToken token

            else
                token.v

        ApplicationEntity application ->
            if application.v == JE.null then
                encodeApplication application

            else
                application.v

        CardEntity card ->
            if card.v == JE.null then
                encodeCard card

            else
                card.v

        ContextEntity context ->
            encodeContext context

        EmojiEntity emoji ->
            encodeEmoji emoji

        EmojiListEntity emojis ->
            JE.list encodeEmoji emojis

        StatusEntity status ->
            if status.v == JE.null then
                encodeStatus status

            else
                status.v

        PollEntity poll ->
            if poll.v == JE.null then
                encodePoll poll

            else
                poll.v

        StatusListEntity statuses ->
            JE.list entityValue (List.map StatusEntity statuses)

        FilterEntity filter ->
            if filter.v == JE.null then
                encodeFilter filter

            else
                filter.v

        FilterListEntity filters ->
            JE.list entityValue (List.map FilterEntity filters)

        InstanceEntity instance ->
            if instance.v == JE.null then
                encodeInstance instance

            else
                instance.v

        ListEntityEntity list ->
            encodeListEntity list

        NotificationEntity notification ->
            if notification.v == JE.null then
                encodeNotification notification

            else
                notification.v

        PushSubscriptionEntity pushSubscription ->
            if pushSubscription.v == JE.null then
                encodePushSubscription pushSubscription

            else
                pushSubscription.v

        RelationshipEntity relationship ->
            if relationship.v == JE.null then
                encodeRelationship relationship

            else
                relationship.v

        RelationshipListEntity relationships ->
            JE.list entityValue (List.map RelationshipEntity relationships)

        ResultsEntity results ->
            if results.v == JE.null then
                encodeResults results

            else
                results.v

        ScheduledStatusEntity scheduledStatus ->
            if scheduledStatus.v == JE.null then
                encodeScheduledStatus scheduledStatus

            else
                scheduledStatus.v

        ConversationEntity conversation ->
            if conversation.v == JE.null then
                encodeConversation conversation

            else
                conversation.v

        ConversationListEntity conversations ->
            JE.list entityValue (List.map ConversationEntity conversations)

        GroupEntity group ->
            if group.v == JE.null then
                encodeGroup group

            else
                group.v

        GroupListEntity groups ->
            JE.list entityValue (List.map GroupEntity groups)

        TagListEntity tags ->
            JE.list encodeTag tags

        StringListEntity stringList ->
            JE.list JE.string stringList

        ValueEntity value ->
            value

        _ ->
            JE.string "TODO"


{-| Encode `Maybe x` to `Json.Encode.null` if it's Nothing,

or with the encoder otherwise.

-}
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
    JE.object <|
        List.concat
            [ [ ( "id", JE.string account.id )
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
            , case account.source of
                Nothing ->
                    []

                Just source ->
                    [ ( "source", encodeSource source ) ]
            , if
                account.is_pro
                    || account.is_verified
                    || account.is_donor
                    || account.is_investor
              then
                [ ( "is_pro", JE.bool account.is_pro )
                , ( "is_verified", JE.bool account.is_verified )
                , ( "is_donor", JE.bool account.is_donor )
                , ( "is_investor", JE.bool account.is_investor )
                ]

              else
                []
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
        |> optional "source" (JD.nullable sourceDecoder) Nothing
        |> optional "is_pro" optionalBoolDecoder False
        |> optional "is_verified" optionalBoolDecoder False
        |> optional "is_donor" optionalBoolDecoder False
        |> optional "is_investor" optionalBoolDecoder False
        |> custom JD.value


{-| Encode an `App`.
-}
encodeApp : App -> Value
encodeApp app =
    JE.object
        [ ( "id", JE.string app.id )
        , ( "name", JE.string app.name )
        , ( "website", encodeMaybe JE.string app.website )
        , ( "redirect_uri", JE.string app.redirect_uri )
        , ( "client_id", JE.string app.client_id )
        , ( "client_secret", JE.string app.client_secret )
        , ( "vapid_key", encodeMaybe JE.string app.vapid_key )
        ]


{-| Decode an `App`.
-}
appDecoder : Decoder App
appDecoder =
    JD.succeed App
        |> required "id" JD.string
        |> required "name" JD.string
        |> optional "website" (JD.nullable JD.string) Nothing
        |> required "redirect_uri" JD.string
        |> required "client_id" JD.string
        |> required "client_secret" JD.string
        |> optional "vapid_key" (JD.nullable JD.string) Nothing
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


{-| Convert `Privacy` to a `String`.
-}
privacyToString : Privacy -> String
privacyToString privacy =
    case privacy of
        PublicPrivacy ->
            "public"

        UnlistedPrivacy ->
            "unlisted"

        PrivatePrivacy ->
            "private"


encodePrivacy : Privacy -> Value
encodePrivacy privacy =
    privacyToString privacy
        |> JE.string


privacyDecoder : Decoder Privacy
privacyDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "public" ->
                        JD.succeed PublicPrivacy

                    "unlisted" ->
                        JD.succeed UnlistedPrivacy

                    "private" ->
                        JD.succeed PrivatePrivacy

                    _ ->
                        JD.fail <| "Unknown privacy: " ++ s
            )


{-| Encode a `Source`.
-}
encodeSource : Source -> Value
encodeSource source =
    JE.object
        [ ( "privacy", encodePrivacy source.privacy )
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
        |> optional "privacy"
            (JD.oneOf
                [ JD.null PublicPrivacy
                , privacyDecoder
                ]
            )
            PublicPrivacy
        |> optional "sensitive" JD.bool False
        |> optional "language" (JD.nullable JD.string) Nothing
        |> required "note" JD.string
        |> optional "fields" (JD.list fieldDecoder) []
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
            JE.string "unknown"

        ImageAttachment ->
            JE.string "image"

        GifvAttachment ->
            JE.string "gifv"

        VideoAttachment ->
            JE.string "video"


attachmentTypeDecoder : Decoder AttachmentType
attachmentTypeDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "unknown" ->
                        JD.succeed UnknownAttachment

                    "image" ->
                        JD.succeed ImageAttachment

                    "gifv" ->
                        JD.succeed GifvAttachment

                    "video" ->
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
        , ( "size", encodeMaybe JE.string size )
        , ( "aspect", encodeMaybe JE.float aspect )
        ]


imageMetaInfoDecoder : Decoder ImageMetaInfo
imageMetaInfoDecoder =
    JD.succeed ImageMetaInfo
        |> optional "width" (JD.nullable JD.int) Nothing
        |> optional "height" (JD.nullable JD.int) Nothing
        |> optional "size" (JD.nullable JD.string) Nothing
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
        , ( "frame_rate", encodeMaybe JE.string frame_rate )
        , ( "duration", encodeMaybe JE.float duration )
        , ( "bitrate", encodeMaybe JE.int bitrate )
        ]


videoMetaInfoDecoder : Decoder VideoMetaInfo
videoMetaInfoDecoder =
    JD.succeed VideoMetaInfo
        |> optional "width" (JD.nullable JD.int) Nothing
        |> optional "height" (JD.nullable JD.int) Nothing
        |> optional "frame_rate" (JD.nullable JD.string) Nothing
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
        , ( "type", encodeAttachmentType attachment.type_ )
        , ( "url", JE.string attachment.url )
        , ( "remote_url", encodeMaybe JE.string attachment.remote_url )
        , ( "preview_url", JE.string attachment.preview_url )
        , ( "text_url", encodeMaybe JE.string attachment.text_url )
        , ( "meta", encodeMaybe encodeMeta attachment.meta )
        , ( "description", encodeMaybe JE.string attachment.description )
        ]


{-| Decode an `Attachment`.
-}
attachmentDecoder : Decoder Attachment
attachmentDecoder =
    JD.field "type" attachmentTypeDecoder
        |> JD.andThen
            (\type_ ->
                JD.succeed Attachment
                    |> required "id" JD.string
                    |> required "type" (JD.succeed type_)
                    |> required "url" JD.string
                    |> optional "remote_url" (JD.nullable JD.string) Nothing
                    |> required "preview_url" JD.string
                    |> optional "text_url" (JD.nullable JD.string) Nothing
                    |> optional "meta" (metaDecoder type_) Nothing
                    |> optional "description" (JD.nullable JD.string) Nothing
            )


{-| Encode a `CardType`
-}
encodeCardType : CardType -> Value
encodeCardType cardType =
    JE.string <|
        case cardType of
            LinkCard ->
                "link"

            PhotoCard ->
                "photo"

            VideoCard ->
                "video"

            RichCard ->
                "rich"


{-| Decode a `CardType`.
-}
cardTypeDecoder : Decoder CardType
cardTypeDecoder =
    JD.string
        |> JD.andThen
            (\t ->
                case t of
                    "link" ->
                        JD.succeed LinkCard

                    "photo" ->
                        JD.succeed PhotoCard

                    "video" ->
                        JD.succeed VideoCard

                    "rich" ->
                        JD.succeed RichCard

                    _ ->
                        JD.fail <| "Unknown CardType: " ++ t
            )


{-| Encode a `Card`.
-}
encodeCard : Card -> Value
encodeCard card =
    JE.object
        [ ( "url", JE.string card.url )
        , ( "title", JE.string card.title )
        , ( "description", JE.string card.description )
        , ( "image", encodeMaybe JE.string card.image )
        , ( "type", encodeCardType card.type_ )
        , ( "author_name", encodeMaybe JE.string card.author_name )
        , ( "author_url", encodeMaybe JE.string card.author_url )
        , ( "provider_name", encodeMaybe JE.string card.provider_name )
        , ( "provider_url", encodeMaybe JE.string card.provider_url )
        , ( "html", encodeMaybe JE.string card.html )
        , ( "width", encodeMaybe JE.int card.width )
        , ( "height", encodeMaybe JE.int card.height )
        ]


{-| Decode a `Card`.
-}
cardDecoder : Decoder Card
cardDecoder =
    JD.succeed Card
        |> required "url" JD.string
        |> required "title" JD.string
        |> required "description" JD.string
        |> optional "image" (JD.nullable JD.string) Nothing
        |> required "type" cardTypeDecoder
        |> optional "author_name" (JD.nullable JD.string) Nothing
        |> optional "author_url" (JD.nullable JD.string) Nothing
        |> optional "provider_name" (JD.nullable JD.string) Nothing
        |> optional "provider_url" (JD.nullable JD.string) Nothing
        |> optional "html" (JD.nullable JD.string) Nothing
        |> optional "width" (JD.nullable JD.int) Nothing
        |> optional "height" (JD.nullable JD.int) Nothing
        |> custom JD.value


{-| Encode a `Context`.
-}
encodeContext : Context -> Value
encodeContext context =
    JE.object
        [ ( "ancestors", JE.list encodeStatus context.ancestors )
        , ( "descendants", JE.list encodeStatus context.descendants )
        ]


{-| Decode a `Context`.
-}
contextDecoder : Decoder Context
contextDecoder =
    JD.succeed Context
        |> required "ancestors" (JD.list statusDecoder)
        |> required "descendants" (JD.list statusDecoder)


encodeWrappedStatus : WrappedStatus -> Value
encodeWrappedStatus (WrappedStatus status) =
    encodeStatus status


{-| Encode a `Visibility`.
-}
encodeVisibility : Visibility -> Value
encodeVisibility visibility =
    JE.string <|
        case visibility of
            PublicVisibility ->
                "public"

            UnlistedVisibility ->
                "unlisted"

            PrivateVisibility ->
                "private"

            DirectVisibility ->
                "direct"


{-| Decode a `Visibility`.
-}
visibilityDecoder : Decoder Visibility
visibilityDecoder =
    JD.string
        |> JD.andThen
            (\v ->
                case v of
                    "public" ->
                        JD.succeed PublicVisibility

                    "unlisted" ->
                        JD.succeed UnlistedVisibility

                    "private" ->
                        JD.succeed PrivateVisibility

                    "direct" ->
                        JD.succeed DirectVisibility

                    _ ->
                        JD.fail <| "Unknown Visibility: " ++ v
            )


encodeMention : Mention -> Value
encodeMention mention =
    JE.object
        [ ( "url", JE.string mention.url )
        , ( "username", JE.string mention.username )
        , ( "acct", JE.string mention.acct )
        , ( "id", JE.string mention.id )
        ]


mentionDecoder : Decoder Mention
mentionDecoder =
    JD.succeed Mention
        |> required "url" JD.string
        |> required "username" JD.string
        |> required "acct" JD.string
        |> required "id" JD.string


{-| Encode a `Tag`.
-}
encodeTag : Tag -> Value
encodeTag { name, url, history } =
    JE.object
        [ ( "name", JE.string name )
        , ( "url", JE.string url )
        , ( "history", JE.list encodeHistory history )
        ]


{-| Decode a `Tag`.
-}
tagDecoder : Decoder Tag
tagDecoder =
    JD.succeed Tag
        |> required "name" JD.string
        |> required "url" JD.string
        |> optional "history" (JD.list historyDecoder) []


encodeHistory : History -> Value
encodeHistory { day, uses, accounts } =
    JE.object
        [ ( "day", JE.string day )
        , ( "uses", JE.int uses )
        , ( "accounts", JE.int accounts )
        ]


stringToIntDecoder : Decoder Int
stringToIntDecoder =
    JD.oneOf
        [ JD.int
        , JD.string
            |> JD.andThen
                (\s ->
                    case String.toInt s of
                        Just int ->
                            JD.succeed int

                        Nothing ->
                            JD.fail <| "Not an integer: " ++ s
                )
        ]


historyDecoder : Decoder History
historyDecoder =
    JD.succeed History
        |> required "day" JD.string
        |> required "uses" stringToIntDecoder
        |> required "accounts" stringToIntDecoder


{-| Encode a `Poll`.
-}
encodePoll : Poll -> Value
encodePoll poll =
    JE.object
        [ ( "id", JE.string poll.id )
        , ( "expires_at", encodeMaybe JE.string poll.expires_at )
        , ( "expired", JE.bool poll.expired )
        , ( "multiple", JE.bool poll.multiple )
        , ( "votes_count", JE.int poll.votes_count )
        , ( "options", JE.list encodePollOption poll.options )
        , ( "voted", JE.bool poll.voted )
        ]


optionalBoolDecoder : Decoder Bool
optionalBoolDecoder =
    JD.nullable JD.bool
        |> JD.andThen (Maybe.withDefault False >> JD.succeed)


{-| Decode a `Poll`.
-}
pollDecoder : Decoder Poll
pollDecoder =
    JD.succeed Poll
        |> required "id" JD.string
        |> optional "expires_at" (JD.nullable JD.string) Nothing
        |> required "expired" JD.bool
        |> required "multiple" JD.bool
        |> required "votes_count" JD.int
        |> required "options" (JD.list pollOptionDecoder)
        |> optional "voted" optionalBoolDecoder False
        |> custom JD.value


encodePollOption : PollOption -> Value
encodePollOption { title, votes_count } =
    JE.object
        [ ( "title", JE.string title )
        , ( "votes_count", JE.int votes_count )
        ]


pollOptionDecoder : Decoder PollOption
pollOptionDecoder =
    JD.succeed PollOption
        |> required "title" JD.string
        |> optional "votes_count"
            (JD.nullable JD.int
                |> JD.andThen (Maybe.withDefault 0 >> JD.succeed)
            )
            0


{-| Encode a `Status`.
-}
encodeStatus : Status -> Value
encodeStatus status =
    JE.object
        [ ( "id", JE.string status.id )
        , ( "uri", JE.string status.uri )
        , ( "url", encodeMaybe JE.string status.url )
        , ( "account", encodeAccount status.account )
        , ( "in_reply_to_id", encodeMaybe JE.string status.in_reply_to_id )
        , ( "in_reply_to_account_id", encodeMaybe JE.string status.in_reply_to_account_id )
        , ( "reblog", encodeMaybe encodeWrappedStatus status.reblog )
        , ( "content", JE.string status.content )
        , ( "created_at", JE.string status.created_at )
        , ( "emojis", JE.list encodeEmoji status.emojis )
        , ( "replies_count", JE.int status.replies_count )
        , ( "reblogs_count", JE.int status.reblogs_count )
        , ( "favourites_count", JE.int status.favourites_count )
        , ( "reblogged", JE.bool status.reblogged )
        , ( "favourited", JE.bool status.favourited )
        , ( "muted", JE.bool status.muted )
        , ( "sensitive", JE.bool status.sensitive )
        , ( "spoiler_text", JE.string status.spoiler_text )
        , ( "visibility", encodeVisibility status.visibility )
        , ( "media_attachments", JE.list encodeAttachment status.media_attachments )
        , ( "mentions", JE.list encodeMention status.mentions )
        , ( "tags", JE.list encodeTag status.tags )
        , ( "card", encodeMaybe encodeCard status.card )
        , ( "poll", encodeMaybe encodePoll status.poll )
        , ( "application", encodeMaybe encodeApplication status.application )
        , ( "language", encodeMaybe JE.string status.language )
        , ( "pinned", JE.bool status.pinned )
        , ( "group_id", encodeMaybe JE.string status.group_id )
        ]


{-| Decode a `Status`.
-}
statusDecoder : Decoder Status
statusDecoder =
    JD.succeed Status
        |> required "id" JD.string
        |> required "uri" JD.string
        |> optional "url" (JD.nullable JD.string) Nothing
        |> required "account" accountDecoder
        |> optional "in_reply_to_id" (JD.nullable JD.string) Nothing
        |> optional "in_reply_to_account_id" (JD.nullable JD.string) Nothing
        |> optional "reblog"
            (JD.lazy
                (\() ->
                    JD.nullable
                        (statusDecoder |> JD.map WrappedStatus)
                )
            )
            Nothing
        |> required "content" JD.string
        |> required "created_at" JD.string
        |> required "emojis" (JD.list emojiDecoder)
        |> required "replies_count" JD.int
        |> required "reblogs_count" JD.int
        |> required "favourites_count" JD.int
        |> optional "reblogged" optionalBoolDecoder False
        |> optional "favourited" optionalBoolDecoder False
        |> optional "muted" optionalBoolDecoder False
        |> required "sensitive" JD.bool
        |> required "spoiler_text" JD.string
        |> required "visibility" visibilityDecoder
        |> required "media_attachments" (JD.list attachmentDecoder)
        |> required "mentions" (JD.list mentionDecoder)
        |> required "tags" (JD.list tagDecoder)
        |> optional "card" (JD.nullable cardDecoder) Nothing
        |> optional "poll" (JD.nullable pollDecoder) Nothing
        |> optional "application" (JD.nullable applicationDecoder) Nothing
        |> optional "language" (JD.nullable JD.string) Nothing
        |> optional "pinned" optionalBoolDecoder False
        |> optional "group_id" (JD.nullable JD.string) Nothing
        |> custom JD.value


{-| Encode an `Error`.

Note that the `httpStatus` field does NOT get encoded.
You'll probably never need to encode one of these, anyway.

-}
encodeError : Error -> Value
encodeError error =
    JE.object
        [ ( "error", JE.string error.error ) ]


{-| Decode an `Error`.

Since the `httpStatus` comes from the HTTP headers, it needs to be
passed in here as a parameter, as it isn't in the JSON.

-}
errorDecoder : String -> Decoder Error
errorDecoder httpStatus =
    JD.succeed Error
        |> hardcoded httpStatus
        |> required "error" JD.string


{-| Encode a `FilterContext`.
-}
encodeFilterContext : FilterContext -> Value
encodeFilterContext context =
    JE.string <|
        case context of
            HomeContext ->
                "HomeContext"

            NotificationsContext ->
                "NotificationsContext"

            PublicContext ->
                "PublicContext"

            ThreadContext ->
                "ThreadContext"


{-| Decode a `FilterContext`.
-}
filterContextDecoder : Decoder FilterContext
filterContextDecoder =
    JD.string
        |> JD.andThen
            (\c ->
                case c of
                    "HomeContext" ->
                        JD.succeed HomeContext

                    "NotificationsContext" ->
                        JD.succeed NotificationsContext

                    "PublicContext" ->
                        JD.succeed PublicContext

                    "ThreadContext" ->
                        JD.succeed ThreadContext

                    _ ->
                        JD.fail <| "Unknown Filter context: " ++ c
            )


{-| Encode a `Filter`.
-}
encodeFilter : Filter -> Value
encodeFilter filter =
    JE.object
        [ ( "id", JE.string filter.id )
        , ( "phrase", JE.string filter.phrase )
        , ( "context", JE.list encodeFilterContext filter.context )
        , ( "expires_at", encodeMaybe JE.string filter.expires_at )
        , ( "irreversible", JE.bool filter.irreversible )
        , ( "whole_word", JE.bool filter.whole_word )
        ]


{-| Decode a `Filter`.
-}
filterDecoder : Decoder Filter
filterDecoder =
    JD.succeed Filter
        |> required "id" JD.string
        |> required "phrase" JD.string
        |> required "context" (JD.list filterContextDecoder)
        |> optional "expires_at" (JD.nullable JD.string) Nothing
        |> required "irreversible" JD.bool
        |> required "whole_word" JD.bool
        |> custom JD.value


encodeUrls : URLs -> Value
encodeUrls { streaming_api } =
    JE.object [ ( "streaming_api", JE.string streaming_api ) ]


urlsDecoder : Decoder URLs
urlsDecoder =
    JD.succeed URLs
        |> required "streaming_api" JD.string


encodeStats : Stats -> Value
encodeStats { user_count, status_count, domain_count } =
    JE.object
        [ ( "user_count", JE.int user_count )
        , ( "status_count", JE.int status_count )
        , ( "domain_count", JE.int domain_count )
        ]


statsDecoder : Decoder Stats
statsDecoder =
    JD.succeed Stats
        |> required "user_count" JD.int
        |> required "status_count" JD.int
        |> required "domain_count" JD.int


{-| Encode an `Instance`.
-}
encodeInstance : Instance -> Value
encodeInstance instance =
    JE.object
        [ ( "uri", JE.string instance.uri )
        , ( "title", JE.string instance.title )
        , ( "description", JE.string instance.description )
        , ( "email", JE.string instance.email )
        , ( "version", JE.string instance.version )
        , ( "thumbnail", encodeMaybe JE.string instance.thumbnail )
        , ( "urls", encodeUrls instance.urls )
        , ( "stats", encodeStats instance.stats )
        , ( "languages", JE.list JE.string instance.languages )
        , ( "contact_account", encodeMaybe encodeAccount instance.contact_account )
        ]


{-| Decode an `Instance`.
-}
instanceDecoder : Decoder Instance
instanceDecoder =
    JD.succeed Instance
        |> required "uri" JD.string
        |> required "title" JD.string
        |> required "description" JD.string
        |> required "email" JD.string
        |> required "version" JD.string
        |> optional "thumbnail" (JD.nullable JD.string) Nothing
        |> required "urls" urlsDecoder
        |> required "stats" statsDecoder
        |> required "languages" (JD.list JD.string)
        |> optional "contact_account" (JD.nullable accountDecoder) Nothing
        |> custom JD.value


{-| Encode a `ListEntity`.
-}
encodeListEntity : ListEntity -> Value
encodeListEntity { id, title } =
    JE.object
        [ ( "id", JE.string id )
        , ( "title", JE.string title )
        ]


{-| Decode a `ListEntity`.
-}
listEntityDecoder : Decoder ListEntity
listEntityDecoder =
    JD.succeed ListEntity
        |> required "id" JD.string
        |> required "title" JD.string


{-| Encode a \`NotificationType.
-}
encodeNotificationType : NotificationType -> Value
encodeNotificationType notificationType =
    JE.string <|
        case notificationType of
            FollowNotification ->
                "FollowNotification"

            MentionNotification ->
                "MentionNotification"

            ReblogNotification ->
                "ReblogNotification"

            FavouriteNotification ->
                "FavouriteNotification"


{-| Decode a \`NotificationType.
-}
notificationTypeDecoder : Decoder NotificationType
notificationTypeDecoder =
    JD.string
        |> JD.andThen
            (\t ->
                case t of
                    "FollowNotification" ->
                        JD.succeed FollowNotification

                    "MentionNotification" ->
                        JD.succeed MentionNotification

                    "ReblogNotification" ->
                        JD.succeed ReblogNotification

                    "FavouriteNotification" ->
                        JD.succeed FavouriteNotification

                    _ ->
                        JD.fail <| "Unknown NotificationType: " ++ t
            )


{-| Encode a `Notification`.
-}
encodeNotification : Notification -> Value
encodeNotification notification =
    JE.object
        [ ( "id", JE.string notification.id )
        , ( "type", encodeNotificationType notification.type_ )
        , ( "created_at", JE.string notification.created_at )
        , ( "account", encodeAccount notification.account )
        , ( "status", encodeMaybe encodeStatus notification.status )
        ]


{-| Decode a `Notification`.
-}
notificationDecoder : Decoder Notification
notificationDecoder =
    JD.succeed Notification
        |> required "id" JD.string
        |> required "type" notificationTypeDecoder
        |> required "created_at" JD.string
        |> required "account" accountDecoder
        |> optional "status" (JD.nullable statusDecoder) Nothing
        |> custom JD.value


{-| Encoder for `PushSubscription`.
-}
encodePushSubscription : PushSubscription -> Value
encodePushSubscription pushSubscription =
    JE.object
        [ ( "id", JE.string pushSubscription.id )
        , ( "endpoint", JE.string pushSubscription.endpoint )
        , ( "server_key", JE.string pushSubscription.server_key )
        , ( "alerts", pushSubscription.alerts )
        ]


{-| Decoder for `PushSubscription`.
-}
pushSubscriptionDecoder : Decoder PushSubscription
pushSubscriptionDecoder =
    JD.succeed PushSubscription
        |> required "id" JD.string
        |> required "endpoint" JD.string
        |> required "server_key" JD.string
        |> required "alerts" JD.value
        |> custom JD.value


{-| Encoder for `Relationship`.
-}
encodeRelationship : Relationship -> Value
encodeRelationship relationship =
    JE.object
        [ ( "id", JE.string relationship.id )
        , ( "following", JE.bool relationship.following )
        , ( "followed_by", JE.bool relationship.followed_by )
        , ( "blocking", JE.bool relationship.blocking )
        , ( "muting", JE.bool relationship.muting )
        , ( "muting_notifications", JE.bool relationship.muting_notifications )
        , ( "requested", JE.bool relationship.requested )
        , ( "domain_blocking", JE.bool relationship.domain_blocking )
        , ( "showing_reblogs", JE.bool relationship.showing_reblogs )
        , ( "endorsed", JE.bool relationship.endorsed )
        ]


{-| Decode a `Relationship`.
-}
relationshipDecoder : Decoder Relationship
relationshipDecoder =
    JD.succeed Relationship
        |> required "id" JD.string
        |> required "following" JD.bool
        |> required "followed_by" JD.bool
        |> required "blocking" JD.bool
        |> required "muting" JD.bool
        |> required "muting_notifications" JD.bool
        |> required "requested" JD.bool
        |> required "domain_blocking" JD.bool
        |> required "showing_reblogs" JD.bool
        |> required "endorsed" JD.bool
        |> custom JD.value


{-| Encoder for `Results`.
-}
encodeResults : Results -> Value
encodeResults results =
    JE.object
        [ ( "accounts", JE.list encodeAccount results.accounts )
        , ( "statuses", JE.list encodeStatus results.statuses )
        , ( "hashtags", JE.list encodeTag results.hashtags )
        ]


{-| Decoder for `Results`.
-}
resultsDecoder : Decoder Results
resultsDecoder =
    JD.succeed Results
        |> required "accounts" (JD.list accountDecoder)
        |> required "statuses" (JD.list statusDecoder)
        |> required "hashtags" (JD.list tagDecoder)
        |> custom JD.value


encodeStatusParams : StatusParams -> Value
encodeStatusParams statusParams =
    JE.object
        [ ( "text", JE.string statusParams.text )
        , ( "in_reply_to_id", encodeMaybe JE.string statusParams.in_reply_to_id )
        , ( "media_ids", JE.list JE.string statusParams.media_ids )
        , ( "sensitive", JE.bool statusParams.sensitive )
        , ( "spoiler_text", encodeMaybe JE.string statusParams.spoiler_text )
        , ( "visibility", encodeVisibility statusParams.visibility )
        , ( "scheduled_at", encodeMaybe JE.string statusParams.scheduled_at )
        , ( "application_id", JE.string statusParams.application_id )
        ]


statusParamsDecoder : Decoder StatusParams
statusParamsDecoder =
    JD.succeed StatusParams
        |> required "text" JD.string
        |> optional "in_reply_to_id" (JD.nullable JD.string) Nothing
        |> optional "media_ids"
            (JD.nullable (JD.list JD.string)
                |> JD.andThen (Maybe.withDefault [] >> JD.succeed)
            )
            []
        |> optional "sensitive" optionalBoolDecoder False
        |> optional "spoiler_text" (JD.nullable JD.string) Nothing
        |> required "visibility" visibilityDecoder
        |> optional "scheduled_at" (JD.nullable JD.string) Nothing
        |> required "application_id" JD.string


{-| Encoder for `ScheduledStatus`.
-}
encodeScheduledStatus : ScheduledStatus -> Value
encodeScheduledStatus scheduledStatus =
    JE.object
        [ ( "id", JE.string scheduledStatus.id )
        , ( "scheduled_at", JE.string scheduledStatus.scheduled_at )
        , ( "params", encodeStatusParams scheduledStatus.params )
        , ( "media_attachments", JE.list encodeAttachment scheduledStatus.media_attachments )
        ]


{-| Decoder for `ScheduledStatus`.
-}
scheduledStatusDecoder : Decoder ScheduledStatus
scheduledStatusDecoder =
    JD.succeed ScheduledStatus
        |> required "id" JD.string
        |> required "scheduled_at" JD.string
        |> required "params" statusParamsDecoder
        |> required "media_attachments" (JD.list attachmentDecoder)
        |> custom JD.value


{-| Encoder for `Conversation`.
-}
encodeConversation : Conversation -> Value
encodeConversation conversation =
    JE.object
        [ ( "id", JE.string conversation.id )
        , ( "accounts", JE.list encodeAccount conversation.accounts )
        , ( "last_status", encodeMaybe encodeStatus conversation.last_status )
        , ( "unread", JE.bool conversation.unread )
        ]


{-| Decoder for `Conversation`.
-}
conversationDecoder : Decoder Conversation
conversationDecoder =
    JD.succeed Conversation
        |> required "id" JD.string
        |> required "accounts" (JD.list accountDecoder)
        |> optional "last_status" (JD.nullable statusDecoder) Nothing
        |> required "unread" JD.bool
        |> custom JD.value


{-| Encoder for `Group`.
-}
encodeGroup : Group -> Value
encodeGroup group =
    JE.object
        [ ( "id", JE.string group.id )
        , ( "title", JE.string group.title )
        , ( "description", JE.string group.description )
        , ( "cover_image_url", JE.string group.cover_image_url )
        , ( "is_archived", JE.bool group.is_archived )
        ]


{-| Decoder for `Group`.
-}
groupDecoder : Decoder Group
groupDecoder =
    JD.succeed Group
        |> required "id" JD.string
        |> required "title" JD.string
        |> required "description" JD.string
        |> required "cover_image_url" JD.string
        |> required "is_archived" JD.bool
        |> custom JD.value


{-| Encoder for `Authorization`.
-}
encodeAuthorization : Authorization -> Value
encodeAuthorization authorization =
    JE.object
        [ ( "clientId", JE.string authorization.clientId )
        , ( "clientSecret", JE.string authorization.clientSecret )
        , ( "token", JE.string authorization.token )
        ]


{-| Decoder for `Authorization`.
-}
authorizationDecoder : Decoder Authorization
authorizationDecoder =
    JD.succeed Authorization
        |> required "clientId" JD.string
        |> required "clientSecret" JD.string
        |> required "token" JD.string
