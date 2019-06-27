----------------------------------------------------------------------
--
-- Entities.elm
-- Mastodon API entities.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Mastodon.Entities exposing
    ( Entity(..)
    , Datetime, UrlString, HtmlString, ISO6391, UnixTimestamp
    , Account, Source, Token, Application
    , Card, Context, Error, Filter, Instance
    , URLs, Stats, ListEntity, Notification
    , PushSubscription, Relationship, Results
    , Status, ScheduledStatus, Conversation
    , Emoji, Field, Attachment, AttachmentType(..)
    , Meta(..), ImageMetaFields, VideoMetaFields
    , ImageMetaInfo, VideoMetaInfo, Focus
    , CardType(..), FilterContext(..), NotificationType(..)
    , Visibility(..), Mention, Tag, History, Poll, PollOption, StatusParams
    , WrappedAccount(..), WrappedStatus(..)
    )

{-| The Mastodon API entities.

These are JSON-encoded over the wire. Mastodon.EncodeDecode knows how to do that.

Documented at <https://docs.joinmastodon.org/api/entities/>

Most of the Entities have a `v` field, which is the raw JS value from
which it was decoded. This is useful if you want to display what you
got over the wire. Code that creates these can set it to
`Json.Encode.null`.


## Entity

@docs Entity


## String aliases

@docs Datetime, UrlString, HtmlString, ISO6391, UnixTimestamp


## Entities

@docs Account, Source, Token, Application
@docs Card, Context, Error, Filter, Instance
@docs URLs, Stats, ListEntity, Notification
@docs PushSubscription, Relationship, Results
@docs Status, ScheduledStatus, Conversation


## Entity field types

@docs Emoji, Field, Attachment, AttachmentType
@docs Meta, ImageMetaFields, VideoMetaFields
@docs ImageMetaInfo, VideoMetaInfo, Focus
@docs CardType, FilterContext, NotificationType
@docs Visibility, Mention, Tag, History, Poll, PollOption, StatusParams


## Wrappers to prevent type recursion

@docs WrappedAccount, WrappedStatus

-}

import Json.Encode as JE exposing (Value)


{-| Alias of `String`
-}
type alias Datetime =
    String


{-| Alias of `String`
-}
type alias UrlString =
    String


{-| Alias of `String`
-}
type alias HtmlString =
    String


{-| Alias of `String`
-}
type alias ISO6391 =
    String


{-| Alias of `String`
-}
type alias UnixTimestamp =
    String


{-| Account entity
-}
type alias Account =
    { id : String
    , username : String
    , acct : String
    , display_name : String
    , locked : Bool
    , created_at : Datetime
    , followers_count : Int
    , following_count : Int
    , statuses_count : Int
    , note : String
    , url : UrlString
    , avatar : UrlString
    , avatar_static : UrlString
    , header : UrlString
    , header_static : UrlString
    , emojis : List Emoji
    , moved : Maybe WrappedAccount
    , fields : List Field
    , bot : Bool
    , v : Value
    }


{-| Wrapped `Account`, to prevent type recursion.
-}
type WrappedAccount
    = WrappedAccount Account


{-| Values for the `Account.fields` and `Source.fields` lists.
-}
type alias Field =
    { name : String
    , value : HtmlString
    , verified_at : Maybe Datetime
    }


{-| Source entity.
-}
type alias Source =
    { privacy : Maybe String
    , sensitive : Bool
    , language : Maybe ISO6391
    , note : String
    , fields : List Field
    , v : Value
    }


{-| Token entity.
-}
type alias Token =
    { access_token : String
    , token_type : String
    , scope : String
    , created_at : Int
    , v : Value
    }


{-| Application entity.
-}
type alias Application =
    { name : String
    , website : Maybe UrlString
    , v : Value
    }


{-| Element of `Status.media_attachments` and `ScheduledStatus.media_attachments`.

Note that it's possible to create an `Attachment` whose `type_` disagrees
with its `meta`. Maybe I should have represented it to prevent that, but I
chose to mostly match the JSON.

-}
type alias Attachment =
    { id : String
    , type_ : AttachmentType
    , url : UrlString
    , remote_url : Maybe UrlString
    , preview_url : UrlString
    , text_url : Maybe UrlString
    , meta : Maybe Meta
    , description : String
    }


{-| Types for the `Attachment.type_` field.
-}
type AttachmentType
    = UnknownAttachment
    | ImageAttachment
    | GifvAttachment
    | VideoAttachment


{-| Value for `Attachment.meta`
-}
type Meta
    = ImageMeta ImageMetaFields
    | VideoMeta VideoMetaFields


{-| Fields for an `ImageMeta`.
-}
type alias ImageMetaFields =
    { small : Maybe ImageMetaInfo
    , original : Maybe ImageMetaInfo
    , focus : Maybe Focus
    }


{-| Fields for a `VideoMeta`.
-}
type alias VideoMetaFields =
    { small : Maybe VideoMetaInfo
    , original : Maybe VideoMetaInfo
    , focus : Maybe Focus
    }


{-| Values for `ImageMetaFields.small` and `ImageMetaFields.original`
-}
type alias ImageMetaInfo =
    { width : Maybe Int
    , height : Maybe Int
    , size : Maybe Int
    , aspect : Maybe Float
    }


{-| Values for `VideoMetaFields.small` and `VideoMetaFields.original`
-}
type alias VideoMetaInfo =
    { width : Maybe Int
    , height : Maybe Int
    , frame_rate : Maybe Int
    , duration : Maybe Float
    , bitrate : Maybe Int
    }


{-| The optional focus of an image attachment.
-}
type alias Focus =
    { x : Float
    , y : Float
    }


{-| Card entity.
-}
type alias Card =
    { url : UrlString
    , title : String
    , description : String
    , image : Maybe UrlString
    , type_ : CardType
    , author_name : Maybe String
    , author_url : Maybe UrlString
    , provider_name : Maybe String
    , provider_url : Maybe UrlString
    , html : Maybe HtmlString
    , width : Maybe Int
    , height : Maybe Int
    , v : Value
    }


{-| Choices for the `Card.type_` field.
-}
type CardType
    = LinkCard
    | PhotoCard
    | VideoCard
    | RichCard


{-| Context entity.
-}
type alias Context =
    { ancestors : List Status
    , descendants : List Status
    }


{-| Values for the `Account.emojis` list.
-}
type alias Emoji =
    { shortcode : String
    , static_url : UrlString
    , url : UrlString
    , visible_in_picker : Bool
    }


{-| Error entity.
-}
type alias Error =
    { httpStatus : String -- From the HTTP header, not the JSON body.
    , error : String
    }


{-| Filter entity.
-}
type alias Filter =
    { id : String
    , phrase : String
    , context : List FilterContext
    , expires_at : Maybe Datetime
    , irreversible : Bool
    , whole_word : Bool
    , v : Value
    }


{-| Choices for the `Filter.context` list.
-}
type FilterContext
    = HomeContext
    | NotificationsContext
    | PublicContext
    | ThreadContext


{-| Instance entity.
-}
type alias Instance =
    { uri : String
    , title : String
    , description : String
    , email : String
    , version : String
    , thumbnail : Maybe UrlString
    , urls : Maybe URLs
    , stats : Stats
    , languages : List ISO6391
    , contact_account : Maybe Account
    , v : Value
    }


{-| Value of `Instance.urls`.
-}
type alias URLs =
    { streaming_api : UrlString
    }


{-| Value of `Instance.stats`.
-}
type alias Stats =
    { user_count : Int
    , status_count : Int
    , domain_count : Int
    }


{-| List entity.
-}
type alias ListEntity =
    { id : String
    , title : String
    , v : Value
    }


{-| Values in `Status.mentions`.
-}
type alias Mention =
    { url : UrlString
    , username : String
    , acct : String
    , id : String
    }


{-| Notification entity.
-}
type alias Notification =
    { id : String
    , type_ : NotificationType
    , created_at : Datetime
    , account : Account
    , status : Maybe Status
    , v : Value
    }


{-| Choices for `Notification.type_`.
-}
type NotificationType
    = FollowNotification
    | MentionNotification
    | ReblogNotification
    | FavouriteNotification


{-| Value for `Status.poll`.
-}
type alias Poll =
    { id : String
    , expires_at : Maybe Datetime
    , expired : Bool
    , multiple : Bool
    , votes_count : Int
    , options : List PollOption
    , voted : Bool
    }


{-| Elements of the `Poll.options` list.
-}
type alias PollOption =
    { title : String
    , votes_count : Int
    }


{-| Push subscription entity.
-}
type alias PushSubscription =
    { id : String
    , endpoint : UrlString
    , server_key : String
    , alerts : Value --not documented
    , v : Value
    }


{-| Relationship entity.
-}
type alias Relationship =
    { id : String
    , following : Bool
    , followed_by : Bool
    , blocking : Bool
    , muting : Bool
    , muting_notifications : Bool
    , requested : Bool
    , domain_blocking : Bool
    , showing_reblogs : Bool
    , endorsed : Bool
    , v : Value
    }


{-| Results entity.
-}
type alias Results =
    { accounts : List Account
    , statuses : List Status
    , hashtags : List Tag
    , v : Value
    }


{-| Status entity.
-}
type alias Status =
    { id : String
    , uri : String
    , url : Maybe UrlString
    , account : Account
    , in_reply_to_id : Maybe String
    , in_reply_to_account_id : Maybe String
    , reblog : Maybe WrappedStatus
    , content : HtmlString
    , created_at : Datetime
    , emojis : List Emoji
    , replies_count : Int
    , reblogs_count : Int
    , favourites_count : Int
    , reblogged : Bool
    , favourited : Bool
    , muted : Bool
    , sensitive : Bool
    , spoiler_text : String
    , visibility : Visibility
    , media_attachments : List Attachment
    , mentions : List Mention
    , tags : List Tag
    , card : Maybe Card
    , poll : Maybe Poll
    , application : Application
    , language : Maybe String
    , pinned : Bool
    , v : Value
    }


{-| Wrapped `Status`, to prevent type recursion.
-}
type WrappedStatus
    = WrappedStatus Status


{-| Values for `Status.visibility`.
-}
type Visibility
    = PublicVisibility
    | UnlistedVisibility
    | PrivateVisibility
    | DirectVisibility


{-| ScheduledStatus entity.
-}
type alias ScheduledStatus =
    { id : String
    , scheduled_at : Datetime
    , params : List StatusParams
    , media_attachments : List Attachment
    , v : Value
    }


{-| Elements of `ScheduledStatus.params`.
-}
type alias StatusParams =
    { text : String
    , in_reply_to_id : Maybe String
    , media_ids : List String
    , sensitive : Bool
    , spoiler_text : Maybe String
    , visibility : Visibility
    , scheduled_at : Maybe Datetime
    , application_id : String
    }


{-| Elements of `Results.hashtags` and `Status.tags`.
-}
type alias Tag =
    { name : String
    , url : UrlString
    , history : List History
    }


{-| Value for `Tag.history`.
-}
type alias History =
    { day : UnixTimestamp
    , uses : Int
    , accounts : Int
    }


{-| Conversation entity.
-}
type alias Conversation =
    { id : String
    , accounts : List Account
    , last_status : Maybe Status
    , unread : Bool
    , v : Value
    }


{-| One type to rule them all.

This is mostly to make tests easier to define. Most code will use
individual entities explicitly.

-}
type Entity
    = AccountEntity Account
    | SourceEntity Source
    | TokenEntity Token
    | ApplicationEntity Application
    | CardEntity Card
    | ContextEntity Context
    | ErrorEntity Error
    | FilterEntity Filter
    | InstanceEntity Instance
    | ListEntityEntity ListEntity
    | NotificationEntity Notification
    | PushSubscriptionEntity PushSubscription
    | RelationshipEntity Relationship
    | ResultsEntity Results
    | StatusEntity Status
    | ScheduledStatusEntity ScheduledStatus
    | ConversationEntity Conversation
