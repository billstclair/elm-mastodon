----------------------------------------------------------------------
--
-- Entity.elm
-- Mastodon API entities.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Mastodon.Entity exposing
    ( Entity(..)
    , Datetime, UrlString, HtmlString, ISO6391, ISO6392, UnixTimestamp
    , Account, Source, Token, Application, App
    , Card, Context, Error, Filter, Instance, Activity
    , ListEntity, Notification
    , PushSubscription, Relationship, Results
    , Status, RawStatus, ScheduledStatus, Conversation
    , Group, GroupRelationship
    , Emoji, Field, Privacy(..), Attachment, AttachmentType(..)
    , Meta(..), ImageMetaFields, VideoMetaFields
    , ImageMetaInfo, VideoMetaInfo, Focus
    , CardType(..), FilterContext(..), URLs, Stats, NotificationType(..)
    , Visibility(..), Mention, Tag, History, Poll, PollOption, StatusParams
    , PleromaStatusContent
    , Authorization
    , WrappedAccount(..), WrappedStatus(..)
    )

{-| The Mastodon API entities.

These are JSON-encoded over the wire. Mastodon.EncodeDecode knows how to do that.

Documented at <https://docs.joinmastodon.org/api/entities/>

Most of the Entity have a `v` field, which is the raw JS value from
which it was decoded. This is useful if you want to display what you
got over the wire. Code that creates these can set it to
`Json.Encode.null`.


# Entity

@docs Entity


# String aliases

@docs Datetime, UrlString, HtmlString, ISO6391, ISO6392, UnixTimestamp


# Entities

@docs Account, Source, Token, Application, App
@docs Card, Context, Error, Filter, Instance, Activity
@docs ListEntity, Notification
@docs PushSubscription, Relationship, Results
@docs Status, RawStatus, ScheduledStatus, Conversation
@docs Group, GroupRelationship


# Entity field types

@docs Emoji, Field, Privacy, Attachment, AttachmentType
@docs Meta, ImageMetaFields, VideoMetaFields
@docs ImageMetaInfo, VideoMetaInfo, Focus
@docs CardType, FilterContext, URLs, Stats, NotificationType
@docs Visibility, Mention, Tag, History, Poll, PollOption, StatusParams
@docs PleromaStatusContent


# Authorization parameters

@docs Authorization


# Wrappers to prevent type recursion

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
type alias ISO6392 =
    String


{-| Alias of `String`
-}
type alias UnixTimestamp =
    String


{-| Account entity

Pleroma servers have an additional field here, not yet supported:

    "pleroma":{"tags":[],
               "skip_thread_containment":false,
               "settings_store":{},
               "relationship":{"subscribing":false,
                               "showing_reblogs":true,
                               "requested":false,
                               "muting_notifications":false,
                               "muting":false,
                               "id":"9kL2555DTtECY9TEQK",
                               "following":true,
                               "followed_by":true,
                               "endorsed":false,
                               "domain_blocking":false,
                               "blocking":false
                              }

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

    -- Not documented
    , source : Maybe Source

    -- Gab extensions
    , is_pro : Bool
    , is_verified : Bool
    , is_donor : Bool
    , is_investor : Bool

    -- Standard input `Value`.
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


{-| Values for `Source.privacy`
-}
type Privacy
    = PublicPrivacy -- "public"
    | UnlistedPrivacy -- "unlisted"
    | PrivatePrivacy -- "private" (Followers-only)


{-| Source entity.

Pleroma servers have an additional field here, not yet supported:

    "pleroma":{"show_role":true,"no_rich_text":false}

-}
type alias Source =
    { privacy : Privacy
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


{-| App entity.

This is not documented, so I requested one and reverse engineered it.

-}
type alias App =
    { id : String
    , name : String
    , website : Maybe String
    , redirect_uri : String
    , client_id : String
    , client_secret : String
    , vapid_key : Maybe String -- I think this is always there, but just in case.
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
    , preview_url : Maybe UrlString
    , text_url : Maybe UrlString
    , meta : Maybe Meta
    , description : Maybe String
    , v : Value
    }


{-| Types for the `Attachment.type_` field.
-}
type AttachmentType
    = UnknownAttachment
    | ImageAttachment
    | GifvAttachment
    | VideoAttachment
    | AudioAttachment
    | UnrecognizedAttachment String


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
    , size : Maybe String
    , aspect : Maybe Float
    }


{-| Values for `VideoMetaFields.small` and `VideoMetaFields.original`
-}
type alias VideoMetaInfo =
    { width : Maybe Int
    , height : Maybe Int
    , frame_rate : Maybe String
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


{-| Values for the `Account.emojis` list. Also returned by `/api/v1/custom_emojis`.
-}
type alias Emoji =
    { shortcode : String
    , static_url : UrlString
    , url : UrlString
    , visible_in_picker : Bool
    , category : Maybe String
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
    , max_toot_chars : Maybe Int
    , languages : List ISO6391
    , contact_account : Maybe Account
    , v : Value
    }


{-| `Activity` entity.

Not documented. A list of these is returned by "GET instance/activity"

-}
type alias Activity =
    { week : Int
    , statuses : Int
    , logins : Maybe Int
    , registrations : Int
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
    , emoji : Maybe String
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
    | PollNotification
    | FollowRequestNotification
    | UpdateNotification
    | Admin_SignupNotification
    | Admin_ReportNotification
    | Pleroma_EmojiReactionNotification
    | UnknownNotification String


{-| Value for `Status.poll`, and an `Entity` in its own right.
-}
type alias Poll =
    { id : String
    , expires_at : Maybe Datetime
    , expired : Bool
    , multiple : Bool
    , votes_count : Int
    , voters_count : Maybe Int
    , options : List PollOption
    , emojis : List Emoji
    , voted : Bool
    , own_votes : List Int
    , v : Value
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
    , hashtags : List String
    , groups : List Group
    , v : Value
    }


{-| Status entity.

The `plain_markdown` and `plain_text` fields are elm-mastodon additions.

`content` is the HTML to display.
`plain_markdown` is the markdown the user entered, if it is available.
`plain_text` is the unformatted text, if it is available.

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
    , plain_markdown : Maybe String
    , plain_text : Maybe String
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
    , application : Maybe Application
    , language : Maybe String
    , pinned : Bool
    , group : Maybe Group --GAB extension
    , quote_of_id : Maybe String
    , quote : Maybe WrappedStatus
    , v : Value
    }


{-| The `pleroma` field of a `RawStatus` entity (Pleroma only).
-}
type alias PleromaStatusContent =
    { content : { plain_text : String } }


{-| RawStatus entity.

Extra Status fields that are not returned by all servers.

-}
type alias RawStatus =
    { plain_markdown : Maybe String
    , rich_content : Maybe String
    , pleroma : Maybe PleromaStatusContent
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
    | PrivateGroupVisibility


{-| ScheduledStatus entity.
-}
type alias ScheduledStatus =
    { id : String
    , scheduled_at : Datetime
    , params : StatusParams
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
    , visibility : Maybe Visibility
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


{-| Group entity.

This is a Gab extension.

-}
type alias Group =
    { id : String
    , title : String
    , description : String
    , cover_image_url : String
    , is_archived : Bool
    , member_count : Int
    , v : Value
    }


{-| Group relationship entity.

This is a Gab extension.

-}
type alias GroupRelationship =
    { id : String
    , member : Bool
    , admin : Bool
    , unread_count : Int
    , v : Value
    }


{-| One type to rule them all.

This is mostly to make tests easier to define. Most code will use
individual entities explicitly.

-}
type Entity
    = NoEntity
    | AccountEntity Account
    | AccountListEntity (List Account)
    | SourceEntity Source
    | TokenEntity Token
    | ApplicationEntity Application
    | AppEntity App
    | CardEntity Card
    | ContextEntity Context
    | EmojiEntity Emoji
    | EmojiListEntity (List Emoji)
    | ErrorEntity Error
    | FilterEntity Filter
    | FilterListEntity (List Filter)
    | InstanceEntity Instance
    | ActivityEntity Activity
    | ActivityListEntity (List Activity)
    | PeersEntity (List String)
    | ListEntityEntity ListEntity
    | ListEntityListEntity (List ListEntity)
    | AttachmentEntity Attachment
    | AttachmentListEntity (List Attachment)
    | NotificationEntity Notification
    | NotificationListEntity (List Notification)
    | PushSubscriptionEntity PushSubscription
    | RelationshipEntity Relationship
    | RelationshipListEntity (List Relationship)
    | ResultsEntity Results
    | StatusEntity Status
    | PollEntity Poll
    | StatusListEntity (List Status)
    | ScheduledStatusEntity ScheduledStatus
    | ScheduledStatusListEntity (List ScheduledStatus)
    | ConversationEntity Conversation
    | ConversationListEntity (List Conversation)
    | GroupEntity Group
    | GroupRelationshipEntity GroupRelationship
    | GroupRelationshipListEntity (List GroupRelationship)
    | GroupListEntity (List Group)
    | StringListEntity (List String)
    | TagListEntity (List Tag)
    | ValueEntity Value


{-| Authorization Parameters.

Applications will usually save this in `localStorage`, use the saved token
until it expires, then use the client ID and secret to mint a new authorization.

The authorization is usually of the type "Bearer <token>".

-}
type alias Authorization =
    { clientId : String
    , clientSecret : String
    , token : String
    }
