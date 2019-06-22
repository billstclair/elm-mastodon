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
    ( Account, Field, Source, Token, Application, Attachment
    , Meta, Card, Context, Emoji, Error, Filter, Instance
    , URLs, Stats, ListEntity, Mention, Notification, Poll
    , PushSubscription, Relationship, Results
    , Status, ScheduledStatus, Tag, History, Conversation
    , AttachmentType(..), MetaInfo(..), Focus, CardType(..)
    , FilterContext(..), NotificationType(..), PollOption
    , Visibility(..), StatusParams
    , WrappedAccount(..), WrappedStatus(..)
    )

{-| The Mastodon API entities.

These are JSON-encoded over the wire. Mastodon.EncodeDecode knows how to do that.

Documented at <https://docs.joinmastodon.org/api/entities/>


## Entities

@docs Account, Field, Source, Token, Application, Attachment
@docs Meta, Card, Context, Emoji, Error, Filter, Instance
@docs URLs, Stats, ListEntity, Mention, Notification, Poll
@docs PushSubscription, Relationship, Results
@docs Status, ScheduledStatus, Tag, History, Conversation


## Entity field types

@docs AttachmentType, MetaInfo, Focus, CardType
@docs FilterContext, NotificationType, PollOption
@docs Visibility, StatusParams


## Wrappers to prevent type recursion

@docs WrappedAccount, WrappedStatus

-}

import Json.Encode as JE exposing (Value)


{-| Account entity
-}
type alias Account =
    { id : String
    , username : String
    , acct : String
    , display_name : String
    , locked : Bool
    , created_at : String
    , followers_count : Int
    , following_count : Int
    , statuses_count : Int
    , note : String
    , url : String
    , avatar : String
    , avatar_static : String
    , header : String
    , header_static : String
    , emojis : List Emoji
    , moved : Maybe WrappedAccount
    , fields : List Field
    , bot : Maybe Bool
    }


{-| Wrapped Account, to prevent type recursion.
-}
type WrappedAccount
    = WrappedAccount Account


{-| Field entity.
-}
type alias Field =
    { name : String
    , value : String
    , verified_at : Maybe String
    }


{-| Source entity.
-}
type alias Source =
    { privacy : String
    , sensitive : Bool
    , language : String
    , note : String
    , fields : List Field
    }


{-| Token entity.
-}
type alias Token =
    { access_token : String
    , token_type : String
    , scope : String
    , created_at : Int
    }


{-| Application entity.
-}
type alias Application =
    { name : String
    , website : Maybe String
    }


{-| Attachment entity.
-}
type alias Attachment =
    { id : String
    , type_ : AttachmentType
    , url : String
    , remote_url : Maybe String
    , preview_url : String
    , text_url : Maybe String
    , meta : Maybe Meta
    , description : String
    }


{-| The types for the `Attachment.type_` field.
-}
type AttachmentType
    = UnknownAttachment
    | ImageAttachment
    | GifvAttachment
    | VideoAttachment


{-| Meta entity.
-}
type alias Meta =
    { small : Maybe MetaInfo
    , original : Maybe MetaInfo
    , focus : Maybe Focus
    }


{-| Possibilities for the `Meta` fields.
-}
type MetaInfo
    = ImageMeta
        { width : Maybe Int
        , height : Maybe Int
        , size : Maybe Int
        , aspect : Maybe Float
        }
    | VideoMeta
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
    { url : String
    , title : String
    , description : String
    , image : Maybe String
    , type_ : CardType
    , author_name : Maybe String
    , author_url : Maybe String
    , provider_name : Maybe String
    , provider_url : Maybe String
    , html : Maybe String
    , width : Maybe Int
    , height : Maybe Int
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
    , descendents : List Status
    }


{-| Emoji entity.
-}
type alias Emoji =
    { shortcode : String
    , static_url : String
    , url : String
    , visible_in_picker : Bool
    }


{-| Error entity.
-}
type alias Error =
    { error : String
    }


{-| Filter entity.
-}
type alias Filter =
    { id : String
    , phrase : String
    , context : List Context
    , expires_at : String
    , irreversible : Bool
    , whole_word : Bool
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
    , thumbnail : Maybe String
    , urls : Maybe URLs
    , stats : Stats
    , languages : List String
    , contact_account : Maybe Account
    }


{-| URLs entity.
-}
type alias URLs =
    { streaming_api : String
    }


{-| Stats entity.
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


{-| Mention entity.
-}
type alias Mention =
    { url : String
    , username : String
    , acct : String
    , id : String
    }


{-| Notification entity.
-}
type alias Notification =
    { id : String
    , type_ : NotificationType
    , created_at : String
    , account : Account
    , status : Maybe Status
    }


{-| Choices for `Notification.type_`.
-}
type NotificationType
    = FollowNotification
    | MentionNotification
    | ReblogNotification
    | FavouriteNotification


{-| Poll entity.
-}
type alias Poll =
    { id : String
    , expires_at : Maybe String
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
    , votes_count : Maybe Int
    }


{-| Push subscription entity.
-}
type alias PushSubscription =
    { id : String
    , endpoint : String
    , server_key : String
    , alerts : Value --not documented
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
    }


{-| Results entity.
-}
type alias Results =
    { accounts : List Account
    , statuses : List Status
    , hashtags : List Tag
    }


{-| Status entity.
-}
type alias Status =
    { id : String
    , uri : String
    , url : Maybe String
    , account : Account
    , in_reply_to_id : Maybe String
    , in_reply_to_account_id : Maybe String
    , reblog : Maybe WrappedStatus
    , content : String
    , created_at : String
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
    }


{-| Wrap a Status to prevent type recursion.
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
    , scheduled_at : String
    , params : List StatusParams
    , media_attachments : List Attachment
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
    , scheduled_at : Maybe String
    , application_id : String
    }


{-| Tag entity.
-}
type alias Tag =
    { name : String
    , url : String
    , history : List History
    }


{-| History entity.
-}
type alias History =
    { day : String
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
    }
