----------------------------------------------------------------------
--
-- Entities.elm
-- Mastodon API entities from https://docs.joinmastodon.org/api/entities/
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Entities exposing (Account)

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
