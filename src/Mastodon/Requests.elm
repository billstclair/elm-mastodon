----------------------------------------------------------------------
--
-- Request.elm
-- Mastodon API requests.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Mastodon.Requests exposing (Request(..), Response)

{-| Types to represent the Mastodon REST API.

Documentation starts at <https://docs.joinmastodon.org/api/rest/accounts>

@docs Request, Response

-}

import File exposing (File)
import Http
import Mastodon.EncodeDecode as ED
import Mastodon.Entities as Entities exposing (Entity(..))


type alias RawRequest =
    { url : String
    , body : Http.Body
    , request : Request
    , processor : ( Request, Http.Response String ) -> Result ( Request, Http.Response String ) Response
    }


apiUrlPrefix : List String
apiUrlPrefix =
    [ "api", "v1" ]


apiReq =
    { accounts = "accounts"
    , apps = "apps"
    , blocks = "blocks"
    , custom_emojis = "custom_emojis"
    , domain_blocks = "domain_blocks"
    , endorsements = "endorsements"
    , favourites = "favourites"
    , filters = "filters"
    , follow_requests = "follow_requests"
    , suggestions = "suggestions"
    , instance = "instance"
    , lists = "lists"
    , media = "media"
    , mutes = "mutes"
    , notifications = "notifications"
    , polls = "polls"
    , reports = "reports"
    , scheduled_statuses = "scheduled_statuses"
    , search = "search"
    , statuses = "statuses"
    , timelines = "timelines"
    }


{-| A response from an API request.
-}
type alias Response =
    { request : Request
    , metadata : Http.Metadata
    , entity : Entity
    }


type alias ServerInfo =
    { server : String
    , token : String
    }


requestToRawRequest : ServerInfo -> Request -> RawRequest
requestToRawRequest serverInfo request =
    { url = ""
    , body = Http.emptyBody
    , request = request
    , processor = \_ -> Err ( request, Http.BadUrl_ "" )
    }


{-| An API request.

Broken down as in the documentation.

-}
type Request
    = NoRequest
    | AccountsRequest AccountsReq
      -- | AppsRequest AppsReq
    | BlocksRequest BlocksReq
      -- | CustomEmojiRequest CustomEmojiReq
      -- | DomainBlocksRequest DomainBlocksReq
      -- | EndorsementsRequest EndorsementReq
    | FavouritesRequest FavouritesReq
      -- | FiltersRequest FiltersReq
    | FollowRequest FollowReq
      -- | FollowSuggestionsRequest FollowSuggestionsReq
    | InstancesRequest
      -- | ListsRequest ListsReq
    | MediaAttachmentsRequest MediaAttachmentsReq
    | MutesRequest MutesReq
    | NotificationsRequest NotificationsReq
    | PollsRequest PollsReq
    | ReportsRequest ReportsReq
      -- | ScheduledStatusesRequest ScheduledStatusesReq
    | SearchRequest SearchReq
    | StatusesRequest StatusesReq
    | TimelinesRequest TimelinesReq


type AccountsReq
    = GetAccount { id : String }
    | VerifyCredentials
      -- UpdateCredentials {...}
    | GetFollowers { id : String, limit : Maybe Int }
    | GetFollowing { id : String, limit : Maybe Int }
    | GetStatuses
        { id : String
        , only_media : Bool
        , pinned : Bool
        , explude_replies : Bool
        , max_id : Maybe String
        , since_id : Maybe String
        , min_id : Maybe String
        , limit : Maybe Int
        , exclude_reblogs : Bool
        }
    | PostFollow { id : String, reblogs : Bool }
    | PostUnfollow { id : String }
    | GetRelationships { ids : List String }
    | GetSearchAccounts
        { q : String
        , limit : Maybe Int
        , resolve : Bool
        , following : Bool
        }


type BlocksReq
    = GetBlocks { limit : Maybe Int }
    | PostBlock { id : String }
    | PostUnblock { id : String }


type FavouritesReq
    = GetFavourites { limit : Maybe Int }
    | PostFavourite { id : String }
    | PostUnfavourite { id : String }


type FollowReq
    = GetFollowRequest { limit : Maybe Int }
    | PostAuthorizeFollow { id : String }
    | PostRejectFollow { id : String }


type MediaAttachmentsReq
    = PostMedia
        { file : File
        , description : String
        , focus : Entities.Focus
        }
    | PutMedia
        { id : String
        , description : String
        , focus : Entities.Focus
        }


type MutesReq
    = GetAccountMutes { limit : Maybe Int }
    | PostAccountMute
        { id : String
        , notifications : Bool
        }
    | PostAccountUnmute { id : String }
    | PostStatusMute { id : String }
    | PostStatusUnmute { id : String }



-- TODO:
-- | PostPushSubscription, GetPushSubscription
-- | PutPushSubscription, DeletePushSubscription


type NotificationsReq
    = GetNotifications
        { max_id : Maybe String
        , since_id : Maybe String
        , min_id : Maybe String
        , limit : Maybe Int
        , exclude_types : List Entities.NotificationType
        , account_id : Maybe String
        }
    | GetNotification { id : String }
    | PostClearNotifications
    | PostDismissNotification { id : String }


type PollsReq
    = GetPoll { id : String }
    | PostVotes
        { id : String
        , choices : List Int
        }


type ReportsReq
    = PostReports
        { account_id : String
        , status_ids : List String
        , comment : String
        , forward : Bool
        }


type SearchReq
    = GetSearch
        { q : String
        , resolve : Bool
        , limit : Maybe Int
        , offset : Maybe Int
        , following : Bool
        }


type alias PollDefinition =
    { options : List String
    , expires_in : Int
    , multiple : Bool
    , hide_totals : Bool
    }


type StatusesReq
    = GetStatus { id : String }
    | GetStatusContext { id : String }
    | GetStatusCard { id : String }
    | GetStatusRebloggedBy
        { id : String
        , limit : Maybe Int
        }
    | GetStatusFavouritedBy
        { id : String
        , limit : Maybe Int
        }
      -- Need to handle unique Idempotency-Key header
    | PostStatus
        { status : Maybe String
        , in_reply_to_id : Maybe String
        , media_ids : List String
        , poll : Maybe PollDefinition

        -- If included, then sensitive will be passed as true
        , spoiler_text : Maybe String
        , visibility : Entities.Visibility
        , scheduled_at : Maybe Entities.Datetime
        , language : Maybe Entities.ISO6391
        }
    | DeleteStatus { id : String }
    | PostReblogStatus { id : String }
    | PostUnreblogStatus { id : String }
    | PostPinStatus { id : String }
    | PostUnpinStatus { id : String }


type TimelinesReq
    = GetHomeTimeline
        { max_id : Maybe String
        , since_id : Maybe String
        , min_id : Maybe String
        , limit : Maybe Int
        }
    | GetConversations
        { max_id : Maybe String
        , since_id : Maybe String
        , min_id : Maybe String
        , limit : Maybe Int
        }
    | GetPublicTimeline
        { local : Bool
        , only_media : Bool
        , max_id : Maybe String
        , since_id : Maybe String
        , min_id : Maybe String
        , limit : Maybe Int
        }
    | GetTagTimeline
        { local : Bool
        , only_media : Bool
        , max_id : Maybe String
        , since_id : Maybe String
        , min_id : Maybe String
        , limit : Maybe Int
        }
    | GetListTimeline
        { list_id : String
        , max_id : Maybe String
        , since_id : Maybe String
        , min_id : Maybe String
        , limit : Maybe Int
        }
