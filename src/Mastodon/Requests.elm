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


module Mastodon.Requests exposing
    ( Request(..), Response
    , AccountsReq(..), AppsReq(..), BlocksReq(..), FavouritesReq(..)
    , FollowReq(..), MediaAttachmentsReq(..), MutesReq(..), NotificationsReq(..)
    , PollsReq(..), ReportsReq(..), SearchReq(..), StatusesReq(..), TimelinesReq(..)
    , Paging, SourceUpdate, PollDefinition
    )

{-| Types to represent the Mastodon REST API.

Documentation starts at <https://docs.joinmastodon.org/api/rest/accounts>


# Request and Response

@docs Request, Response


# The server requests.

@docs AccountsReq, AppsReq, BlocksReq, FavouritesReq
@docs FollowReq, MediaAttachmentsReq, MutesReq, NotificationsReq
@docs PollsReq, ReportsReq, SearchReq, StatusesReq, TimelinesReq


# Non-atomic data in requests

@docs Paging, SourceUpdate, PollDefinition

-}

import File exposing (File)
import Http
import Mastodon.EncodeDecode as ED
import Mastodon.Entities as Entities exposing (Entity(..))


{-| An API request.

Broken down as in the documentation.

`InstanceRequest` and `CustomEmojisRequest` do not require an authentication token.

-}
type Request
    = AccountsRequest AccountsReq
    | AppsRequest AppsReq
    | BlocksRequest BlocksReq
    | CustomEmojisRequest
      -- | DomainBlocksRequest DomainBlocksReq
      -- | EndorsementsRequest EndorsementReq
    | FavouritesRequest FavouritesReq
      -- | FiltersRequest FiltersReq
    | FollowRequest FollowReq
      -- | FollowSuggestionsRequest FollowSuggestionsReq
    | InstanceRequest
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


{-| Parameters to control paging for requests that return lists.
-}
type alias Paging =
    { max_id : Maybe String
    , since_id : Maybe String
    , min_id : Maybe String
    , limit : Maybe Int
    }


{-| Updated account `Source` information
-}
type alias SourceUpdate =
    { privacy : Maybe String
    , sensitive : Maybe Bool
    , language : Maybe Entities.ISO6391
    }


{-| GET/POST/PATCH /api/v1/accounts

`GetAccount` does not require an authentication token.

-}
type AccountsReq
    = GetAccount { id : String }
    | GetVerifyCredentials
    | PatchUpdateCredentials
        { display_name : Maybe String
        , note : Maybe String
        , avatar : Maybe File
        , header : Maybe File
        , locked : Maybe Bool
        , source : Maybe SourceUpdate
        , fields_attributes : Maybe (List Entities.Field)
        }
    | GetFollowers { id : String, limit : Maybe Int }
    | GetFollowing { id : String, limit : Maybe Int }
    | GetStatuses
        { id : String
        , only_media : Bool
        , pinned : Bool
        , exclude_replies : Bool
        , paging : Maybe Paging
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


{-| GET/POST /api/v1/apps
-}
type AppsReq
    = PostApp
        { client_name : String
        , redirect_uris : String
        , scopes : List String
        , website : Maybe String
        }
    | GetVerifyAppCredentials


{-| GET/POST /api/v1/blocks
-}
type BlocksReq
    = GetBlocks { limit : Maybe Int }
    | PostBlock { id : String }
    | PostUnblock { id : String }


{-| GET/POST /api/v1/favourites
-}
type FavouritesReq
    = GetFavourites { limit : Maybe Int }
    | PostFavourite { id : String }
    | PostUnfavourite { id : String }


{-| GET/POST /api/v1/follow\_requests
-}
type FollowReq
    = GetFollowRequest { limit : Maybe Int }
    | PostAuthorizeFollow { id : String }
    | PostRejectFollow { id : String }


{-| GET/POST /api/v1/media
-}
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


{-| GET/POST /api/v1/mutes
-}
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


{-| GET/POST /api/v1/notifications
-}
type NotificationsReq
    = GetNotifications
        { paging : Maybe Paging
        , exclude_types : List Entities.NotificationType
        , account_id : Maybe String
        }
    | GetNotification { id : String }
    | PostClearNotifications
    | PostDismissNotification { id : String }


{-| GET/POST /api/v1/polls

`GetPoll` does not require an authentication token.

-}
type PollsReq
    = GetPoll { id : String }
    | PostVotes
        { id : String
        , choices : List Int
        }


{-| POST /api/v1/reports
-}
type ReportsReq
    = PostReports
        { account_id : String
        , status_ids : List String
        , comment : String
        , forward : Bool
        }


{-| GET/POST /api/v1/search
-}
type SearchReq
    = GetSearch
        { q : String
        , resolve : Bool
        , limit : Maybe Int
        , offset : Maybe Int
        , following : Bool
        }


{-| Define a Poll as part of a posted new Status
-}
type alias PollDefinition =
    { options : List String
    , expires_in : Int
    , multiple : Bool
    , hide_totals : Bool
    }


{-| GET/POST /api/v1/statuses

None of the `GetXxx` requests require an authentication token.

-}
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


{-| GET/POST /api/v1/timelines

`GetPublicTimeline` and `GetTagTimeline` do not require an authentication token.

-}
type TimelinesReq
    = GetHomeTimeline
        { paging : Maybe Paging
        }
    | GetConversations
        { paging : Maybe Paging
        }
    | GetPublicTimeline
        { local : Bool
        , only_media : Bool
        , paging : Maybe Paging
        }
    | GetTagTimeline
        { local : Bool
        , only_media : Bool
        , paging : Maybe Paging
        }
    | GetListTimeline
        { list_id : String
        , paging : Maybe Paging
        }


{-| A response from an API request.

The `request` is a copy of the `Request` that was sent over the wire,
for cases where that isn't obvious from the `Entity` tag.

The `metadata` is `Http.Metadata` for a successful request, mostly so
you can get to the headers, if you need them.

-}
type alias Response =
    { request : Request
    , metadata : Http.Metadata
    , entity : Entity
    }


{-| Used to create the HTTP URL and fill in its authentication token.

Example `server`: "mastodon.social". It's the host name for the URL.

A few requests do not require a token. Most do.

-}
type alias ServerInfo =
    { server : String
    , token : String
    }


type alias RawRequest =
    { url : String
    , body : Http.Body
    , request : Request
    , processor : ( Request, Http.Response String ) -> Result ( Request, Http.Response String ) Response
    }


apiUrlPrefix : List String
apiUrlPrefix =
    [ "api", "v1" ]


{-| Prevent misspellings of the URL components.
-}
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


requestToRawRequest : ServerInfo -> Request -> RawRequest
requestToRawRequest serverInfo request =
    { url = ""
    , body = Http.emptyBody
    , request = request
    , processor = \_ -> Err ( request, Http.BadUrl_ "" )
    }
