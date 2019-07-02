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
    ( ServerInfo, Request(..), Response, Error(..)
    , serverRequest
    , AccountsReq(..), AppsReq(..), BlocksReq(..), FavouritesReq(..)
    , FollowReq(..), MediaAttachmentsReq(..), MutesReq(..), NotificationsReq(..)
    , PollsReq(..), ReportsReq(..), SearchReq(..), StatusesReq(..), TimelinesReq(..)
    , Paging, SourceUpdate, PollDefinition
    , emptyRawRequest, emptyServerInfo, requestToRawRequest
    )

{-| Types to represent the Mastodon REST API.

Documentation starts at <https://docs.joinmastodon.org/api/rest/accounts>


# Basic Types

@docs ServerInfo, Request, Response, Error


# Creating an HTTP request

@docs serverRequest


# Request details

@docs AccountsReq, AppsReq, BlocksReq, FavouritesReq
@docs FollowReq, MediaAttachmentsReq, MutesReq, NotificationsReq
@docs PollsReq, ReportsReq, SearchReq, StatusesReq, TimelinesReq


# Non-atomic data in requests

@docs Paging, SourceUpdate, PollDefinition


# Testing

@docs emptyRawRequest, emptyServerInfo, requestToRawRequest

-}

import File exposing (File)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Mastodon.EncodeDecode as ED
import Mastodon.Entities as Entities exposing (Entity(..))
import OAuthMiddleware exposing (ResponseToken)
import Task
import Url.Builder as Builder exposing (QueryParameter, relative)


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

The `GetXxx` requests require no authentication token, unless the status has `Private` visibility.

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

A few requests do not require a token. Most do, and will error if you don't include one.

-}
type alias ServerInfo =
    { server : String
    , token : Maybe ResponseToken
    }


apiUrlPrefix : String
apiUrlPrefix =
    "/api/v1/"


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


{-| Encodes an error from the server request.

Same as `Http.Error`, but includes `Http.Metadata` when it's available.

-}
type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String
    | BadBody Http.Metadata String


type alias RawRequest =
    { method : String
    , token : Maybe ResponseToken
    , url : String
    , body : Http.Body
    , request : Request
    , decoder : Decoder Entity
    }


{-| Create an HTTP request for the server.

The `id` is whatever you need, besides the `Request`, to identify the returned
`Error` or `Response`

-}
serverRequest : (id -> Result Error Response -> msg) -> ServerInfo -> id -> Request -> Cmd msg
serverRequest tagger serverInfo id request =
    requestToRawRequest serverInfo request
        |> rawRequestToCmd (tagger id)


rawRequestToCmd : (Result Error Response -> msg) -> RawRequest -> Cmd msg
rawRequestToCmd tagger rawRequest =
    if rawRequest.method == "" then
        Cmd.none

    else
        Http.request
            { method = rawRequest.method
            , headers =
                case rawRequest.token of
                    Nothing ->
                        []

                    Just token ->
                        OAuthMiddleware.use token [ userAgentHeader ]
            , url = rawRequest.url
            , body = rawRequest.body
            , expect =
                Http.expectStringResponse tagger <| processResponse rawRequest
            , timeout = Nothing
            , tracker = Nothing
            }


processResponse : RawRequest -> Http.Response String -> Result Error Response
processResponse rawRequest response =
    case response of
        Http.BadUrl_ s ->
            Err <| BadUrl s

        Http.Timeout_ ->
            Err <| Timeout

        Http.NetworkError_ ->
            Err <| NetworkError

        Http.BadStatus_ metadata body ->
            Err <| BadStatus metadata body

        Http.GoodStatus_ metadata body ->
            case JD.decodeString rawRequest.decoder body of
                Err _ ->
                    Err <| BadBody metadata ("JSON decoding error on: " ++ body)

                Ok entity ->
                    Ok
                        { request = rawRequest.request
                        , metadata = metadata
                        , entity = entity
                        }


{-| Only required by GitHub that I know of, but can't hurt.
-}
userAgentHeader : Http.Header
userAgentHeader =
    Http.header "User-Agent" "Mammudeck"


{-| An empty raw request.

Exposed only for testing in `elm repl`.

-}
emptyRawRequest : RawRequest
emptyRawRequest =
    { method = ""
    , token = Nothing
    , url = ""
    , body = Http.emptyBody
    , request = InstanceRequest
    , decoder = JD.fail "Unspecified decoder"
    }


{-| For testing in `elm repl`.
-}
emptyServerInfo : ServerInfo
emptyServerInfo =
    { server = "mastodon.social"
    , token = Nothing
    }


{-| Exposed for testing in `elm repl`.
-}
requestToRawRequest : ServerInfo -> Request -> RawRequest
requestToRawRequest serverInfo request =
    let
        raw =
            { emptyRawRequest
                | token = serverInfo.token
                , request = request
            }

        res =
            case request of
                AccountsRequest req ->
                    accountsReq req raw

                AppsRequest req ->
                    appsReq req raw

                _ ->
                    -- TODO, all the other `Request` types
                    raw
    in
    { res
        | url = "https://" ++ serverInfo.server ++ apiUrlPrefix ++ res.url
    }



---
--- Some utility functions
---


qps : List (Maybe QueryParameter) -> List QueryParameter
qps params =
    List.filterMap identity params


sp : String -> Maybe String -> Maybe QueryParameter
sp name value =
    case value of
        Nothing ->
            Nothing

        Just v ->
            Just <| Builder.string name v


ip : String -> Maybe Int -> Maybe QueryParameter
ip name value =
    case value of
        Nothing ->
            Nothing

        Just v ->
            Just <| Builder.int name v


bp : String -> Bool -> Maybe QueryParameter
bp name value =
    if value then
        Just <| Builder.string name "true"

    else
        Nothing


bpt : String -> Bool -> Maybe QueryParameter
bpt name value =
    if value then
        Nothing

    else
        Just <| Builder.string name "false"


m =
    { get = "GET"
    , post = "POST"
    , put = "PUT"
    , patch = "PATCH"
    }


pagingParameters : Maybe Paging -> List QueryParameter
pagingParameters paging =
    case paging of
        Nothing ->
            []

        Just { max_id, since_id, min_id, limit } ->
            qps
                [ sp "max_id" max_id
                , sp "since_id" since_id
                , sp "min_id" min_id
                , ip "limit" limit
                ]


idListValue : List String -> String
idListValue ids =
    let
        idlist =
            List.map (\s -> "'" ++ s ++ "'") ids
                |> String.join ","
    in
    "[" ++ idlist ++ "]"


decoders =
    { account = ED.accountDecoder |> JD.map AccountEntity
    , accountList = JD.list ED.accountDecoder |> JD.map AccountListEntity
    , status = ED.statusDecoder |> JD.map StatusEntity
    , statusList = JD.list ED.statusDecoder |> JD.map StatusListEntity
    , relationship = ED.relationshipDecoder |> JD.map RelationshipEntity
    , relationshipList =
        JD.list ED.relationshipDecoder
            |> JD.map RelationshipListEntity
    , app = ED.appDecoder |> JD.map AppEntity
    }



---
--- Modify the incoming RawRequest to perform the requested operation.
--- This means at least setting the `method`, `url`, and `decoder`.
--- For POST, PUT, and PATCH requests, this also sets the `body`,
--- usually to a JSON string.
--- The `url` value is just the part after "https://<server>/api/v1/"
---


accountsReq : AccountsReq -> RawRequest -> RawRequest
accountsReq req rawreq =
    let
        res =
            { rawreq | method = m.get }

        r =
            apiReq.accounts
    in
    case req of
        GetAccount { id } ->
            { res
                | url = relative [ r, id ] []
                , decoder = decoders.account
            }

        GetVerifyCredentials ->
            { res
                | url = relative [ r, "verify_credentials" ] []
                , decoder = decoders.account
            }

        PatchUpdateCredentials _ ->
            -- TODO
            rawreq

        GetFollowers { id, limit } ->
            { res
                | url =
                    relative [ r, id, "followers" ] <|
                        qps [ ip "limit" limit ]
                , decoder = decoders.accountList
            }

        GetFollowing { id, limit } ->
            { res
                | url =
                    relative [ r, id, "following" ] <|
                        qps [ ip "limit" limit ]
                , decoder = decoders.accountList
            }

        GetStatuses { id, only_media, pinned, exclude_replies, paging, exclude_reblogs } ->
            { res
                | url =
                    relative [ r, id, "statuses" ] <|
                        List.concat
                            [ qps
                                [ bp "only_media" only_media
                                , bp "pinned" pinned
                                , bp "exclude_replies" exclude_replies
                                , bp "exclude_reblogs" exclude_reblogs
                                ]
                            , pagingParameters paging
                            ]
                , decoder = decoders.statusList
            }

        PostFollow { id, reblogs } ->
            { res
                | method = m.post
                , url =
                    relative [ r, id, "follow" ] []
                , body =
                    if not reblogs then
                        -- Defaults to false
                        res.body

                    else
                        Http.jsonBody <|
                            JE.object
                                [ ( "reblogs", JE.bool reblogs ) ]
                , decoder = decoders.relationship
            }

        PostUnfollow { id } ->
            { res
                | method = m.post
                , url =
                    relative [ r, id, "unfollow" ] []
                , decoder = decoders.relationship
            }

        -- This is sending ?id=['1'] (with the value url-encoded)
        -- That's how I read the documentation.
        -- The web clients I have send ?id[]=1
        -- I don't know how to get them to request multiple ids.
        -- So this may not work.
        GetRelationships { ids } ->
            { res
                | url =
                    relative [ r, "relationships" ]
                        [ Builder.string "id" <| idListValue ids ]
                , decoder = decoders.relationshipList
            }

        GetSearchAccounts { q, limit, resolve, following } ->
            { res
                | url =
                    relative [ r, "search" ] <|
                        qps
                            [ sp "q" <| Just q
                            , ip "limit" limit
                            , bp "resolve" resolve
                            , bp "following" following
                            ]
                , decoder = decoders.accountList
            }


appsReq : AppsReq -> RawRequest -> RawRequest
appsReq req rawreq =
    let
        res =
            { rawreq | method = m.get }

        r =
            apiReq.apps
    in
    case req of
        PostApp { client_name, redirect_uris, scopes, website } ->
            { res
                | method = m.post
                , url =
                    relative [ r ] []
                , body =
                    Http.jsonBody <|
                        JE.object
                            (List.concat
                                [ [ ( "client_name", JE.string client_name )
                                  , ( "redirect_uris", JE.string redirect_uris )
                                  , ( "scopes"
                                    , JE.string <| String.join " " scopes
                                    )
                                  ]
                                , case website of
                                    Nothing ->
                                        []

                                    Just w ->
                                        [ ( "website", JE.string w ) ]
                                ]
                            )
                , decoder = decoders.app
            }

        GetVerifyAppCredentials ->
            { res
                | url =
                    relative [ r, "verify_credentials" ] []
                , decoder = decoders.app
            }