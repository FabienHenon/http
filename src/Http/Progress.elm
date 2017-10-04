effect module Http.Progress
    where { subscription = MySub }
    exposing
        ( Progress(..)
        , TrackingType(..)
        , track
        )

{-| Track the progress of an HTTP request. This can be useful if you are
requesting a large amount of data and want to show the user a progress bar
or something.

Here is an example usage: [demo][] and [code][].

[demo]: https://hirafuji.com.br/elm/http-progress-example/
[code]: https://gist.github.com/pablohirafuji/fa373d07c42016756d5bca28962008c4

**Note:** If you stop tracking progress, you cancel the request.

# Progress
@docs Progress, TrackingType, track

-}

import Dict
import Http
import Http.Internal exposing (Request(Request))
import Task exposing (Task)
import Platform exposing (Router)
import Process


-- PROGRESS


{-| The progress of an HTTP request.

You start with `None`. As data starts to come in, you will see `Some`. The
`bytesExpected` field will match the `Content-Length` header, indicating how
long the response body is in bytes (8-bits). The `bytes` field indicates how
many bytes have been loaded so far, so if you want progress as a percentage,
you would say:

    Some { bytes, bytesExpected } ->
      toFloat bytes / toFloat bytesExpected

You will end up with `Fail` or `Done` depending on the success of the request.
-}
type Progress data
    = None
    | Some TrackingType { bytes : Int, bytesExpected : Int }
    | Fail Http.Error
    | Done data


{-| Tracking type. You can either track the upload progress, or the download progress of the request.
Or both of them
-}
type TrackingType
    = Upload
    | Download
    | Both



-- TRACK


{-| Create a subscription that tracks the progress of an HTTP request.

See it in action in this example: [demo][] and [code][].

[demo]: https://hirafuji.com.br/elm/http-progress-example/
[code]: https://gist.github.com/pablohirafuji/fa373d07c42016756d5bca28962008c4
-}
track : String -> (Progress data -> msg) -> TrackingType -> Http.Request data -> Sub msg
track id toMessage trackingType (Request request) =
    subscription <|
        Track id <|
            { request = Http.Internal.map (Done >> toMessage) request
            , toDownloadProgress =
                if trackingType == Download || trackingType == Both then
                    Just (Some Download >> toMessage)
                else
                    Nothing
            , toUploadProgress =
                if trackingType == Upload || trackingType == Both then
                    Just (Some Upload >> toMessage)
                else
                    Nothing
            , toError = Fail >> toMessage
            }


type alias TrackedRequest msg =
    { request : Http.Internal.RawRequest msg
    , toDownloadProgress : Maybe ({ bytes : Int, bytesExpected : Int } -> msg)
    , toUploadProgress : Maybe ({ bytes : Int, bytesExpected : Int } -> msg)
    , toError : Http.Error -> msg
    }


map : (a -> b) -> TrackedRequest a -> TrackedRequest b
map func { request, toDownloadProgress, toUploadProgress, toError } =
    { request = Http.Internal.map func request
    , toDownloadProgress = Maybe.map (flip (>>) func) toDownloadProgress
    , toUploadProgress = Maybe.map (flip (>>) func) toUploadProgress
    , toError = toError >> func
    }



-- SUBSCRIPTIONS


type MySub msg
    = Track String (TrackedRequest msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (Track id trackedRequest) =
    Track id (map func trackedRequest)



-- EFFECT MANAGER


type alias State =
    Dict.Dict String Process.Id


init : Task Never State
init =
    Task.succeed Dict.empty



-- APP MESSAGES


onEffects : Platform.Router msg Never -> List (MySub msg) -> State -> Task Never State
onEffects router subs state =
    let
        subDict =
            collectSubs subs

        leftStep id process ( dead, ongoing, new ) =
            ( Process.kill process :: dead
            , ongoing
            , new
            )

        bothStep id process _ ( dead, ongoing, new ) =
            ( dead
            , Dict.insert id process ongoing
            , new
            )

        rightStep id trackedRequest ( dead, ongoing, new ) =
            ( dead
            , ongoing
            , ( id, trackedRequest ) :: new
            )

        ( dead, ongoing, new ) =
            Dict.merge leftStep bothStep rightStep state subDict ( [], Dict.empty, [] )
    in
        Task.sequence dead
            |> Task.andThen (\_ -> spawnRequests router new ongoing)


spawnRequests : Router msg Never -> List ( String, TrackedRequest msg ) -> State -> Task Never State
spawnRequests router trackedRequests state =
    case trackedRequests of
        [] ->
            Task.succeed state

        ( id, trackedRequest ) :: others ->
            Process.spawn (toTask router trackedRequest)
                |> Task.andThen (\process -> spawnRequests router others (Dict.insert id process state))


toTask : Router msg Never -> TrackedRequest msg -> Task Never ()
toTask router { request, toDownloadProgress, toUploadProgress, toError } =
    Native.Http.toTask request (Maybe.map ((<<) (Platform.sendToApp router)) toDownloadProgress) (Maybe.map ((<<) (Platform.sendToApp router)) toUploadProgress)
        |> Task.andThen (Platform.sendToApp router)
        |> Task.onError (Platform.sendToApp router << toError)



-- COLLECT SUBS AS DICT


type alias SubDict msg =
    Dict.Dict String (TrackedRequest msg)


collectSubs : List (MySub msg) -> SubDict msg
collectSubs subs =
    List.foldl addSub Dict.empty subs


addSub : MySub msg -> SubDict msg -> SubDict msg
addSub (Track id trackedRequest) subDict =
    let
        request =
            trackedRequest.request

        uid =
            id ++ request.method ++ request.url
    in
        Dict.insert uid trackedRequest subDict



-- SELF MESSAGES


onSelfMsg : Platform.Router msg Never -> Never -> State -> Task Never State
onSelfMsg router _ state =
    Task.succeed state
