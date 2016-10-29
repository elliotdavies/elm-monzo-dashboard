module Monzo.Monzo
    exposing
        ( Msg
        , Token
        , Endpoint(..)
        , makeApiRequest
        , whoAmIHandler
        , accountsHandler
        )

import Http
import Task
import Json.Decode as Json exposing ((:=))
import Debug


{-| Types used by this module
-}
type Msg
    = RequestSucceed Http.Response
    | RequestFail Http.RawError


type alias Token =
    String


type alias Url =
    String


type Endpoint
    = WhoAmI
    | Accounts


{-| Internal module setup
-}
baseUrl : String
baseUrl =
    "https://api.monzo.com"


{-| Makes a GET request with authentication
-}
getRequestWithAuth : Token -> Url -> Task.Task Http.RawError Http.Response
getRequestWithAuth token url =
    Http.send Http.defaultSettings
        { verb = "GET"
        , headers = [ ( "Authorization", "Bearer " ++ token ) ]
        , url = url
        , body = Http.empty
        }


makeRequest : Token -> Url -> Cmd Msg
makeRequest token url =
    Task.perform RequestFail RequestSucceed (getRequestWithAuth token url)


{-| Makes an API request to the given endpoint
-}
makeApiRequest : Maybe Token -> Endpoint -> Cmd Msg
makeApiRequest maybeToken endpoint =
    let
        url =
            case endpoint of
                WhoAmI ->
                    baseUrl ++ "/ping/whoami"

                Accounts ->
                    baseUrl ++ "/accounts"
    in
        case maybeToken of
            Just token ->
                makeRequest token url

            Nothing ->
                Cmd.none


{-| Extracts either the "value" of an HTTP response from the API, or the error if the request failed
-}
extractValueOrError : Msg -> Result String String
extractValueOrError msg =
    case msg of
        RequestSucceed data ->
            case data.value of
                Http.Text value ->
                    Ok value

                _ ->
                    Debug.crash "Somehow hit Http.Blob"

        RequestFail failure ->
            Err ("Request failed: " ++ (toString failure))


{-| Code related to the /accounts endpoint
-}
type alias AccountData =
    { id : String
    , created : String
    , description : String
    }


accountsHandler : Msg -> Result String (List AccountData)
accountsHandler msg =
    case extractValueOrError msg of
        Ok data ->
            Json.decodeString accountsDecoder data

        Err err ->
            Err err


accountsDecoder : Json.Decoder (List AccountData)
accountsDecoder =
    let
        accountDecoder =
            Json.object3 AccountData
                ("id" := Json.string)
                ("created" := Json.string)
                ("description" := Json.string)
    in
        "accounts" := Json.list accountDecoder


{-| Code related to the /whoami endpoint
-}
type alias WhoAmIData =
    { client_id : String
    , user_id : String
    }


whoAmIHandler : Msg -> Result String WhoAmIData
whoAmIHandler msg =
    case extractValueOrError msg of
        Ok data ->
            Json.decodeString whoAmIDecoder data

        Err err ->
            Err err


whoAmIDecoder : Json.Decoder WhoAmIData
whoAmIDecoder =
    Json.object2 WhoAmIData
        ("client_id" := Json.string)
        ("user_id" := Json.string)
