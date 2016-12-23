module Monzo.Monzo
    exposing
        ( Msg
        , Endpoint(..)
        , makeApiRequest
        , WhoAmIData
        , whoAmIDecoder
        , AccountsData
        , accountsDecoder
        )

import Http
import Json.Decode as Json


{-| Types used by this module
-}
type Msg
    = RequestResult (Result Http.Error String)


type alias Token =
    String


type alias Url =
    String


type Endpoint
    = WhoAmI
    | Accounts


{-| Internal module setup
-}
baseUrl : Url
baseUrl =
    "https://api.monzo.com"


buildUrl : Endpoint -> Url
buildUrl endpoint =
    case endpoint of
        WhoAmI ->
            baseUrl ++ "/ping/whoami"

        Accounts ->
            baseUrl ++ "/accounts"


{-| Makes a GET request with authentication
-}
buildAuthRequest : Token -> Url -> Json.Decoder a -> Http.Request a
buildAuthRequest token url decoder =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


{-| Makes an API request to the given endpoint
-}
makeApiRequest : Token -> Endpoint -> Json.Decoder a -> (Result String a -> msg) -> Cmd msg
makeApiRequest token endpoint decoder msg =
    let
        url =
            buildUrl endpoint

        request =
            buildAuthRequest token url decoder

        transformError : Result Http.Error a -> Result String a
        transformError =
            Result.mapError handleApiError
    in
        Http.send (transformError >> msg) request


{-| Transform an Http.Error into a nicely formatted String
-}
handleApiError : Http.Error -> String
handleApiError error =
    case error of
        Http.BadUrl str ->
            "Error: " ++ str ++ " is not a valid URL"

        Http.Timeout ->
            "Error: Response timed out"

        Http.NetworkError ->
            "Error: Network error (check your connection)"

        Http.BadStatus response ->
            "Error: " ++ response.body

        Http.BadPayload str response ->
            "Error: " ++ str


{-| Code related to the /accounts endpoint
-}
type alias AccountData =
    { id : String
    , created : String
    , description : String
    }


type alias AccountsData =
    List AccountData


accountsDecoder : Json.Decoder AccountsData
accountsDecoder =
    let
        accountDecoder =
            Json.map3 AccountData
                (Json.field "id" Json.string)
                (Json.field "created" Json.string)
                (Json.field "description" Json.string)
    in
        Json.field "accounts" (Json.list accountDecoder)


{-| Code related to the /whoami endpoint
-}
type alias WhoAmIData =
    { client_id : String
    , user_id : String
    }


whoAmIDecoder : Json.Decoder WhoAmIData
whoAmIDecoder =
    Json.map2 WhoAmIData
        (Json.field "client_id" Json.string)
        (Json.field "user_id" Json.string)
