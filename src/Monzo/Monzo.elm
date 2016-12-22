module Monzo.Monzo
    exposing
        ( Msg
        , Endpoint(..)
        , makeApiRequest
        , handleApiResponse
        , whoAmIDecoder
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
buildAuthRequest : Token -> Url -> Http.Request String
buildAuthRequest token url =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-| Makes an API request to the given endpoint
-}
makeApiRequest : Token -> Endpoint -> Cmd Msg
makeApiRequest token endpoint =
    let
        url =
            buildUrl endpoint

        request =
            buildAuthRequest token url
    in
        Http.send RequestResult request


{-| Extracts either the "value" of an HTTP response from the API, or the error if the request failed
-}
handleApiResponse : Msg -> Result String String
handleApiResponse msg =
    case msg of
        RequestResult (Ok data) ->
            Ok data

        RequestResult (Err failure) ->
            Err ("Request failed: " ++ (toString failure))


{-| Code related to the /accounts endpoint
-}
type alias AccountData =
    { id : String
    , created : String
    , description : String
    }


type alias AccountsData =
    List AccountData


decodeAccounts : String -> Result String AccountsData
decodeAccounts =
    Json.decodeString accountsDecoder


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
