module Monzo
    exposing
        ( Msg
        , auth
        , Token
        , WhoAmI
        , whoAmIInit
        , whoAmIRequest
        , whoAmIHandler
        )

import Http
import Task
import Json.Decode as Json exposing ((:=))
import Debug


type Msg
    = RequestSucceed Http.Response
    | RequestFail Http.RawError


type alias Token =
    String


type alias Url =
    String


auth : Token
auth =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJjaSI6Im9hdXRoY2xpZW50XzAwMDA5NFB2SU5ER3pUM2s2dHo4anAiLCJleHAiOjE0NzUyNTc2MDcsImlhdCI6MTQ3NTIzNjAwNywianRpIjoidG9rXzAwMDA5Q3Ezdnp1Qkh6MXhMdlhYb3YiLCJ1aSI6InVzZXJfMDAwMDk0Umd5NGJoUm55MmVURkdySiIsInYiOiIyIn0.xRBJ9J8brldCSwx9TCK4_HOzVaPC4ajvomLMViDCLfQ"


getWithAuth : Token -> Url -> Task.Task Http.RawError Http.Response
getWithAuth token url =
    Http.send Http.defaultSettings
        { verb = "GET"
        , headers = [ ( "Authorization", "Bearer " ++ token ) ]
        , url = url
        , body = Http.empty
        }


makeRequest : Token -> Url -> Cmd Msg
makeRequest token url =
    Task.perform RequestFail RequestSucceed (getWithAuth token url)


{-| Code related to the whoAmI method
-}
type alias WhoAmI =
    { client_id : String
    , user_id : String
    }


whoAmIInit : WhoAmI
whoAmIInit =
    WhoAmI "" ""


whoAmIRequest : Token -> Cmd Msg
whoAmIRequest token =
    let
        url =
            "https://api.monzo.com/ping/whoami"
    in
        makeRequest token url


whoAmIHandler : Msg -> WhoAmI
whoAmIHandler msg =
    let
        extract value =
            case Json.decodeString whoAmIDecoder value of
                Ok ok ->
                    WhoAmI ok.client_id ok.user_id

                Err err ->
                    WhoAmI "e" "f"
    in
        case msg of
            RequestSucceed data ->
                case data.value of
                    Http.Text value ->
                        extract value

                    _ ->
                        Debug.crash "Somehow hit Http.Blob"

            RequestFail fail ->
                WhoAmI "c" "d"


whoAmIDecoder : Json.Decoder WhoAmI
whoAmIDecoder =
    Json.object2 WhoAmI
        ("client_id" := Json.string)
        ("user_id" := Json.string)
