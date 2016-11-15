port module Monzo.Auth
    exposing
        ( Flags
        , Model
        , Msg(LogOut)
        , init
        , update
        , initialAuthUrl
        , loggedIn
        , storeStateToken
        , deleteStateToken
        , storeAccessToken
        , deleteAccessToken
        )

import Http
import Json.Decode as Json
import Task
import Time


port storeStateToken : String -> Cmd msg


port deleteStateToken : () -> Cmd msg


port storeAccessToken : String -> Cmd msg


port deleteAccessToken : () -> Cmd msg


type alias Flags =
    { clientId : String
    , clientSecret : String
    , url : String
    , stateToken : Maybe String
    , accessToken : Maybe String
    }


type alias Model =
    { clientId : String
    , accessToken : Maybe String
    , stateToken : Maybe String
    , redirectUri : String
    , errorMsg : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        parsedAddress : String
        parsedAddress =
            parseAddressFromUrl flags.url

        parsedTokens : Maybe ( String, String )
        parsedTokens =
            parseTokensFromUrl flags.url

        authorizationCodeFromUrl : Maybe String
        authorizationCodeFromUrl =
            -- If we're in the middle of the auth process, Monzo will have returned an auth code
            Maybe.map Tuple.first parsedTokens

        stateTokenFromUrl : Maybe String
        stateTokenFromUrl =
            -- If we're in the middle of the auth process, Monzo will have returned a state token
            Maybe.map Tuple.second parsedTokens

        hasStoredStateToken : Bool
        hasStoredStateToken =
            -- If we're in the middle of the auth process, we should have a state token already in localstorage
            case flags.stateToken of
                Just token ->
                    True

                Nothing ->
                    False

        stateTokensMatch : Bool
        stateTokensMatch =
            -- If Monzo returned a state token and we also have one in localstorage, check they match
            case Maybe.map2 (==) stateTokenFromUrl flags.stateToken of
                Just True ->
                    True

                _ ->
                    False

        cmds : Cmd Msg
        cmds =
            let
                -- If the tokens match we can finish the auth process
                authCmd =
                    if hasStoredStateToken && stateTokensMatch then
                        finishAuth (Maybe.withDefault "" authorizationCodeFromUrl) flags.clientId flags.clientSecret parsedAddress
                    else
                        Cmd.none

                -- If we're just starting the auth process, generate a state token
                tokenCmd =
                    if not hasStoredStateToken then
                        generateStateToken
                    else
                        Cmd.none
            in
                Cmd.batch [ authCmd, tokenCmd ]
    in
        ( { clientId = flags.clientId
          , accessToken = flags.accessToken
          , stateToken = flags.stateToken
          , redirectUri = parsedAddress
          , errorMsg = Nothing
          }
        , cmds
        )


loggedIn : Model -> Bool
loggedIn model =
    case model.accessToken of
        Just token ->
            True

        Nothing ->
            False


parseAddressFromUrl : String -> String
parseAddressFromUrl url =
    let
        address =
            url
                |> String.split "?"
                |> List.head
    in
        Maybe.withDefault "" address


parseTokensFromUrl : String -> Maybe ( String, String )
parseTokensFromUrl url =
    let
        toTuple list =
            case list of
                key :: value :: _ ->
                    Just ( key, value )

                _ ->
                    Nothing

        findIn list key =
            list
                |> List.filter (\a -> Tuple.first a == key)
                |> List.head

        args =
            url
                |> String.split "?"
                |> List.drop 1
                |> List.concatMap (String.split "&")
                |> List.map (String.split "=")
                |> List.filterMap toTuple

        code =
            findIn args "code"

        state =
            findIn args "state"
    in
        case ( code, state ) of
            ( Just c, Just s ) ->
                Just ( Tuple.second c, Tuple.second s )

            _ ->
                Nothing


type Msg
    = AuthResult (Result Http.Error String)
    | NewStateToken String
    | LogOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthResult (Err err) ->
            ( { model | errorMsg = Just (toString err) }, Cmd.none )

        AuthResult (Ok token) ->
            let
                accessTokenCmd =
                    storeAccessToken token

                stateTokenCmd =
                    deleteStateToken ()
            in
                ( { model | accessToken = Just token }, Cmd.batch [ accessTokenCmd, stateTokenCmd ] )

        NewStateToken token ->
            ( { model | stateToken = Just token }, storeStateToken token )

        LogOut ->
            ( { model | accessToken = Nothing }, deleteAccessToken () )


generateStateToken : Cmd Msg
generateStateToken =
    -- Take the current time to use as a state token
    Task.map toString Time.now
        |> Task.perform NewStateToken


encodeUriParameters : List ( String, String ) -> String
encodeUriParameters keyValuePairs =
    keyValuePairs
        |> List.map (\( k, v ) -> k ++ "=" ++ (Http.encodeUri v))
        |> String.join "&"


finishAuth : String -> String -> String -> String -> Cmd Msg
finishAuth authorizationCode clientId clientSecret redirectUri =
    let
        url =
            "https://api.monzo.com/oauth2/token"

        fields =
            [ ( "grant_type", "authorization_code" )
            , ( "code", authorizationCode )
            , ( "client_id", clientId )
            , ( "client_secret", clientSecret )
            , ( "redirect_uri", redirectUri )
            ]

        body =
            fields
                |> encodeUriParameters
                |> Http.stringBody "application/x-www-form-urlencoded"

        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = url
                , body = body
                , expect = Http.expectJson accessTokenDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send AuthResult request


accessTokenDecoder : Json.Decoder String
accessTokenDecoder =
    Json.field "access_token" Json.string



-- These fields are also returned from the final auth POST
--(Json.field "client_id" Json.string)
--(Json.field "expires_in" Json.int)
--(Json.field "token_type" Json.string)
--(Json.field "user_id" Json.string)


initialAuthUrl : Model -> String
initialAuthUrl model =
    case model.stateToken of
        Just token ->
            let
                params =
                    [ ( "client_id", model.clientId )
                    , ( "redirect_uri", model.redirectUri )
                    , ( "response_type", "code" )
                    , ( "state", token )
                    ]
            in
                "https://auth.getmondo.co.uk?" ++ (encodeUriParameters params)

        Nothing ->
            "/"
