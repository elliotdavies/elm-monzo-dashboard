port module Monzo.Auth
    exposing
        ( Flags
        , Model
        , Msg(LogOut)
        , init
        , update
        , initialAuthUrl
        , storeStateToken
        , deleteStateToken
        , storeAccessToken
        )

import String
import Time exposing (now)
import Http
import Json.Decode as Json exposing ((:=))
import Task


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
            Maybe.map fst parsedTokens

        stateTokenFromUrl : Maybe String
        stateTokenFromUrl =
            -- If we're in the middle of the auth process, Monzo will have returned a state token
            Maybe.map snd parsedTokens

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
                |> List.filter (\a -> fst a == key)
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
                Just ( snd c, snd s )

            _ ->
                Nothing


type Msg
    = AuthFail Http.RawError
    | AuthSuccess Http.Response
    | TokenGenFail String
    | TokenGenSuccess String
    | LogOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthFail err ->
            ( { model | errorMsg = Just (toString err) }, Cmd.none )

        AuthSuccess response ->
            let
                accessToken =
                    decodeAccessToken response.value

                accessTokenCmd =
                    case accessToken of
                        Just token ->
                            storeAccessToken token

                        Nothing ->
                            Cmd.none

                -- Delete the old state token now that we're finished authing
                stateTokenCmd =
                    deleteStateToken ()
            in
                ( { model | accessToken = accessToken }, Cmd.batch [ accessTokenCmd, stateTokenCmd ] )

        TokenGenFail err ->
            ( { model | errorMsg = Just err }, Cmd.none )

        TokenGenSuccess token ->
            ( { model | stateToken = Just token }, storeStateToken token )

        LogOut ->
            ( { model | accessToken = Nothing }, deleteAccessToken () )


generateStateToken : Cmd Msg
generateStateToken =
    -- Take the current time to use as a state token
    Task.map toString Time.now
        |> Task.perform TokenGenFail TokenGenSuccess


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
                |> List.map (\( k, v ) -> k ++ "=" ++ (Http.uriEncode v))
                |> String.join "&"
                |> Http.string

        request =
            { verb = "POST"
            , headers =
                [ ( "Content-Type", "application/x-www-form-urlencoded" ) ]
            , url = url
            , body = body
            }
    in
        Http.send Http.defaultSettings request
            |> Task.perform AuthFail AuthSuccess


decodeAccessToken : Http.Value -> Maybe String
decodeAccessToken value =
    case value of
        Http.Text str ->
            Result.toMaybe (Json.decodeString accessTokenDecoder str)

        _ ->
            Nothing


accessTokenDecoder : Json.Decoder String
accessTokenDecoder =
    "access_token" := Json.string



-- These fields are also returned from the final auth POST
--("client_id" := Json.string)
--("expires_in" := Json.int)
--("token_type" := Json.string)
--("user_id" := Json.string)


initialAuthUrl : Model -> String
initialAuthUrl model =
    case model.stateToken of
        Just token ->
            Http.url
                "https://auth.getmondo.co.uk"
                [ ( "client_id", model.clientId )
                , ( "redirect_uri", model.redirectUri )
                , ( "response_type", "code" )
                , ( "state", token )
                ]

        Nothing ->
            "/"
