module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Json.Decode as Json
import Monzo.Auth as Auth
import Monzo.Monzo as Monzo


main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }


type alias Model =
    { auth : Auth.Model
    , err : Maybe String
    , whoAmIData :
        Maybe
            { client_id : String
            , user_id : String
            }
    , accountsData :
        Maybe
            (List
                { id : String
                , created : String
                , description : String
                }
            )
    }


init : Auth.Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( authModel, authCmds ) =
            Auth.init flags
    in
        ( { auth = authModel
          , err = Nothing
          , whoAmIData = Nothing
          , accountsData = Nothing
          }
        , Cmd.map AuthMsg authCmds
        )


type Msg
    = MonzoRequest Monzo.Endpoint
    | MonzoResponse Monzo.Endpoint Monzo.Msg
    | AuthMsg Auth.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthMsg authMsg ->
            let
                ( authModel, authCmds ) =
                    Auth.update authMsg model.auth
            in
                ( { model | auth = authModel }, Cmd.map AuthMsg authCmds )

        MonzoRequest endpoint ->
            let
                token =
                    Maybe.withDefault "" model.auth.accessToken

                request =
                    Monzo.makeApiRequest token endpoint

                responseMsg =
                    MonzoResponse endpoint
            in
                ( model, Cmd.map responseMsg request )

        MonzoResponse endpoint monzoMsg ->
            ( handleMonzoResponse model endpoint monzoMsg, Cmd.none )


handleMonzoResponse : Model -> Monzo.Endpoint -> Monzo.Msg -> Model
handleMonzoResponse model endpoint monzoMsg =
    let
        handledResponse =
            Monzo.handleApiResponse monzoMsg

        decodeAndUpdate data =
            case endpoint of
                Monzo.WhoAmI ->
                    { model | whoAmIData = Result.toMaybe (Json.decodeString Monzo.whoAmIDecoder data) }

                Monzo.Accounts ->
                    { model | accountsData = Result.toMaybe (Json.decodeString Monzo.accountsDecoder data) }
    in
        case handledResponse of
            Ok data ->
                decodeAndUpdate data

            Err err ->
                { model | err = Just (toString err) }


view : Model -> Html Msg
view model =
    if (Auth.loggedIn model.auth) then
        loggedInView model
    else
        loggedOutView model


loggedInView : Model -> Html Msg
loggedInView model =
    let
        buttons =
            [ button [ onClick <| MonzoRequest Monzo.WhoAmI ] [ text "Get details" ]
            , button [ onClick <| MonzoRequest Monzo.Accounts ] [ text "Get accounts" ]
            ]

        print key value =
            div []
                [ strong [] [ text (key ++ ": ") ]
                , text value
                ]

        whoAmIData =
            case model.whoAmIData of
                Just data ->
                    div []
                        [ h2 [] [ text "Your user data:" ]
                        , print "client_id" data.client_id
                        , print "user_id" data.user_id
                        ]

                Nothing ->
                    div [] []

        accountsData =
            case model.accountsData of
                Just data ->
                    let
                        accountData datum =
                            div []
                                [ print "account id" datum.id
                                , print "created" datum.created
                                , print "description" datum.description
                                , hr [] []
                                ]
                    in
                        div []
                            [ h2 [] [ text "Your accounts data:" ]
                            , div [] (List.map accountData data)
                            ]

                Nothing ->
                    div [] []
    in
        div []
            [ h1 [] [ text "You are logged in" ]
            , hr [] []
            , div [] buttons
            , hr [] []
            , whoAmIData
            , hr [] []
            , accountsData
            , a [ onClick <| AuthMsg Auth.LogOut ] [ text "Log out" ]
            , hr [] []
            , pre [] [ text <| toString model.err ]
            ]


loggedOutView : Model -> Html Msg
loggedOutView model =
    div []
        [ h1 [] [ text "You are not logged in" ]
        , div []
            [ a [ href (Auth.initialAuthUrl model.auth) ] [ text "Log in now" ]
            ]
        ]
