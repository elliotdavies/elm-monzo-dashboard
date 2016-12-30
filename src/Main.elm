module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Monzo.Auth as Auth
import Monzo.Monzo as Monzo


main : Program Auth.Flags Model Msg
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
    = AuthMsg Auth.Msg
    | WhoAmIRequest
    | WhoAmIResponse (Result String Monzo.WhoAmIData)
    | AccountsRequest
    | AccountsResponse (Result String Monzo.AccountsData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        token =
            Maybe.withDefault "" model.auth.accessToken
    in
        case msg of
            AuthMsg authMsg ->
                let
                    ( authModel, authCmds ) =
                        Auth.update authMsg model.auth
                in
                    ( { model | auth = authModel }, Cmd.map AuthMsg authCmds )

            WhoAmIRequest ->
                ( model, Monzo.requestWhoAmI token WhoAmIResponse )

            WhoAmIResponse result ->
                let
                    newModel =
                        case result of
                            Ok data ->
                                { model | whoAmIData = Just data }

                            Err err ->
                                { model | err = Just err }
                in
                    ( newModel, Cmd.none )

            AccountsRequest ->
                ( model, Monzo.requestAccounts token AccountsResponse )

            AccountsResponse result ->
                let
                    newModel =
                        case result of
                            Ok data ->
                                { model | accountsData = Just data }

                            Err err ->
                                { model | err = Just err }
                in
                    ( newModel, Cmd.none )


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
            [ button [ onClick WhoAmIRequest ] [ text "Get details" ]
            , button [ onClick AccountsRequest ] [ text "Get accounts" ]
            ]

        empty =
            div [] []

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
                    empty

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
                    empty
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
