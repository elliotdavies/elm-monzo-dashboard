module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (onClick)
import Monzo


main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }


type alias Model =
    { authToken : Monzo.Token
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


type Msg
    = QueryMonzo Monzo.Endpoint
    | WhoAmIHandler Monzo.Msg
    | AccountsHandler Monzo.Msg


init : ( Model, Cmd Msg )
init =
    ( { authToken = Monzo.auth
      , err = Nothing
      , whoAmIData = Nothing
      , accountsData = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueryMonzo endpoint ->
            let
                handler =
                    case endpoint of
                        Monzo.WhoAmI ->
                            WhoAmIHandler

                        Monzo.Accounts ->
                            AccountsHandler
            in
                ( model, Cmd.map handler <| Monzo.makeApiRequest model.authToken endpoint )

        WhoAmIHandler monzoMsg ->
            case Monzo.whoAmIHandler monzoMsg of
                Ok success ->
                    ( { model | err = Nothing, whoAmIData = Just success }, Cmd.none )

                Err err ->
                    ( { model | err = Just err, whoAmIData = Nothing }, Cmd.none )

        AccountsHandler monzoMsg ->
            case Monzo.accountsHandler monzoMsg of
                Ok success ->
                    ( { model | err = Nothing, accountsData = Just success }, Cmd.none )

                Err err ->
                    ( { model | err = Just err, accountsData = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        errHtml =
            let
                message =
                    case model.err of
                        Just err ->
                            err

                        Nothing ->
                            "No errors... yet"
            in
                div []
                    [ h3 [] [ text "Errors:" ]
                    , div [] [ text message ]
                    ]

        whoAmIHtml =
            let
                heading =
                    h3 [] [ text "Who am I:" ]
            in
                case model.whoAmIData of
                    Just data ->
                        div []
                            [ heading
                            , div []
                                [ text <| "client_id: " ++ data.client_id
                                , br [] []
                                , text <| "user_id: " ++ data.user_id
                                ]
                            ]

                    Nothing ->
                        div []
                            [ heading
                            , text "..."
                            ]

        accountsHtml =
            let
                heading =
                    h3 [] [ text "Accounts:" ]
            in
                case model.accountsData of
                    Just data ->
                        div []
                            [ heading
                            , div [] [ text <| toString data ]
                            , div [] [ text <| "length: " ++ (toString <| List.length data) ]
                            ]

                    Nothing ->
                        div []
                            [ heading
                            , text "..."
                            ]

        buttonsHtml =
            div []
                [ button [ onClick <| QueryMonzo Monzo.WhoAmI ] [ text "Get details" ]
                , button [ onClick <| QueryMonzo Monzo.Accounts ] [ text "Get accounts" ]
                ]
    in
        div []
            [ buttonsHtml
            , errHtml
            , whoAmIHtml
            , accountsHtml
            ]
