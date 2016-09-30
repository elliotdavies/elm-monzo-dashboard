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
        , subscriptions = subscriptions
        }


type alias Model =
    { authToken : Monzo.Token
    , whoAmIData : Monzo.WhoAmI
    }


type Msg
    = QueryMonzo
    | FromMonzo Monzo.Msg


init : ( Model, Cmd Msg )
init =
    ( { authToken = Monzo.auth
      , whoAmIData = Monzo.whoAmIInit
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueryMonzo ->
            ( model, Cmd.map FromMonzo <| Monzo.whoAmIRequest model.authToken )

        FromMonzo details ->
            ( { model | whoAmIData = Monzo.whoAmIHandler details }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text model.whoAmIData.client_id ]
        , div [] [ text model.whoAmIData.user_id ]
        , button [ onClick QueryMonzo ] [ text "Get details" ]
        ]
