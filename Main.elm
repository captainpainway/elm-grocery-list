port module Main exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import String
import Task
import Round exposing (..)

-- MAIN

main : Program (Maybe Model) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none }

port setStorage : Model -> Cmd msg

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
      ( newModel, cmds ) =
        update msg model
    in
      ( newModel
      , Cmd.batch [ setStorage newModel, cmds ]
      )

-- MODEL    

type alias Model =
    { entries : List Entry
    , field : String
    , priceField : String
    , uid : Int
    , visibility : String }

type alias Entry =
    { description : String
    , incart : Bool
    , editing : Bool
    , id : Int
    , price : String }

emptyModel : Model
emptyModel = 
    { entries = []
    , visibility = "All"
    , field = ""
    , priceField = ""
    , uid = 0 }

newEntry : String -> String -> Int -> Entry
newEntry desc priceField id = 
    { description = desc
    , price = priceField
    , incart = False
    , editing = False
    , id = id }

total : Int
total =
    0

init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    Maybe.withDefault emptyModel savedModel ! []

-- UPDATE

type Msg
    = NoOp
    | UpdateField String
    | UpdatePriceField String
    | EditingEntry Int Bool
    | UpdateEntry Int String
    | UpdatePrice Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Add ->
            { model
            | uid = model.uid + 1
            , field = ""
            , priceField = ""
            , entries = 
                if String.isEmpty model.field then
                    model.entries
                else
                    model.entries ++ [ newEntry model.field model.priceField model.uid ]
            }
                ! []

        UpdateField str ->
            { model | field = str }
                ! []

        UpdatePriceField str ->
            { model | priceField = str }
                ! []
        
        EditingEntry id isEditing ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | editing = isEditing }
                    else
                        t

                focus =
                    Dom.focus ("item-" ++ toString id)
            in
                { model | entries = List.map updateEntry model.entries }
                    ! [ Task.attempt (\_ -> NoOp) focus ]

        UpdateEntry id item ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | description = item }
                    else
                        t
            in
                { model | entries = List.map updateEntry model.entries }
                    ! []

        UpdatePrice id dollars ->
            let updatePrice t =
                if t.id == id then
                    { t | price = dollars }
                else
                    t
            in
                { model | entries = List.map updatePrice model.entries }
                    ! []

        Delete id ->
            { model | entries = List.filter (\t -> t.id /= id) model.entries }
                ! []

        DeleteComplete ->
            { model | entries = List.filter (not << .incart) model.entries }
                ! []
                
        Check id isInCart ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | incart = isInCart }
                    else
                        t
            in
                { model | entries = List.map updateEntry model.entries }
                    ! []

        CheckAll isInCart ->
            let
                updateEntry t =
                    { t | incart = isInCart }
            in
                { model | entries = List.map updateEntry model.entries }
                    ! []

        ChangeVisibility visibility ->
            { model | visibility = visibility }
                ! []

-- VIEW

view : Model -> Html Msg
view model =
    div
        []
        [ section
            [ class "groceries" ]
            [ lazy2 viewInput model.field model.priceField
            , lazy2 viewEntries model.visibility model.entries
            , lazy2 viewControls model.visibility model.entries ]
        ]

viewInput : String -> String -> Html Msg
viewInput item price =
    header
        []
        [ h1 [] [ text "GROCERY LIST" ]
        , input
            [ class "new-price"
            , type_ "number"
            , placeholder "$"
            , autofocus False
            , value price
            , name "newPrice"
            , onInput UpdatePriceField
            , onEnter Add ]
            []
        , input
            [ class "new-item"
            , placeholder "Add groceries"
            , autofocus True
            , value item
            , name "newItem"
            , onInput UpdateField
            , onEnter Add ]
            []
        ]

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)

viewEntries : String -> List Entry -> Html Msg
viewEntries visibility entries = 
    let
        isVisible item =
            case visibility of 
                "In Cart" ->
                    item.incart

                "Remaining" ->
                    not item.incart

                _ ->
                    True

        allInCart =
            List.all .incart entries

        cssVisibility =
            if List.isEmpty entries then
                "hidden"
            else
                "visible"
    in
        section
            [ class "main" 
            , style [ ( "visibility", cssVisibility ) ]
            ]
            -- [ div 
            --     [ class "toggler" ] 
            --     [ input
            --         [ type_ "checkbox"
            --         , class "toggle-all"
            --         , name "toggle"
            --         , checked allInCart
            --         , onClick (CheckAll (not allInCart))
            --         ]
            --         []
            --     , label
            --         [ for "toggle-all" ]
            --         [ text "Mark all as in cart" ]
            --     ]
            -- , 
            [ Keyed.ul [ class "grocery-list" ] <|
                List.map viewKeyedEntry (List.filter isVisible entries)
            ]

viewKeyedEntry : Entry -> ( String, Html Msg )
viewKeyedEntry item =
    ( toString item.id, lazy viewEntry item )

viewEntry : Entry -> Html Msg
viewEntry item =
    li
        [ classList [ ( "incart", item.incart ), ( "editing", item.editing ) ] ]
        [ div 
            [ class "view" ]
            [ input 
                [ class "toggle" 
                , type_ "checkbox"
                , checked item.incart
                , onClick (Check item.id (not item.incart))
                ]
                []
            , label
                [ onDoubleClick (EditingEntry item.id True) ]
                [ text item.description ]
            ]
        , div
            [class "prices" ]
            [ span
                []
                [ text "$" ]
            , input
                [ class "price"
                , type_ "number"
                , value item.price
                , onInput (UpdatePrice item.id)
                , onBlur (Check item.id (not (String.isEmpty item.price)))
                ]
                []
            , button
                [ class "destroy" 
                , onClick (Delete item.id)
                ]
                [ text "X" ]
            , input
                [ class "edit" 
                , value item.description
                , name "title"
                , id ("item" ++ toString item.id)
                , onInput (UpdateEntry item.id)
                , onBlur (EditingEntry item.id False)
                , onEnter (EditingEntry item.id False)
                ]
                []
            ]
        ]

viewControls : String -> List Entry -> Html Msg
viewControls visibility entries =
    let
        entriesInCart =
            List.length (List.filter .incart entries)

        entriesLeft =
            List.length entries - entriesInCart

        debugEntry = List.map .price entries
            |> List.map String.toFloat

    in
        footer
            [ hidden (List.isEmpty entries) ]
            [ lazy viewTotalPrice debugEntry
            , lazy viewControlsFilters visibility
            -- , lazy viewControlsClear entriesInCart
            ]

viewTotalPrice : List (Result String Float) -> Html Msg
viewTotalPrice debugEntry =
    let
        totalPrice = List.map (Result.withDefault 0) debugEntry
            |> List.sum
            |> Round.round 2
    in
        span
            [ class "total-price" ]
            [ div [] [ text (String.concat ["TOTAL: $", totalPrice]) ]
            ]

viewControlsFilters : String -> Html Msg
viewControlsFilters visibility =
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" "All" visibility
        , text " "
        , visibilitySwap "#/remaining" "Remaining" visibility
        , text " "
        , visibilitySwap "#/incart" "In Cart" visibility
        ]

visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
    li
        [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text visibility ]
        ]

viewControlsClear : Int -> Html Msg
viewControlsClear entriesInCart =
    button
        [ class "clear-incart"
        , hidden (entriesInCart == 0)
        , onClick DeleteComplete
        ]
        [ text ("Clear items in cart (" ++ toString entriesInCart ++ ")")
        ]