module PlantCalendar exposing (Model, Msg(..), init, main, update, subscriptions, view)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, div, span, text, h2, blockquote, ul, li, a, main_, textarea, button, strong, br, p, input, form, label)
import Html.Attributes exposing (class, id, placeholder, value, href, target, type_, for, checked)
import Html.Events exposing (onClick, onInput, onFocus, onBlur)
import Svg exposing (svg)
import Svg.Attributes exposing (style, x, y, x1, x2, y1, y2, stroke)
import Array exposing (Array, fromList, get, slice)
import Set exposing (fromList, toList)
import Task

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


-- MODEL

type alias Plant = 
  { name : String 
  , selected : Bool
  }


type alias Model =
    { zipcode : Int
    , phz : Int
    , plants : List ( Plant )
    }


init : () -> (Model, Cmd Msg)
init _ =
    (Model
      11234
      5
      [ {name = "Carrots", selected = True}
      , {name = "Beans", selected = True}
      , {name = "Potatos", selected = False}
      , {name = "Arugula", selected = True}
      , {name = "Spinach", selected = False}]
    , Cmd.none)


-- UPDATE

type Msg
    = SetZipcode
    | TogglePlant Plant


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
      SetZipcode ->
          ( { model | zipcode = 12345 }
          , Cmd.none)
          
      TogglePlant plant ->
        let
          newPlants = List.map (togglePlant plant) model.plants
        in
          ( { model | plants = newPlants }
          , Cmd.none)

togglePlant : Plant -> Plant -> Plant
togglePlant target test =
    if test == target then { test | selected = (not test.selected) } else test

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ 
    ]


-- VIEW
getPlants : Int -> List (Plant) -> List (Html Msg)
getPlants index plants =
  case plants of
    p::ps ->
      div [] [ input [ type_ "checkbox", id ("p"++ (String.fromInt index)), checked p.selected, onClick (TogglePlant p) ] [], label [ for ("p"++ (String.fromInt index)) ] [ text p.name ] ] :: getPlants (index + 1) ps
    _ -> []

drawSVG : List (Plant) -> Html Msg
drawSVG plants =
    svg [ style "width:100%; height: 2220px; stroke: #888; fill; stroke-width: 1", Svg.Attributes.shapeRendering "crispEdges" ]
    (List.append
        [ Svg.text_ [y "25", x "28%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;"] [ Svg.text "JAN" ]
        , Svg.text_ [y "25", x "34%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;"] [ Svg.text "FEB" ]
        , Svg.text_ [y "25", x "40%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;"] [ Svg.text "MAR" ]
        , Svg.text_ [y "25", x "46%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;"] [ Svg.text "APR" ]
        , Svg.text_ [y "25", x "52%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;"] [ Svg.text "MAY" ]
        , Svg.text_ [y "25", x "58%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;"] [ Svg.text "JUN" ]
        , Svg.text_ [y "25", x "64%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;"] [ Svg.text "JUL" ]
        , Svg.text_ [y "25", x "70%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;"] [ Svg.text "AUG" ]
        , Svg.text_ [y "25", x "76%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;"] [ Svg.text "SEP" ]
        , Svg.text_ [y "25", x "82%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;"] [ Svg.text "OCT" ]
        , Svg.text_ [y "25", x "88%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;"] [ Svg.text "NOV" ]
        , Svg.text_ [y "25", x "94%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;"] [ Svg.text "DEC" ]
        , Svg.line [y2 "100%", y1 "35", x2 "99.9%", x1 "99.9%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "35", x2 "94%", x1 "94%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "35", x2 "88%", x1 "88%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "35", x2 "82%", x1 "82%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "35", x2 "76%", x1 "76%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "35", x2 "70%", x1 "70%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "35", x2 "64%", x1 "64%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "35", x2 "58%", x1 "58%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "35", x2 "52%", x1 "52%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "35", x2 "46%", x1 "46%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "35", x2 "40%", x1 "40%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "35", x2 "34%", x1 "34%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "35", x2 "28%", x1 "28%", stroke "#d8d8d8"] []
        ]
        (List.concat (List.indexedMap drawRow plants)))

drawRow : Int -> Plant -> List (Svg.Svg Msg)
drawRow index plant =
    let
      y0 = 50 * (index+1) + 30
    in
        [ Svg.line [ y1 (String.fromInt (y0-10)), y2 (String.fromInt (y0+10)), x1 "28%", x2 "28%" ] []
        , Svg.line [ y1 (String.fromInt (y0-10)), y2 (String.fromInt (y0+10)), x1 "34%", x2 "34%" ] []
        , Svg.line [ y1 (String.fromInt (y0-10)), y2 (String.fromInt (y0+10)), x1 "40%", x2 "40%" ] []
        , Svg.line [ y1 (String.fromInt (y0-10)), y2 (String.fromInt (y0+10)), x1 "46%", x2 "46%" ] []
        , Svg.line [ y1 (String.fromInt (y0-10)), y2 (String.fromInt (y0+10)), x1 "52%", x2 "52%" ] []
        , Svg.line [ y1 (String.fromInt (y0-10)), y2 (String.fromInt (y0+10)), x1 "58%", x2 "58%" ] []
        , Svg.line [ y1 (String.fromInt (y0-10)), y2 (String.fromInt (y0+10)), x1 "64%", x2 "64%" ] []
        , Svg.line [ y1 (String.fromInt (y0-10)), y2 (String.fromInt (y0+10)), x1 "70%", x2 "70%" ] []
        , Svg.line [ y1 (String.fromInt (y0-10)), y2 (String.fromInt (y0+10)), x1 "76%", x2 "76%" ] []
        , Svg.line [ y1 (String.fromInt (y0-10)), y2 (String.fromInt (y0+10)), x1 "82%", x2 "82%" ] []
        , Svg.line [ y1 (String.fromInt (y0-10)), y2 (String.fromInt (y0+10)), x1 "88%", x2 "88%" ] []
        , Svg.line [ y1 (String.fromInt (y0-10)), y2 (String.fromInt (y0+10)), x1 "94%", x2 "94%" ] []
        , Svg.line [ y1 (String.fromInt (y0-10)), y2 (String.fromInt (y0+10)), x1 "99.9%", x2 "99.9%" ] []
        , Svg.text_ [ y (String.fromInt (y0-2)), x "23%", style "fill: #444; stroke: none; text-anchor: end; font-weight: 400; font-family: 'Gloria Hallelujah'; font-size: 1.3vw;" ] [ Svg.text plant.name]
        , Svg.line [ y1 (String.fromInt y0), y2 (String.fromInt y0), x1 "99.9%", x2 "0%" ] []
        ]
    {--
            <text y="78" x="23%" style="fill: #444; stroke: none; text-anchor: end; font-weight: 400; font-family: 'Gloria Hallelujah'; font-size: 1.3vw;">Carrots</text>
            <line y2="80" y1="80" x2="0%" x1="99.9%"></line>
            <rect x="32%" y="62" width="20%" height="14" stroke="#517f6c" fill="rgba(56, 165, 116, .7)" rx="0"></rect>
            <rect x="52%" y="84" width="3%" height="14" stroke="#7c6650" fill="rgba(211, 122, 41, .7)" rx="0"></rect>
       --}     

view : Model -> Html Msg
view model =
  let
    sidebarPlants = model.plants
    selectedPlants = List.filter .selected model.plants
  in
    form []
    [ div [ class "header" ] [ text "plant-calendar" ]
    , div [ class "container" ]
      [ div [ class "sidebar" ]
        [ div [ class "sidebar__top" ]
          [ div [ class "location" ]
            [ input [ type_ "text", id "location", placeholder "Zip code" ] []
            , button [] [ text "Set" ]
            ]
          , div [ class "search" ]
            [ input [ type_ "search", id "filter", placeholder "Filter" ] []
            ]
          ]
        , div [ class "sidebar__bottom" ]
            [ div [ class "sidebar__bottom" ] (getPlants 0 sidebarPlants) ]
        ]
      , main_ [] [ drawSVG selectedPlants ]
      ]
    ]
{--
                  
    <main>
        <svg style="width:100%; height: 2220px;" stroke="#888" fill="white" stroke-width="1" shape-rendering="crispEdges">
            
            <text y="25" x="94%" style="fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;">DEC</text>
            <text y="25" x="88%" style="fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;">NOV</text>
            <text y="25" x="82%" style="fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;">OCT</text>
            <text y="25" x="76%" style="fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;">SEP</text>
            <text y="25" x="70%" style="fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;">AUG</text>
            <text y="25" x="64%" style="fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;">JUL</text>
            <text y="25" x="58%" style="fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;">JUN</text>
            <text y="25" x="52%" style="fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;">MAY</text>
            <text y="25" x="46%" style="fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;">APR</text>
            <text y="25" x="40%" style="fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;">MAR</text>
            <text y="25" x="34%" style="fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;">FEB</text>
            <text y="25" x="28%" style="fill: #444; stroke: none; text-anchor: middle; font-size: 0.8vw;">JAN</text>

            <line y2="100%" y1="35" x2="99.9%" x1="99.9%" stroke="#d8d8d8"></line>
            <line y2="100%" y1="35" x2="94%" x1="94%" stroke="#d8d8d8"></line>
            <line y2="100%" y1="35" x2="88%" x1="88%" stroke="#d8d8d8"></line>
            <line y2="100%" y1="35" x2="82%" x1="82%" stroke="#d8d8d8"></line>
            <line y2="100%" y1="35" x2="76%" x1="76%" stroke="#d8d8d8"></line>
            <line y2="100%" y1="35" x2="70%" x1="70%" stroke="#d8d8d8"></line>
            <line y2="100%" y1="35" x2="64%" x1="64%" stroke="#d8d8d8"></line>
            <line y2="100%" y1="35" x2="58%" x1="58%" stroke="#d8d8d8"></line>
            <line y2="100%" y1="35" x2="52%" x1="52%" stroke="#d8d8d8"></line>
            <line y2="100%" y1="35" x2="46%" x1="46%" stroke="#d8d8d8"></line>
            <line y2="100%" y1="35" x2="40%" x1="40%" stroke="#d8d8d8"></line>
            <line y2="100%" y1="35" x2="34%" x1="34%" stroke="#d8d8d8"></line>
            <line y2="100%" y1="35" x2="28%" x1="28%" stroke="#d8d8d8"></line>
            
            <line y2="90" y1="70" x2="99.9%" x1="99.9%"></line><line y2="90" y1="70" x2="94%" x1="94%"></line>
            <line y2="90" y1="70" x2="88%" x1="88%"></line><line y2="90" y1="70" x2="82%" x1="82%"></line>
            <line y2="90" y1="70" x2="76%" x1="76%"></line><line y2="90" y1="70" x2="70%" x1="70%"></line>
            <line y2="90" y1="70" x2="64%" x1="64%"></line><line y2="90" y1="70" x2="58%" x1="58%"></line>
            <line y2="90" y1="70" x2="52%" x1="52%"></line><line y2="90" y1="70" x2="46%" x1="46%"></line>
            <line y2="90" y1="70" x2="40%" x1="40%"></line><line y2="90" y1="70" x2="34%" x1="34%"></line>
            <line y2="90" y1="70" x2="28%" x1="28%"></line>

            <text y="78" x="23%" style="fill: #444; stroke: none; text-anchor: end; font-weight: 400; font-family: 'Gloria Hallelujah'; font-size: 1.3vw;">Carrots</text>
            <line y2="80" y1="80" x2="0%" x1="99.9%"></line>
            <rect x="32%" y="62" width="20%" height="14" stroke="#517f6c" fill="rgba(56, 165, 116, .7)" rx="0"></rect>
            <rect x="52%" y="84" width="3%" height="14" stroke="#7c6650" fill="rgba(211, 122, 41, .7)" rx="0"></rect>
            

            <line y2="140" y1="120" x2="99.9%" x1="99.9%"></line><line y2="140" y1="120" x2="94%" x1="94%"></line>
            <line y2="140" y1="120" x2="88%" x1="88%"></line><line y2="140" y1="120" x2="82%" x1="82%"></line>
            <line y2="140" y1="120" x2="76%" x1="76%"></line><line y2="140" y1="120" x2="70%" x1="70%"></line>
            <line y2="140" y1="120" x2="64%" x1="64%"></line><line y2="140" y1="120" x2="58%" x1="58%"></line>
            <line y2="140" y1="120" x2="52%" x1="52%"></line><line y2="140" y1="120" x2="46%" x1="46%"></line>
            <line y2="140" y1="120" x2="40%" x1="40%"></line><line y2="140" y1="120" x2="34%" x1="34%"></line>
            <line y2="140" y1="120" x2="28%" x1="28%"></line>

            <text y="128" x="23%" style="fill: #444; stroke: none; text-anchor: end; font-weight: 400; font-family: 'Gloria Hallelujah'; font-size: 1.3vw;">Black-eyed susans</text>
            <line y2="130" y1="130" x2="0%" x1="99.9%"></line>
            <rect x="42%" y="112" width="15%" height="14" stroke="#517f6c" fill="rgba(56, 165, 116, .7)" rx="0"></rect>
            <rect x="57%" y="134" width="3%" height="14" stroke="#7c6650" fill="rgba(211, 122, 41, .7)" rx="0"></rect>
            

            <line y2="190" y1="170" x2="99.9%" x1="99.9%"></line><line y2="190" y1="170" x2="94%" x1="94%"></line>
            <line y2="190" y1="170" x2="88%" x1="88%"></line><line y2="190" y1="170" x2="82%" x1="82%"></line>
            <line y2="190" y1="170" x2="76%" x1="76%"></line><line y2="190" y1="170" x2="70%" x1="70%"></line>
            <line y2="190" y1="170" x2="64%" x1="64%"></line><line y2="190" y1="170" x2="58%" x1="58%"></line>
            <line y2="190" y1="170" x2="52%" x1="52%"></line><line y2="190" y1="170" x2="46%" x1="46%"></line>
            <line y2="190" y1="170" x2="40%" x1="40%"></line><line y2="190" y1="170" x2="34%" x1="34%"></line>
            <line y2="190" y1="170" x2="28%" x1="28%"></line>

            <text y="178" x="23%" style="fill: #444; stroke: none; text-anchor: end; font-weight: 400; font-family: 'Gloria Hallelujah'; font-size: 1.3vw;">Lemongrass</text>
            <line y2="180" y1="180" x2="0%" x1="99.9%"></line>
            <rect x="46%" y="162" width="6%" height="14" stroke="#517f6c" fill="rgba(56, 165, 116, .7)" rx="0"></rect>
            <rect x="52%" y="184" width="3%" height="14" stroke="#7c6650" fill="rgba(211, 122, 41, .7)" rx="0"></rect>

        </svg>
    </main>
  </div>
  </form>
  --}