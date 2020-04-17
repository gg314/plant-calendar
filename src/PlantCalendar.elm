module PlantCalendar exposing (Model, Msg(..), init, main, update, subscriptions, view)

import Browser
import Browser.Dom
import Browser.Events
import Http
import Html exposing (Html, div, span, text, h2, blockquote, ul, li, a, main_, textarea, button, strong, br, p, input, form, label)
import Html.Attributes exposing (class, id, placeholder, value, href, target, type_, for, checked)
import Html.Events exposing (onClick, onInput, onFocus, onBlur)
import Svg exposing (svg)
import Svg.Attributes exposing (style, x, y, x1, x2, y1, y2, stroke, fill, width, height)
import Array exposing (Array, fromList, get, slice)
import Set exposing (fromList, toList)
import Task
import Json.Decode

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
type HTTPStatus
  = Success String
  | Failure
  | Loading


type alias Zipcode =
  { zone : String
  , lat : String
  , lon : String
  }

type alias Plant = 
  { name : String 
  , category : String
  , selected : Bool
  }


type alias Model =
    { zipcode : Zipcode
    , zipcodetext : String
    , phz : HTTPStatus
    , plants : List ( Plant )
    , filter : String
    }


init : () -> (Model, Cmd Msg)
init _ =
    (Model
      (Zipcode "-1" "-1" "-1")
      ""
      Failure
      plantData
      ""
    , Cmd.none)


-- UPDATE

type Msg
    = SetZipcode
    | SetZipcodeText String
    | TogglePlant Plant
    | SetFilter String
    | GotZipcode String (Result Http.Error Zipcode)


jsonDecoder : Json.Decode.Decoder Zipcode
jsonDecoder =
    Json.Decode.map3
        Zipcode
            (Json.Decode.field "zone" Json.Decode.string)
            (Json.Decode.field "rangemin" Json.Decode.string)
            (Json.Decode.field "rangemax" Json.Decode.string)


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
      SetZipcode ->
        let
            searchZip = model.zipcodetext
        in
            ( { model | phz = Loading, zipcodetext = "" }
            , Http.get { url = "https://c0bra.api.stdlib.com/zipcode-to-hardiness-zone/?zipcode="++ searchZip,
                         expect = Http.expectJson (GotZipcode searchZip) jsonDecoder }
            )

      SetZipcodeText str ->
        ( { model | zipcodetext = str }, Cmd.none)
      
      GotZipcode searchZip result ->
        case result of
            Ok z ->
              ({ model | zipcode = z, phz = Success z.zone, zipcodetext = ""}, Cmd.none)

            Err _ ->
              ({ model | zipcode = Zipcode "-1" "-1" "-1", phz = Failure}, Cmd.none)
        

      TogglePlant plant ->
        let
          newPlants = List.map (togglePlant plant) model.plants
        in
          ( { model | plants = newPlants }, Cmd.none)
          
      SetFilter string ->
        ( { model | filter = String.trim string }, Cmd.none)

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
      div [] [ input [ type_ "checkbox", id ("p"++ (String.fromInt index)), checked p.selected, onClick (TogglePlant p) ] [], label [ class p.category, for ("p"++ (String.fromInt index)) ] [ text p.name ] ] :: getPlants (index + 1) ps
    _ -> []

drawSVG : List (Plant) -> Html Msg
drawSVG plants =
    svg [ style ("width:100%; height: "++(String.fromInt ((List.length plants)*60+65))++"px; stroke: #888; fill; stroke-width: 1"), Svg.Attributes.shapeRendering "crispEdges" ]
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
      y0 = 60 * (index+1) + 30
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
        , Svg.rect [ x "32%", y (String.fromInt (y0-18)), width "20%", height "14", stroke "#517f6c", fill "rgba(56, 165, 116, .7)"] []
        , Svg.rect [ x "52%", y (String.fromInt (y0+4)), width "3%", height "14", stroke "#7c6650", fill "rgba(211, 122, 41, .7)"] []
        ] 

plantNameContains : String -> Plant -> Bool
plantNameContains search plant =
    String.contains search (String.toLower (.name plant))

getPHZ : HTTPStatus -> String
getPHZ status =
    case status of
        Success good ->
          good
        Loading ->
          "Loading..."
        Failure ->
          "Please set ZIP Code to find Plant Hardiness Zone. The default (Zone 5) will be used until you do."

view : Model -> Html Msg
view model =
  let
    sidebarPlants = List.filter (plantNameContains (String.toLower model.filter)) model.plants
    selectedPlants = List.filter .selected model.plants
  in
    div []
    [ div [ class "header" ] [ text "plant-calendar" ]
    , div [ class "container" ]
      [ div [ class "sidebar" ]
        [ div [ class "sidebar__top" ]
          [ div [ class "location" ]
            [ input [ type_ "text", id "location", placeholder "ZIP Code", onInput SetZipcodeText, value model.zipcodetext ] []
            , button [ onClick SetZipcode ] [ text ("Set") ]
            ]
          , div [ class "search" ]
            [ input [ type_ "search", id "filter", placeholder "Filter", onInput SetFilter, value model.filter ] []
            , button [ class (if String.isEmpty model.filter then "inactive" else "reset"), onClick (SetFilter "")] [ text "Clear" ]
            ]
          ]
        , div [ class "sidebar__bottom" ]
            [ div [ class "crop__list" ] (getPlants 0 sidebarPlants) ]
        ]
      , main_ []
        [ text (getPHZ model.phz)
        , drawSVG selectedPlants
        ]
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
  
plantData : List (Plant)
plantData = 
    [ {name = "Anemone", category = "Flowers", selected = False}
    , {name = "Anise Hyssop", category = "Flowers", selected = False}
    , {name = "Asters", category = "Flowers", selected = False}
    , {name = "Astilbe", category = "Flowers", selected = False}
    , {name = "Baby's Breath", category = "Flowers", selected = False}
    , {name = "Bee Balm", category = "Flowers", selected = False}
    , {name = "Black-Eyed Susans", category = "Flowers", selected = False}
    , {name = "Butterfly Bush", category = "Flowers", selected = False}
    , {name = "Borage (star flower)", category = "Flowers", selected = False}
    , {name = "Calendula", category = "Flowers", selected = False}
    , {name = "Cannas", category = "Flowers", selected = False}
    , {name = "Carnations", category = "Flowers", selected = False}
    , {name = "Celosia", category = "Flowers", selected = False}
    , {name = "Chrysanthemum", category = "Flowers", selected = False}
    , {name = "Clematis", category = "Flowers", selected = False}
    , {name = "Columbine", category = "Flowers", selected = False}
    , {name = "Coneflowers", category = "Flowers", selected = False}
    , {name = "Coreopsis", category = "Flowers", selected = False}
    , {name = "Cosmos", category = "Flowers", selected = False}
    , {name = "Crocuses", category = "Flowers", selected = False}
    , {name = "Daffodils", category = "Flowers", selected = False}
    , {name = "Dahlias", category = "Flowers", selected = False}
    , {name = "Daisies", category = "Flowers", selected = False}
    , {name = "Daylilies", category = "Flowers", selected = False}
    , {name = "Delphiniums", category = "Flowers", selected = False}
    , {name = "Echinacea", category = "Flowers", selected = False}
    , {name = "Gardenias", category = "Flowers", selected = False}
    , {name = "Geraniums", category = "Flowers", selected = False}
    , {name = "Gladiolus", category = "Flowers", selected = False}
    , {name = "Hibiscus", category = "Flowers", selected = False}
    , {name = "Hollyhock", category = "Flowers", selected = False}
    , {name = "Honeysuckle", category = "Flowers", selected = False}
    , {name = "Hyacinth", category = "Flowers", selected = False}
    , {name = "Hydrangea", category = "Flowers", selected = False}
    , {name = "Impatiens", category = "Flowers", selected = False}
    , {name = "Irises", category = "Flowers", selected = False}
    , {name = "Jasmine", category = "Flowers", selected = False}
    , {name = "Lilies", category = "Flowers", selected = False}
    , {name = "Marigolds", category = "Flowers", selected = False}
    , {name = "Morning Glories", category = "Flowers", selected = False}
    , {name = "Nasturtium", category = "Flowers", selected = False}
    , {name = "Pansies", category = "Flowers", selected = False}
    , {name = "Peonies", category = "Flowers", selected = False}
    , {name = "Petunias", category = "Flowers", selected = False}
    , {name = "Phlox", category = "Flowers", selected = False}
    , {name = "Poppy", category = "Flowers", selected = False}
    , {name = "Roses", category = "Flowers", selected = False}
    , {name = "Salvia", category = "Flowers", selected = False}
    , {name = "Sedum", category = "Flowers", selected = False}
    , {name = "Shasta Daisies", category = "Flowers", selected = False}
    , {name = "Strawflowers", category = "Flowers", selected = False}
    , {name = "Sunflowers", category = "Flowers", selected = False}
    , {name = "Sweet Peas", category = "Flowers", selected = False}
    , {name = "Tuberose", category = "Flowers", selected = False}
    , {name = "Tulips", category = "Flowers", selected = False}
    , {name = "Verbenas", category = "Flowers", selected = False}
    , {name = "Veronica (Speedwell)", category = "Flowers", selected = False}
    , {name = "Viola", category = "Flowers", selected = False}
    , {name = "Yarrow", category = "Flowers", selected = False}
    , {name = "Zinnias", category = "Flowers", selected = False}
    , {name = "Artichoke", category = "Vegetables", selected = False}
    , {name = "Arugula", category = "Vegetables", selected = False}
    , {name = "Asparagus", category = "Vegetables", selected = False}
    , {name = "Beans", category = "Vegetables", selected = False}
    , {name = "Beets", category = "Vegetables", selected = False}
    , {name = "Bell Peppers", category = "Vegetables", selected = False}
    , {name = "Broccoli", category = "Vegetables", selected = False}
    , {name = "Brussels Sprouts", category = "Vegetables", selected = False}
    , {name = "Cabbage", category = "Vegetables", selected = False}
    , {name = "Carrots", category = "Vegetables", selected = False}
    , {name = "Cauliflower", category = "Vegetables", selected = False}
    , {name = "Celery", category = "Vegetables", selected = False}
    , {name = "Collards", category = "Vegetables", selected = False}
    , {name = "Corn", category = "Vegetables", selected = False}
    , {name = "Cucumbers", category = "Vegetables", selected = False}
    , {name = "Edamame", category = "Vegetables", selected = False}
    , {name = "Eggplants", category = "Vegetables", selected = False}
    , {name = "Endive", category = "Vegetables", selected = False}
    , {name = "Fava Beans", category = "Vegetables", selected = False}
    , {name = "Garlic", category = "Vegetables", selected = False}
    , {name = "Gourds", category = "Vegetables", selected = False}
    , {name = "Green Beans", category = "Vegetables", selected = False}
    , {name = "Horseradish", category = "Vegetables", selected = False}
    , {name = "Kale", category = "Vegetables", selected = False}
    , {name = "Leeks", category = "Vegetables", selected = False}
    , {name = "Lettuce", category = "Vegetables", selected = False}
    , {name = "Okra", category = "Vegetables", selected = False}
    , {name = "Onions", category = "Vegetables", selected = False}
    , {name = "Parsnips", category = "Vegetables", selected = False}
    , {name = "Peas", category = "Vegetables", selected = False}
    , {name = "Potatoes", category = "Vegetables", selected = False}
    , {name = "Pumpkins", category = "Vegetables", selected = False}
    , {name = "Radishes", category = "Vegetables", selected = False}
    , {name = "Rhubarb", category = "Vegetables", selected = False}
    , {name = "Rutabagas", category = "Vegetables", selected = False}
    , {name = "Shallots", category = "Vegetables", selected = False}
    , {name = "Snap Peas", category = "Vegetables", selected = False}
    , {name = "Soybean", category = "Vegetables", selected = False}
    , {name = "Spinach", category = "Vegetables", selected = False}
    , {name = "Squash", category = "Vegetables", selected = False}
    , {name = "Sweet Potatoes", category = "Vegetables", selected = False}
    , {name = "Swiss Chard", category = "Vegetables", selected = False}
    , {name = "Tomatoes", category = "Vegetables", selected = False}
    , {name = "Turnips", category = "Vegetables", selected = False}
    , {name = "Zucchini", category = "Vegetables", selected = False}
    , {name = "Basil", category = "Herbs", selected = False}
    , {name = "Catnip", category = "Herbs", selected = False}
    , {name = "Chives", category = "Herbs", selected = False}
    , {name = "Coriander/Cilantro", category = "Herbs", selected = False}
    , {name = "Dill", category = "Herbs", selected = False}
    , {name = "Fennel", category = "Herbs", selected = False}
    , {name = "Lavender", category = "Herbs", selected = False}
    , {name = "Lemon Grass", category = "Herbs", selected = False}
    , {name = "Marjoram", category = "Herbs", selected = False}
    , {name = "Mint", category = "Herbs", selected = False}
    , {name = "Mustard", category = "Herbs", selected = False}
    , {name = "Oregano", category = "Herbs", selected = False}
    , {name = "Parsley", category = "Herbs", selected = False}
    , {name = "Rosemary", category = "Herbs", selected = False}
    , {name = "Sage", category = "Herbs", selected = False}
    , {name = "Scallion", category = "Herbs", selected = False}
    , {name = "Stevia", category = "Herbs", selected = False}
    , {name = "Tarragon", category = "Herbs", selected = False}
    , {name = "Thyme", category = "Herbs", selected = False}
    , {name = "Apples", category = "Fruits", selected = False}
    , {name = "Blackberries", category = "Fruits", selected = False}
    , {name = "Blueberries", category = "Fruits", selected = False}
    , {name = "Cantaloupes", category = "Fruits", selected = False}
    , {name = "Currant", category = "Fruits", selected = False}
    , {name = "Cherries", category = "Fruits", selected = False}
    , {name = "Figs", category = "Fruits", selected = False}
    , {name = "Grapes", category = "Fruits", selected = False}
    , {name = "Honeydew", category = "Fruits", selected = False}
    , {name = "Lemons & Oranges", category = "Fruits", selected = False}
    , {name = "Peaches", category = "Fruits", selected = False}
    , {name = "Pears", category = "Fruits", selected = False}
    , {name = "Plums", category = "Fruits", selected = False}
    , {name = "Raspberries", category = "Fruits", selected = False}
    , {name = "Strawberries", category = "Fruits", selected = False}
    , {name = "Watermelon", category = "Fruits", selected = False}
    ]