port module PlantCalendar exposing (Model, Msg(..), init, main, update, subscriptions, view)

import Browser
import Http
import Html exposing (Html, div, span, text, h2, blockquote, ul, li, a, main_, textarea, button, strong, br, p, input, form, label, object, abbr)
import Html.Attributes exposing (class, id, placeholder, value, href, target, type_, for, checked, disabled, attribute, title)
import Html.Events exposing (onClick, onInput, onSubmit)
import Svg exposing (svg)
import Svg.Attributes exposing (style, x, y, x1, x2, y1, y2, stroke, fill, width, height)
import Json.Decode
import Json.Encode
import Regex
import Basics exposing (identity)

import Config exposing (api_key_NOAA)
import USASVG exposing (usaSVG)

import WSData exposing (ZipRecord, initialRecord, findRecord, getStationName, getLocationName, getStationCoords, getStationDist, getStation1ID, getStation2ID, getStation3ID, getStationElev)

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
  = Success Zipcode
  | Failure
  | Loading
  | Unset

type alias JsonResult =
  { date: String,
    datatype: String,
    station: String,
    attributes: String,
    value: Int
  }

type alias Zipcode =
  { zipcode : String
  , closestStation : String
  , zone : String
  , temp_range: String
  , avg_winter_temp: Float
  , min_winter_temp: Float
  , springFrost : Int
  , winterFrost : Int
  }

type alias Plant = 
  { name : String 
  , category : String
  , selected : Bool
  , disabled : Bool
  , minzone : Float
  , maxzone : Float
  , defaultPeriods : List ((Int, Int), (Int, Int), (Int, Int))
  }

type alias PackedPlant = 
  { name : String 
  , boxes : List ( (String, Int, Int) )
  }

type alias Model =
    { zipcode : HTTPStatus
    , zipcodetext : String
    , plants : List ( Plant )
    , filter : String
    , sortMode : String
    , zipRecord : ZipRecord
    }


init : () -> (Model, Cmd Msg)
init _ =
    (Model
      Unset
      ""
      (plantData 120)
      ""
      "ABC"
      initialRecord
    , Cmd.none)


-- UPDATE

type Msg
    = SetZipcode
    | SetZipcodeText String
    | TogglePlant Plant
    | SetFilter String
    | GotZipcode (Result Http.Error Zipcode)
    | GotResults String String (Result Http.Error (List JsonResult))
    | SendPDF
    | ClearAll
    | ToggleSort


jsonDecoder : Json.Decode.Decoder (List (JsonResult))
jsonDecoder =
  Json.Decode.field "results"
     (Json.Decode.list decodeJsonResult)

decodeJsonResult : Json.Decode.Decoder JsonResult
decodeJsonResult =
  Json.Decode.map5
    JsonResult
      (Json.Decode.field "date" Json.Decode.string)
      (Json.Decode.field "datatype" Json.Decode.string)
      (Json.Decode.field "station" Json.Decode.string)
      (Json.Decode.field "attributes" Json.Decode.string)
      (Json.Decode.field "value" Json.Decode.int)

-- jsonDecoder : String -> Json.Decode.Decoder Zipcode
-- jsonDecoder s =
    -- Json.Decode.map3
    --     (Zipcode s)
    --         (Json.Decode.field "zone" Json.Decode.string)
    --         (Json.Decode.field "coordinates" (Json.Decode.map2
    --             Tuple.pair
    --             (Json.Decode.field "lat" Json.Decode.float)
    --             (Json.Decode.field "lon" Json.Decode.float)
    --         ))
    --         (Json.Decode.field "temperature_range" Json.Decode.string)

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
      SetZipcode ->
        let
            searchZip = Debug.log "zip " model.zipcodetext
            searchStation = "remove"
            newZipcode = Loading
            newZipRecord = findRecord searchZip
            stationString = "&stationid=" ++ (getStation1ID newZipRecord) ++ "&stationid=" ++ (getStation2ID newZipRecord) ++ "&stationid=" ++ (getStation3ID newZipRecord)
        in
            ( { model | zipcode = newZipcode
                      , zipcodetext = ""
                      , zipRecord = newZipRecord  }
            -- , Http.get { url = "https://phzmapi.org/"++ searchZip ++ ".json", expect = Http.expectJson GotZipcode (jsonDecoder searchZip) }
            , Http.request { method = "GET"
                           , headers = [ Http.header "token" api_key_NOAA]
                           , url = ("https://www.ncdc.noaa.gov/cdo-web/api/v2/data?datasetid=NORMAL_ANN&datatypeid=ANN-TMIN-PRBLST-T28FP30&datatypeid=DJF-TMIN-NORMAL&datatypeid=ANN-TMIN-PRBFST-T28FP30&startdate=2010-01-01&enddate=2010-01-01&" ++ stationString)
                           , body = Http.emptyBody
                           , expect = Http.expectJson (GotResults searchZip searchStation) jsonDecoder
                           , timeout = Just 5000
                           , tracker = Nothing }
            )

      SetZipcodeText str ->
        ( { model | zipcodetext = str }, Cmd.none)
      
      GotZipcode result ->
        case result of
            Ok z ->
              ({ model | zipcode = Success z, plants = List.map (enablePlants (getZoneFloat (Success z))) model.plants, zipcodetext = ""}, Cmd.none)

            Err _ ->
              ({ model | zipcode = Failure}, Cmd.none)
      
      GotResults searchZip searchStation results ->
        case results of
            Ok rs ->
              let
                springFrostDay = round (getAvg "ANN-TMIN-PRBLST-T28FP30" rs)
                winterFrostDay = round (getAvg "ANN-TMIN-PRBFST-T28FP30" rs)
                minWinterTemp = 0
                avgWinterTemp = toFloat (round (getAvg "DJF-TMIN-NORMAL" rs)) / 10.0
                z = Zipcode searchZip searchStation (tempToZone minWinterTemp) "" minWinterTemp avgWinterTemp springFrostDay winterFrostDay
              in
                ({ model | zipcode = Success z
                         , plants = List.map (enablePlants (getZoneFloat (Success z))) model.plants
                         , zipcodetext = ""}, Cmd.none)

            Err _ ->
              let
                dummy = Debug.log "JSON:" results
              in
                ({ model | zipcode = Failure}, Cmd.none)
        

      TogglePlant plant ->
        let
          newPlants = List.map (togglePlant plant) model.plants
        in
          ( { model | plants = newPlants }, Cmd.none)
          
      SetFilter string ->
        ( { model | filter = String.trim string }, Cmd.none)
          
      SendPDF ->
        ( model
        , let
            sortedPlants = if model.sortMode == "ABC" then (List.filter .selected model.plants) else List.sortWith sortByEarly (List.filter .selected model.plants)
            (springFrost, winterFrost) = getFrostInts model.zipcode
          in
            outputPort
              ( Json.Encode.encode
                2
                (Json.Encode.object
                [ ("location", Json.Encode.string (getLocationName model.zipRecord))
                , ("sf", Json.Encode.int springFrost)
                , ("wf", Json.Encode.int winterFrost)
                , ("sfstr", Json.Encode.string (getFrostString "spring" model.zipcode))
                , ("wfstr", Json.Encode.string (getFrostString "winter" model.zipcode))
                , ("plants", Json.Encode.list encodePlant (List.map (packPlant springFrost) sortedPlants) )
                ]
              ))
        )
          
      ClearAll ->
        ( { model | zipcode = Unset, plants = List.map unselect model.plants }, Cmd.none)
        
      ToggleSort ->
        ( { model | sortMode = if model.sortMode == "ABC" then "Date" else "ABC" }, Cmd.none)

packBoxes : Int -> ((Int, Int), (Int, Int), (Int, Int)) -> List ( (String, Int, Int) )
packBoxes offset ((i1, i2), (t1, t2), (o1, o2)) =
  []
    |> (if i2 > 0 then ((::) ("i", i1+offset, i2)) else identity)
    |> (if t2 > 0 then ((::) ("t", t1+offset, t2)) else identity)
    |> (if o2 > 0 then ((::) ("o", o1+offset, o2)) else identity)


packPlant : Int -> Plant -> PackedPlant
packPlant springFrost plant =
  PackedPlant (.name plant)
              ( List.concatMap (packBoxes (springFrost-120)) (.defaultPeriods plant)
              )

{-
  List.map (.defaultPeriods plant)
    sfOffset = sf - 120
    sx1 = ss + sfOffset
    sx2 = sx1 + sw
    px1 = ps + sfOffset
    px2 = px1 + pw
    hx1 = hs + sfOffset
    hx2 = hx1 + hw
  in
    [ Svg.rect [ x (toPercentOffset ss (getFrostOffset sf wf cycle)), y (String.fromInt (y0-9)), width (toPercentStr sw), height "17", stroke "#77734f", fill "rgba(209, 193, 42, .7)"]
      (if sf > 0 then [ Svg.title [] [ text (dayToDateString sx1 daysInAYearFull ++ " through " ++ dayToDateString sx2 daysInAYearFull) ] ] else []) {- -23 -}
    , Svg.rect [ x (toPercentOffset ps (getFrostOffset sf wf cycle)), y (String.fromInt (y0-9)), width (toPercentStr pw), height "17", stroke "#517f6c", fill "rgba(56, 165, 116, .7)"] 
      (if sf > 0 then [ Svg.title [] [ text (dayToDateString px1 daysInAYearFull ++ " through " ++ dayToDateString px2 daysInAYearFull) ] ] else [])
    , Svg.rect [ x (toPercentOffset hs (getFrostOffset sf wf cycle)), y (String.fromInt (y0-9)), width (toPercentStr hw), height "17", stroke "#7c6650", fill "rgba(211, 122, 41, .7)"]
      (if sf > 0 then [ Svg.title [] [ text (dayToDateString hx1 daysInAYearFull ++ " through " ++ dayToDateString hx2 daysInAYearFull) ] ] else []) {- +9 -}
    ]
  :: 
  
  --}


encodePlant : PackedPlant -> Json.Encode.Value
encodePlant plant =
  Json.Encode.object [ ("name", Json.Encode.string (.name plant)),
                       ("boxes", Json.Encode.list encodeBox (.boxes plant) )
                     ]

encodeBox : (String, Int, Int) -> Json.Encode.Value
encodeBox (boxtype, start, duration) =
  Json.Encode.object [ ("type", Json.Encode.string boxtype)
                     , ("start", Json.Encode.int start)
                     , ("duration", Json.Encode.int duration)
                     ]


daysInAYear : List ((String, Int))
daysInAYear = [("Jan.", 31), ("Feb.", 28), ("Mar.", 31), ("Apr.", 30), ("May", 31), ("June", 30), ("July", 31), ("Aug.", 31), ("Sep.", 30), ("Oct.", 31), ("Nov.", 30), ("Dec.", 31)]

daysInAYearFull : List ((String, Int))
daysInAYearFull = [("January", 31), ("February", 28), ("March", 31), ("April", 30), ("May", 31), ("June", 30), ("July", 31), ("August", 31), ("September", 30), ("October", 31), ("November", 30), ("December", 31)]

dayToDateString : Int -> List ((String, Int)) -> String
dayToDateString n months =
  if n < 0 then "N/A" else
    case months of
      (str, month)::ms -> if n <= month then str ++ " " ++ String.fromInt n else dayToDateString (n-month) ms
      _ -> dayToDateString (n-365) daysInAYear


getAvg : String -> List (JsonResult) -> Float
getAvg key list =
  let
    flist = List.filter (.datatype >> (==) key) list {- (\{datatype} -> datatype == key)   or   (\x -> x.datatype == key -}
  in
    toFloat (List.sum (List.map .value flist)) / toFloat (List.length flist)


getZoneFloat : HTTPStatus -> Float
getZoneFloat z =
  case z of
    Success y ->
      case y.zone of
        "1a" -> 1.0
        "1b" -> 1.5
        "2a" -> 2.0
        "2b" -> 2.5
        "3a" -> 3.0
        "3b" -> 3.5
        "4a" -> 4.0
        "4b" -> 4.5
        "5a" -> 5.0
        "5b" -> 5.5
        "6a" -> 6.0
        "6b" -> 6.5
        "7a" -> 7.0
        "7b" -> 7.5
        "8a" -> 8.0
        "8b" -> 8.5
        "9a" -> 9.0
        "9b" -> 9.5
        "10a" -> 10.0
        "10b" -> 10.5
        _ -> 6.0
    _ -> 6.0

tempToZone : Float -> String
tempToZone t =
  if t <= -60 then "1a" else
    if t > 70 then "13b" else
      let
        t1 = t + 70
        t2 = floor (t1/10.0)
      in
        String.fromInt t2 ++ (if (t1 - toFloat (t2*10)) > 5.0 then "b" else "a")

enablePlants : Float -> Plant -> Plant
enablePlants zone plant =
  if plant.minzone <= zone && plant.maxzone >= zone then { plant | disabled = False } else { plant | disabled = True }


unselect : Plant -> Plant
unselect plant =
    { plant | selected = False }

togglePlant : Plant -> Plant -> Plant
togglePlant target test =
    if test == target then { test | selected = not test.selected } else test

-- SUBSCRIPTIONS
port outputPort : (String) -> Cmd msg

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
      div [] [ input [ type_ "checkbox"
                     , id ("p"++ String.fromInt index)
                     , checked p.selected
                     , onClick (TogglePlant p)
                     , disabled p.disabled ] [], label [ class p.category, for ("p"++ (String.fromInt index)) ] [ text p.name ] ] :: getPlants (index + 1) ps
    _ -> []

drawSVG : (Int, Int) -> List (Plant) -> Html Msg
drawSVG (sf, wf) plants =
    svg [ style ("width:100%; height: "++ String.fromInt (List.length plants*62+130) ++"px; stroke: #888; fill; stroke-width: 1"), Svg.Attributes.shapeRendering "crispEdges" ]
    (List.append
        [ Svg.linearGradient [ x1 "0", y1 "0", x2 "100%", y2 "0", Svg.Attributes.id "g1" ]
          [ Svg.stop [ Svg.Attributes.stopColor "#e1e2e7", Svg.Attributes.stopOpacity "1.0", Svg.Attributes.offset "97%" ] []
          , Svg.stop [ Svg.Attributes.stopColor "#e1e2e7", Svg.Attributes.stopOpacity "0.0", Svg.Attributes.offset "100%" ] [] ]
        , Svg.linearGradient [ x1 "100%", y1 "0", x2 "0", y2 "0", Svg.Attributes.id "g2" ]
          [ Svg.stop [ Svg.Attributes.stopColor "#e1e2e7", Svg.Attributes.stopOpacity "1.0", Svg.Attributes.offset "97%" ] []
          , Svg.stop [ Svg.Attributes.stopColor "#e1e2e7", Svg.Attributes.stopOpacity "0.0", Svg.Attributes.offset "100%" ] [] ]
        , Svg.rect [ Svg.Attributes.class "spring-frost", x "28%", y "115", width (toPercentStr sf), height "100%", stroke "none", fill "url(#g1)"] []
        , Svg.rect [ Svg.Attributes.class "winter-frost", x (toPercentOffset wf 0), y "115", width (toPercentStr (365-sf)), height "100%", stroke "none", fill "url(#g2)"] []
        , Svg.rect [ x "10%", y "12", width "5%", height "17", stroke "#77734f", fill "rgba(209, 193, 42, .7)"] []
        , Svg.text_ [y "34", x "12.5%", style "fill: #444; stroke: none; text-anchor: middle; dominant-baseline: hanging; font-size: 1.0rem;"] [ Svg.text "Plant indoors" ]
        , Svg.rect [ x "35%", y "12", width "5%", height "17", stroke "#517f6c", fill "rgba(56, 165, 116, .7)"] []
        , Svg.text_ [y "34", x "37.5%", style "fill: #444; stroke: none; text-anchor: middle; dominant-baseline: hanging; font-size: 1.0rem;"] [ Svg.text "Transplant seedlings" ]
        , Svg.rect [ x "60%", y "12", width "5%", height "17", stroke "#7c6650", fill "rgba(211, 122, 41, .7)"] []
        , Svg.text_ [y "34", x "62.5%", style "fill: #444; stroke: none; text-anchor: middle; dominant-baseline: hanging; font-size: 1.0rem;"] [ Svg.text "Plant outdoors" ]
        , Svg.rect [ x "85%", y "12", width "5%", height "17", stroke "none", fill "#e1e2e7"] []
        , Svg.text_ [y "34", x "87.5%", style "fill: #444; stroke: none; text-anchor: middle; dominant-baseline: hanging; font-size: 1.0rem;"] [ Svg.text "Frost danger" ]
        , Svg.text_ [y "105", x "28.00%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.85rem;"] [ Svg.text "JAN" ]
        , Svg.text_ [y "105", x "34.31%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.85rem;"] [ Svg.text "FEB" ]
        , Svg.text_ [y "105", x "39.84%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.85rem;"] [ Svg.text "MAR" ]
        , Svg.text_ [y "105", x "45.95%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.85rem;"] [ Svg.text "APR" ]
        , Svg.text_ [y "105", x "51.87%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.85rem;"] [ Svg.text "MAY" ]
        , Svg.text_ [y "105", x "57.98%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.85rem;"] [ Svg.text "JUN" ]
        , Svg.text_ [y "105", x "63.90%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.85rem;"] [ Svg.text "JUL" ]
        , Svg.text_ [y "105", x "70.02%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.85rem;"] [ Svg.text "AUG" ]
        , Svg.text_ [y "105", x "76.13%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.85rem;"] [ Svg.text "SEP" ]
        , Svg.text_ [y "105", x "82.05%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.85rem;"] [ Svg.text "OCT" ]
        , Svg.text_ [y "105", x "88.16%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.85rem;"] [ Svg.text "NOV" ]
        , Svg.text_ [y "105", x "94.08%", style "fill: #444; stroke: none; text-anchor: middle; font-size: 0.85rem;"] [ Svg.text "DEC" ]
        , Svg.line [y2 "100%", y1 "115", x2 "28.00%", x1 "28.00%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "115", x2 "34.31%", x1 "34.31%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "115", x2 "39.84%", x1 "39.84%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "115", x2 "45.95%", x1 "45.95%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "115", x2 "51.87%", x1 "51.87%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "115", x2 "57.98%", x1 "57.98%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "115", x2 "63.90%", x1 "63.90%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "115", x2 "70.02%", x1 "70.02%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "115", x2 "76.13%", x1 "76.13%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "115", x2 "82.05%", x1 "82.05%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "115", x2 "88.16%", x1 "88.16%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "115", x2 "94.08%", x1 "94.08%", stroke "#d8d8d8"] []
        , Svg.line [y2 "100%", y1 "115", x2 "99.9%", x1 "99.9%", stroke "#d8d8d8"] []
        ]
        (List.concat (List.indexedMap (drawRow (sf, wf)) plants)))

drawRow : (Int, Int) -> Int -> Plant -> List (Svg.Svg Msg)
drawRow (sf, wf) index plant =
    let
      y0 = 62 * (index+1) + 105
    in
      List.append
        [ Svg.line [ y1 (String.fromInt (y0-14)), y2 (String.fromInt (y0+14)), x1 "28.00%", x2 "28.00%" ] []
        , Svg.line [ y1 (String.fromInt (y0-14)), y2 (String.fromInt (y0+14)), x1 "34.31%", x2 "34.31%" ] []
        , Svg.line [ y1 (String.fromInt (y0-14)), y2 (String.fromInt (y0+14)), x1 "39.84%", x2 "39.84%" ] []
        , Svg.line [ y1 (String.fromInt (y0-14)), y2 (String.fromInt (y0+14)), x1 "45.95%", x2 "45.95%" ] []
        , Svg.line [ y1 (String.fromInt (y0-14)), y2 (String.fromInt (y0+14)), x1 "51.87%", x2 "51.87%" ] []
        , Svg.line [ y1 (String.fromInt (y0-14)), y2 (String.fromInt (y0+14)), x1 "57.98%", x2 "57.98%" ] []
        , Svg.line [ y1 (String.fromInt (y0-14)), y2 (String.fromInt (y0+14)), x1 "63.90%", x2 "63.90%" ] []
        , Svg.line [ y1 (String.fromInt (y0-14)), y2 (String.fromInt (y0+14)), x1 "70.02%", x2 "70.02%" ] []
        , Svg.line [ y1 (String.fromInt (y0-14)), y2 (String.fromInt (y0+14)), x1 "76.13%", x2 "76.13%" ] []
        , Svg.line [ y1 (String.fromInt (y0-14)), y2 (String.fromInt (y0+14)), x1 "82.05%", x2 "82.05%" ] []
        , Svg.line [ y1 (String.fromInt (y0-14)), y2 (String.fromInt (y0+14)), x1 "88.16%", x2 "88.16%" ] []
        , Svg.line [ y1 (String.fromInt (y0-14)), y2 (String.fromInt (y0+14)), x1 "94.08%", x2 "94.08%" ] []
        , Svg.line [ y1 (String.fromInt (y0-14)), y2 (String.fromInt (y0+14)), x1 "99.9%", x2 "99.9%" ] []
        , Svg.line [ y1 (String.fromInt y0), y2 (String.fromInt y0), x1 "99.9%", x2 "0%" ] []
        , Svg.text_ [ y (String.fromInt (y0-2)), x "26.5%", width "25%", style "fill: #444; stroke: none; text-anchor: end; font-weight: 400; font-family: 'Gloria Hallelujah'; font-size: 1.5rem;" ] [ Svg.text plant.name]
        ] 
        (List.concat (List.indexedMap (drawCycle y0 (sf, wf)) plant.defaultPeriods))

getZoneOffset : Int -> Int -> Float
getZoneOffset zone cycle =
  case zone of
    7 -> if cycle == 1 then -3.5617 else 3.5617
    6 -> 0
    5 -> if cycle == 1 then 4.931 else -4.931
    4 -> if cycle == 1 then 8.493 else -8.493
    _ -> -1
    
  -- To do: include winter frost for second cycle
getFrostOffset : Int -> Int -> Int -> Float
getFrostOffset sfday wfday cycle =
  if sfday < 0
  then 0
  else ((toFloat sfday) - 120.0)/3.65

toPercentStr : Int -> String
toPercentStr d =
  String.fromFloat ((toFloat d)/365.0 * 72) ++ "%"

toPercentOffset : Int -> Float -> String
toPercentOffset d offset =
  String.fromFloat ((toFloat d)/365.0 * 72 + 28 + offset) ++ "%"

drawCycle : Int -> (Int, Int) -> Int -> ((Int, Int), (Int, Int), (Int, Int)) -> List (Svg.Svg Msg)
drawCycle y0 (sf, wf) cycle ((ss, sw), (ps, pw), (hs, hw)) =
  let 
    sfOffset = sf - 120
    sx1 = ss + sfOffset
    sx2 = sx1 + sw
    px1 = ps + sfOffset
    px2 = px1 + pw
    hx1 = hs + sfOffset
    hx2 = hx1 + hw
  in
    [ Svg.rect [ x (toPercentOffset ss (getFrostOffset sf wf cycle)), y (String.fromInt (y0-9)), width (toPercentStr sw), height "17", stroke "#77734f", fill "rgba(209, 193, 42, .7)"]
      (if sf > 0 then [ Svg.title [] [ text (dayToDateString sx1 daysInAYearFull ++ " through " ++ dayToDateString sx2 daysInAYearFull) ] ] else []) {- -23 -}
    , Svg.rect [ x (toPercentOffset ps (getFrostOffset sf wf cycle)), y (String.fromInt (y0-9)), width (toPercentStr pw), height "17", stroke "#517f6c", fill "rgba(56, 165, 116, .7)"] 
      (if sf > 0 then [ Svg.title [] [ text (dayToDateString px1 daysInAYearFull ++ " through " ++ dayToDateString px2 daysInAYearFull) ] ] else [])
    , Svg.rect [ x (toPercentOffset hs (getFrostOffset sf wf cycle)), y (String.fromInt (y0-9)), width (toPercentStr hw), height "17", stroke "#7c6650", fill "rgba(211, 122, 41, .7)"]
      (if sf > 0 then [ Svg.title [] [ text (dayToDateString hx1 daysInAYearFull ++ " through " ++ dayToDateString hx2 daysInAYearFull) ] ] else []) {- +9 -}
    ]

plantNameContains : String -> Plant -> Bool
plantNameContains search plant =
    String.contains search (String.toLower (.name plant))

getTempString : HTTPStatus -> String
getTempString status =
    case status of
        Success {temp_range} -> case (List.map .submatches (Regex.find (Maybe.withDefault Regex.never <| Regex.fromString "(.*) to (.*)") temp_range)) of
                                        [[Just a, Just b]] -> a ++ "° to " ++ b ++ "° F"
                                        _ -> ""
        _ -> ""

getPHZ : HTTPStatus -> String
getPHZ status =
    case status of
        Success good ->
          String.toUpper good.zone
        Loading ->
          "Loading..."
        _ ->
          "6A (default)"

getMinTemp : HTTPStatus -> String
getMinTemp status =
    case status of
        Success good ->
          (String.fromFloat good.min_winter_temp) ++ "° F"
        Loading ->
          "Loading..."
        _ ->
          ""

getFrostInts : HTTPStatus -> (Int, Int)
getFrostInts status =
    case status of
        Success good ->
          (good.springFrost, good.winterFrost)
        Loading ->
          (-1000, 1000)
        _ ->
          (-1000, 1000)

getFrostString : String -> HTTPStatus -> String
getFrostString which status =
    case status of
        Success good ->
          dayToDateString (if which == "winter" then good.winterFrost else good.springFrost) daysInAYear
        Loading ->
          "Loading..."
        Unset ->
          "—"
        _ ->
          "Data not found"

getZIP : HTTPStatus -> String
getZIP status =
  case status of
    Success zipcode -> zipcode.zipcode
    Loading -> "Loading"
    Unset -> "Unset"
    _ -> "Error, please try again"


drawBottomInstructions : Html Msg
drawBottomInstructions =
    div [ class "bottom_info__instructions" ] []

drawBottomContent : List (Plant) -> HTTPStatus -> Html Msg
drawBottomContent selectedPlants status =
    drawSVG (getFrostInts status) selectedPlants {- If status is unset, make it clear we're using Default zone -}


drawTopInstructions : Html Msg
drawTopInstructions =
  div [ class "top_info" ]
  [ div [ class "top_info--third" ] [ text "1. Set your zip code" ]
  , div [ class "top_info--third" ] [ text "2. Select some plants" ]
  , div [ class "top_info--third" ] [ text "3. See the calendar", br [] [], text "(and save a PDF!)" ] ]


drawTopContent : ZipRecord -> HTTPStatus -> Html Msg
drawTopContent zr status = 
  div [ class "top_info" ]
    [ div [ class "top_info__left" ]
      [ div [] [ strong [] [ text "Location: " ], text (getLocationName zr) ]
      , div [] [ strong [] [ text "Weather station: " ], abbr [ title ((getStation1ID zr) ++ ", " ++ (getStationDist zr)) ] [ text (getStationName zr) ] ]
      , div [] [ strong [] [ text "ZIP Code: " ], text (getZIP status) ]
      , div [] [ strong [] [ text "Coordinates: " ], text (getStationCoords zr) ]
      , div [] [ strong [] [ text "Elevation: " ], text (getStationElev zr) ]
      , div [] [ strong [] [ text "Average last spring frost: " ], text (getFrostString "spring" status)]
      , div [] [ strong [] [ text "Average winter min. temperature: " ], text (getMinTemp status)]
      , div [] [ strong [] [ text "Average first winter frost: " ], text (getFrostString "winter" status)]
      -- , div [] [ strong [] [ text "Extreme low temperature: " ], text (getTempString status) ]
      , div [] [ strong [] [ text "USDA plant hardiness zone: " ], text "N/A"]
      ]
    , div [ class "top_info__right" ] [ usaSVG (Maybe.withDefault 0 (String.toInt (String.slice 0 3 (getZIP status)))) ]
    ]

view : Model -> Html Msg
view model =
  let
    sidebarPlants = List.filter (plantNameContains (String.toLower model.filter)) model.plants
    selectedPlants = if model.sortMode == "ABC" then (List.filter .selected model.plants) else List.sortWith sortByEarly (List.filter .selected model.plants)
    topContent = if model.zipcode /= Unset then drawTopContent model.zipRecord model.zipcode else drawTopInstructions
    plantsAreSelected = List.length selectedPlants > 0
    bottomContent = if plantsAreSelected then drawBottomContent selectedPlants model.zipcode else drawBottomInstructions
  in
    div []
    [ div [ class "header" ]
      [ div [ class "header_left" ] [ text "plant-calendar" ]
      , div [ class "header_right" ] [
        ul []
        [ li [ class (if plantsAreSelected then "sort show" else "hide") ] [ a [ onClick ToggleSort ] [ span [ class "icon" ] [], text (if model.sortMode == "ABC" then "Sort: Date" else "Sort: Name") ] ]
        , li [ class (if plantsAreSelected then "clear show" else "hide") ] [ a [ onClick ClearAll ] [ span [ class "icon" ] [], text "Clear All" ] ]
        , li [ class (if plantsAreSelected then "pdf show" else "hide") ] [ a [ onClick SendPDF ] [ span [ class "icon" ] [], text "Save PDF" ] ]
        , li [ class "donate" ] [ a [ href "#", target "_blank" ] [ span [ class "icon" ] [], text "Donate" ] ]
        , li [ class "github" ] [ a [ href "https://github.com/gg314/plant-calendar", target "_blank" ] [ span [ class "icon" ] [], text "Github" ] ]
        ]
      ]
    ]
    , div [ class "container" ]
      [ div [ class "sidebar" ]
        [ div [ class "sidebar__top" ]
          [ div [ class "location" ]
            [ form [ onSubmit SetZipcode ]
              [ input [ type_ "text", id "location", placeholder "ZIP Code", onInput SetZipcodeText, value model.zipcodetext ] []
              , button [ ] [ text "Set" ]
              ]
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
            [ topContent
            , bottomContent
            ]
      ]
    ]

w1 : Float
w1 = 30/285.0

w2 : Float
w2 = 70/285.0

w3 : Float
w3 = 80/285.0

m : Float
m = 1/12.0


sortByEarly : Plant -> Plant -> Order
sortByEarly p1 p2 =
  case p1.defaultPeriods of 
    ((s1, _), (t1, _), (u1, _))::_ ->
      case p2.defaultPeriods of
        ((s2, _), (t2, _), (u2, _))::_ ->
          case compare s1 s2 of
            EQ ->
              case compare t1 t2 of
                EQ ->
                  compare u1 u2
                c -> c
            c -> c
        _ -> LT
    _ -> GT


plantData : Int -> List (Plant)
plantData sfday =
  let
    t0 = 120
  in
    [ {name = "Basil", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 57, 15), (t0, 22), (0, 0))]}
    , {name = "Beets", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0 - 14, 8))]}
    , {name = "Bell Peppers", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 71, 14), (t0 + 14, 21), (0, 0))]}
    , {name = "Broccoli", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 42, 14), (t0 - 21, 21), (0, 0))]}
    , {name = "Brussels Sprouts", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 42, 14), (t0 - 28, 21), (0, 0))]}
    , {name = "Cabbage", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 57, 14), (t0 - 37, 14), (0, 0))]}
    , {name = "Cantaloupes", category = "Fruits", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 28, 7), (t0 + 14, 21), (0, 0))]}
    , {name = "Carrots", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0 - 35, 14))]}
    , {name = "Cauliflower", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 42, 14), (t0 - 28, 14), (0, 0))]}
    , {name = "Celery", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 71, 14), (t0 + 7, 14), (0, 0))]}
    , {name = "Chives", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0 - 28, 7))]}
    , {name = "Cilantro", category = "Herbs", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0, 14))]}
    , {name = "Corn", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0, 14))]}
    , {name = "Cucumber", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 28, 7), (t0 + 14, 21), (0, 0))]}
    , {name = "Dill", category = "Herbs", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0 - 35, 14))]}
    , {name = "Eggplants", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 70, 14), (t0 + 14, 21), (0, 0))]}
    , {name = "Green Beans", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0 + 7, 21))]}
    , {name = "Kale", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 42, 14), (t0 - 28, 14), (0, 0))]}
    , {name = "Lettuce", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 42, 14), (t0 - 14, 28), (0, 0))]}
    , {name = "Okra", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0 + 14, 14))]}
    , {name = "Onions", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0 - 28, 21))]}
    , {name = "Oregano", category = "Herbs", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 70, 28), (t0, 21), (0, 0))]}
    , {name = "Parsley", category = "Herbs", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0 - 28, 14))]}
    , {name = "Parsnips", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0 - 21, 21))]}
    , {name = "Peas", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0 - 42, 21))]}
    , {name = "Potatoes", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0 - 7, 21))]}
    , {name = "Pumpkins", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 28, 14), (t0 + 14, 21), (0, 0))]}
    , {name = "Radishes", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0 - 56, 22))]}
    , {name = "Rosemary", category = "Herbs", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 70, 14), (t0 + 7, 21), (0, 0))]}
    , {name = "Sage", category = "Herbs", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 56, 15), (t0, 14), (0, 0))]}
    , {name = "Spinach", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0 - 42, 21))]}
    , {name = "Squash", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 28, 14), (t0 + 14, 21), (0, 0))]}
    , {name = "Sweet Potatoes", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 28, 7), (t0 + 14, 21), (0, 0))]}
    , {name = "Swiss Chard", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 42, 14), (t0 - 21, 14), (0, 0))]}
    , {name = "Thyme", category = "Herbs", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 70, 28), (t0, 21), (0, 0))]}
    , {name = "Tomatoes", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 57, 15), (t0 + 7, 21), (0, 0))]}
    , {name = "Turnips", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((-1000, -1000), (-1000, -1000), (t0 - 28, 21))]}
    , {name = "Watermelon", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 28, 7), (t0 + 14, 21), (0, 0))]}
    , {name = "Zucchini", category = "Vegetables", selected = True, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((t0 - 28, 14), (t0 + 14, 21), (0, 0))]}
    ]


-- Early data
{-   
plantData2 : List (Plant)
plantData2 = 
    [ {name = "Anemone", category = "Flowers", selected = False, disabled = False, minzone = 2.0, maxzone = 13.5, defaultPeriods = [((0.10137, 0.246), (0.217414, 0.2456), (0 ,0)), ((0.605479, 0.2456), (0.721523, 0.2456), (0, 0))]}
    , {name = "Anise Hyssop", category = "Flowers", selected = False, disabled = False, minzone = 2.0, maxzone = 13.5, defaultPeriods = [((0.75*m, w2), (2.6*m, w2), (0, 0)), ((7.7*m, w2), (9.3*m, w2), (0, 0))]}
    , {name = "Asters", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.1*m, w2), (2.6*m, w2), (0, 0)), ((6.4*m, w2), (8.1*m, w2), (0, 0))]}
    , {name = "Astilbe", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.4*m, w1), (2.5*m, w1), (0, 0)), ((8.2*m, w1), (9.3*m, w1), (0, 0))]}
    , {name = "Baby's Breath", category = "Flowers", selected = False, disabled = False, minzone = 2.0, maxzone = 11.5, defaultPeriods = [((2.5*m, w1), (2.5*m, w1), (0, 0)), ((8.6*m, w1), (10.0*m, w1), (0, 0))]}
    , {name = "Bee Balm", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Black-Eyed Susans", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Butterfly Bush", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Borage (star flower)", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Calendula", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Cannas", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Carnations", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Celosia", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Chrysanthemum", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Clematis", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Columbine", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Coneflowers", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Coreopsis", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Cosmos", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Crocuses", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Daffodils", category = "Flowers", selected = False, disabled = False, minzone = 2.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Dahlias", category = "Flowers", selected = False, disabled = False, minzone = 2.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Daisies", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Daylilies", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Delphiniums", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Echinacea", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Gardenias", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Geraniums", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Gladiolus", category = "Flowers", selected = False, disabled = False, minzone = 2.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Hibiscus", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Hollyhock", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Honeysuckle", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Hyacinth", category = "Flowers", selected = False, disabled = False, minzone = 2.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Hydrangea", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Impatiens", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Irises", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Jasmine", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Lilies", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Marigolds", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Morning Glories", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Nasturtium", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Pansies", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Peonies", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Petunias", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Phlox", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Poppies", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Roses", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Salvia", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Sedum", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Shasta Daisies", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Strawflowers", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Sunflowers", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Sweet Peas", category = "Flowers", selected = False, disabled = False, minzone = 2.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Tuberose", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Tulips", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Verbenas", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Veronica (Speedwell)", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Viola", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Yarrow", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Zinnias", category = "Flowers", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (0, 0)), ((1.0*m, w2), (1.0*m, w2), (0, 0))]}
    , {name = "Artichoke", category = "Vegetables", selected = False, disabled = False, minzone = 3.0, maxzone = 9.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Arugula", category = "Vegetables", selected = False, disabled = False, minzone = 3.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Asparagus", category = "Vegetables", selected = False, disabled = False, minzone = 3.0, maxzone = 8.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Beans", category = "Vegetables", selected = False, disabled = False, minzone = 3.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Beets", category = "Vegetables", selected = False, disabled = False, minzone = 3.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Bell Peppers", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Broccoli", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Brussels Sprouts", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Cabbage", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Carrots", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Cauliflower", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Celery", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Collards", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Corn", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Cucumbers", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Edamame", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Eggplants", category = "Vegetables", selected = False, disabled = False, minzone = 3.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Endive", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Fava Beans", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Garlic", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Gourds", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Green Beans", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Horseradish", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 8.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Kale", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Leeks", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Lettuce", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Okra", category = "Vegetables", selected = False, disabled = False, minzone = 3.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Onions", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Parsnips", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Peas", category = "Vegetables", selected = False, disabled = False, minzone = 3.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Potatoes", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Pumpkins", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Radishes", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Rhubarb", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 8.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Rutabagas", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Shallots", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Snap Peas", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Soybean", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Spinach", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Squash", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Sweet Potatoes", category = "Vegetables", selected = False, disabled = False, minzone = 3.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Swiss Chard", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Tomatoes", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Turnips", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Zucchini", category = "Vegetables", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Basil", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Catnip", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Chives", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Coriander/Cilantro", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Dill", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Fennel", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Lavender", category = "Herbs", selected = False, disabled = False, minzone = 5.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Lemon Grass", category = "Herbs", selected = False, disabled = False, minzone = 3.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Marjoram", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Mint", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Mustard", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Oregano", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Parsley", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Rosemary", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Sage", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Scallion", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Stevia", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Tarragon", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Thyme", category = "Herbs", selected = False, disabled = False, minzone = 1.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Apples", category = "Fruits", selected = False, disabled = False, minzone = 5.0, maxzone = 8.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Blackberries", category = "Fruits", selected = False, disabled = False, minzone = 5.0, maxzone = 8.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Blueberries", category = "Fruits", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Cantaloupes", category = "Fruits", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Currant", category = "Fruits", selected = False, disabled = False, minzone = 1.0, maxzone = 8.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Cherries", category = "Fruits", selected = False, disabled = False, minzone = 3.0, maxzone = 9.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Figs", category = "Fruits", selected = False, disabled = False, minzone = 5.0, maxzone = 13.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Grapes", category = "Fruits", selected = False, disabled = False, minzone = 5.0, maxzone = 8.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Honeydew", category = "Fruits", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Lemons & Oranges", category = "Fruits", selected = False, disabled = False, minzone = 5.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Peaches", category = "Fruits", selected = False, disabled = False, minzone = 5.0, maxzone = 8.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Pears", category = "Fruits", selected = False, disabled = False, minzone = 5.0, maxzone = 8.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Plums", category = "Fruits", selected = False, disabled = False, minzone = 5.0, maxzone = 8.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Raspberries", category = "Fruits", selected = False, disabled = False, minzone = 5.0, maxzone = 8.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Strawberries", category = "Fruits", selected = False, disabled = False, minzone = 5.0, maxzone = 8.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    , {name = "Watermelon", category = "Fruits", selected = False, disabled = False, minzone = 1.0, maxzone = 11.5, defaultPeriods = [((1.0*m, w2), (1.0*m, w2), (1.0*m, w3)), ((1.0*m, w2), (1.0*m, w2), (1.0*m, w3))]}
    ]
    -}