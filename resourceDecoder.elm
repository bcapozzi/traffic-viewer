import Http
import String as String
import StartApp
import Effects exposing (Effects,Never)
import Task
import Html exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, (:=))

import Svg exposing (svg, rect, polygon, circle, polyline, g)
import Svg.Attributes exposing (..)
import Array

resourcesUrl = "./resources.json"
resourceCountsUrl = "./resource-counts.json"

-- StartApp plumbing
app =
  StartApp.start { init = init, view = view, update = update, inputs = [] }

main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


type Action
  = NoOp
  | GetResources
  | GetResourceCounts
  | ShowResources (Maybe Resources)
  | ShowResourceCounts (Maybe ResourceCounts)

type alias Model =
  { resources : Maybe Resources
   ,resourceCounts : Maybe ResourceCounts}

init =
  ({ resources = Nothing
    ,resourceCounts = Nothing }, Effects.none)

update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    GetResources ->
      ({ model | resources = Nothing }, getResources)
    GetResourceCounts ->
      ({ model | resourceCounts = Nothing }, getResourceCounts)
    ShowResources maybeResources ->
      ({ model | resources = maybeResources }, Effects.none)
    ShowResourceCounts maybeResourceCounts ->
      ({ model | resourceCounts = maybeResourceCounts }, Effects.none)

createLabelForResourceCount resourceCount =
  Svg.text' [x "-400", y "-80"][text resourceCount.id]

createCountSvg resourceCount = 
      Svg.svg [ width "400", height "200", viewBox "0 0 400 200"]
      [g [ transform ("translate(400, 100)")]
      (List.append [(Svg.rect [x "-400", y "-100", width "400", height "200", style "fill:#FFFFFF;stroke:#222222"][])] (List.append [(createLabelForResourceCount resourceCount),(createSparkLineForResourceCount resourceCount)]  createTicks))
      ]

combine tc1 tc2 =
  Array.fromList [tc1, { t = tc2.t,
    count = tc1.count}]
    
getValue maybeValue =
  case maybeValue of
    Nothing -> 
      {t=0, count=0}
    Just value ->
      value
      
createStep counts =
  combine (getValue (Array.get 0 counts)) (getValue (Array.get 1 counts))

stairs resourceCount =
  Array.toList (toStairs Array.empty (Array.fromList resourceCount.counts))

toStairs sofar counts =
    if (Array.length counts < 2) then
      Array.append sofar (Array.slice 0 2 counts)
    else
      toStairs (Array.append sofar (createStep counts)) (Array.slice 1 10 counts)

--  
toSvgs maybeResourceCounts =
  case maybeResourceCounts of
    Nothing ->
      []
    Just resourceCounts ->
      List.map (\r -> createCountSvg r) resourceCounts

view address model =
  div []
    (List.append 
    [ button [ onClick address GetResources ] [ text "Click to get resources!" ]
    , button [ onClick address GetResourceCounts ] [ text "Click to get resource counts!" ]
    , viewResources model.resources
    , displayResources model.resources
    , displayResourceCounts model.resourceCounts
    , viewResourceCounts model.resourceCounts
    ] (List.intersperse (br [][]) (toSvgs model.resourceCounts)))



getTickHeight x =
  if ((x%60) == 0) then
    "10"
  else 
    "5"

createTicks = 
  List.map (\x -> Svg.line [x1 (toString x), x2 (toString x), y1 ("-" ++ (getTickHeight x)), y2 (getTickHeight x), stroke "black"][]) (List.reverse(List.map (\n -> (-n*15)) [0..24]))

toDisplayTime t = 
  (toString -t)

toDisplayCount c =
  (toString ((toFloat -c)/3.0*100.0))

toXYPointString : ResourceCount -> String
toXYPointString resourceCount = 
  String.concat (List.map (\p -> ( (toDisplayTime p.t) ++ "," ++ (toDisplayCount p.count) ++ " ")) (stairs resourceCount))

createSparkLineForResource maybeResourceCount = 
  case maybeResourceCount of
    Nothing ->
      Svg.line [x1 "-200", x2 "0", y1 "50", y2 "25", stroke "black"][]
    Just resourceCount ->
--      Svg.line [x1 "-200", x2 "0", y1 "-50", y2 "-25", stroke "black"][]
      polyline [points (toXYPointString resourceCount), stroke "blue", fill "none"][]


createSparkLineForResourceCount resourceCount = 
  polyline [points (toXYPointString resourceCount), stroke "blue", fill "none"][]

displayResourceCounts maybeResourceCounts =
  case maybeResourceCounts of
    Nothing ->
    Svg.svg [ width "400", height "200", viewBox "0 0 400 200"]
      [g [ transform ("translate(400, 100)")]
      [Svg.rect [x "-400", y "-100", width "400", height "200", style "fill:#FF3333;"][]
      ]
    ]
    Just resourceCounts ->
     Svg.svg [ width "360", height "200", viewBox "0 0 360 200"]
      [g [ transform ("translate(360, 100)")]
      ((Svg.text' [x "-360", y "20"][text "resource"]) ::
      ((createSparkLineForResource (List.head resourceCounts)) :: createTicks))
--(Svg.line [x1 "-200", x2 "0", y1 "40", y2 "20", stroke "black"][]) :: createTicks)
--(createSparkLineForResource (List.head resourceCounts)) :: 
--       (Svg.rect [x "-360", y "-100", width "360", height "200", style "fill:#33FF33;"][] :: createTicks))
       
      ]
 

getNth n maybeResources = 
  case maybeResources of
    Nothing ->
      Nothing
    Just resources ->
      (List.head (List.drop n resources))

toStringList maybeResource =
  case maybeResource of
    Nothing ->
      []
    Just resource -> 
      List.map (\c -> ((toString c.x) ++ "," ++ (toString c.y))) resource.coords

toDisplayX cx =
  toString (cx/4.5)

toDisplayY cy = 
  toString (-cy/4.5)

getXVals: List Coord -> List Float
getXVals coords =
  List.map (\c -> c.x) coords

getMaxX: List Coord -> Float
getMaxX coords =
  getMaxValueOrDefault (List.head (List.reverse (List.sort (getXVals coords))))

getMaxValueOrDefault maybeMax = 
  case maybeMax of
    Nothing ->
      200
    Just max ->
      max

--toSvgCircles: Resource -> List svg
toSvgCircles resource =
  List.map (\c -> Svg.circle [ cx (toDisplayX c.x)
                             ,  cy (toDisplayY c.y)
                             , r "2"
                             , style "fill: #60B5CC;"
                             ]
                            []) resource.coords


toSvgTextList maybeResource = 
  case maybeResource of
    Nothing ->
      []
    Just resource -> 
      List.map (\c -> Svg.circle [ cx (toDisplayX c.x)
                                ,  cy (toDisplayY c.y)
                                , r "2"
                                , style "fill: #60B5CC;"
                                ]
                              []) resource.coords
 
--text ((toString c.x) ++ "," ++ (toString c.y)) ]) resource.coords

--toSvgTextList coordStringList =
--    List.map (\c -> Svg.text' [] [ text c ]) coordStringList

-- convert single resource coords to SVG polygon
-- polygon [ fill "#60B5CC", points "323.298,143.724 323.298,0 179.573,0" ] []

toPointString resource =
  String.concat(List.map (\c -> (toDisplayX c.x) ++ "," ++ (toDisplayY c.y) ++ " ") resource.coords)

toSvgPolygon resource =
  polygon [style "stroke:#FF0000; fill:#FFFFFF", points (toPointString resource) ] []

toSvgPolygons resources =
  List.map (\r -> (toSvgPolygon r)) resources

toPolygons maybeResources = 
  [
    polygon [ fill "#60B5CC", points "23.298,143.724 23.298,0 179.573,0"][]
  ]

--toSvgShapes: Maybe Resources -> List (List svg)
toSvgShapes maybeResources =
  case maybeResources of
    Nothing ->
      []
    Just resources ->
      List.map (\r -> (toSvgCircles r)) resources

toSvgPolygonsOrNothing maybeResources =
  case maybeResources of
    Nothing ->
      []
    Just resources ->
      (toSvgPolygons resources)

displayResources maybeResources = 
  Svg.svg [ width "200", height "200", viewBox "0 0 200 200", fill "#333333" ]
    [g [ transform ("translate(100, 100)") ]
    (toSvgPolygonsOrNothing maybeResources)
    ]
--    ((Svg.rect [x "-100", y "-100", width "200", height "200", style "fill:#FF3333;"][]) :: (List.concat(toSvgShapes maybeResources)))
--(toSvgTextList (getNth 2 maybeResources)))


viewCoords coords = 
  List.map (\c -> toString c.x) coords

viewResourceCounts maybeResourceCounts = 
  case maybeResourceCounts of
    Nothing ->
      div [] [ text "No resource counts to display.  Try clicking the button" ]
    Just resourceCounts ->
      div [] [ text ("Got " ++ (toString (List.length resourceCounts)) ++ " resource counts, baby!") ]

viewResources maybeResources =
  let
    viewCoord coord =
      text ("x: " ++ (toString coord.x) ++ ", y: " ++ (toString coord.y))
    viewResource resource =
      li [] ((text ("ID: " ++ resource.id ++ "-->")) :: (List.map viewCoord resource.coords))
  in
    case maybeResources of
      Nothing ->
        div [] [ text "No resources to display. Try clicking the button" ]
      Just resources ->
        ul [] (List.map viewResource resources)

getResourceCounts : Effects Action
getResourceCounts = 
  Http.get countDecoderColl resourceCountsUrl
    |> toMaybeWithLogging
    |> Task.map ShowResourceCounts
    |> Effects.task

-- This is the key to map the result of the HTTP GET to an Action
-- Note: Task.toMaybe swallows any HTTP or JSON decoding errors
getResources : Effects Action
getResources =
  Http.get decoderColl resourcesUrl
    |> Task.toMaybe
    |> Task.map ShowResources
    |> Effects.task

-- An alternative to Task.toMaybe which dumps error information to the console log
toMaybeWithLogging : Task.Task x a -> Task.Task y (Maybe a)
toMaybeWithLogging task =
  Task.map Just task `Task.onError` (\msg -> Debug.log (toString msg) (Task.succeed Nothing))

type alias Coord = {
   x : Float
  ,y : Float
}

type alias Resource = {
    id : String
  , coords : List Coord
}

type alias Resources  = List Resource

-- The updated Resource decoder
decoder : Decoder Resource
decoder =
  Decode.object2 Resource
    ("id" := Decode.string)
    ("coords" := Decode.list coordDecoder)

coordDecoder : Decoder Coord
coordDecoder = 
  Decode.object2 Coord
    ("x" := Decode.float)
    ("y" := Decode.float)

decoderColl : Decoder Resources
decoderColl =
  Decode.object1 identity
    ("resources" := Decode.list decoder)

type alias TimedCount = {
   t : Int
  ,count : Int
}

type alias ResourceCount = {
   id : String
  ,counts : List TimedCount
}

type alias ResourceCounts = List ResourceCount

countDecoderColl : Decoder ResourceCounts
countDecoderColl =
  Decode.object1 identity
    ("resourceCounts" := Decode.list resourceCountDecoder)

resourceCountDecoder : Decoder ResourceCount
resourceCountDecoder =
  Decode.object2 ResourceCount
    ("id" := Decode.string)
    ("counts" := Decode.list timedCountDecoder)

timedCountDecoder : Decoder TimedCount
timedCountDecoder = 
  Decode.object2 TimedCount
    ("t" := Decode.int)
    ("count" := Decode.int)
