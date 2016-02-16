module ResourceDecoder where
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

import GeoUtils exposing (..)

resourcesUrl = "./resources.json"
resourceCountsUrl = "./resource-counts.json"
routesUrl = "./routes1.json"

getTimeOrigin =
  0

getDisplayTimeOrigin = 
  200

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
  | GetRoutes
  | GetResources
  | GetResourceCounts
  | ShowResources (Maybe Resources)
  | ShowResourceCounts (Maybe ResourceCounts)
  | ShowRoutes (Maybe Routes)

type alias Model =
  { resources : Maybe Resources
   ,resourceCounts : Maybe ResourceCounts
   ,routes : Maybe Routes}

init =
  ({ resources = Nothing
    ,resourceCounts = Nothing
    ,routes = Nothing }, Effects.none)

update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    GetRoutes ->
      ({ model | routes = Nothing }, getRoutes)
    GetResources ->
      ({ model | resources = Nothing }, getResources)
    GetResourceCounts ->
      ({ model | resourceCounts = Nothing }, getResourceCounts)
    ShowResources maybeResources ->
      ({ model | resources = maybeResources }, Effects.none)
    ShowResourceCounts maybeResourceCounts ->
      ({ model | resourceCounts = maybeResourceCounts }, Effects.none)
    ShowRoutes maybeRoutes ->
      ({ model | routes = maybeRoutes }, Effects.none)

createLabelForResourceCount resourceCount =
  Svg.text' [x (toString (getDisplayTimeOrigin-400)), y "-80"][text resourceCount.id]

createCountSvg resourceCount = 
      Svg.svg [ width "400", height "200", viewBox "0 0 400 200", style "margin-left:auto; margin-right:auto; display:block;"]
      [g [ transform (("translate(" ++ (toString getDisplayTimeOrigin) ++", 100)"))]
      (List.append [(Svg.rect [x ("-" ++ (toString getDisplayTimeOrigin)), y "-100", width "400", height "200", style "fill:#FFFFFF;stroke:#222222"][])] (List.append [(createLabelForResourceCount resourceCount),(createSparkLineForResourceCount resourceCount)]  createTicks))
      ]

{-
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

-}
-- alternate implementation of stairs function
combine prev next =
  {t = next.t,
   count = prev.count}
   
steps tc =
  List.map2 combine (List.take ((List.length tc)-1) tc) (List.drop 1 tc)
  
stairs resourceCount =
  interleave (steps resourceCount.counts) resourceCount.counts

interleave : List a -> List a -> List a
interleave list1 list2 = 
  case list1 of
    []      -> list2
    headA :: tailA -> 
      case list2 of
        []      -> list1
        headB :: tailB -> headB :: headA :: interleave tailA tailB

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
    , button [ onClick address GetRoutes ] [ text "Click to get routes!" ]
    , br [][]
    , displayResources model
    , displayRoutes model
    , viewResourceCounts model.resourceCounts
    , viewRoutes model.routes
    ] (List.intersperse (br [][]) (toSvgs model.resourceCounts)))



getTickHeight x =
  if ((x%60) == 0) then
    "4"
  else 
    "2"

createTicks = 
  List.append (List.map (\x -> Svg.line [x1 (toString x), x2 (toString x), y1 ("-" ++ (getTickHeight x)), y2 (getTickHeight x), stroke "gray"][]) (List.reverse(List.map (\n -> (-n*15)) [-12..12]))) createTickLabels

createTickLabels = 
  (List.map (\n -> Svg.text' [x (toDisplayTime n), y "20", fill "gray", textAnchor "middle", alignmentBaseline "middle"][text (toString n)]) [0,60,120,180]) 

toDisplayTime t = 
  (toString (t - getTimeOrigin))

toDisplayCount c =
  (toString ((toFloat -c)/4.0*100.0))

toXYPointString : ResourceCount -> String
toXYPointString resourceCount = 
  String.concat (List.map (\p -> ( (toDisplayTime p.t) ++ "," ++ (toDisplayCount p.count) ++ " ")) (stairs resourceCount))

createSparkLineForResource maybeResourceCount = 
  case maybeResourceCount of
    Nothing ->
      Svg.line [x1 "-200", x2 "0", y1 "50", y2 "25", stroke "black"][]
    Just resourceCount ->
      polyline [points (toXYPointString resourceCount), stroke "blue", fill "none"][]


createSparkLineForResourceCount resourceCount = 
  polyline [points (toXYPointString resourceCount), stroke "blue", fill "none"][]

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


getXScaleFactor minX maxX = 
  getMapDisplayWidth / (2.0*(maxX-minX))

getYScaleFactor minY maxY = 
  getMapDisplayHeight / (2.0*(maxY-minY))

toDisplayX cx minX maxX =
-- 600nmi --> 800pixels wide
  toString (cx*(getXScaleFactor minX maxX))

toDisplayY cy minY maxY  = 
  toString (-cy*(getYScaleFactor minY maxY))

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

 
-- convert single resource coords to SVG polygon

toPointString resource minX maxX minY maxY =
  String.concat(List.map (\c -> (toDisplayX c.x minX maxX) ++ "," ++ (toDisplayY c.y minY maxY) ++ " ") resource.coords)

toSvgPolygon resource minX maxX minY maxY =
  polygon [style "stroke:#FF0000; fill:#FFFFFF", points (toPointString resource minX maxX minY maxY) ] []

toSvgPolygonColoredByCount resource minX maxX minY maxY fillColorString =
  polygon [fillOpacity "0.4", style ("stroke:#FF0000; fill:" ++ fillColorString), points (toPointString resource minX maxX minY maxY) ] []

getXCoordValue maybeCoord = 
  case maybeCoord of
    Nothing ->
      0
    Just coord ->
      coord.x

getMinXCoord resource = 
  getXCoordValue (List.head (List.sortBy .x resource.coords))

getMaxXCoord resource = 
  getXCoordValue (List.head (List.reverse (List.sortBy .x resource.coords)))

getMaxXCoords resources = 
  List.map (\r -> getMaxXCoord r) resources

getMinXCoords resources = 
  List.map (\r -> getMinXCoord r) resources

getYCoordValue maybeCoord = 
  case maybeCoord of
    Nothing ->
      0
    Just coord ->
      coord.y

getMinYCoord resource = 
  getYCoordValue (List.head (List.sortBy .y resource.coords))

getMaxYCoord resource = 
  getYCoordValue (List.head (List.reverse (List.sortBy .y resource.coords)))

getMaxYCoords resources = 
  List.map (\r -> getMaxYCoord r) resources

getMinYCoords resources = 
  List.map (\r -> getMinYCoord r) resources

getBoundValue maybeValue =
  case maybeValue of
    Nothing ->
      0
    Just value ->
      value

getXBounds resources = 
  let
    maxX = getBoundValue (List.head (List.reverse (List.sort (getMaxXCoords resources))))
    minX = getBoundValue (List.head (List.sort (getMinXCoords resources)))
  in
  (minX, maxX)

getYBounds resources = 
  let
    maxY = getBoundValue( List.head (List.reverse (List.sort (getMaxYCoords resources))))
    minY = getBoundValue( List.head (List.sort (getMinYCoords resources)))
  in
  (minY, maxY)

toSvgPolygons resources =
  let 
    (minX,maxX) = getXBounds resources
    (minY,maxY) = getYBounds resources
  in
  List.map (\r -> (toSvgPolygon r minX maxX minY maxY)) resources

getCountValue maybeValue = 
   case maybeValue of 
     Nothing ->
       0
     Just value ->
       value

getMaxCountForResource resourceCount =
  getCountValue (List.head (List.reverse (List.sort (List.map (\c -> c.count) resourceCount.counts))))

getMaxCount resourceCounts =
  getCountValue (List.head (List.reverse (List.sort (List.map (\r -> getMaxCountForResource r) resourceCounts))))

getResourceCountsValue maybeResourceCounts = 
  case maybeResourceCounts of
    Nothing ->
      0
    Just resourceCounts ->
      getMaxCountForResource resourceCounts

getMaxCountValueForResource : Resource -> ResourceCounts -> Int
getMaxCountValueForResource resource resourceCounts = 
  getResourceCountsValue (List.head (List.filter (\r -> r.id == resource.id) resourceCounts))

getColorForResource resource resourceCounts =
  let
    maxCount = getMaxCount resourceCounts
    count = getMaxCountValueForResource resource resourceCounts
  in
    if (toFloat(count) / toFloat(maxCount) > 0.8) then
      "#0000FF"
    else
      if (toFloat(count) / toFloat(maxCount) > 0.5) then
        "#FF00FF"
      else
        "#FFFFFF"
    
-- sort by count

toSvgPolygonsColoredByCount resources resourceCounts =
  let 
    (minX,maxX) = getXBounds resources
    (minY,maxY) = getYBounds resources
  in
  List.map (\r -> (toSvgPolygonColoredByCount r minX maxX minY maxY (getColorForResource r resourceCounts))) resources

toPolygons maybeResources = 
  [
    polygon [ fill "#60B5CC", points "23.298,143.724 23.298,0 179.573,0"][]
  ]


toSvgPolygonsOrNothing model =
  case model.resources of
    Nothing ->
      []
    Just resources ->
      case model.resourceCounts of
        Nothing ->
          (toSvgPolygons resources)
        Just resourceCounts ->
          (toSvgPolygonsColoredByCount resources resourceCounts)

getMapDisplayWidth = 
  600

getMapDisplayHeight = 
  600

getMapDisplayViewBox =
  "0 0 " ++ (toString getMapDisplayWidth) ++ " " ++ (toString getMapDisplayHeight)

-- leverage GeoUtils function
toXYPoint p1 p2 =
  let 
    (deast,dnorth) = getEastNorthOffsetNmiBetween p1 p2
  in
    {x = deast,
     y = dnorth
    }

toWestLongitude g =
  {latDeg = g.latDeg,
   lonDeg = -g.lonDeg
  }

toPolyline route minX maxX minY maxY =
  let
    refPoint = {latDeg=33.637, lonDeg=84.2567}
    xypoints = List.map (\g -> toXYPoint refPoint (toWestLongitude g)) route.points 
    pointString = String.concat (List.map (\p -> ((toDisplayX p.x minX maxX) ++ "," ++ (toDisplayY p.y minY maxY) ++ " ")) xypoints)
  in
    polyline [points pointString, stroke "blue", fill "none"][]


getMaxRouteX route =
  let
    xvals = List.map (\p -> p.x) route.points
    xmax = List.maximum xvals
  in
    case xmax of 
      Nothing ->
        0
      Just xmax ->
        xmax
    

getMinRouteX route =
  let
    xvals = List.map (\p -> p.x) route.points
    xmin = List.minimum xvals
  in
    case xmin of 
      Nothing ->
        0
      Just xmin ->
        xmin



getRouteXBounds routes = 
  let 
    maxX = List.maximum (List.map (\r -> getMaxRouteX r) routes)
    minX = List.minimum (List.map (\r -> getMinRouteX r) routes)
  in
    ((getValueForBound minX), (getValueForBound maxX)) 

getValueForBound maybeValue =
   case maybeValue of
     Nothing ->
       0
     Just value ->
       value

getRouteYBounds routes = 
  let 
    maxY = List.maximum (List.map (\r -> getMaxRouteY r) routes)
    minY = List.minimum (List.map (\r -> getMinRouteY r) routes)
  in
    ((getValueForBound minY), (getValueForBound maxY))

getMaxRouteY route =
  let
    yvals = List.map (\p -> p.y) route.points
    ymax = List.maximum yvals
  in
    case ymax of
      Nothing -> 
        0
      Just ymax ->
        ymax

getMinRouteY route =
  let
    yvals = List.map (\p -> p.y) route.points
    ymin = List.minimum yvals
  in
    case ymin of
      Nothing -> 
        0
      Just ymin ->
        ymin

toPolylines model =
  case model.routes of
    Nothing ->
      [Svg.line [x1 "-200", x2 "0", y1 "50", y2 "25", stroke "black"][]]
    Just routes ->
      case model.resources of
        Nothing ->
          let 
            (minX,maxX) = (-1000,1000)
            (minY,maxY) = (-1000,1000)
          in
            List.map (\r -> toPolyline r minX maxX minY maxY) routes  
        Just resources ->
          let
            (minX,maxX) = getXBounds resources
            (minY,maxY) = getYBounds resources
          in
            List.map (\r -> toPolyline r minX maxX minY maxY) routes  

displayRoutes model =
  Svg.svg [ width (toString getMapDisplayWidth), 
            height (toString getMapDisplayHeight), 
            viewBox getMapDisplayViewBox, 
            fill "#333333", style "margin-left:auto; margin-right:auto; display:block;" ]
    [g [ transform (("translate(" ++ (toString (getMapDisplayWidth/2)) ++ ", " ++ (toString (getMapDisplayHeight/2)) ++ ")")) ]
    ((Svg.rect [x ("-" ++ (toString (getMapDisplayWidth/2))), y ("-" ++ (toString (getMapDisplayHeight/2))), width (toString getMapDisplayWidth), height (toString getMapDisplayHeight), style "fill:#FFFFFF;stroke:#222222"][]) ::
    (toPolylines model))
    ]

displayResources model = 
  Svg.svg [ width (toString getMapDisplayWidth), 
            height (toString getMapDisplayHeight), 
            viewBox getMapDisplayViewBox, 
            fill "#333333", style "margin-left:auto; margin-right:auto; display:block;" ]
    [g [ transform (("translate(" ++ (toString (getMapDisplayWidth/2)) ++ ", " ++ (toString (getMapDisplayHeight/2)) ++ ")")) ]
    ((Svg.rect [x ("-" ++ (toString (getMapDisplayWidth/2))), y ("-" ++ (toString (getMapDisplayHeight/2))), width (toString getMapDisplayWidth), height (toString getMapDisplayHeight), style "fill:#FFFFFF;stroke:#222222"][]) ::
    (toSvgPolygonsOrNothing model))
    ]

viewCoords coords = 
  List.map (\c -> toString c.x) coords

viewRoutes maybeRoutes =
  case maybeRoutes of
    Nothing ->
      div [] [ text "No routes to display.  Try clicking the button" ]
    Just routes ->
      div [] [ text ("Got " ++ (toString (List.length routes)) ++ " routes, baby!") ]
    
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


getRoutes : Effects Action
getRoutes =
  Http.get routeCollectionDecoder routesUrl
    |> toMaybeWithLogging
    |> Task.map ShowRoutes
    |> Effects.task

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

type alias Routes = List Route

type alias Route = {
  id : String
 ,points: List GeoPoint2D
}
 
routeCollectionDecoder : Decoder Routes
routeCollectionDecoder = 
  Decode.object1 identity
    ("routes" := Decode.list routeDecoder)

routeDecoder : Decoder Route
routeDecoder =
  Decode.object2 Route
    ("id" := Decode.string)
    ("points" := Decode.list geoPointDecoder)

geoPointDecoder : Decoder GeoPoint2D
geoPointDecoder = 
  Decode.object2 GeoPoint2D
    ("latDeg" := Decode.float)
    ("lonDeg" := Decode.float)
