module ResourceDecoder where
import Http
import String as String
import StartApp
import Effects exposing (Effects,Never)
import Task
import Html exposing (..)
import Html.Events as E exposing (..)
import Html.Attributes as HA exposing (..)
import Json.Decode as Decode exposing (Decoder, (:=))

import Svg exposing (svg, rect, polygon, circle, polyline, g)
import Svg.Attributes as SA exposing (..)
import Array
import Time exposing (..)
import Date exposing (..)
import GeoUtils exposing (..)

resourcesUrl = "./resources2.json"
resourceCountsUrl = "./resource-counts.json"
routesUrl = "./routes1.json"
trajectoryUrl = "./trajectory1.json"
crossingTimeUrl = "./crossingTimeSummary1.json"

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
  | UpdateCurrentTime (String)
  | GetCrossingTimeSummary
  | GetTrajectory
  | GetRoutes
  | ShiftTimeForward (Int)
  | ShiftTimeBackward (Int)
  | GetResources
  | GetResourceCounts
  | ShowResources (Maybe Resources)
  | ShowResourceCounts (Maybe ResourceCounts)
  | ShowRoutes (Maybe Routes)
  | ShowTrajectory (Maybe Trajectory)
  | ShowCrossingTimeSummary (Maybe CrossingTimeSummary)

type alias Model =
  { resources : Maybe Resources
   ,resourceCounts : Maybe ResourceCounts
   ,routes : Maybe Routes
   ,trajectory : Maybe Trajectory 
   ,crossingTimeSummary : Maybe CrossingTimeSummary
   ,currentTime : Int
   ,currentSlider : String
   ,minTimestamp : Time
   ,maxTimestamp : Time
   ,currentTimestamp : Time
  }

init =
  ({ resources = Nothing
    ,resourceCounts = Nothing
    ,routes = Nothing
    ,trajectory = Nothing
    ,crossingTimeSummary = Nothing
    ,currentTime = 0
    ,currentSlider = "0"
    ,minTimestamp = 0
    ,maxTimestamp = 6*60*60*1000 
    ,currentTimestamp = 0 }, Effects.none)

getFirstTimestamp trajectory = 
  case (List.head trajectory) of
    Nothing ->
      0
    Just trajectoryPoint ->
      (toFloat trajectoryPoint.timestamp)

getMinTimestamp maybeTrajectory = 
  case maybeTrajectory of 
    Nothing ->
      0
    Just trajectory ->
      getFirstTimestamp trajectory

getMaxTimestamp maybeTrajectory = 
  case maybeTrajectory of 
    Nothing ->
      6*60*60*1000
    Just trajectory ->
      getLastTimestamp trajectory

getLastTimestamp trajectory =
  let 
    reversed = List.reverse trajectory
  in
    case (List.head reversed) of
      Nothing ->
        6*60*60*1000
      Just trajectoryPoint ->
        (toFloat trajectoryPoint.timestamp)


   
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    GetCrossingTimeSummary ->
      ({ model | crossingTimeSummary = Nothing }, getCrossingTimeSummary)
    GetTrajectory ->
      ({ model | trajectory = Nothing }, getTrajectory)
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
    ShowTrajectory maybeTrajectory ->
      ({ model | 
           trajectory = maybeTrajectory,
           minTimestamp = (getMinTimestamp maybeTrajectory), 
           maxTimestamp = (getMaxTimestamp maybeTrajectory) 
       }, Effects.none)
    ShowCrossingTimeSummary maybeCrossingTimeSummary ->
      ({ model | crossingTimeSummary = maybeCrossingTimeSummary }, Effects.none)
    ShiftTimeForward timeNow ->
      ({ model | currentTime = timeNow}, Effects.none)
    ShiftTimeBackward timeNow ->
      ({ model | currentTime = timeNow}, Effects.none)
    UpdateCurrentTime value ->
      ({ model | currentSlider = value}, Effects.none)
 
createLabelForResourceCount resourceCount =
  Svg.text' [x (toString (getDisplayTimeOrigin-400))
            ,y "-80"]
            [text resourceCount.id]

createBorder =
  (Svg.rect [x ("-" ++ (toString getDisplayTimeOrigin))
                   ,y "-100"
                   , SA.width "400"
                   , SA.height "200"
                   , SA.style "fill:#FFFFFF;stroke:#222222"][])

createCountSvg resourceCount refTime = 
      Svg.svg [ SA.width "400"
              , SA.height "200"
              , SA.viewBox "0 0 400 200"
              , SA.style "margin-left:auto; margin-right:auto; display:block;"
              ]
      [g [ transform (("translate(" ++ (toString getDisplayTimeOrigin) ++", 100)"))]
      (List.append 
        [(createBorder)
        ,(createLabelForResourceCount resourceCount)
        ,(createSparkLineForResourceCount resourceCount refTime)
        ]  
        createTicks)
      ]

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
toSvgs maybeResourceCounts refTime =
  case maybeResourceCounts of
    Nothing ->
      []
    Just resourceCounts ->
      List.map (\r -> createCountSvg r refTime) resourceCounts

view address model =
  let 
    evth = E.on "input" E.targetValue (\v -> (Signal.message address (UpdateCurrentTime v)))
  in
  div []
    (List.append 
    [ button [ onClick address GetResources ] [ text "Click to get resources!" ]
    , button [ onClick address GetResourceCounts ] [ text "Click to get resource counts!" ]
    , button [ onClick address GetRoutes ] [ text "Click to get routes!" ]
    , button [ onClick address GetTrajectory ] [ text "Click for trajectory!" ]
    , button [ onClick address GetCrossingTimeSummary ] [ text "Click for crossing time summary!" ]
    , br [][]
    , displayResources model
    , displayRoutes model
    , viewResourceCounts model.resourceCounts
    , viewRoutes model.routes
    , viewTrajectory model.trajectory
    , viewCrossingTimeSummary model.crossingTimeSummary
    , button [ onClick address (ShiftTimeForward (model.currentTime + 15)) ] [ text "+15" ]
    , button [ onClick address (ShiftTimeBackward (model.currentTime - 15)) ] [ text "-15" ]
    , text ("current Time: " ++ (toString model.currentTime))
    , input [ HA.type' "range"
            , HA.min "0"
            , HA.max "100"
            , HA.value model.currentSlider
            , evth
            ][]
    , input [ HA.type' "text"
            , HA.value (toDateTimeString model.minTimestamp model.maxTimestamp model.currentSlider)
            ][]
    ] (List.intersperse (br [][]) (toSvgs model.resourceCounts model.currentTime)))


convertToFloat rawString =
  case String.toFloat rawString of
    Err message ->
      0.0
    Ok n ->
      n
  
toTwoDigitString value =
  if (value < 10) then
    (toString (0) ++ (toString value))
  else
    toString value

toLocalTime : Time -> Time -> String -> Int
toLocalTime minTime maxTime currentSliderValue = 
  let 
    value = convertToFloat currentSliderValue
  in
    floor (minTime + value*(maxTime-minTime)/100.0)

toDateTimeString minTime maxTime currentValue =
  let 
    value = convertToFloat currentValue
    localTime = minTime + value*(maxTime-minTime)/100.0
-- account for local timezone
    gmtTime = localTime + 6*60*60*1000
    date' = fromTime gmtTime
    hour' = toTwoDigitString (Date.hour date')
    minute' = toTwoDigitString (Date.minute date')
    second' = toTwoDigitString (Date.second date')
    year' = toString (year date')
  in
    (hour' ++ ":" ++ minute' ++ ":" ++ second')

getTickHeight x =
  if ((x%60) == 0) then
    "4"
  else 
    "2"

createTicks = 
  List.append (List.map (\x -> Svg.line [ x1 (toString x)
                                        , x2 (toString x)
                                        , y1 ("-" ++ (getTickHeight x))
                                        , y2 (getTickHeight x)
                                        , stroke "gray"][]) 
                                        (List.reverse(List.map (\n -> (-n*15)) [-12..12]))) createTickLabels

createTickLabels = 
  (List.map (\n -> Svg.text' [ x (toDisplayTime n)
                             , y "20"
                             , fill "gray"
                             , textAnchor "middle"
                             , alignmentBaseline "middle"
                             ]
                             [text (toString n)]) [0,60,120,180]) 

toDisplayTime t = 
  (toString (t - getTimeOrigin))

toRelativeDisplayTime t refTime = 
  (toString (t - refTime - getTimeOrigin))

toDisplayCount c =
  (toString ((toFloat -c)/4.0*100.0))

toXYPointString : ResourceCount -> Int -> String
toXYPointString resourceCount refTime = 
  let 
    stairsData = stairs resourceCount
  in
    String.concat (List.map (\p -> 
      ( (toRelativeDisplayTime p.t refTime) ++ "," ++ (toDisplayCount p.count) ++ " ")) stairsData)

createSparkLineForResource maybeResourceCount = 
  case maybeResourceCount of
    Nothing ->
      Svg.line [x1 "-200", x2 "0", y1 "50", y2 "25", stroke "black"][]
    Just resourceCount ->
      polyline [points (toXYPointString resourceCount 0), stroke "blue", fill "none"][]


createSparkLineForResourceCount resourceCount refTime = 
  polyline [points (toXYPointString resourceCount refTime), stroke "blue", fill "none"][]

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
      List.map (\c -> ((toString c.latDeg) ++ "," ++ (toString c.lonDeg))) resource.coords


getXScaleFactor minX maxX = 
  getMapDisplayWidth / (2.0*(maxX-minX))

getYScaleFactor minY maxY = 
  getMapDisplayHeight / (2.0*(maxY-minY))

toDisplayX cx minX maxX =
-- 600nmi --> 800pixels wide
  toString (cx*(getXScaleFactor minX maxX))

toDisplayY cy minY maxY  = 
  toString (-cy*(getYScaleFactor minY maxY))

toDisplayXF cx minX maxX =
-- 600nmi --> 800pixels wide
  (cx*(getXScaleFactor minX maxX))

toDisplayYF cy minY maxY  = 
  (-cy*(getYScaleFactor minY maxY))

 
-- convert single resource coords to SVG polygon

toPointString resource minX maxX minY maxY =
  let
    xypoints = toXYPoints resource.coords
  in
    String.concat (List.map (\p -> ((toDisplayX p.x minX maxX) ++ "," ++ (toDisplayY p.y minY maxY) ++ " ")) xypoints)

toSvgPolygon resource minX maxX minY maxY =
  polygon [fillOpacity "0.4", SA.style "stroke:#FF0000; fill:#FFFFFF", points (toPointString resource minX maxX minY maxY) ] []

toSvgPolygonColoredByCount resource minX maxX minY maxY fillColorString =
  let
    xypoints = toXYPoints resource.coords 
    pointString = String.concat (List.map (\p -> ((toDisplayX p.x minX maxX) ++ "," ++ (toDisplayY p.y minY maxY) ++ " ")) xypoints)
  in
    polygon [fillOpacity "0.4", SA.style ("stroke:#FF0000; fill:" ++ fillColorString), points pointString] []


getBoundValue maybeValue =
  case maybeValue of
    Nothing ->
      0
    Just value ->
      value

getAllXYPoints resources = 
  List.concat (List.map (\r -> toXYPoints r.coords) resources)

getXBounds resources = 
  let
    xypoints = getAllXYPoints resources
  in
    findXBounds xypoints


getYBounds resources = 
  let
    xypoints = getAllXYPoints resources
  in
    findYBounds xypoints

toSvgPolygons resources =
  let 
    (minX,maxX) = getXBounds resources
    (minY,maxY) = getYBounds resources
  in
  List.map (\r -> (toSvgPolygon r minX maxX minY maxY)) resources

toSvgPolygonsColoredOnEntry resources currentTime maybeCrossingTimeSummary =
  case maybeCrossingTimeSummary of
    Nothing ->
      toSvgPolygons resources
    Just crossingTimeSummary ->
      let 
        (minX,maxX) = getXBounds resources
        (minY,maxY) = getYBounds resources
      in
        List.map (\r -> (toSvgPolygonColoredOnEntry r minX maxX minY maxY currentTime crossingTimeSummary)) resources

determineColor resource currentTime crossingTimeSummary =
  let 
    isInSector = List.foldl (\c tf -> tf || (c.resourceID == resource.id && c.entryTimestamp < currentTime && c.exitTimestamp > currentTime)) False crossingTimeSummary
  in
    if (isInSector) then
      "FF0000"
    else
      "FFFFFF"

toSvgPolygonColoredOnEntry resource minX maxX minY maxY currentTime crossingTimeSummary =
  let
    fillColor = determineColor resource currentTime crossingTimeSummary
  in
    polygon [fillOpacity "0.4", SA.style ("stroke:#FF0000; fill:#" ++ fillColor), points (toPointString resource minX maxX minY maxY) ] []


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

findXBounds xypoints =
  let
    xvals = List.map (\p -> p.x) xypoints
    maxX = List.maximum xvals
    minX = List.minimum xvals
  in
   (getBoundValue minX,getBoundValue maxX)  

findYBounds xypoints =
  let
    yvals = List.map (\p -> p.y) xypoints
    maxY = List.maximum yvals
    minY = List.minimum yvals
  in
   ((getBoundValue minY),(getBoundValue maxY))  


toSvgPolygonsColoredByCount resources resourceCounts =
  let 
    xypoints = getAllXYPoints resources
    (minX,maxX) = findXBounds xypoints
    (minY,maxY) = findYBounds xypoints
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

getRefPoint =
  {latDeg=33.637, lonDeg=84.2567}

-- leverage GeoUtils function
toXYPoints coordList =
  List.map (\g -> toXYPoint g) coordList

toXYPoint p =
  let 
    refPoint = getRefPoint
    (deast,dnorth) = getEastNorthOffsetNmiBetween refPoint (toWestLongitude p)
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
    xypoints = toXYPoints route.points 
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
            currentTime = toLocalTime model.minTimestamp model.maxTimestamp model.currentSlider
            routeLines = List.map (\r -> toPolyline r minX maxX minY maxY) routes  
            sectorPolygons = toSvgPolygonsColoredOnEntry resources currentTime model.crossingTimeSummary
            trajectoryLine = trajectoryToPolyline model.trajectory minX maxX minY maxY
            trajectoryWaypoints = trajectoryToCircles model.trajectory minX maxX minY maxY
            currentPosn = interpolatePosn model.trajectory currentTime
            currentXY = toXYPoint (toGeoPoint currentPosn)
            displayX = toString ((toDisplayXF currentXY.x minX maxX)-2)
            displayY = toString ((toDisplayYF currentXY.y minY maxY)-2)
            posnIcon = Svg.rect [x displayX, y displayY, SA.width "4", SA.height "4", SA.style "fill:#0000FF;stroke:#0000FF"][]
            dataTag = Svg.text' [x displayX, y displayY, SA.style "stroke:#0000FF"][text (toString (floor currentPosn.altFeet))]
          in
            List.append  (List.append routeLines sectorPolygons)(List.append [trajectoryLine, posnIcon, dataTag] trajectoryWaypoints)

getPointBefore earlierPoints trajectory = 
  if (List.isEmpty earlierPoints) then
    List.head trajectory
  else
    List.head (List.drop ((List.length earlierPoints)-1) earlierPoints)

getPointAfter laterPoints trajectory =
  if (List.isEmpty laterPoints) then
    List.head trajectory
  else
    List.head laterPoints

interpolatePosn : Maybe Trajectory -> Int -> TrajectoryPoint
interpolatePosn maybeTrajectory currentTime = 
  case maybeTrajectory of
    Nothing ->
      { timestamp = 0
      , latDeg = 0.0
      , lonDeg = 0.0
      , altFeet = 0.0
      }
    Just trajectory ->
      let 
        earlierPoints = List.filter (\p -> p.timestamp < currentTime) trajectory
        laterPoints = List.filter (\p -> p.timestamp >= currentTime) trajectory
        prevPoint = getPointBefore earlierPoints trajectory
        nextPoint = getPointAfter laterPoints trajectory
      in
        case prevPoint of
          Nothing ->
           { timestamp = 0
           , latDeg = 0.0
           , lonDeg = 0.0
           , altFeet = 0.0
           }
          Just trajectoryPoint ->
            case nextPoint of 
              Nothing ->
               { timestamp = 0
               , latDeg = 0.0
               , lonDeg = 0.0
               , altFeet = 0.0
               }
              Just anotherTrajectoryPoint ->
                let
                  dt' = currentTime - trajectoryPoint.timestamp
                  dt = anotherTrajectoryPoint.timestamp - trajectoryPoint.timestamp
                  dlat = anotherTrajectoryPoint.latDeg - trajectoryPoint.latDeg
                  dlon = anotherTrajectoryPoint.lonDeg - trajectoryPoint.lonDeg
                  daltFeet = anotherTrajectoryPoint.altFeet - trajectoryPoint.altFeet
                  lat' = trajectoryPoint.latDeg + dlat/(toFloat dt)*(toFloat dt')
                  lon' = trajectoryPoint.lonDeg + dlon/(toFloat dt)*(toFloat dt')
                  alt' = trajectoryPoint.altFeet + daltFeet/(toFloat dt)*(toFloat dt')
                in
                  if (dt /= 0) then
                    { timestamp = currentTime
                    , latDeg = lat'
                    , lonDeg = lon'
                    , altFeet = alt'
                    }
                  else 
                    { timestamp = trajectoryPoint.timestamp
                    , latDeg = trajectoryPoint.latDeg
                    , lonDeg = trajectoryPoint.lonDeg
                    , altFeet = trajectoryPoint.altFeet
                    }
 


trajectoryToCircles maybeTrajectory minX maxX minY maxY = 
  case maybeTrajectory of
    Nothing ->
      [Svg.rect [x "20", y "20", SA.width "4", SA.height "4", SA.style "fill:#FFFFFF;stroke:#222222"][]
      ]
    Just trajectory ->
      let 
        xypoints = List.map (\p -> toXYPoint (toGeoPoint p)) trajectory
      in
        List.map (\p -> Svg.rect [x (toString ((toDisplayXF p.x minX maxX)-2)), y (toString ((toDisplayYF p.y minY maxY)-2)), SA.width "4", SA.height "4", SA.style "fill:#00FF00;stroke:#002200"][]) xypoints
      



trajectoryToPolyline maybeTrajectory minX maxX minY maxY = 
  case maybeTrajectory of
    Nothing ->
      polyline [points "0,0 100,0", stroke "red", fill "none"][]
    Just trajectory ->
      let
        xypoints = trajectoryToXYPoints trajectory
        pointString = String.concat (List.map (\p -> ((toDisplayX p.x minX maxX) ++ "," ++ (toDisplayY p.y minY maxY) ++ " ")) xypoints)
      in
        polyline [points pointString, stroke "orange", fill "none"][]

trajectoryToXYPoints trajectory =
  let
    gpoints = List.map (\p -> toGeoPoint p) trajectory
  in
    List.map (\g -> toXYPoint g) gpoints

toGeoPoint trajectoryPoint = 
  { 
    latDeg = trajectoryPoint.latDeg
   ,lonDeg = trajectoryPoint.lonDeg
  }

displayRoutes model =
  Svg.svg [ SA.width (toString getMapDisplayWidth), 
            SA.height (toString getMapDisplayHeight), 
            SA.viewBox getMapDisplayViewBox, 
            SA.fill "#333333", SA.style "margin-left:auto; margin-right:auto; display:block;" ]
    [g [ transform (("translate(" ++ (toString (getMapDisplayWidth/2)) ++ ", " ++ (toString (getMapDisplayHeight/2)) ++ ")")) ]
    ((Svg.rect [x ("-" ++ (toString (getMapDisplayWidth/2))), y ("-" ++ (toString (getMapDisplayHeight/2))), SA.width (toString getMapDisplayWidth), SA.height (toString getMapDisplayHeight), SA.style "fill:#FFFFFF;stroke:#222222"][]) ::
    (toPolylines model))
    ]

displayResources model = 
  Svg.svg [ SA.width (toString getMapDisplayWidth), 
            SA.height (toString getMapDisplayHeight), 
            SA.viewBox getMapDisplayViewBox, 
            SA.fill "#333333", SA.style "margin-left:auto; margin-right:auto; display:block;" ]
    [g [ transform (("translate(" ++ (toString (getMapDisplayWidth/2)) ++ ", " ++ (toString (getMapDisplayHeight/2)) ++ ")")) ]
    ((Svg.rect [x ("-" ++ (toString (getMapDisplayWidth/2))), y ("-" ++ (toString (getMapDisplayHeight/2))), SA.width (toString getMapDisplayWidth), SA.height (toString getMapDisplayHeight), SA.style "fill:#FFFFFF;stroke:#222222"][]) ::
    (toSvgPolygonsOrNothing model))
    ]

viewCrossingTimeSummary maybeCrossingTimeSummary =
  case maybeCrossingTimeSummary of
    Nothing ->
      div [] [ text "No crossing time summary to display.  Try clicking the button" ]
    Just crossingTimeSummary ->
      div [] [ text ("Found " ++ (toString (List.length crossingTimeSummary)) ++ " crossings in summary, baby!") ]

viewTrajectory maybeTrajectory =
  case maybeTrajectory of
    Nothing ->
      div [] [ text "No trajectory to display.  Try clicking the button" ]
    Just trajectory ->
      div [] [ text ("Found " ++ (toString (List.length trajectory)) ++ " points in trajectory, baby!") ]
      
viewCoords coords = 
  List.map (\c -> toString c.latDeg) coords

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
      text ("latDeg: " ++ (toString coord.latDeg) ++ ", lonDeg: " ++ (toString coord.lonDeg))
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
getTrajectory : Effects Action
getTrajectory = 
  Http.get trajectoryDecoder trajectoryUrl
    |> toMaybeWithLogging
    |> Task.map ShowTrajectory
    |> Effects.task
    
getCrossingTimeSummary : Effects Action
getCrossingTimeSummary =
  Http.get crossingTimeSummaryDecoder crossingTimeUrl
    |> toMaybeWithLogging
    |> Task.map ShowCrossingTimeSummary
    |> Effects.task

getResources : Effects Action
getResources =
  Http.get decoderColl resourcesUrl
    |> toMaybeWithLogging
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
  , coords : List GeoPoint2D
}

type alias Resources  = List Resource

-- The updated Resource decoder
decoder : Decoder Resource
decoder =
  Decode.object2 Resource
    ("id" := Decode.string)
    ("coords" := Decode.list geoPointDecoder)

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

type alias Trajectory = List TrajectoryPoint
type alias TrajectoryPoint = {
   timestamp: Int
  ,latDeg: Float
  ,lonDeg: Float
  ,altFeet: Float
}

trajectoryDecoder : Decoder Trajectory
trajectoryDecoder = 
  Decode.object1 identity
    ("trajectory" := Decode.list trajectoryPointDecoder)

trajectoryPointDecoder : Decoder TrajectoryPoint
trajectoryPointDecoder = 
  Decode.object4 TrajectoryPoint
    ("timestamp" := Decode.int)
    ("latDeg" := Decode.float)
    ("lonDeg" := Decode.float)
    ("altFeet" := Decode.float)

type alias CrossingTimeSummary = List CrossingTime

type alias CrossingTime = {
   resourceID: String
  ,entryTimestamp: Int
  ,exitTimestamp: Int
}

crossingTimeSummaryDecoder : Decoder CrossingTimeSummary
crossingTimeSummaryDecoder = 
  Decode.object1 identity
    ("crossingTimeSummary" := Decode.list crossingTimeDecoder)

crossingTimeDecoder : Decoder CrossingTime
crossingTimeDecoder = 
  Decode.object3 CrossingTime
    ("resourceID" := Decode.string)
    ("entryTimestamp" := Decode.int)
    ("exitTimestamp" := Decode.int)  
