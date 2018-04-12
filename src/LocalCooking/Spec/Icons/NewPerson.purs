module LocalCooking.Spec.Icons.NewPerson where



import React as R
import React.DOM.SVG as R
import React.DOM.Props as RP


newPerson :: R.ReactElement
newPerson = R.svg [RP.viewBox "0 0 200 200", RP.width "200", RP.height "200"]
  [ R.defs []
    [ R.linearGradient
      [ RP._id "linearGradient4167"
      , x1 "100"
      , y1 "50"
      , x2 "100"
      , y2 "200"
      ]
      [ R.stop
         [ RP.style {stopColor: "#ff5f52", stopOpacity: 1}
         , offset "0"
         , RP._id "stop4169"
         ] []
      , R.stop
         [ RP.style {stopColor: "#c62828", stopOpacity: 1}
         , offset "0.5"
         , RP._id "stop4175"
         ] []
      , R.stop
         [ RP.style {stopColor: "#8e0000", stopOpacity: 1}
         , offset "1"
         , RP._id "stop4171"
         ] []
      ]
    , R.linearGradient
      [ RP._id "linearGradient4157"
      , x1 "100"
      , y1 "0"
      , x2 "100"
      , y2 "200"
      ]
      [ R.stop
         [ RP.style {stopColor: "#ffe97d", stopOpacity: 1}
         , offset "0"
         , RP._id "stop4159"
         ] []
      , R.stop
         [ RP.style {stopColor: "#ffb74d", stopOpacity: 1}
         , offset "0.5"
         , RP._id "stop4165"
         ] []
      , R.stop
         [ RP.style {stopColor: "#ffb74d", stopOpacity: 1}
         , offset "1"
         , RP._id "stop4161"
         ] []
      ]
    ]
  , R.rect
    [ RP.style {fill: "url(#linearGradient4167)", fillOpacity: 1}
    , RP.height "200"
    , RP.width "200"
    ] []
  , R.path
    [ RP.d dVal
    , RP.style {fill: "url(#linearGradient4157)", fillOpacity: 1}
    ] []
  ]
  where
    x1 = RP.unsafeMkProps "x1"
    x2 = RP.unsafeMkProps "x2"
    y1 = RP.unsafeMkProps "y1"
    y2 = RP.unsafeMkProps "y2"
    offset = RP.unsafeMkProps "offset"


dVal :: String
dVal = "M 99.642578 50.714844 A 33.571434 33.571434 0 0 0 66.072266 84.285156 A 33.571434 33.571434 0 0 0 83.46875 113.69922 A 57.425505 110.28268 0 0 0 44.513672 200.17773 L 157.68359 200.17773 A 57.425505 110.28268 0 0 0 117.24609 112.8418 A 33.571434 33.571434 0 0 0 133.21484 84.285156 A 33.571434 33.571434 0 0 0 99.642578 50.714844 z "
