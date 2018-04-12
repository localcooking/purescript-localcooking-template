module LocalCooking.Spec.Flags.USA where

import Prelude
import React as R
import React.DOM as RD
import React.DOM.SVG as R
import React.DOM.Props as RP


usaFlagViewBox :: String
usaFlagViewBox = "0 0 7410 3900"


usaFlag :: Array R.ReactElement
usaFlag =
    [ R.rect [RP.width "7410", RP.height "3900", RP.fill "#b22234"] []
    , R.path [RP.d "M0,450H7410m0,600H0m0,600H7410m0,600H0m0,600H7410m0,600H0", RP.stroke "#fff", RP.strokeWidth 300] []
    , R.rect [RP.width "2964", RP.height "2100", RP.fill "#3c3b6e"] []
    , R.g [RP.fill "#fff"]
      [ s18 {x: 0}
      , s18 {x: 988}
      , s9 {x: 1976}
      , s5 {x: 2470}
      ]
    ]
  where
    s18 {x} = R.g [translate {x,y: 0}]
      [ s9 {x: 0}
      , s9 {x: 494}
      ]
    s9 {x} = R.g [translate {x,y: 0}]
      [ s5 {x: 0}
      , s4 {x: 247, y: 210}
      ]
    s5 {x} = R.g [translate {x,y: 0}]
      [ s4 {x: 0, y: 0}
      , s {x: 0, y: 1680}
      ]
    s4 {x,y} = R.g [translate {x,y}]
      [ s {x: 0, y: 0}
      , s {x: 0, y: 420}
      , s {x: 0, y: 840}
      , s {x: 0, y: 1260}
      ]
    s {x,y} = R.path [translate {x,y}, RP.d "M247,90 317.534230,307.082039 132.873218,172.917961H361.126782L176.465770,307.082039z"] []
    translate {x,y} = RP.unsafeMkProps "transform" $ "translate(" <> show x <> "," <> show y <> ")"
