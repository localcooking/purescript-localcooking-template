module LocalCooking.Spec.Flags.Colorado where

import Prelude (show)
import React as R
import React.DOM as RD
import React.DOM.SVG as R
import React.DOM.Props as RP


coloradoFlagViewBox :: String
coloradoFlagViewBox = "0 0 1800 1200"


coloradoFlag :: Array R.ReactElement
coloradoFlag =
    [ R.rect [RP.width "1800", RP.height "1200", RP.fill "#102e82"] []
    , R.rect [RP.width "1800", RP.height "400", RP.y 400, RP.fill "#ffffff"] []
    , R.path [RP.d "M1130.81,750A400,400 0 1,1 1130.81,450L760,600Z", RP.fill "#BF2C34"] []
    , R.circle [RP.cx 760, RP.cy 600, RP.r 200, RP.fill "#F6D047"] []
    ]
