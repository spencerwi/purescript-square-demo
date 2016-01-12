module Main where

import Prelude
import Control.Monad.Eff
import DOM
import Signal
import Signal.DOM
import Signal.Time (every)
import Data.Tuple
import Data.Foldable (foldl)
import Text.Smolder.Markup
import qualified Text.Smolder.HTML as H
import qualified Text.Smolder.HTML.Attributes as A
import Text.Smolder.Renderer.String (render)

data Color = White | Red | Blue | Green | Cyan
type PlayerState = { x :: Int, y :: Int, color :: Color }
type MouseInput = { x :: Int, y :: Int, isClicked :: Boolean }

frameRate :: Signal Number
frameRate = every 60.0

initialPlayerOne :: PlayerState
initialPlayerOne = { x: 0, y: 0, color: White }

updatePlayer :: MouseInput -> PlayerState -> PlayerState
updatePlayer ({x: newX, y: newY, isClicked: clicked}) ({color: currentColor}) =
    { x: newX, y: newY, color: newColor }
        where
            newColor = updateColor clicked currentColor
            updateColor :: Boolean -> Color -> Color
            updateColor false c     = c
            updateColor true  White = Red
            updateColor true  Red   = Blue
            updateColor true  Blue  = Green
            updateColor true  Green = Cyan
            updateColor true  Cyan  = White

renderColor :: Color -> String
renderColor White = "#fff"
renderColor Red   = "#f00"
renderColor Blue  = "#00f"
renderColor Green = "#0f0"
renderColor Cyan  = "#77f"

renderPlayer ::  PlayerState -> Markup
renderPlayer ({x: currentX, y: currentY, color: currentColor}) =
    H.div ! (A.style currentStyle) ! (A.id "playerOne") $ do text ""
    where
        currentStyle = foldl (++) "" [
            (" background-color: " ++ (renderColor currentColor)) ++ "; ",
            (" top: "  ++ (show currentY) ++ "px; "),
            (" left: " ++ (show currentX) ++ "px; "),
            (" position: absolute; ")
        ]

-- asBody replaces the contents of the <body> with the given HTML.
-- There's probably a faster rendering method.
foreign import asBody :: forall eff. String -> Eff (dom:: DOM | eff) Unit

playerOne :: Signal MouseInput -> Signal PlayerState
playerOne mouseInputSignal = foldp updatePlayer initialPlayerOne (sampleOn frameRate mouseInputSignal)

main = do
    -- mousePos and mouseButton both wrap their signals in a nasty Eff monad
    -- so the value I care about is two layers deep. So we get them here in do-notation-land.
    mousePosSignal <- mousePos
    leftClickSignal <- mouseButton 0
    let mouseSignal = combineMouseSignals <~ mousePosSignal ~ leftClickSignal
    runSignal (playerOne mouseSignal ~> (renderPlayer >>> render >>> asBody))
    where
        combineMouseSignals :: CoordinatePair -> Boolean -> MouseInput
        combineMouseSignals pos clicked = {x: pos.x, y: pos.y, isClicked: clicked}
