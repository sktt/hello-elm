import Color exposing (Color)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window
import Mouse
type alias Pos = (Float, Float)

type alias Triangle =
  { coords : List Pos
  , color : Color
  , width : Float
  }

initial : Float -> Triangle
initial n =
  { coords = [(0, -n), (n, 0), (-n, 0)]
  , color = rgb' <| List.map (\n -> n*100) [-n, n, -n]
  , width = 1/100
  }

rgb' : List Int -> Color
rgb' colors =
  case colors of
    r::g::b::[] -> Color.rgb r g b
    _ -> Color.rgb 0 0 0

next : Triangle -> Float -> Triangle
next tri width =
  { coords = List.map (\(x,y) -> (x*2, -y*2 - 1) ) tri.coords
  , color = rgb' <| List.map (\(x, y) -> round ((y+x) * 100) % 255) tri.coords
  , width = 1/width
  }

unfoldTriangles : List Triangle -> Int -> Float -> List Triangle
unfoldTriangles tris n s =
  if n > 0 then
    case tris of
      hd::tl -> unfoldTriangles ((next hd s)::hd::tl) (n-1) s
      [] -> []
  else
    tris

main : Signal Element
main =
  Signal.map3 draw Window.dimensions Mouse.y Mouse.x


draw : (Int, Int) -> Int -> Int -> Element
draw (w, h) s r =
  let
    -- scale
    s' = max (10^(-5)) <| (toFloat (s^4)) / 10^9
    -- width
    w' = toFloat w
    -- height
    h' = toFloat h
    -- rotation
    r' = toFloat r
    bg = move (0,0) <| filled (Color.rgb 0 0 0) <| rect w' h'
    triangles = (unfoldTriangles [initial 1] 30 s'
      |> List.map (makeForm
        >> move (0,0)
        >> scale s'
        >> rotate (degrees r')
      )
    )
  in
    collage w h (bg::triangles)


makeForm : Triangle -> Form
makeForm tri =
  let
    points = case tri.coords of
      hd::tl -> hd::tl ++ [hd]
      [] -> []

    style = solid tri.color
  in
    traced ({style | width = tri.width}) (path points)

