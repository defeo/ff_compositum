module LFSR where

import Window
import Text
import Maybe
import Debug
import Time
import Json

{- JS interaction and applet definition -}

port interval : Float
port registers : [(Int,Int)]
port io : [Int]
port transpose : Bool
port animate : Signal Bool

port mulregs : Maybe [(String, String, Int)]
port mulin : String

(style, lfsrcolor, signal) = case transpose of
                               False -> (defaultStyle, black, modsig registers io)
                               True -> (transStyle, rgb 255 0 0, fibsig registers (length io))

main = case mulregs of
         Nothing -> mainLFSR ~ Window.width
         Just regs -> mainMul regs <~ Window.width

mainLFSR : Signal (Int -> Element)
mainLFSR = (\(r, i) w -> (tlfsr . style . toFloat <| w `div` (length registers + length io + 1)) r i)
           <~ signal (keepWhen animate 0 <| every <| interval*second)

mainMul : [(String, String, Int)] -> Int -> Element
mainMul regs w = let width = toFloat <| w `div` (length regs + 2)
                 in multiplier (style width) lfsrcolor regs [mulin]


{- Circuit definitions -}

data NodeType = Copy | Add
type LFSRStyle = 
    { line : LineStyle
    , text : Text.Style
    , size : Float
    , nodesize : Float
    , nodetype : NodeType
    , transpose : Path -> Path
    }
defaultStyle : Float -> LFSRStyle
defaultStyle size = let text = Text.defaultStyle
                    in { line = { defaultLine | width <- size / 40, cap <- Round }
                       , size = size
                       , text = { text | height <- Just (size * 0.7) }
                       , nodesize = size / 6
                       , nodetype = Copy
                       , transpose = \x -> x
                       }
transStyle : Float -> LFSRStyle
transStyle size = let s = defaultStyle size
                  in { s | nodetype <- Add, transpose <- reverse }

tlfsr : LFSRStyle -> [(a,Int)] -> [b] -> Element
tlfsr style regs io = let n = length regs
                          registers = flow right 
                                      (map
                                       (\(r,_) -> (register style <| shownoquot r)) 
                                      regs)
                          iopipe = arrows style.line
                                   <| style.transpose [(style.size/2,0), (0,0)]
                          feedback = group 
                                     [ arrows style.line <| style.transpose 
                                                 [ (-style.size/2,0)
                                                 , (-style.size,0)
                                                 , (-style.size,-style.size)
                                                 ]
                                     , traced style.line 
                                                 [ (-style.size,-style.size)
                                                 , (-style.size+style.nodesize,-style.size)
                                                 ]
                                     ]
                          taps = map ( \((_,t), i) -> 
                                 move (toFloat i * style.size, -style.size) 
                                          <| tap style t )
                                 <| zip regs [0..length regs]
                          size = round style.size
                          input = centered . Text.style style.text 
                                  <| toText <| join ", " <| map shownoquot io
                          width = toFloat (widthOf input) + style.size * toFloat (n+2)
                      in
                        collage 
                        (round width)
                        (round style.size * 2)
                        [move (toFloat -(widthOf input) / 2, style.size / 4) <| group
                         [ toForm registers
                         , moveX (style.size * toFloat n / 2) iopipe
                         , moveX (-style.size * toFloat (n - 1) / 2) . group
                                     <| feedback::taps
                         , moveX 
                           ((toFloat (widthOf input) + style.size * toFloat (n+1) ) / 2)
                           (toForm input)
                         ]
                        ]

register : LFSRStyle -> String -> Element
register style val = let size = round style.size 
                     in collage size size [
                             (outlined style.line) . square <| style.size,
                             toForm . centered . Text.style style.text
                             <| toText val]

tap : LFSRStyle -> Int ->  Form
tap s t = case t of
            0 -> traced s.line <| s.transpose [(-s.size+s.nodesize,0), (s.nodesize,0)]
            _ -> group [arrow s.line <| s.transpose [(-s.size+s.nodesize,0), (-s.nodesize,0)],
                        move (0,0) <| node s,
                        arrow s.line <| s.transpose [(0,s.nodesize), (0,s.size/2)]]

node : LFSRStyle -> Form
node style = let s = style.nodesize 
             in group ([
                     filled white (circle s),
                     outlined style.line (circle s)] ++
                       case style.nodetype of
                         Copy -> []
                         Add  -> [traced style.line <| segment (-s,0) (s,0),
                                  traced style.line <| segment (0,-s) (0,s)])

arrow : LineStyle -> Path -> Form
arrow style path = group [traced style path,
                          let (lx, ly) = last path
                              (px, py) = head . tail . reverse <| path 
                              (dx, dy) = (px - lx, py - ly)
                              d = atan2 dx dy
                              len = 4 * style.width
                              angle = pi/6
                              (nx1, ny1) = (sin (d + angle), cos (d + angle))
                              (nx2, ny2) = (sin (d - angle), cos (d - angle))
                          in 
                            traced style [(lx + len*nx1, ly + len*ny1),
                                          (lx, ly),
                                          (lx + len*nx2, ly + len*ny2)
                                         ]]

arrows : LineStyle -> Path -> Form
arrows style path = group (map (\(start, end) -> arrow style <| segment start end) (split path))

{--- Multiplier circuit ---------}

multiplier : LFSRStyle -> Color -> [(a,b,Int)] -> [c] -> Element
multiplier style lfsrcolor regs io = let line = style.line
                                         text = style.text
                                         lfsrstyle = { style | 
                                                       line <- { line |
                                                                 color <- lfsrcolor }, 
                                                       text <- { text |
                                                                 color <- lfsrcolor }
                                                     }
                                         lfsr = tlfsr lfsrstyle (map (\(a,b,t) -> (b,t)) regs) io
                                         scalars = flow right 
                                                   (map
                                                    (\(r,_,_) -> (scalar style <| shownoquot r)) 
                                                   regs)
                                         w = (toFloat (widthOf lfsr) - 
                                              toFloat (length regs) * style.size) / 2
                                     in
                                       collage 
                                       (widthOf lfsr)
                                       (heightOf lfsr + 2 * round style.size)
                                       [ moveY (-style.size * 3/4) <| toForm lfsr
                                       , move (-w + style.size, style.size) <| toForm scalars]

scalar : LFSRStyle -> String -> Element
scalar style val = let size = round style.size 
                   in collage size (2*size) 
                      [ moveY (style.size/2)  <| toForm <| register style val
                      , arrow style.line [(0,0),(0,-style.size)]]


{---------- Helpers -------------}

split : [a] -> [(a,a)]
split list = case list of
               []             -> []
               [single]       -> []
               one::two::tail -> (one, two)::split(two::tail)

shownoquot : a -> String
shownoquot v = String.filter (\c -> c /= '"' && c /= '\'') <| show v {-"-}

{----------- LOGIC -------------}

fibonacci : [(Int, Int)] -> ([(Int, Int)], Int)
fibonacci regs = let new = foldl (\(a,b) p -> p + a*b ) 0 regs
                 in foldl (\(a,b) (r,iin) -> (r ++ [(iin,b)], a)) ([], new) regs 

fibout : [(Int, Int)] -> [Int] -> ([(Int, Int)], [Int])
fibout regs prev = let (nregs, out) = fibonacci regs
                   in (nregs, out :: prev)

fibsig : [(Int, Int)] -> Int -> Signal a -> Signal ([(Int, Int)], [Int])
fibsig initial count sig = foldp
                            (\sig (regs, out) -> if count == 0 || count > length out
                                                 then fibout regs out
                                                 else (initial, []))
                            (initial, []) sig

modred : [(Int, Int)] -> Int -> [(Int, Int)]
modred regs input = let (out, tap) = head regs
                    in fst <| foldl
                           (\(a,b) (r,tap) -> (r ++ [(a+out*tap, tap)], b))
                           ([], tap)
                           ((tail regs) ++ [(input,0)])

modsig : [(Int, Int)] -> [Int] -> Signal a -> Signal ([(Int, Int)], [Int])
modsig initial initins sig = foldp
                             (\sig (regs, ins) -> case ins of
                                                    [] -> (initial, initins)
                                                    hd::tail -> (modred regs hd, tail))
                             (initial, initins) sig
