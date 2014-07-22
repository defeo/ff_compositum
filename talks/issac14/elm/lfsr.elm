import Window
import Text
import Maybe
import Debug
import Time

main : Signal Element
main = uncurry (tlfsr (transStyle <| toFloat 100))
       <~ fibsig [(1,1),(0,1)] 10 (2*second)

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

tlfsr : LFSRStyle -> [(Int,Int)] -> [Int] -> Element
tlfsr style regs io = let n = length regs
                          registers = flow right 
                                      (map
                                       (\(r,_) -> (register style <| show r)) 
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
                                  <| toText <| join ", " <| map show io
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

split : [a] -> [(a,a)]
split list = case list of
               []             -> []
               [single]       -> []
               one::two::tail -> (one, two)::split(two::tail)

{----------- LOGIC -------------}

fibonacci : [(Int, Int)] -> ([(Int, Int)], Int)
fibonacci regs = let new = foldl (\(a,b) p -> p + a*b ) 0 regs
                 in foldl (\(a,b) (r,iin) -> (r ++ [(iin,b)], a)) ([], new) regs 

fibout : [(Int, Int)] -> [Int] -> ([(Int, Int)], [Int])
fibout regs prev = let (nregs, out) = fibonacci regs
                   in (nregs, out :: prev)

fibsig initial count freq = foldp
                            (\sig (regs, out) -> if count == 0 || count > length out
                                                 then fibout regs out
                                                 else (initial, []))
                            (initial, []) (every freq)
