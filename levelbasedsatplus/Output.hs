{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Output where

import qualified Graphics.SVGFonts as SF
import Diagrams.Prelude
import Diagrams.Backend.SVG
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)

import Debug.Trace

import Input
import Lin

fs :: Diagram B -> Diagram B
fs name = frame 0.5 (((name) #alignL )  === (((base #alignR ) ||| ((lamps #alignR  === stem #alignR) #alignBL)))#alignL)  #alignTL
  where
    base = rect 0.2 2.0 # fc black
    stem = (fromOffsets [r2 (6.0, 0.0)])
    lamps = hcat $ replicate 2 (circle 1.0)


hs3fs :: Diagram B -> Diagram B
hs3fs name = frame 0.5 (((name) #alignL )  === (((base #alignR ) ||| ((sideLamps #alignR  === stem #alignR) #alignBL ||| topLamps)))#alignL)  #alignTL
  where
    base = rect 0.2 2.0 # fc black
    stem = (fromOffsets [r2 (6.0, 0.0)])
    sideLamps = hcat $ replicate 2 (circle 1.0)
    topLamps = hcat $ replicate 3 (circle 1.0)

ai :: Diagram B -> Diagram B
ai name = frame 0.5 (square 1.0 === name) #alignT



-- Some bug with the string "Fs" ??
-- loadedFont = unsafePerformIO SF.lin2
loadedFont = unsafePerformIO SF.bit
shapeText d s = (strokeP $ SF.textSVG' (SF.TextOpts loadedFont SF.INSIDE_H SF.KERN False d d) s) # lw none
txt d s = shapeText d s # fc black
ftxt s = frame 0.5 $ txt 2.0 s

mkLine :: (Double,Double) -> Double -> (Double,Double) -> [(Double,Double)]
mkLine (x1,y1) y (x2,y2) = concat [[(x1,y1)],
    if absy1 > 1e-5 then [(x1+absy1,y)] else [],
    if absy2 > 1e-5 then [(x2-absy2,y)] else [],[(x2,y2)]]
  where
    absy1 = (abs (y-y1))
    absy2 = (abs (y2-y))

myRender fn w d = renderPretty fn (mkWidth w) d
drawSchematic :: LayoutOutput -> String -> IO ()

getSymbol :: String -> String -> Diagram B
getSymbol "fs" text = fs (ftxt text)
getSymbol _ _ = error "Unknown symbol (getSymbol)"

drawSchematic (LayoutOutput nodes edges labels) fileName = do
  --let diag  = (fs (ftxt "11010(A)" # showOrigin)) #showOrigin 

  let trackScale = 10.0
  let lines = [ mkLine (s*nax,s*nay) (s*ey) (s*nbx,s*nby) | (((n1,_),(n2,_)),ey) <- edges 
              , let (_,(nax,nay)) = nodes !! n1, let (_,(nbx,nby)) = nodes !! n2 ]
        where s = trackScale

  let lineDiags = [ map p2 vs | vs <- lines ]
  let lineDiag = (foldl1 (<>) (map fromVertices lineDiags)) # lw 1.3
  let labelDiag = mconcat 
        [ let (edgeRelPos,edgeTangent) = (((map trailFromVertices lineDiags) !! ei) `atArcParam` 0.5) 
              edgePos = edgeRelPos .+^ (r2 $ head (lines !! ei))
            in (getSymbol name "Fs.11001(A)"  ) # rotate (edgeTangent ^. _theta) # translate (traceShowId edgePos ) 
        | ((Label (ei,ni,offset) align level name),x) <- labels]

  let switches = mconcat [  (drawSwitch (trackScale*0.25) n (zip (map fst edges) lineDiags)) # strokeLoop #fc black # translate ((trackScale*nx) ^& (trackScale*ny)) | n@(_,((SwitchNode _ _),(nx,ny))) <- zip [0..] nodes ]

  let diag = ((frame 0.5 (lineDiag)) <> (labelDiag ) ) # lw 0.6 <> switches

  let width = 50*(last [ nx | (_,(nx,_)) <- nodes ])
  myRender fileName width diag 

--examplexx = pdiag <> (mconcat $ fmap (\z -> (circle 0.1) # translate (atArcParam tr z)) [0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]) -- (mconcat (map (place (circle 0.1) () [ tr `atParam` x | x <- [0.0]]))
--  where pdiag = (fromVertices $ (traceShowId (map p2 ps )))  # lw 1.3
--        ps = [(0.0,0.0),(1.0,0.5),(2.0,0.5),(3.0,0.0),(4.0,-10.0)]
--        --ppp t x = stdArcLengthToParam t (x * (stdArcLength t))
--        tr = trailFromVertices $ map p2 ps
        
--atArcParam t x = Debug.Trace.trace ("high" ++ (show (domainUpper t))) $ t `atParam` (traceShowId (stdArcLengthToParam t (x* (traceShowId (stdArcLength t)))))
--

--drawSwitch :: Node -> [Edge, 
drawSwitch l (ni,((SwitchNode side dir),_)) edgeLines = closeLine $ fromVertices [origin, origin .+^ go straight, origin .+^ (1.414 *^ (go branch)) ]
  where 
    go v = l *^ (fromDirection (direction v))
    origin = p2 (0.0,0.0)
    straight = case (side,dir) of 
      (SLeft,Up)    -> head [ tangentAtStart (trailFromVertices line) | (((n1,p1),(n2,p2)),line) <- edgeLines, n1 == ni, p1 == PRight ]
      (SLeft,Down)  -> head [ rotate halfTurn (tangentAtEnd (trailFromVertices line)) | (((n1,p1),(n2,p2)),line) <- edgeLines, n2 == ni, p2 == PRight ]
      (SRight,Up)   -> head [ tangentAtStart (trailFromVertices line) | (((n1,p1),(n2,p2)),line) <- edgeLines, n1 == ni, p1 == PLeft ]
      (SRight,Down) -> head [ rotate halfTurn (tangentAtEnd (trailFromVertices line)) | (((n1,p1),(n2,p2)),line) <- edgeLines, n2 == ni, p2 == PLeft ]
    branch = case (side,dir) of 
      (SLeft,Up)    -> head [ tangentAtStart (trailFromVertices line) | (((n1,p1),(n2,p2)),line) <- edgeLines, n1 == ni, p1 == PLeft ]
      (SLeft,Down)  -> head [ rotate halfTurn (tangentAtEnd (trailFromVertices line)) | (((n1,p1),(n2,p2)),line) <- edgeLines, n2 == ni, p2 == PLeft ]
      (SRight,Up)   -> head [ tangentAtStart (trailFromVertices line) | (((n1,p1),(n2,p2)),line) <- edgeLines, n1 == ni, p1 == PRight ]
      (SRight,Down) -> head [ rotate halfTurn (tangentAtEnd (trailFromVertices line)) | (((n1,p1),(n2,p2)),line) <- edgeLines, n2 == ni, p2 == PRight ]
  

atArcParam t x = (t `atParam` p, t `tangentAtParam` p)
  where p = stdArcLengthToParam t (x*(stdArcLength t))

test :: IO ()
test = do
  let (SolverInput n e _ ) = example5
  let (LayoutOutput ln le _) = layout n e []
  let label1 = Label (1,1,0.5) ALeft (-1) "fs"
  --drawSchematic (LayoutOutput ln le [(label1,1.2)]) "test.svg"
  drawSchematic (LayoutOutput ln le []) "ex.svg"

