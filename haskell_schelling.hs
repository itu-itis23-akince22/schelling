import System.Environment (getArgs)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad
import Control.Monad.ST
import System.IO

-- Configuration
gridSize :: Int
gridSize = 200

threshold :: Float
threshold = 0.4

type Grid = V.Vector (V.Vector Int)
type Coord = (Int, Int)

-- Entry point
main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> do
            contents <- readFile inputFile
            let grid1D = map read (lines contents) :: [Int]
            let grid2D = toGrid gridSize grid1D
            finalGrid <- simulateUntilHappyWithCount grid2D 0
            let finalGrid1D = fromGrid finalGrid
            writeFile outputFile (unlines $ map show finalGrid1D)
        _ -> putStrLn "Usage: haskell_schelling inputFile outputFile"

-- Convert 1D list to 2D Vector grid
toGrid :: Int -> [Int] -> Grid
toGrid _ [] = V.empty
toGrid n xs = V.fromList (map V.fromList (chunk n xs))

-- Split list into chunks of n elements
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

-- Flatten 2D Vector grid to 1D list
fromGrid :: Grid -> [Int]
fromGrid = concatMap V.toList . V.toList

-- 8-neighbor coordinates
neighbors :: Coord -> [Coord]
neighbors (i,j) =
    [ (x, y) |
      x <- [i-1 .. i+1],
      y <- [j-1 .. j+1],
      (x, y) /= (i, j),
      x >= 0, x < gridSize,
      y >= 0, y < gridSize ]

-- Check if an agent is happy
isHappy :: Grid -> Coord -> Bool
isHappy grid (i,j) =
    let me = grid V.! i V.! j
        neighs = [grid V.! x V.! y | (x, y) <- neighbors (i,j)]
        relevant = filter (/= 0) neighs
        same = length (filter (== me) relevant)
        total = length relevant
    in total == 0 || fromIntegral same / fromIntegral total >= threshold

-- Check if all agents are happy
allAgentsHappy :: Grid -> Bool
allAgentsHappy grid =
    all (\(i,j) ->
        let v = grid V.! i V.! j
        in v == 0 || isHappy grid (i,j))
        [ (i,j) | i <- [0..gridSize-1], j <- [0..gridSize-1] ]

-- Perform one simulation step by relocating unhappy agents
runSchellingStep :: Grid -> Grid
runSchellingStep grid = runST $ do
    mgrid <- V.thaw grid
    forM_ unhappyAgents $ \((i,j), agent) -> do
        let tryTarget [] = return ()
            tryTarget ((ti,tj):rest) = do
                val <- MV.read mgrid ti >>= \row -> return (row V.! tj)
                if val == 0
                  then do
                    satisfied <- isHappyAt mgrid (ti,tj) agent
                    when satisfied $ do
                        MV.modify mgrid (\row -> row V.// [(tj, agent)]) ti
                        MV.modify mgrid (\row -> row V.// [(j, 0)]) i
                  else return ()
                tryTarget rest
        tryTarget emptySpots
    V.freeze mgrid
  where
    coords = [ (i,j) | i <- [0..gridSize-1], j <- [0..gridSize-1] ]
    unhappyAgents = [ ((i,j), grid V.! i V.! j)
                    | (i,j) <- coords
                    , let val = grid V.! i V.! j
                    , val /= 0 && not (isHappy grid (i,j)) ]
    emptySpots = [ (i,j) | (i,j) <- coords, grid V.! i V.! j == 0 ]

-- Check if an agent would be happy at a target position (used during relocation)
isHappyAt :: MV.MVector s (V.Vector Int) -> Coord -> Int -> ST s Bool
isHappyAt mgrid (i,j) agent = do
    neighVals <- forM (neighbors (i,j)) $ \(x,y) -> do
        row <- MV.read mgrid x
        return (row V.! y)
    let relevant = filter (/= 0) neighVals
        same = length (filter (== agent) relevant)
        total = length relevant
    return $ total == 0 || fromIntegral same / fromIntegral total >= threshold

-- Run the simulation loop until convergence and print iteration count
simulateUntilHappyWithCount :: Grid -> Int -> IO Grid
simulateUntilHappyWithCount grid count = do
    let newGrid = runSchellingStep grid
    if newGrid == grid || allAgentsHappy newGrid
      then do
        putStrLn $ "Simulation finished in " ++ show (count + 1) ++ " iterations."
        return newGrid
      else simulateUntilHappyWithCount newGrid (count + 1)
