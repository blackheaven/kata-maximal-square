module Square  where

import qualified Data.Set as S

maximalSquare :: [String] -> Int
maximalSquare lines = maximum $ ([0] ++) $ map (\coord -> sizeSquare coord presentNeighbours) $ S.toList presentCoordinates
  where presentCoordinates :: S.Set Coord
        presentCoordinates = extractPresentCoordinates lines
        presentNeighbours :: S.Set Coord
        presentNeighbours = buildPresentNeighbours presentCoordinates

sizeSquare :: Coord -> S.Set Coord -> Int
sizeSquare coord presentNeighbours = searchNeighbours [coord] 1
  where searchNeighbours :: [Coord] -> Int -> Int
        searchNeighbours coords size = if all (\coord -> S.member coord presentNeighbours) coords
                                    then searchNeighbours (concatMap neighbours coords) (size + 1)
                                    else size * size

neighbours :: Coord -> [Coord]
neighbours (x, y) = [(x+1, y), (x+1, y+1), (x,y+1)]

buildPresentNeighbours :: S.Set Coord -> S.Set Coord
buildPresentNeighbours presents = S.fromList (filter allNeighboursPresent (S.toList presents))
  where allNeighboursPresent coord = all (\neighbor -> S.member neighbor presents) (neighbours coord)

type Coord = (Int, Int) 

extractPresentCoordinates :: [String] -> S.Set Coord
extractPresentCoordinates lines = S.fromList (map fst (filter snd (indexLines lines)))

indexLines :: [String] -> [(Coord, Bool)]
indexLines lines = concatMap assignCoords indexedLines
  where indexedLines :: [(Int, [(Int, Bool)])]
        indexedLines = zip [0..] (map indexColumns lines)
        assignCoords :: (Int, [(Int, Bool)]) -> [(Coord, Bool)]
        assignCoords (line, indexedColumns) = map (\(column, present) -> ((line, column), present)) indexedColumns

indexColumns :: String -> [(Int, Bool)]
indexColumns items = zip [0..] (map (\item -> item == '1') items)