{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module AoC2024.Solutions.Day15 (parse, part1, part2) where

import Data.List
import Data.Bifunctor
import Control.Monad
import Control.Monad.ST
import Data.Ix
import qualified Data.Array as Array
import Data.Array.ST
import AoC2024.Utils

type Direction = (Int, Int)

parseDirection :: Char -> Direction
parseDirection = \case
  '^' -> (-1, 0)
  'v' -> (1, 0)
  '<' -> (0, -1)
  '>' -> (0, 1)
  _ -> undefined

data CellState a
  = Empty
  | Wall
  | Box a
  deriving (Eq, Functor)

type BoardState a = (Grid (CellState a), (Int, Int))

parseCellState :: Char -> CellState ()
parseCellState = \case
  '.' -> Empty
  '@' -> Empty
  '#' -> Wall
  'O' -> Box ()
  _ -> undefined

class Step a where
  step :: STArray s (Int, Int) (CellState a) -> (Int, Int) -> Direction -> ST s (Int, Int)

stepAll :: Step a => [Direction] -> BoardState a -> BoardState a
stepAll dirs (board, pos) = runST $ do
  mut <- thaw board
  pos' <- foldM (step mut) pos dirs
  board' <- freeze mut
  return (board', pos')

instance Step () where
  step board pos dir = do
    let ray = tail (iterate (add2 dir) pos)
    (boxes, head -> nonBox) <- spanM (fmap (== Box ()) . readArray board) ray
    readArray board nonBox >>= \case
      Empty -> do
        case boxes of
          [] -> return ()
          firstBox:_ -> do
            writeArray board firstBox Empty
            writeArray board nonBox (Box ())
        return (add2 dir pos)
      Wall -> return pos
      Box () -> undefined

boxIndices :: STArray s (Int, Int) (CellState Bool) -> Direction -> (Int, Int) -> ST s (Maybe [(Int, Int)])
boxIndices board dir pos = do
  let pos' = add2 dir pos
  cell <- readArray board pos'
  return $ case cell of
    Empty -> Just []
    Wall -> Nothing
    Box isRight
      | fst dir == 0 -> Just [pos']
      | isRight -> Just [add2 (0, -1) pos', pos'] 
      | otherwise -> Just [pos', add2 (0, 1) pos']

nextLayer :: STArray s (Int, Int) (CellState Bool) -> Direction -> [(Int, Int)] -> ST s (Maybe [(Int, Int)])
nextLayer board dir layer = do
  results <- mapM (boxIndices board dir) layer
  return (nub . concat <$> sequence results)

allLayers :: STArray s (Int, Int) (CellState Bool) -> Direction -> [(Int, Int)] -> [(Int, Int)] -> ST s (Maybe [(Int, Int)])
allLayers board dir prev layer
  | null layer = return (Just prev)
  | otherwise = do
    layer' <- nextLayer board dir layer
    maybe (return Nothing) (allLayers board dir (layer ++ prev)) layer'

allBoxIndices :: STArray s (Int, Int) (CellState Bool) -> Direction -> (Int, Int) -> ST s (Maybe [(Int, Int)])
allBoxIndices board dir pos = do
  layer1 <- boxIndices board dir pos 
  maybe (return Nothing) (allLayers board dir []) layer1

instance Step Bool where
  step board pos dir = 
    allBoxIndices board dir pos >>= \case
      Nothing -> return pos -- Blocked by wall
      Just boxes -> do
        cells <- mapM (readArray board) boxes
        forM_ boxes (\i -> writeArray board i Empty)
        let boxes' = map (add2 dir) boxes
        forM_ (zip boxes' cells) (uncurry (writeArray board))
        return (add2 dir pos)
      
enlarge :: BoardState () -> BoardState Bool
enlarge (board, pos) = let
  (m, n) = dims board
  assocs = Array.assocs board
  assocs' = flip concatMap assocs $ \((i, j), cell) ->
    [ ((i, 2*j), False <$ cell), ((i, 2*j+1), True <$ cell) ]
  board' = Array.array ((0, 0), (m-1, 2*n-1)) assocs'
  pos' = second (*2) pos
  in (board', pos')

parse :: String -> (BoardState (), [Direction])
parse str = let
  (top, bottom) = break null (lines str)
  grid = fromList top
  board = fmap parseCellState grid
  pos = indexOf '@' grid
  dirs = map parseDirection (concat bottom)
  in ((board, pos), dirs)

part1 :: (BoardState (), [Direction]) -> Int
part1 (board, dirs) = let
  (board', _) = stepAll dirs board
  boxes = map fst (filter ((== Box ()) . snd) (Array.assocs board')) -- TODO: use indicesWhere
  in sumMap (\(i, j) -> 100 * i + j) boxes

part2 :: (BoardState (), [Direction]) -> Int
part2 (board, dirs) = let
  (board', _) = stepAll dirs (enlarge board)
  boxes = map fst (filter ((== Box False) . snd) (Array.assocs board')) -- TODO: use indicesWhere
  in sumMap (\(i, j) -> 100 * i + j) boxes