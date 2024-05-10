{-# LANGUAGE GADTs, DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications, Rank2Types, LambdaCase #-}

module Main (main) where

-- import Lib
import GHC.TypeLits
import Data.Proxy (Proxy)
import Data.Kind (Type)
import System.Environment
import Data.List

data Mark = X | O | Nil deriving (Eq)
data SomeIndexedChild (child :: Nat -> Type) = forall n. KnownNat n => Some (child n)
data TicTacToe (sideLen :: Nat) = TicTacToe [Either (TicTacToe sideLen) (Mark)]

sideLength :: (KnownNat n, Integral m) => TicTacToe n -> m
sideLength = fromIntegral . natVal

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs = as:(groupN n bs)
    where
        (as, bs) = splitAt n xs

instance Show Mark where
    show = \case
        X -> "X"
        O -> "O"
        Nil -> ""

renderMark :: String -> Mark -> [String]
renderMark fallback Nil = lines fallback
renderMark _ m = lines . show $ m

-- Returns the each dimension for some rectangular string
bbox :: Integral m => [String] -> (m, m)
bbox r = (fromIntegral . length . head $ r, fromIntegral . length $ r)

maxBbox :: Integral m => [[String]] -> (m, m)
maxBbox = go 0 0
    where
        go x y (r:rects) = case bbox r of
            (x', y') -> go (max x x') (max y y') rects
        go x y [] = (x, y)

padToBbox :: Integral m => (m, m) -> [String] -> [String]
padToBbox (x, y) = padVert y . fmap (padHoriz x)
    where
        padCenter :: Integral m => a -> m -> [a] -> [a]
        padCenter padElem x' els
            | needed <= 0 = els
            | needed `mod` 2 == 0 = let
                  pad = replicate (fromIntegral needed `div` 2) padElem
                  in pad ++ els ++ pad                             -- even number of padding elems required
            | otherwise = padCenter padElem x' (padElem:els) -- odd number of padding elems required
            where
                needed = x' - (fromIntegral $ length els)
        padHoriz = padCenter ' '
        padVert = padCenter (replicate (fromIntegral x) ' ')

instance KnownNat n => Show (TicTacToe n) where
    show ttt@(TicTacToe board) = let
        boxes = coshow <$> board
        bbox = maxBbox boxes
        paddedBoxes = padToBbox bbox <$> boxes
        groupedBoxes = (groupN (sideLength ttt) paddedBoxes) :: [[[String]]]
        rowSeparator = concat . intersperse "+" . replicate (sideLength ttt) . replicate (fst bbox) $ '-'
        in
            unlines .
            intersperse rowSeparator .
            fmap (concat . intersperse "\n") .
            fmap boxJoin $ groupedBoxes        
        where
            coshow :: KnownNat n => Either (TicTacToe n) Mark -> [String]
            coshow (Left ttt') = lines $ show ttt'
            coshow (Right mark) = renderMark " " mark

            boxJoin :: [[String]] -> [String]
            boxJoin = foldr1 (zipWith (\x y -> x ++ "|" ++ y)) 
            
ticTacToe :: (Integral n1) => Natural -> n1 -> SomeIndexedChild TicTacToe
ticTacToe depth n = ticTacToeWithContinuation depth n Some

ticTacToeWithContinuation :: (Integral n1) => Natural -> n1 -> (forall n2. KnownNat n2 => TicTacToe n2 -> r) -> r
ticTacToeWithContinuation depth n continuation = case someNatVal (fromIntegral n) of
    Just (SomeNat (_ :: Proxy nat)) -> continuation (ttt depth)
        where
            ttt 0 = TicTacToe @nat . replicate (fromIntegral $ n*n) . Right $ Nil
            ttt d = TicTacToe @nat . replicate (fromIntegral $ n*n) . Left $ ttt (d - 1)
    Nothing -> error "Negative size passed to ticTacToe"


replace :: Natural -> a -> [a] -> [a]
replace 0 el' (el:els) = el':els
replace i el' (el:els) = el:(replace (i - 1) el' els)
replace i el' [] = []

-- Accepts a list of indices to move into recursively, starting from 0 in the top left through sideLength in the bottom right 
move :: KnownNat n => TicTacToe n -> [Natural] -> Mark -> Maybe (TicTacToe n)

move ttt@(TicTacToe board') = fmap TicTacToe . go ([], board')
    -- (idx, rest) <- uncons dig
    -- case (board !? (fromIntegral idx)) of
    --     Nothing -> Nothing -- Tried to place a mark out of bounds!
    --     Just box -> case box of
    --         Left (TicTacToe board') -> do
    --             rec <- move board' rest mark
    --             replace idx rec board
    --         Right Nil -> Just $ replace idx (Right mark) board
    --         Right _ -> Nothing -- Tried to place a mark over an existing X or O!
    where
        go :: ([Either (TicTacToe sideLen) Mark], [Either (TicTacToe sideLen) Mark]) -> [Natural] -> Mark -> Maybe [Either (TicTacToe sideLen) Mark]
        go (done, ((Right mark):rest)) [] mark' = pure $ (reverse rest) ++ ((Right mark'):done)
        go (done, ((Right mark):rest)) (0:dig) mark' = Nothing
        go (done, (Left (TicTacToe board):rest)) [] mark = Nothing
--        go (done, (Left (TicTacToe board):rest)) (0:dig) mark = Just $ (reverse rest) ++ ((Left $ TicTacToe (g)):done)))
        go (done, (box:rest)) (idx:dig) mark = go (box:done, rest) ((idx - 1):dig) mark

winner :: KnownNat n => TicTacToe n -> Mark
winner (TicTacToe board) = let
    recurse = \case {
        Left ttt -> winner ttt;
        Right mark -> mark
    }
    row = recurse <$> take 3 board 
    markset = nub row
    wincon = (length markset == 1) && (head markset /= Nil)
    in if wincon then head markset else Nil

_main :: Int -> IO ()
_main n = ticTacToeWithContinuation 3 n (\game -> do 
        print game
        print $ winner game
    )

main :: IO ()
main = do
    args <- getArgs
    _main (read . head $ args)

