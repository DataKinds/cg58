{-# LANGUAGE GADTs, DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications, Rank2Types, LambdaCase #-}

module Main (main) where

-- import Lib
import GHC.TypeLits
import Data.Proxy (Proxy)
import Data.Kind (Type)
import System.Environment
import Data.List
import Text.Read (readMaybe)

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
ticTacToe depth n = ticTacToeCont depth n Some

ticTacToeCont :: (Integral n1) => Natural -> n1 -> (forall n2. KnownNat n2 => TicTacToe n2 -> r) -> r
ticTacToeCont depth n continuation = case someNatVal (fromIntegral n) of
    Just (SomeNat (_ :: Proxy nat)) -> continuation (ttt depth)
        where
            ttt 0 = TicTacToe @nat . replicate (fromIntegral $ n*n) . Right $ Nil
            ttt d = TicTacToe @nat . replicate (fromIntegral $ n*n) . Left $ ttt (d - 1)
    Nothing -> error "Negative size passed to ticTacToe"


replace :: Natural -> a -> [a] -> [a]
replace 0 el' (el:els) = el':els
replace i el' (el:els) = el:replace (i - 1) el' els
replace i el' [] = []


update :: Natural -> (a -> a) -> [a] -> [a]
update 0 f (el:els) = f el:els
update i f (el:els) = el:update (i - 1) f els
update i f [] = []


updateMaybe :: Natural -> (a -> a) -> [a] -> Maybe [a]
updateMaybe _ _ [] = Nothing
updateMaybe 0 f (el:els) = Just $ f el:els
updateMaybe i f (el:els) = do 
    rec <- updateMaybe (i - 1) f els
    pure $ el:rec

(!?) :: [a] -> Natural -> Maybe a
(!?) [] _ = Nothing
(!?) (x:xs) 0 = pure x
(!?) (x:xs) n = xs !? (n - 1)


-- Accepts a list of indices to move into recursively, starting from 0 in the top left through sideLength in the bottom right 
move :: KnownNat n => TicTacToe n -> [Natural] -> Mark -> Maybe (TicTacToe n)
move ttt@(TicTacToe board) (idx:idxs) mark = do
    successes <- updateMaybe idx go (Just <$> board)
    board' <- sequence successes
    pure $ TicTacToe board'
    where
        go :: KnownNat n => Maybe (Either (TicTacToe n) Mark) -> Maybe (Either (TicTacToe n) Mark)
        go Nothing = Nothing
        go (Just t) = case t of
            Left ttt' -> do
                rec <- move ttt' idxs mark
                pure . Left $ rec
            Right mark' | null idxs && mark' == Nil -> Just $ Right mark
                        | otherwise -> Nothing
    

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


gameStep :: KnownNat n => TicTacToe n -> IO (TicTacToe n)
gameStep ttt = do
    print ttt
    putStr "Please enter a string of space-delimited moves: "
    rawMoves <- filter (/= " ") . groupBy (\a b -> a /= ' ' && b /= ' ') <$> (readLn :: IO String)
    let validMoves :: Maybe [Natural] = mapM readMaybe rawMoves
    case validMoves of 
        Nothing -> do
            putStrLn "Could not parse move! Try again!"
            gameStep ttt
        Just moves -> case move ttt moves X of
            Nothing -> do
                putStrLn "Invalid move! Try again!"
                gameStep ttt
            Just ttt' -> gameStep ttt'

_main :: Int -> IO ()
_main n = ticTacToeCont (fromIntegral n) 3 (\game -> do
        gameStep game
        return ())

main :: IO ()
main = do
    args <- getArgs
    _main (read . head $ args)

