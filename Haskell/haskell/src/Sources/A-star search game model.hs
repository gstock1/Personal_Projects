{-# LANGUAGE FlexibleInstances #-}
module MP.MP4 where
import Prelude hiding (lines)
import Data.Maybe
import Data.Ord
import Data.List hiding (lines)
import Data.List.Split (chunksOf)
import Data.Tree
import Data.Map (Map, empty, fromList, findWithDefault, member, insertWith)
import System.Random
import System.Random.Shuffle
import Control.Monad.State
import System.IO
import System.Console.ANSI
import Control.Concurrent
import GHC.IO

data Piece = X deriving (Eq, Show, Read)
data Player = Player1 | Player2 deriving (Eq, Show, Read)

type Stack = [Maybe Piece]

data Board = Board { stack :: Stack
                    , currentTurn :: Player
                    } deriving (Show, Eq) 


startingBoard :: Int -> Player -> Board
startingBoard n p = (Board (replicate n (Just X)) p)

loseBoard :: Board -> Player -> Bool
loseBoard (Board b p) player = lose b && p == player
      where lose :: Stack -> Bool
            lose [] = True 
            lose [b] = b == (Just X)
            lose (b:bs) = if b == Nothing then lose bs else False

instance {-# OVERLAPPING #-} Show Stack where
  show = intercalate "\n" . chunksOf 1 . concat . map showSquare
    where showSquare Nothing = "-"
          showSquare (Just p) = show p

getStack :: Board -> Stack
getStack (Board stack _) = stack

turn :: Board -> Player
turn (Board b p) = p

setTurn :: Board -> Board
setTurn (Board b Player1) = Board b Player2
setTurn (Board b Player2) = Board b Player1

opponent :: Player -> Player
opponent Player1 = Player2
opponent Player2 = Player1

-- play the specified nim amount
playMove :: Int -> Board -> Board
playMove n (Board b p) = (Board (clearStack b n) (opponent p))
  where clearStack :: Stack -> Int -> Stack
        clearStack [b] _ = [b]
        clearStack b 0 = b
        clearStack (b:bs) n = if b == Nothing then Nothing : (clearStack bs n) else Nothing : (clearStack bs (n-1))

remaining :: Board -> Int
remaining (Board [] p) = 0
remaining (Board (b:bs) p) = if b == Nothing then remaining (Board bs p) else 1 + remaining (Board bs p)

playInteractive :: IO ()
playInteractive = play (startingBoard 21 Player1) True
  where play board first
          | first = do 
              putStrLn "Enter starting number of tokens: " 
              tokens <- readLn
              if tokens < 1 
                then do 
                  putStrLn "Not a valid board. Enter a new value." 
                  play board first
                else do 
                  print $ getStack (startingBoard tokens Player1) 
                  play (startingBoard tokens Player1) False
          | loseBoard board (turn board) = if (turn board) == Player1 then putStrLn "Player2 Wins!" else putStrLn "Player1 Wins!"
          | otherwise = do
              if turn board == Player1 then putStrLn "Player1's turn..." else putStrLn "Player2's turn..."
              putStr "Enter amount to nim: "
              move <- readLn
              case move > 3 of
                True -> do putStrLn "Illegal move"
                           play board False
                False -> do case move > 0 of
                             True -> do let remainingPieces = remaining board
                                        case remainingPieces <= move of 
                                         True -> do putStrLn "Illegal move" 
                                                    play board False
                                         False -> do let board' = playMove move board
                                                     print $ getStack (board')
                                                     play board' False
                             False -> do putStrLn "Illegal move" 
                                         play board False
                

modding :: Int -> Int -> Int
modding x o | x >= o = modding (x-1) o
            | otherwise = if x > 4 then (if x`mod`4 == 0 then (if x+1 < o then x+1 else modding (x-1) o) else modding (x-1) o) else case x of 4 -> 3
                                                                                                                                              3 -> 2
                                                                                                                                              2 -> 1

count :: Board -> Int
count (Board [] _) = 0
count (Board (b:bs) p)  = if b == Nothing then count (Board bs p) else 1 + count (Board bs p)

--nextMove :: Board -> Int
--nextMove b = if ((count b) - (modding (count b) (count b))) < 4 then (count b) - (modding (count b)(count b)) else 1

moves :: Board -> [Int]
moves b = if (count b) > 3 then [1..3] else case count b of 1 -> []
                                                            2 -> [1]
                                                            3 -> [1,2]

nextBoards :: Board -> [Board]
nextBoards b = map (flip playMove b) (moves b)  

gameTree :: Board -> Tree Board
gameTree b = Node b $ map gameTree $ nextBoards b 


printTree :: Board -> IO ()
printTree = putStrLn . drawTree . fmap show . gameTree

data Scored a = Scored { score :: Int, scoredVal :: a }

instance Eq (Scored a) where
  (Scored x _) == (Scored y _) = x == y

instance Ord (Scored a) where
  compare (Scored x _) (Scored y _) = compare x y

instance Show a => Show (Scored a) where
  show (Scored s v) = "Score: " ++ show s ++ "\n\n" ++ show v



wins :: Board -> Player -> Bool
wins b@(Board bs p) player = ((count b) `mod` 2 == 0) && player == p 

scoreBoard :: Player -> Board -> Scored Board
scoreBoard p b | (count b) == 2 && (turn b) == p = Scored (100) b
               | (count b) == 2 && (turn b) == (opponent p) = Scored (-100) b
               | wins b p = Scored (50) b
               | wins b (opponent p) = Scored (-50) b
               | loseBoard b p = Scored (-150) b
               | loseBoard b (opponent p) = Scored (150) b
               | otherwise = Scored 0 b

--the computer is player1 
printScoredTree :: Board -> IO ()
printScoredTree = putStrLn . drawTree . fmap (show . scoreBoard Player1) . gameTree

minimax :: (a -> Scored a) -> Tree a -> Scored a
minimax scorefn (Node _ ns) = maximize ns
  where maximize = maximum . map (eval minimize)
        minimize = minimum . map (eval maximize)
        eval _ (Node x []) = scorefn x
        eval f (Node x ns) = f ns

minimax' :: (a -> Scored a) -> Tree a -> Scored a
minimax' scorefn (Node _ ns) = maximize ns
  where maximize = maximumBy (comparing score) . map (eval minimize)
        minimize = minimumBy (comparing score) . map (eval maximize)
        eval _ (Node x []) = scorefn x
        eval f (Node x ns) = let Scored s _ = f ns in Scored s x


--player1 is the computer and player2 is the player
playAI :: IO ()
playAI = play (startingBoard 21 Player1) True 
  where play board first 
          | first = do 
               putStrLn "Enter starting number of tokens: " 
               tokens <- readLn
               if tokens < 1 
                 then do 
                        putStrLn "Not a valid board. Enter a new value." 
                        play board True 
                  else do 
                        print $ getStack (startingBoard tokens Player1)
                        play (startingBoard tokens Player1) False
          | loseBoard board (turn board) = if (turn board) == Player1 then putStrLn "Player wins!" else putStrLn "Computer Wins!"
          | (turn board) == Player2 = do 
              putStr "Enter amount to nim: "
              move <- readLn
              case move > 3 of
                True -> do putStrLn "Illegal move"
                           play board False
                False -> do case move > 0 of
                             True -> do let remainingPieces = remaining board
                                        case remainingPieces <= move of 
                                         True -> do putStrLn "Illegal move" 
                                                    play board False
                                         False -> do let board' = playMove move board
                                                     print $ getStack (board')
                                                     play  board' False
                             False -> do putStrLn "Illegal move" 
                                         play board False
          | otherwise = do 
                  putStrLn "Computing....."
                  print $ getStack ((scoredVal $ minimax' (scoreBoard Player1) (gameTree board)))
                  play (((scoredVal $ minimax' (scoreBoard Player1) (gameTree board)))) False