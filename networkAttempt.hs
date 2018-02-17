import EasyNetwork
import System.IO
import Control.Monad
--Defines a data type in Haskell. Closes we can get to an object in this stupid language. Represents the choice that the user makes
--Inherits from Show which allows all data types to be cast to a string
data RoShambo = Rock | Paper | Scissors deriving (Show, Eq, Read)

--Again, defines a data type. This is for the results form so to speak
data RoShamboResult = Win | Lose | Tie deriving (Show, Eq)

--Defines our functionality for rock paper scissors
evaluate :: RoShambo -> RoShambo -> RoShamboResult
--This is the part of a function where you just match shiz
evaluate Rock Paper     = Win
evaluate Rock Scissors  = Lose
evaluate Scissors Rock  = Lose
evaluate Scissors Paper = Win
evaluate Paper Rock     = Win
evaluate Paper Scissors = Lose
--Basically says that we don't care what the two arguments are. If nothings matched yet, return a Tie
evaluate _ _            = Tie

getChoice :: String -> RoShambo
getChoice "Rock" = Rock
getChoice "Paper" = Paper
getChoice "Scissors" = Scissors

main = do
  putStrLn "Enter IP Address of the server"
  ip <- getLine
  conn <- client ip "12345"
  loop conn

loop :: Handle -> IO()
loop conn = do
  putStrLn "Rock:1 Paper:2 Scissors:3 Make Selection"
  userChoice <- getLine
  let userResult = getChoice userChoice
  hPutStrLn conn userChoice
  putStrLn "Waiting for other user"
  resp <- hGetLine conn
  let otherUser = getChoice resp
  putStr "Got Response. Result:  "
  case userResult `evaluate` otherUser of
    Win   -> putStrLn "Victory" 
    Lose  -> putStrLn "Loss"
    Tie   -> putStrLn "Tie"
  loop conn
