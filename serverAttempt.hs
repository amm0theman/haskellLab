import EasyNetwork
import System.IO

main = do
  ip <- ipAddress
  putStr "Server Ip = "
  putStrLn ip
  conn <- server "12345"
  loop conn

loop :: Handle -> IO()
loop conn = do
  putStrLn "Waiting for player response"
  msg <- hGetLine conn
  putStr "Received player response "
  putStrLn "Rock:1 Paper:2 Scissors:3 Make Selection"
  userChoice <- getLine
  hPutStrLn conn userChoice
  putStrLn "Game has been played. Result: "
  loop conn
