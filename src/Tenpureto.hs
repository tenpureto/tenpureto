module Tenpureto where

createProject :: String -> Bool -> IO ()
createProject template unattended = putStrLn ("Creating from " ++ template)

updateProject :: Maybe String -> Bool -> IO ()
updateProject template unattended = putStrLn "Updating"
