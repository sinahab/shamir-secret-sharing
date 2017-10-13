module Main where

import Shamir

main :: IO ()
main = do
  let shares = createShares 1234 2 5
  putStrLn "Shares.."

  return ()
