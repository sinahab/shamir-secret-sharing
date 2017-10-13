
import Shamir (createShares, combineShares)

main :: IO ()
main = do
  let shares = createShares 1234 2 5
  let availableShares = [shares !! i | i <- [0..length shares - 1], i < 4]
  combineShares availableShares

