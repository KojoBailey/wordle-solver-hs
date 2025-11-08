import System.IO
import Data.Char


calculate_letter_frequency :: [(Char, Integer)] -> [Char] -> [(Char, Integer)]
calculate_letter_frequency list cs =
  [(char, toInteger . length $ filter (== char) cs) | (char, count) <- list] 

calculate_positional_letter_frequency :: [String] -> [[(Char, Integer)]]
calculate_positional_letter_frequency ws
  | any null ws = []
  | otherwise   = calculate_letter_frequency empty_list (map head ws) : calculate_positional_letter_frequency (map tail ws)
  where empty_list = [(c, 0) | c <- ['a'..'z']]

calculate_likelihood :: [String] -> [(Char, Integer)] -> [(String, Integer)]
calculate_likelihood ws freqs = map (\ w -> (w, sum [map snd freqs !! char_to_int c | c <- w])) ws
  where char_to_int c = ord c - ord 'a'


main :: IO ()
main = do
  putStrLn "Reading file of valid Worlde words..."
  contents <- readFile "valid-wordle-words.txt"
  let words = lines contents

  putStrLn "Calculating letter frequencies..."
  print $ calculate_positional_letter_frequency words
  -- let letter_frequencies = calculate_letter_frequency [(c, 0) | c <- ['a'..'z']] words
  -- print letter_frequencies

  -- putStrLn "Calculating likelihood score for each word..."
  -- let result = calculate_likelihood words letter_frequencies

  -- let output_filename = "wordle-stats.txt"
  -- putStrLn $ "Writing data to " ++ output_filename ++ "..."
  -- writeFile output_filename (show result)

  putStrLn "Complete!"