import System.IO
import Data.Char
import Data.Ord
import Data.List


calculate_letter_frequency :: [(Char, Integer)] -> [Char] -> [(Char, Integer)]
calculate_letter_frequency list cs =
  [(char, toInteger . length $ filter (== char) cs) | (char, count) <- list] 

calculate_positional_letter_frequency :: [String] -> [[(Char, Integer)]]
calculate_positional_letter_frequency ws
  | any null ws = []
  | otherwise   = calculate_letter_frequency empty_list (map head ws) : calculate_positional_letter_frequency (map tail ws)
  where empty_list = [(c, 0) | c <- ['a'..'z']]


calculate_likelihood_word :: [[(Char, Integer)]] -> String -> Integer
calculate_likelihood_word freqs w = sum [map snd freq !! char_to_int c | (c, freq) <- zip w freqs]
  where char_to_int c = ord c - ord 'a'

calculate_likelihood :: [String] -> [[(Char, Integer)]] -> [(String, Integer)]
calculate_likelihood ws freqs = [(w, calculate_likelihood_word freqs w) | w <- ws]


main :: IO ()
main = do
  putStrLn "Reading file of valid Worlde words..."
  contents <- readFile "valid-wordle-words.txt"
  let words = lines contents

  putStrLn "Calculating letter frequencies..."
  let positional_letter_frequencies = calculate_positional_letter_frequency words
  print positional_letter_frequencies

  putStrLn "Calculating likelihood score for each word..."
  let unsorted_result = calculate_likelihood words positional_letter_frequencies
  let result = sortOn (Down . snd) unsorted_result

  let output_filename = "wordle-stats.txt"
  putStrLn $ "Writing data to " ++ output_filename ++ "..."
  writeFile output_filename (show result)

  putStrLn "Complete!"