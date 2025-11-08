import System.IO
import Data.Char

calculate_letter_frequency_word :: [(Char, Integer)] -> String -> [(Char, Integer)]
calculate_letter_frequency_word result [] = result
calculate_letter_frequency_word previous (c:string) = calculate_letter_frequency_word foo string
  where foo = map (\ (char, freq) -> if char == c then (char, succ freq) else (char, freq)) previous

calculate_letter_frequency :: [(Char, Integer)] -> [String] -> [(Char, Integer)]
calculate_letter_frequency result [] = result
calculate_letter_frequency previous words =
  calculate_letter_frequency (calculate_letter_frequency_word previous (head words)) $ tail words

char_to_int :: Char -> Int
char_to_int c = ord c - ord 'a'

calculate_likelihood :: [String] -> [(Char, Integer)] -> [(String, Integer)]
calculate_likelihood ws freqs = map (\ w -> (w, sum [map snd freqs !! char_to_int c | c <- w])) ws

main :: IO ()
main = do
  putStrLn "Reading file of valid Worlde words..."
  contents <- readFile "valid-wordle-words.txt"
  let words = lines contents

  putStrLn "Calculating letter frequencies..."
  let letter_frequencies = calculate_letter_frequency [(c, 0) | c <- ['a'..'z']] words
  print letter_frequencies

  putStrLn "Calculating likelihood score for each word..."
  let result = calculate_likelihood words letter_frequencies

  let output_filename = "wordle-stats.txt"
  putStrLn $ "Writing data to " ++ output_filename ++ "..."
  writeFile output_filename (show result)

  putStrLn "Complete!"