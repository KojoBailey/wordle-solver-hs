import System.IO

calculate_letter_frequency_word :: [(Char, Int)] -> String -> [(Char, Int)]
calculate_letter_frequency_word result [] = result
calculate_letter_frequency_word previous (c:string) = calculate_letter_frequency_word foo string
  where foo = map (\ (char, freq) -> if char == c then (char, succ freq) else (char, freq)) previous

calculate_letter_frequency :: [(Char, Int)] -> [String] -> [(Char, Int)]
calculate_letter_frequency result [] = result
calculate_letter_frequency previous words =
  calculate_letter_frequency (calculate_letter_frequency_word previous (head words)) $ tail words

main :: IO ()
main = do
  putStrLn "Reading file of valid Worlde words..."
  contents <- readFile "valid-wordle-words.txt"
  putStrLn "Calculating letter frequencies..."
  print $ calculate_letter_frequency (map (\ c -> (c, 0)) ['a'..'z']) (lines contents)