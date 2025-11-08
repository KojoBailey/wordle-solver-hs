import System.IO

data WordleCorrectness = NotUsed | WrongPosition | CorrectPosition
  deriving (Eq, Show, Read, Ord)

type WordleString = [(Char, WordleCorrectness)]


match2 :: (Eq b, Foldable t) => (b, b) -> t b -> WordleCorrectness
match2 chars target
  | uncurry (==) chars      = CorrectPosition
  | fst chars `elem` target = WrongPosition
  | otherwise               = NotUsed

match :: String -> String -> WordleString
match input target = zip input $ zipWith (curry (`match2` target)) input target


-- Infer if word is possible based on WordleString input.
  -- If word contains letter from input that is NotUsed, remove from list.
  -- If letter in WrongPosition is not in word, remove from list.
  -- If letter in WrongPosition is in same position in word, remove from list.
  -- If letter from input that is CorrectPosition is not in same position as word, remove from list.
is_possible :: WordleString -> String -> Bool
is_possible input word
  | any (\ (char, correctness) -> correctness == NotUsed && char `elem` word) input = False
  | any (\ (char, corr) -> corr == WrongPosition && char `notElem` word) input = False
  | any (\ ((char1, correctness), char2) -> correctness == WrongPosition && char1 == char2) zipped = False
  | any (\ ((char1, correctness), char2) -> correctness == CorrectPosition && char1 /= char2) zipped = False
  | otherwise = True
  where zipped = zip input word

-- Take WordleString from input and find possibilities.
generate_possibilities :: WordleString -> [String] -> [String]
generate_possibilities input = filter (is_possible input)


convert_correct_char :: Char -> WordleCorrectness
convert_correct_char '=' = CorrectPosition
convert_correct_char '/' = WrongPosition
convert_correct_char 'X' = NotUsed
convert_correct_char  _  = NotUsed

convert_correctness_string :: String -> [WordleCorrectness]
convert_correctness_string = map convert_correct_char


wordle_solve :: [String] -> IO ()
wordle_solve [ ] = print "No solution found..."
wordle_solve [_] = print "Solution reached!"
wordle_solve remaining_words = do
  putStrLn "Enter word:"
  word_input <- getLine

  putStrLn "Enter correctness ('=' : correct, '/' : wrong position, 'X' : not used)"
  correctness_input <- getLine

  let possibilities = generate_possibilities (zip word_input $ convert_correctness_string correctness_input) remaining_words
  putStrLn $ "Number of possibilties: " ++ (show . length) possibilities
  print possibilities

  wordle_solve possibilities


main :: IO ()
main = do
  wordle_words <- readFile "valid-wordle-words.txt"
  wordle_stats <- readFile "wordle-stats.txt"

  let word_likelihoods = read wordle_stats :: [(String, Integer)]
  wordle_solve $ map fst word_likelihoods