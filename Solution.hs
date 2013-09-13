import Data.List.Split

--Lista cu parametri posibili ai operatiilor
f :: [(String, Int)]
f = [("zero", 0), ("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9), ("ten", 10)]

--Functie ce imi intoarce lista cu atomii operatiei
faSplit :: String -> [String]
faSplit string = splitOn "_" string

--Functie ce simuleaza un HashTable cu perechi (Key, Value). Inspired name, I know -_-
hashTable :: String -> [(String, Int)] -> Int
hashTable init [] = 404
hashTable init ((x,y):l) = if x == init then y else hashTable init l

--Functie ce intoarce lista cu rezultatele operatiilor aplicate pe parametrul i: func 4 [add_two, multiply_by_five] = [6, 20]
func :: Int -> [String] -> [Int]
func i [] = []
func i (x:xs) = case faSplit x !! 0 of
		"add" 	    -> (i + (hashTable (faSplit x !! 1) f)):(func i xs)
		"subtract" -> (i - (hashTable (faSplit x !! 1) f)):(func i xs)
		"multiply"  -> (i * (hashTable (faSplit x !! 2) f)):(func i xs)
		"divide"    -> (i `div` (hashTable (faSplit x !! 2) f)):(func i xs)
		_           -> []
