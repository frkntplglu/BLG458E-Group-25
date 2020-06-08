import System.IO
import Control.Monad
import Data.Char
import System.Environment
import System.IO
import Data.Maybe
import Data.List
import Data.Ord

data Ninja = Ninja {name:: String, country:: Char,
                    status:: String, exam1:: Float,
                    exam2:: Float, ability1:: String, ability2:: String, r::Int, score::Float} deriving(Show)

fire :: [Ninja] -- add the junior ninjas of Land of Fire to that list
fire = []
lightning :: [Ninja] -- add the junior ninjas of Land of Lightning to that list lightning = []
lightning = []
water :: [Ninja] -- add the junior ninjas of Land of Water to that list
water = []
wind :: [Ninja] -- add the junior ninjas of Land of Wind to that list
wind = []
earth :: [Ninja] -- add the junior ninjas of Land of Earth to that list
earth = []

decAbility :: [Char] -> Float
decAbility "Clone" = 20
decAbility "Hit" = 10
decAbility "Lightning" = 50
decAbility "Vision" = 30
decAbility "Sand" = 50
decAbility "Fire" = 40
decAbility "Water" = 30
decAbility "Blade" = 20
decAbility "Summon" = 50
decAbility "Storm" = 10
decAbility "Rock" = 20

furkanNinjas = [
    Ninja {
        name = "Naruto", 
        country = 'F', 
        status = "Junior", 
        exam1 = 40.0, 
        exam2 = 45.0, 
        ability1 = "Clone", 
        ability2 = "Summon", 
        r = 0, 
        score = 103.5
    },
    Ninja {
        name = "Midare", 
        country = 'W', 
        status = "Junior", 
        exam1 = 35.0, 
        exam2 = 45.0, 
        ability1 = "Hit", 
        ability2 = "Water", 
        r = 0, 
        score = 71.0
    },
    Ninja {
        name = "Midare", 
        country = 'W', 
        status = "Junior", 
        exam1 = 35.0, 
        exam2 = 45.0, 
        ability1 = "Hit", 
        ability2 = "Water", 
        r = 0, 
        score = 45.0
    },
    Ninja {
        name = "Midare", 
        country = 'W', 
        status = "Junior", 
        exam1 = 35.0, 
        exam2 = 45.0, 
        ability1 = "Hit", 
        ability2 = "Water", 
        r = 0, 
        score = 65.0
    }]

parse :: String -> Ninja
parse ninja = 
        let [n,c,e1,e2,a1,a2] = words ninja
            ability1 = decAbility a1
            ability2 = decAbility a2
            score = 0.5 * (read e1) + 0.3 * (read e2) + ability1 + ability2
            country = case c of
                        "Water" -> 'W'
                        "Wind" -> 'N'
                        otherwise -> head c
            status = "Junior"
        in Ninja {name = n,country = country, status = status, exam1 = read e1, exam2 = read e2, ability1 = a1, ability2 = a2, r = 0,score = score}

sortByScore :: [Ninja] -> [Ninja]
sortByScore = sortBy (flip (comparing score))

sortByRound :: [Ninja] -> [Ninja]
sortByRound = sortBy (comparing r)

tellNinja :: Ninja -> String  
tellNinja (Ninja {name = n, country = c, status = s, exam1 = e1, exam2 = e2, ability1 = a1, ability2 = a2, r = r, score = score}) = n ++ ", Score: " ++ show score ++ ", Status: " ++ s ++ ", Round: " ++ show r

isSameCountryCode :: Char -> Char -> Bool
isSameCountryCode c n = c == n

isSameCountry :: Char -> Ninja -> Bool 
isSameCountry c ninja = 
        isSameCountryCode c countryy
        where countryy = (country ninja)
        
filterByCountry :: [Ninja] -> Char -> [Ninja]
filterByCountry xs c =
  filter (isSameCountry (toUpper(c))) xs

sortNinjasOfCountry :: [Ninja] -> Char -> [Ninja]
sortNinjasOfCountry ninjas countryCode =
    (sortByRound (sortByScore (filterByCountry ninjas countryCode)))

controlCountryCode :: Char -> Bool
controlCountryCode c 
    | toUpper c == 'E' || toUpper c == 'L' || toUpper c ==  'W' || toUpper c ==  'N' || toUpper c ==  'F' = True
    | otherwise = False
main = do
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        args <- getArgs
        case args of 
            [file] -> do
                handle <- readFile file
                let fileLines = lines handle
                let ninjas = sortByRound ( sortByScore (map parse fileLines) )
                printMenu ninjas
          --  _ -> putStrLn "Wrong number of arguments"
            _  -> do -- Delete this block before submit
                handle <- readFile "csereport.txt"
                let fileLines = lines handle
                let ninjas = sortByRound ( sortByScore (map parse fileLines) )
                printMenu ninjas

sortAllNinjas :: [Ninja] -> [Ninja]
sortAllNinjas ninjas = 
    sortByRound (sortByScore (ninjas))

printMenu :: [Ninja] -> IO()
printMenu ninjas = do
    putStrLn "\na) View a County's Ninja Information"  
    putStrLn "b) View All Countries' Ninja Information"  
    putStrLn "c) Make a Round Between Ninjas"  
    putStrLn "d) Make a Round Between Countries"  
    putStrLn "e) Exit"  
    do
        putStr "Enter the action: "
        command <- getChar
        case toUpper command of
            'A'     -> do   putStr "\nEnter the country code: "
                            countryCode <- getChar
                            case controlCountryCode countryCode of
                                True -> do
                                    putStr "\n" 
                                    let countryNinjas = (sortNinjasOfCountry ninjas countryCode)
                                    mapM_ putStrLn (map tellNinja countryNinjas)
                                False -> putStrLn "\nInvalid Country Code"
                            printMenu ninjas  
            'B'     -> do   putStr "\n"
                            let allNinjas = sortAllNinjas ninjas 
                            mapM_ putStrLn (map tellNinja allNinjas)
                            printMenu ninjas 
            'C'     -> do   putStr "\nEnter the name of the first ninja: "
                            nameNinja1 <- getLine
                            putStr "Enter the country code of the first ninja: "
                            countryNinja1 <- getChar  
                            case (findNinja ninjas nameNinja1 countryNinja1) of 
                                Just n1 -> do 
                                    putStr "\nEnter the name of the second ninja: "
                                    nameNinja2 <- getLine
                                    putStr "Enter the country code of the second ninja: "
                                    countryNinja2 <- getChar
                                    case (findNinja ninjas nameNinja2 countryNinja2) of
                                            Just n2 -> do
                                                let [newList,winner] = (makeRoundBetweenTwoNinjas ninjas n1 n2)
                                                printWinner (head winner)
                                                printMenu newList
                                            Nothing -> putStrLn "\nInvalid Ninja2"
                                Nothing -> putStrLn "\nInvalid Ninja1"
                            putStr "\n"
                            printMenu ninjas
            'D'     -> do   putStr "\nEnter the first country code: "
                            countryCode1 <- getChar
                            case controlCountryCode countryCode1 of
                                True -> do 
                                    putStr "\nEnter the second country code: "
                                    countryCode2 <- getChar
                                    case controlCountryCode countryCode2 of
                                        True -> do
                                            let countryNinjas1 = (sortNinjasOfCountry ninjas countryCode1)
                                            let countryNinjas2 = (sortNinjasOfCountry ninjas countryCode2)
                                            let [newList,winner] = (makeRoundBetweenTwoCountries ninjas countryNinjas1 countryNinjas2)
                                            printWinner (head winner)
                                            printMenu newList
                                        False -> do
                                            putStrLn "Invalid Country Code"
                                False -> putStrLn "Invalid Country Code" 
                            printMenu ninjas
            'E'     -> do   print("Exit")
                            printMenu ninjas
            otherwise -> do printMenu ninjas

{-
This function checks whether the desired ninja(with name and Country Code) is in the given list.
If it is in the list it returns Just ninja. If it is not in the list it returns Nothing.
case findNinja ninjas nameNinja nameNinja2 of
    Just n -> Do operations using returned ninja(n)
    Nothing -> Request informations again.
-}
findNinja :: [Ninja]-> [Char] -> Char -> Maybe Ninja
findNinja (x:xs) nameOfNinja countryOfNinja
    | ((map toUpper (name x)) == (map toUpper nameOfNinja)) && (country x == (toUpper countryOfNinja)) = Just x
    | otherwise = findNinja (tail (x:xs)) nameOfNinja (toUpper countryOfNinja)
findNinja [] c countryOfNinja = Nothing

{-
This function makes round between two ninjas. Determines the winner of the Round with the whoiswin function.
Then makes call to updateWinnerInNinjaList function to update list.
-}
makeRoundBetweenTwoNinjas :: [Ninja] -> Ninja -> Ninja -> [[Ninja]]
makeRoundBetweenTwoNinjas allNinjas ninja1 ninja2 =
    let [winner,loser] = (whoIsWin ninja1 ninja2)
    in (updateWinnerInNinjaList allNinjas winner loser)

{-
This function returns a list that the winning(has high score) is in the first place and the loser(has lower score) is in the second place.
-}
whoIsWin :: Ninja -> Ninja -> [Ninja]
whoIsWin ninja1 ninja2
    | (score ninja1) > (score ninja2) = [ninja1,ninja2]
    | (score ninja1) < (score ninja2) = [ninja2,ninja1]

updateWinnerInNinjaList :: [Ninja] -> Ninja -> Ninja -> [[Ninja]]
updateWinnerInNinjaList allNinjas winner loser =
    let updatedWinner = controlNumberOfRoundOfNinja (incrementRoundofWinner winner)
    in [deleteLoserInNinjaList ((otherNinjas allNinjas updatedWinner) ++ [updatedWinner] ++ reverse (otherNinjas (reverse allNinjas) updatedWinner)) loser,[updatedWinner]]

{-
This function returns the ninjas behind or in front of given ninja.
    -If it is desired to return the ninjas behind of given ninja.
    otherNinjas ninjaList ninja
    -If it is desired to return the ninjas in front of given ninja.
    reverse (otherNinjas (reverse ninjaList) ninja)
-}
otherNinjas::[Ninja]->Ninja->[Ninja]
otherNinjas (x:xs) c
    | name x == name c = xs
    | otherwise = otherNinjas (tail (x:xs)) c

incrementRoundofWinner :: Ninja -> Ninja
incrementRoundofWinner ninja =
    let round = (r ninja)+1
    in Ninja {name = name ninja,country = country ninja, status = status ninja, exam1 = (exam1 ninja), exam2 = (exam2 ninja), ability1 = (ability1 ninja), ability2 = (ability2 ninja), r = round,score = score ninja}

controlNumberOfRoundOfNinja :: Ninja -> Ninja
controlNumberOfRoundOfNinja ninja
    | r ninja == 3 =
    let status = "Journeyman"
    in Ninja {name = name ninja,country = country ninja, status = status, exam1 = (exam1 ninja), exam2 = (exam2 ninja), ability1 = (ability1 ninja), ability2 = (ability2 ninja), r = r ninja,score = score ninja}
    | otherwise = ninja
    
deleteLoserInNinjaList :: [Ninja] -> Ninja -> [Ninja]
deleteLoserInNinjaList allNinjas loser = ((otherNinjas allNinjas loser) ++ reverse (otherNinjas (reverse allNinjas) loser))
    
makeRoundBetweenTwoCountries :: [Ninja] -> [Ninja] -> [Ninja] -> [[Ninja]]
makeRoundBetweenTwoCountries allNinjas country1 country2 =
    let [winner,loser] = (whoIsWin (head country1) (head country2))
    in (updateWinnerInNinjaList allNinjas winner loser)

printWinner :: Ninja -> IO()
printWinner ninja = do 
    putStrLn $ "\nWinner: " ++ name ninja ++ ", Round: " ++ show (r ninja) ++ ", Status: " ++ status ninja

instance Show (a -> b) where
         show a= "funcion"

