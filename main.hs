import System.IO
import Control.Monad
import Data.Char
import System.Environment
import System.IO
import Data.Maybe

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

merge :: [Ninja] -> [Ninja] -> [Ninja]
merge x [] = x
merge [] y = y  
merge (x:xs) (y:ys) = x : y : merge xs ys

isSameCountryCode :: Char -> Char -> Bool
isSameCountryCode c n = c == n

isSameCountry :: Char -> Ninja -> Bool 
isSameCountry c ninja = 
        isSameCountryCode c countryy
        where countryy = (country ninja)
        
filterByCountry :: [Ninja] -> Char -> [Ninja]
filterByCountry xs c =
  filter (isSameCountry c) xs

main = do
        handle <- readFile "csereport.txt"
        let fileLines = lines handle
        let ninjas = map parse fileLines
        ---print(ninjas)
        printMenu ninjas

printMenu :: [Ninja] -> IO()
printMenu ninjas = do
    putStrLn "a) View a County's Ninja Information"  
    putStrLn "b) View All Countries' Ninja Information"  
    putStrLn "c) Make a Round Between Ninjas"  
    putStrLn "d) Make a Round Between Countries"  
    putStrLn "e) Exit"  
    putStr "Enter the action: "
    do
        command <- getChar
        case toUpper command of
            'A'     -> do   putStr "\nEnter the country code: "
                            c <- getChar
                            print("Print Ninjas on %d",toUpper c)
                            let ls = (filterByCountry ninjas (toUpper c))
                            printMenu ninjas  
            'B'     -> do   print("Print Ninjas of all countries")
                            printMenu ninjas 
            'C'     -> do   putStr "\nEnter the name of the first ninja: "
                            nameNinja1 <- getLine
                            putStr "Enter the country code of the first ninja: "
                            countryNinja1 <- getChar  
                            putStr "\nEnter the name of the second ninja: "
                            nameNinja2 <- getLine
                            putStr "Enter the country code of the second ninja: "
                            countryNinja2 <- getChar
                            case (findNinja ninjas nameNinja1 countryNinja1) of 
                                Just n1 -> case (findNinja ninjas nameNinja2 countryNinja2) of
                                            Just n2 -> do
                                                let [newList,winner] = (makeRoundBetweenTwoNinjas ninjas n1 n2)
                                                printWinner (head winner)
                                                print(newList)
                                                printMenu newList
                                            Nothing -> putStrLn "Invalid Ninja2"
                                Nothing -> putStrLn "Invalid Ninja1"
            'D'     -> do   print("Make a round between two countries")
                            printMenu ninjas
            'E'     -> do   print("Exit")
                            printMenu ninjas

{-
This function checks whether the desired ninja(with name and Country Code) is in the given list.
If it is in the list it returns Just ninja. If it is not in the list it returns Nothing.
case findNinja ninjas nameNinja nameNinja2 of
    Just n -> Do operations using returned ninja(n)
    Nothing -> Request informations again.
-}
findNinja :: [Ninja]-> [Char] -> Char -> Maybe Ninja
findNinja (x:xs) nameOfNinja countryOfNinja
    | (name x == nameOfNinja) && (country x == countryOfNinja) = Just x
    | otherwise = findNinja (tail (x:xs)) nameOfNinja countryOfNinja
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
    let updatedWinner = (incrementRoundofWinner winner)
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



---CONSOLE TEST CASES---
-- let objects = [Ninja {name = "Naruto", country = 'F', status = "Junior", exam1 = 40.0, exam2 = 45.0, ability1 = "Clone", ability2 = "Summon", r = 0, score = 103.5},Ninja {name = "Sasuke", country = 'F', status = "Junior", exam1 = 50.0, exam2 = 60.0, ability1 = "Lightning", ability2 = "Fire", r = 0, score = 133.0},Ninja {name = "Neiji", country = 'F', status = "Junior", exam1 = 40.0, exam2 = 75.0, ability1 = "Vision", ability2 = "Hit", r = 0, score = 82.5},Ninja {name = "Gaara", country = 'N', status = "Junior", exam1 = 55.0, exam2 = 80.0, ability1 = "Vision", ability2 = "Sand", r = 0, score = 131.5},Ninja {name = "Temari", country = 'N', status = "Junior", exam1 = 40.0, exam2 = 60.0, ability1 = "Hit", ability2 = "Blade", r = 0, score = 68.0},Ninja {name = "Kankuro", country = 'N', status = "Junior", exam1 = 30.0, exam2 = 50.0, ability1 = "Hit", ability2 = "Storm", r = 0, score = 50.0},Ninja {name = "Midare", country = 'W', status = "Junior", exam1 = 35.0, exam2 = 45.0, ability1 = "Hit", ability2 = "Water", r = 0, score = 71.0},Ninja {name = "Suiu", country = 'W', status = "Junior", exam1 = 45.0, exam2 = 55.0, ability1 = "Water", ability2 = "Blade", r = 0, score = 89.0},Ninja {name = "Samidare", country = 'W', status = "Junior", exam1 = 30.0, exam2 = 55.0, ability1 = "Water", ability2 = "Hit", r = 0, score = 71.5},Ninja {name = "Haruki", country = 'E', status = "Junior", exam1 = 50.0, exam2 = 65.0, ability1 = "Blade", ability2 = "Rock", r = 0, score = 84.5},Ninja {name = "Miyazaki", country = 'E', status = "Junior", exam1 = 45.0, exam2 = 55.0, ability1 = "Rock", ability2 = "Hit", r = 0, score = 69.0},Ninja {name = "Hiroshi", country = 'E', status = "Junior", exam1 = 40.0, exam2 = 60.0, ability1 = "Storm", ability2 = "Rock", r = 0, score = 68.0},Ninja {name = "Sana", country = 'L', status = "Junior", exam1 = 55.0, exam2 = 65.0, ability1 = "Lightning", ability2 = "Hit", r = 0, score = 107.0},Ninja {name = "Aimi", country = 'L', status = "Junior", exam1 = 60.0, exam2 = 65.0, ability1 = "Blade", ability2 = "Rock", r = 0, score = 89.5},Ninja {name = "Kira", country = 'L', status = "Junior", exam1 = 40.0, exam2 = 60.0, ability1 = "Storm", ability2 = "Rock", r = 0, score = 68.0}]

-- let fire = (filterByCountry objects 'F')
-- let lightning = (filterByCountry objects 'L')
-- let water = (filterByCountry objects 'W')
-- let wind = (filterByCountry objects 'N')
-- let earth = (filterByCountry objects 'E')

-- makeRoundBetweenTwoCountries objects fire water

-- let ninja1 = Ninja {name = "Naruto", country = 'F', status = "Junior", exam1 = 40.0, exam2 = 45.0, ability1 = "Clone", ability2 = "Summon", r = 0, score = 103.5}
-- let ninja2 = Ninja {name = "Kankuro", country = 'N', status = "Junior", exam1 = 30.0, exam2 = 50.0, ability1 = "Hit", ability2 = "Storm", r = 0, score = 50.0}
-- let ninja3 = Ninja {name = "Gaara", country = 'N', status = "Junior", exam1 = 55.0, exam2 = 80.0, ability1 = "Vision", ability2 = "Sand", r = 0, score = 131.5}
-- let ninja4 = Ninja {name = "Suiu", country = 'W', status = "Junior", exam1 = 45.0, exam2 = 55.0, ability1 = "Water", ability2 = "Blade", r = 0, score = 89.0}

-- makeRoundBetweenTwoCountries objects fire water


-- [Ninja] -> Ninja -> Ninja -> [Ninja]
-- makeRoundBetweenTwoNinjas allNinjas ninja1 ninja2
-- Iki Ninja arasinda round yapip yeni listeyi ve kazanan ninjayi dondurecek fonksiyon
    -- Eski Listeden kazanan Ninjayi Bulacak ve round sayisini arttiracak,kaybeden ninjayi silecek fonksiyon
    -- Kazanani print edecek

-- [Ninja]-> Char -> Char -> [Ninja]
-- makeRoundBetweenTwoCountries allNinjas c1 c2
-- Iki Ulkenin ilk ninjalari round yapip yeni listeyi ve kazanan ninjayi dondurecek fonksiyon
    -- Eski Listeden kazanan Ninjayi Bulacak ve round sayisini arttiracak,kaybeden ninjayi silecek fonksiyon
    -- Kazanani print edecek


