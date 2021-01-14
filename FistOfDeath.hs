import Data.Char (ord)
import Fields
import System.Exit

-- Select the n-th element in a list
select :: Int -> [a] -> a
select n xs = head (drop (n -1) (take n xs))

-- Change n-th element in a list
replace :: Int -> [a] -> a -> [a]
replace n xs x = take (n -1) xs ++ [x] ++ drop n xs

arena :: Field
arena = take fieldSize $ repeat $ take fieldSize $ repeat False

-- Converts user's desired strings for use (ordZero ensures the value is exactly the User's input)
convertStringToCoordinates :: String -> Position
convertStringToCoordinates ['(', x, ',', y, ')'] = Position (ord x - ordZero) (ord y - ordZero) -- usable for our functions
convertStringToCoordinates _ = Position (-1) (-1) -- if input not valid it will return -1 -1 to show its not valid and get a valid input

-- ensure the Co-ordinate is within the 10x10 grid
validateCoordinate :: Position -> Bool
validateCoordinate (Position x y)
  | x >= 1 && y >= 1 && x <= fieldSize && y <= fieldSize = True
  | otherwise = False

-- Enemy Functions

-- Input all enemies from a static list
inputEnemies :: [Enemy]
inputEnemies = defaultEnemies

-- remove an enemy from the enemy list
removeEnemy :: [Enemy] -> [Enemy]
removeEnemy [] = []
removeEnemy (x : xs)
  | null x = removeEnemy xs
  | otherwise = x : removeEnemy xs

-- when there is only one enemy left, the enemy becomes a boss enemy to create a challenge for the player
isBossEnemy :: [Enemy] -> Int
isBossEnemy enemies
  | length enemies == 1 = bossHealth
  | otherwise = enemyHealth

-- mark the given field as found/defeated based on x and y coordinates
markDefeated :: Field -> Int -> Int -> Field
markDefeated field x y = replace x field (replace y (select x field) True)

-- check if the enemy has been defeated on the field based on the position
checkEnemyDefeated :: Field -> Enemy -> Position -> (Enemy, Bool)
checkEnemyDefeated theField enemy (Position x y) =
  if not (or [Position x y == coord | coord <- enemy])
    then do
      (enemy, False) -- Miss
    else do
      if and [select x (select y theField) == True | coord <- enemy, coord /= Position x y] == False
        then (enemy, True) -- Found, but not defeated
        else ([], True) -- Found and defeated

-- Game Functions

-- uses markDefeated, remove enemy and check if enemy was defeated to ensure that the attack was updated on the field given
attack :: (Field, [Enemy]) -> Position -> (Field, [Enemy], Bool)
attack (theField, enemies) (Position x y) =
  ( markDefeated theField x y,
    removeEnemy [fst (checkEnemyDefeated theField enemy (Position x y)) | enemy <- enemies],
    or [snd (checkEnemyDefeated theField enemy (Position x y)) | enemy <- enemies]
  )

-- gives the user the choices required to defeat the enemy by reducing its enemy hp to 0.
combat :: (Player, Field, [Enemy]) -> Int -> IO ()
combat (player, theField, enemies) 0 = return ()
combat (player, theField, enemies) hp = do
  if isDead player -- if the player reaches 0 or below health the game shall end automatically
    then do
      putStrLn ("You have died in the arena. Play again " ++ name player ++ " to conquer the arena!")
      exitSuccess
    else putStrLn ("Defend Yourself! Enemy: " ++ show hp ++ "HP (Enter the number linked to your choice)")
  putStrLn (printPlayer player)
  putStrLn " 1) Light Attack (10 damage)\n 2) Godfist (50 damage)\n 3) Cursed Attack (20 damage, -10 hp)\n 4) Block (0 damage, +5hp)"
  val <- getLine
  let choice = (read val :: Int) -- allows the input ([char]) to be read as a Int
  let move = chooseMove choice -- collects the assigned move based on user input if possible
  if validateChoice choice -- check if the choice is 1-4
    then do
      let newplayer = evaluatePlayerHealth (getMove move) player -- ensure the enemy damage is updated within the enemy fight only
      putStrLn (printMove move) -- display the damage the user did to enemy
      putStrLn ("The enemy is powerful, you suffered a wound losing " ++ show enemyDamage ++ " HP")
      if evaluateMove move hp == 0 -- ensure the enemy is defeated, evaluateMove handles if hp goes below 0
        then putStrLn "Enemy has been defeated"
        else combat (newplayer, theField, enemies) (evaluateMove move hp)
      return ()
    else combat (player, theField, enemies) hp -- if choice not valid, recursion occurs and asks for an input again

-- returns the player
returnPlayer :: Player -> Player
returnPlayer p = p

-- checks if the enemy hp has reached 0 (sometimes the moves given can reach below 0 and must be handled for smooth operation of combat function)
evaluateMove :: Moves -> Int -> Int
evaluateMove m hp
  | hp - getMove m < 0 = 0
  | otherwise = hp - getMove m

-- ensures the option given is within 1-4
validateChoice :: Int -> Bool
validateChoice i
  | i > 4 = False
  | i == 0 = False
  | otherwise = True

-- coordinates are taken from the user and converted to be accepted by the functions and then attack and combat functions are executed
-- to allow the user to find and fight the enemies based on co-ordinates
findEnemies :: (Player, Field, [Enemy]) -> IO (Field, [Enemy])
findEnemies (player, theField, enemies) =
  do
    putStrLn "Enter the coordinates to find an enemy (x,y) within the Grid"
    string <- getLine
    let coord = convertStringToCoordinates string
    if validateCoordinate coord -- check the input is valid based on the grid
      then do
        let (theNewField, newEnemies, hit) = attack (theField, enemies) coord -- attack updates the fields and ensuring its valid then returns the updated field for use
        if hit -- if the enemy is found
          then do combat (player, theNewField, newEnemies) (isBossEnemy enemies) -- begin the fight with the options
          else putStrLn ("Going to coordinate (" ++ show coord ++ "), No Enemies")
        return (theNewField, newEnemies)
      else findEnemies (player, theField, enemies)

-- The player

-- Input the name of the player
inputNames :: IO String
inputNames = do
  putStrLn "What is your name?"
  getLine

-- Inspiration from https://mmhaskell.com/blog/2016/12/17/making-your-own-data-types-in-haskell articles
-- generate the player based on name given
generatePlayer :: String -> Player
generatePlayer name = Player name defaultHealth defaultAttack

-- Print all the details about the player
printPlayer :: Player -> String
printPlayer p =
  name p ++ "'s Health: "
    ++ show (health p)
    ++ ", Attack Power: "
    ++ show (atk p)

-- Every turn the player loses 20hp due to the enemy's power
-- depending on their choice they may lose more than 20hp
evaluatePlayerHealth :: Int -> Player -> Player
evaluatePlayerHealth damage player
  | damage == 20 = player {health = health player - enemyDamage}
  | damage == 0 = player {health = health player - enemyDamage + 10}
  | otherwise = player {health = health player - (enemyDamage + 10)}

-- if the player reaches 0 HP, they are dead and the game must end.
isDead :: Player -> Bool
isDead player
  | health player < 0 = True
  | otherwise = False

-- take an input and return the type of move the user has chosen
chooseMove :: Int -> Moves
chooseMove i
  | i == 1 = LightAttack
  | i == 2 = GodFist
  | i == 3 = CursedAttack
  | i == 4 = Block
  | otherwise = chooseMove i

-- based on user's move choice the damage (int) is created to be used in evaluate move (combat)
getMove :: Moves -> Int
getMove LightAttack = defaultAttack
getMove GodFist = defaultAttack * 5
getMove CursedAttack = defaultAttack * 2
getMove Block = 0

-- show the user how much damage they will do on the enemy
printMove :: Moves -> String
printMove LightAttack = "You chose Light Attack, it hits with " ++ show defaultAttack ++ " damage"
printMove GodFist = "Special Move: God Fist, it hits with " ++ show (defaultAttack * 5) ++ " damage"
printMove CursedAttack = "The Cursed Attack hits with " ++ show (defaultAttack * 2) ++ " damage, you lost 5 HP!"
printMove Block = "Blocked Enemy's Attack, you only lost 10HP instead of 20HP"

-- this loads the findEnemies function to begin the game and recurvisely loops until there are no remaining enemies avaliable on the field
-- once there is no enemies they win and the game ends
play :: Player -> Field -> [Enemy] -> IO ()
play player theField enemies = do
  putStrLn (name player ++ " its time to find the enemies")
  (newField, newEnemies) <- findEnemies (player, theField, enemies)
  putStrLn ("Enemies Remaining: " ++ show (length newEnemies))
  if null newEnemies
    then do
      putStrLn ("Congratulations! " ++ name player ++ " you have defeated all enemies in the Arena\n")
      exitSuccess
    else play player newField newEnemies

-- will allow the user input and generate the game to play (by executing the play function)
main :: IO ()
main = do
  input <- inputNames
  let user = generatePlayer input
  putStrLn "This arena is a 5x5 grid, be ready to fight"
  putStrLn ("Welcome to the arena " ++ name user ++ " your chance has arrived")
  play user arena inputEnemies