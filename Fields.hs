module Fields where

-- This allows the ord of an input - ord zero to give the correct output for the game
ordZero :: Int
ordZero = 48

-- Default field that will be used to generate the game
fieldSize :: Int
fieldSize = 5

-- Default Health and Attack to prevent hardcoding - allows developer changes in the future
defaultHealth :: Int
defaultHealth = 100

defaultAttack :: Int
defaultAttack = 10

-- unable to randomise enemies so a default selection was made
defaultEnemies :: [Enemy]
defaultEnemies = [[Position 1 4], [Position 1 5], [Position 2 2], [Position 3 1], [Position 4 5], [Position 5 1]]

-- Default enemy and boss health/attack to prevent hardcoding - allows developer changes in the future
enemyHealth :: Int
enemyHealth = 20

-- only used when there is one enemy left to create a challenge
bossHealth :: Int
bossHealth = 100

-- the user gets a damage of this rate
enemyDamage :: Int
enemyDamage = 20

-- the field consists of true or false to ensure that the enemies are marked as defeated (true) to prevent refighting the same enemy
type Field = [[Bool]]

-- Enemy consists of a x and y co-ordiante
type Enemy = [Position]

-- the player is in record syntax to allow access to its own data it contains
data Player = Player {name :: String, health :: Int, atk :: Int}
  deriving (Eq, Show)

-- Represents a position on the board with x and y coordinates.
data Position = Position {x :: Int, y :: Int}
  deriving (Eq, Show)

-- a set of moves to access
data Moves = GodFist | LightAttack | CursedAttack | Block

-- data Enemy = Enemy {position :: Position, hp :: Int}

-- Random Number Generator (system.random did not work and this was not used due to its high computation level.)
generate :: Int -> [Int]
generate = iterate (\n -> 224149 * n + 1)