import System.Random
import Control.Monad.RWS

type Dice = Int
type DiceGame = RWS
                (Int, Int) -- Reader (dice bounds)
                [Dice]     -- Writer (a history of rolls)
                StdGen     -- State (random generator)

dice :: DiceGame Dice
dice = do
  bds <- ask
  g <- get
  let (r, g') = randomR bds g
  put g'
  r <- state (randomR bds) -- whenever our state is StdGen,
                           -- we can get generate a stateful computation
                           -- without explicitly using 'get' and 'put'
  tell [r]
  pure r

diceSimpler :: DiceGame Dice
diceSimpler = do
  bds <- ask
  r <- state (randomR bds) -- whenever our state is StdGen,
                           -- we can get generate a stateful computation
                           -- without explicitly using 'get' and 'put'
  tell [r]
  pure r

diceOneLiner :: DiceGame Dice
diceOneLiner = ask >>= state . randomR >>= \r -> tell [r] >> pure r

doubleDice :: DiceGame (Dice, Dice)
doubleDice = (,) <$> dice <*> dice

dices :: Int -> DiceGame [Dice]
dices n = replicateM n dice

main = do
  putStrLn "Running dice"
  g <- newStdGen
  let roll = evalRWS (dices 10) (1, 6) g
  putStrLn $ "Rolls are: " <> show roll
  putStrLn "Done.\n"
