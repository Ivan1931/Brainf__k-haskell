import Text.ParserCombinators.Parsec
import Control.Monad.State
import Control.Monad.Identity
import Data.Char(ord)
import qualified Data.Map as M

--
--Inc - increments the program counter to the cell to the right
--Dec - increments the program counter to the cell to the left
--Add - increments the byte at the data pointer by one
--Sub - decrements the byte at the data pointer by one
--Out - Outputs byte at pointer
--In  - Takes a single byte of input and stores it at the current location
-- the program counter
--

type Env = M.Map Int Int

data Command = Inc | Dec | Add | Sub | Out | In | Loop [Command]
             deriving(Eq, Show)

--Need to keep track of, current register, current instruction
type Context a = StateT Int (StateT Env IO) a

inc :: (Num a) => a -> a
inc = (+1)

dec :: (Num a) => a -> a
dec = (-) 1

evalBf :: [Command] -> Context ()
evalBf [] = return ()
evalBf (Inc:xs) = do modify inc
                     evalBf xs
evalBf (Dec:xs) = do modify dec
                     evalBf xs
evalBf (Add:xs) = do reg <- get
                     env <- lift $ get
                     if reg `M.member` env
                       then lift $ modify (M.adjust inc reg)
                       else lift $ modify (M.insert reg 1)
                     evalBf xs

evalBf (Sub:xs) = do reg <- get
                     env <- lift $ get
                     if reg `M.member` env
                       then lift $ modify (M.adjust dec reg)
                       else lift $ modify (M.insert reg 1)
                     evalBf xs

evalBf (Out:xs) = do env <- lift $ get
                     reg <- get
                     case reg `M.lookup` env of
                       Nothing -> liftIO $ print "0"
                       Just a  -> liftIO $ print . show $ a
                     evalBf xs

evalBf (In:xs) = do n <- liftIO $ getChar
                    reg <- get
                    lift $ modify (M.adjust (\ _ -> ord n) reg)
                     env <- lift
                    evalBf xs

evalBf loop@((Loop xs):xss) =
    do env <- lift $ get
       reg <- get
       case reg `M.lookup` env of
         Nothing -> evalBf xss -- Move to next instruction after ]
         Just a  -> if a == 0
                      then evalBf xss
                      else do evalBf xs -- Evaluate the inner loop
                              evalBf loop -- Evaluate the main loop again

runBf commands = evalStateT (evalStateT (evalBf commands) 0) M.empty

opParser :: Parser Command
opParser = do c <- oneOf "><+-.,"
              return $
                case c of
                  '>' -> Inc
                  '<' -> Dec
                  '+' -> Add
                  '-' -> Sub
                  '.' -> Out
                  ',' -> In

whiteSpace = oneOf "\n\0\t"

commandParser :: Parser [Command]
commandParser = many (try loopParser <|> opParser)

loopParser :: Parser Command
loopParser = do char '['
                commands <- commandParser
                char ']'
                return $ Loop commands

parseBf :: String -> Either ParseError [Command]
parseBf input = parse commandParser "Brainf**k ->" input

brainf__k :: String -> IO ()
brainf__k str = case parseBf str of
                  Left err -> putStrLn ("Error: " ++ show err)
                  Right bf -> runBf bf
