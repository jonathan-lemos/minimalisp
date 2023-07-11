module Minimalisp.Control.Terminal where

import Control.Monad.Free
import System.Console.ANSI
import qualified System.Console.Terminal.Size as ST
import System.Exit

data TerminalDimensions = TerminalDimensions
  { height :: Int,
    width :: Int
  }

data PrintCmd = PrintCmd [SGR] String
  deriving (Eq)

instance Show PrintCmd where
  show :: PrintCmd -> String
  show (PrintCmd _ s) = s

data TerminalF a
  = Dimensions (Maybe TerminalDimensions -> a)
  | InputLine (Maybe String -> a)
  | Print PrintCmd a
  | Exit

instance Functor TerminalF where
  fmap f v = case v of
    InputLine s -> InputLine $ f . s
    Dimensions d -> Dimensions $ f . d
    Print pc a -> Print pc $ f a
    Exit -> Exit

type Terminal = Free TerminalF

run :: Terminal a -> IO a
run (Pure a) = return a
run (Free (InputLine fn)) = getLine >>= (run . fn) . Just
run (Free (Dimensions fn)) =
  let mapWindow w = TerminalDimensions {height = ST.height w, width = ST.width w}
      getDims = (fmap . fmap) mapWindow ST.size
   in getDims >>= run . fn
run (Free (Print p a)) =
  let printIo (PrintCmd sgr txt) = do
        setSGR sgr
        putStr txt
        setSGR [Reset]
   in printIo p >> run a
run (Free Exit) = exitSuccess

inputLine :: Terminal (Maybe String)
inputLine = liftF $ InputLine id

dimensions :: Terminal (Maybe TerminalDimensions)
dimensions = liftF $ Dimensions id

printCmd :: PrintCmd -> Terminal ()
printCmd cmd = liftF $ Print cmd ()

printColoredText :: [SGR] -> String -> PrintCmd
printColoredText = PrintCmd

printText :: String -> PrintCmd
printText = PrintCmd []

printColoredLine :: [SGR] -> String -> PrintCmd
printColoredLine sgr = printColoredText sgr . (<> "\n")

printLine :: String -> PrintCmd
printLine = printColoredLine []

printNewLine :: PrintCmd
printNewLine = printLine ""

exit :: Terminal a
exit = liftF Exit
