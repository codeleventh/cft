module Main where

import Lib
import Options.Applicative
import Text.Read
import Data.Maybe
import System.IO
import Control.Monad

data Intype = I|S -- Int|String
  deriving Eq
data Order = ASC | DESC
  deriving Eq
data Args = Args
  { order   :: Order
  , intype  :: Intype
  , files  :: [String]
  }

args :: Parser Args
args = Args
  <$> (flag ASC DESC (short 'd') <|> flag' ASC (short 'a'))
  <*> (flag' I ( short 'i') <|> flag' S (short 's'))
  <*> some (strArgument (metavar "FILE(S)"))

main :: IO ()
main = handle =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Merge and mergesorts the input files"
     <> header "CFT" )

handle :: Args -> IO ()
handle (Args o i (out:inputs)) =
  if null inputs
    then error "No input files"
      else do
        content <- fmap mconcat (sequence (fmap readFile inputs))
        let f = if o == ASC
                  then id
                  else reverse
        let func list = if i == I
                           then map show $ catMaybes $ mergesort (fmap readMaybe list :: [Maybe Int])
                           else mergesort list
        let ans = if o == ASC
                     then func (lines content)
                     else reverse $ func (lines content)
        when 
          (length ans /= length (lines content) && i == I)
          (hPutStrLn stderr "WARN: some strings haven't been parsed as as integers")
        writeFile out $ unlines ans