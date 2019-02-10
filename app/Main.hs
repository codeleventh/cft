module Main where

import Lib
import Options.Applicative
import Text.Read
import Data.Maybe
import System.IO

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
  <*> (flag' I (short 'i') <|> flag' S (short 's'))
  <*> some (strArgument (metavar "FILE(S)"))

mergesort :: Ord a => [a] -> [a]
mergesort []   = []
mergesort [a]  = [a]
mergesort list = merge (mergesort left) (mergesort right)
  where
    (left, right) = split list [] []
      where
        split [] l r = (l,r)
        split (x:xs) l r = split xs (x:r) l
    merge l [] = l
    merge [] r = r
    merge l@(x:xs) r@(y:ys) = if x <= y
                                then x : merge xs r
                                else y : merge l ys
main :: IO ()
main = handle =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Sorts the input files"
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
        writeFile out $ unlines $ func (lines content)