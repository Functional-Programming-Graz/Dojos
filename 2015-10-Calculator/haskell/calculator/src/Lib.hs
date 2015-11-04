module Lib
where

import qualified Data.Text as T
import Control.Applicative
import Control.Monad.Identity

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

lexer       = P.makeTokenParser haskellDef    

integer     = P.integer lexer

delimiter = oneOf ",\n"

-- nParse :: Parsec String Int
mainParse = do
  a <- integer
  -- modifyState (+ a)
  md <- optionMaybe delimiter
  b <- case md of
    Just d -> mainParse
    Nothing -> return 0
  return $ a + b

someFunc :: IO ()
someFunc = putStrLn "someFunc"

add :: String -> Int
add "" = 0
add str = case runParser mainParse 0 "test" str of
            Left err -> error $ show err
            Right result -> fromIntegral result 

add' :: String -> Int
add' "" = 0
add' str' = sum $ map (read . T.unpack) $ T.split (\c -> c == '\n' || c == ',') $ T.pack str'



