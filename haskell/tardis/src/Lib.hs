{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( someFunc
    ) where

import Control.Monad.Tardis
import Data.Map.Lazy
import Prelude hiding (lookup)
import Data.Maybe

someFunc :: IO ()
someFunc = print $ runTardis test (empty, empty) 

type Info = ([String], Int)
type Env = Map String Int

type EnvM = Tardis Env Env


getValue :: String -> EnvM (Maybe Int)
getValue s = lookup s <$> getAll

defineConst :: String -> Int -> EnvM Int
--defineConst s i = tardis (\(bw, fw) -> (i, (insert s i bw, insert s i fw)))
defineConst s i = do
    modifyBackwards (insert s i)
    modifyForwards (insert s i)
    return i

getAll :: EnvM Env
getAll = union <$> getPast <*> getFuture
    
op :: (Int -> Int -> Int) -> String -> String -> String -> EnvM Int
op operation name param1 param2 = do
    dict <- getAll
    let Just val1 = lookup param1 dict
        Just val2 = lookup param2 dict
        result = operation val1 val2
    defineConst name result
    return result

add = op (+)
sub = op (-)

--test :: EnvM ()
test = do  
    v <- getValue "k" 
    defineConst "f" $ fromMaybe 0 v
    defineConst "i" 2
    defineConst "j" 3
    add "sum" "i" "j"
    defineConst "k" 5
    add "sum2" "k" "sum"

    add "t" "f1" "f2"
    defineConst "f1" 1000
    defineConst "f2" 2000
    defineConst "f1" 0
    getValue "t"

data Expression = Define String Int | Add String String String | Sub String String String | Value String 


trans (Define s i) = defineConst s i
trans (Add s1 s2 s3) = add s1 s2 s3
trans (Sub s1 s2 s3) = sub s1 s2 s3
trans (Value s) = fromJust <$> getValue s

