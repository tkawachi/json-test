{-# LANGUAGE OverloadedStrings #-}

{-
Expect Type of JSON then validate another JSON.

Usage: json-test sample1.json sample2.json [sampleN.json...] test.json
-}

module Main where

import Data.Aeson
import GHC.Exts
import qualified Data.Text as T

-- Expected JSON Types
data JType = JString | JNumber | JBool | JArray JType | JUnknown
    deriving (Read, Show, Eq)

testValue :: Maybe Value
testValue = decode "{\"foo\": [\"abc\",\"def\"], \"bar\":123.11, \"baz\":{\"abc\":true}}" :: Maybe Value

expectValue :: Value -> JType
expectValue (String _) = JString
expectValue (Number _) = JNumber
expectValue (Bool _) = JBool
-- If all elements of Array is the same type, then use it otherwise use unknown.
expectValue (Array a) = expectArrayType $ toList a
expectValue (Object _) = JUnknown
expectValue Null = JUnknown

-- If all elements of Array is the same type, then use it otherwise use unknown.
expectArrayType :: [Value] -> JType
expectArrayType ary = JArray et
    where jTypes = map expectValue $ ary
          et = case jTypes of
            t : restTypes -> if all (\x -> t == x) restTypes then t else JUnknown
            [] -> JUnknown

expect :: Value -> [(String, JType)]
expect (String _) = [("", JString)]
expect (Number _) = [("", JNumber)]
expect (Bool _) = [("", JBool)]
expect (Array a) = [("", expectArrayType $ toList a)]
expect (Object obj) = list
    where
        f :: (String, Value) -> [(String, JType)]
        f (k, v) = map (\(kk, vv) -> (k ++ (if kk == "" then kk else "." ++ kk), vv)) $ expect v
        f' :: (T.Text, Value) -> [(String, JType)]
        f' (k, v) = f (T.unpack k, v)
        list = concat $ map f' $ toList obj
expect Null = [("", JUnknown)]

main :: IO ()
main = do
    print testValue
    print $ testValue >>= Just . expect

