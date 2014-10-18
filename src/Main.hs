{-# LANGUAGE OverloadedStrings #-}

{-
Expect Type of JSON then validate another JSON.

Usage: json-test sample1.json sample2.json [sampleN.json...] test.json
-}

module Main where

import Data.Aeson
import GHC.Exts
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

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
    where jTypes = map expectValue ary
          et = case jTypes of
            t : restTypes -> if all (\x -> t == x) restTypes then t else JUnknown
            [] -> JUnknown

expect :: Value -> [(Maybe String, JType)]
expect (String _) = [(Nothing, JString)]
expect (Number _) = [(Nothing, JNumber)]
expect (Bool _) = [(Nothing, JBool)]
expect (Array a) = [(Nothing, expectArrayType $ toList a)]
expect (Object obj) = list
    where
        f :: (String, Value) -> [(Maybe String, JType)]
        f (k, v) = map (\(kk, vv) -> case kk of
            Just suffix -> (Just $ k ++ "." ++ suffix, vv)
            Nothing -> (Just k, vv)
            ) $ expect v
        f' :: (T.Text, Value) -> [(Maybe String, JType)]
        f' (k, v) = f (T.unpack k, v)
        list = concatMap f' $ toList obj
expect Null = [(Nothing, JUnknown)]

expect' :: Value -> HM.HashMap String JType
expect' value = foldr f HM.empty (expect value)
    where
          f :: (Maybe String, JType) -> HM.HashMap String JType -> HM.HashMap String JType
          f (Just k, v) acc = HM.insert k v acc
          f (Nothing, _) acc = acc

merge2 :: HM.HashMap String JType -> HM.HashMap String JType -> HM.HashMap String JType
merge2 = HM.unionWith f
    where
        f :: JType -> JType -> JType
        f a b
            | a == b = a
            | otherwise = JUnknown

merge :: [HM.HashMap String JType] -> HM.HashMap String JType
merge = foldr merge2 HM.empty

main :: IO ()
main = do
    print testValue
    print $ testValue >>= Just . expect'

