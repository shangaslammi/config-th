{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Test.Hspec
import Test.Hspec.HUnit

import Data.Config.TH

data ConfTest1 = ConfTest1
    { stringField :: String
    , intField    :: Int
    , floatField  :: Float
    , optionalInt :: Maybe Int
    }

mkConfig ''ConfTest1

confTest1 = unlines $
    [ "stringField = foobar"
    , "intField    = 10"
    , "floatField  = 2.5"
    , "optionalInt = 4"
    ]

main = hspec $ describe "Config-TH" $
    [ it "can parse basic named fields" $ do
        let (Right cfg) = parseConf confTest1 :: Either ConfigError ConfTest1
        (cfg @@ stringField) @?= "foobar"
        (cfg @@ intField)    @?= 10
        (cfg @@ floatField)  @?= 2.5
        (cfg @@ optionalInt) @?= Just 4
    ]
