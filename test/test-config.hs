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

data ConfTest2 = ConfTest2
    { customField1 :: MyType
    , customField2 :: MyType
    , customField3 :: MyType
    }

data MyType = Foo | Bar | Asdf deriving (Show, Eq)

mkConfig ''ConfTest1
mkConfig ''ConfTest2

confTest1_1 = unlines $
    [ "stringField = foobar"
    , "intField    = 10"
    , "floatField  = 2.5"
    , "optionalInt = 4"
    ]

confTest1_2 = unlines $
    [ "stringField = foobar"
    , "intField    = 10"
    , "floatField  = 2.5"
    ]

confTest1_3 = unlines $
    [ "stringField = foobar"
    , "intField    = 10"
    ]

confTest2_1 = unlines $
    [ "customField1 = foo"
    , "customField2 = bar"
    , "customField3 = asdf"
    ]

main = hspec $ describe "Config-TH" $
    [ it "can parse basic named fields" $ do
        let (Right cfg) = parseConfig confTest1_1
        (cfg @@ stringField) @?= "foobar"
        (cfg @@ intField)    @?= 10
        (cfg @@ floatField)  @?= 2.5
        (cfg @@ optionalInt) @?= Just 4

    , it "returns missing optional fields as Nothing" $ do
        let (Right cfg) = parseConfig confTest1_2
        (cfg @@ stringField) @?= "foobar"
        (cfg @@ intField)    @?= 10
        (cfg @@ floatField)  @?= 2.5
        (cfg @@ optionalInt) @?= Nothing

    , it "reports an error for missing mandatory fields" $ do
        let (Left err) = parseConfig confTest1_3 :: Either ConfigError ConfTest1
        err @?= MissingField "floatField"

    , it "can parse custom algebraic data types" $ do
        let (Right cfg) = parseConfig confTest2_1
        (cfg @@ customField1) @?= Foo
        (cfg @@ customField2) @?= Bar
        (cfg @@ customField3) @?= Asdf
    ]
