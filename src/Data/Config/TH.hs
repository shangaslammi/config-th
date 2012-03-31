{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Config.TH
    ( mkConfig
    , Config(..)
    , ConfigValue(..)
    , ConfigError(..)
    , (@@)
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Applicative
import Control.Monad
import Data.List
import Data.Char (toLower)

data ConfigError
    = MissingField String
    | InvalidValue String
    | SyntaxError
    deriving (Eq, Show)

mkConfig :: Name -> DecsQ
mkConfig name = configInstance where

    configInstance :: DecsQ
    configInstance = [d|
        instance Config $(conT name) where
            parseConfig s = case parseFields s of
                Just fields -> $buildConfig fields
                Nothing     -> Left SyntaxError
        |]

    buildConfig :: ExpQ
    buildConfig = do
        info <- reify name
        case info of
            TyConI (DataD _ _ _ cons _) -> handleRecord "" cons
            _ -> error "mkConfig can only be called for data types"

    handleRecord :: String -> [Con] -> ExpQ
    handleRecord prefix [RecC cname vars] = lam1E (varP parName) expr where
        parName   = mkName "fields"
        (expr, _) = foldl' step (conE cname, [|(<$>)|]) builders
        builders  = map (buildRecField prefix) vars
        fldParam  = varE parName
        step (e,op) bldr = (infixApp e op (appE bldr fldParam), [|(<*>)|])

    handleRecord _ _ = error $
        "mkConfig can only be called for data types with one record constructor"

    consItem :: Con -> ExpQ
    consItem (NormalC name []) = [|(nameStr, $(conE name))|] where
        nameStr = map toLower . nameBase $ name
    consItem _ = error "Constructors for algebraic datatypes cannot take arguments"

    buildRecField :: String -> VarStrictType -> ExpQ
    buildRecField prefix (name, _, typ) = impl where

        impl = case typ of
            ConT tnam -> reify tnam >>= conField
            AppT t _ | t == (ConT ''Maybe) -> optionalField
            _ -> defaultHandler

        conField (TyConI (DataD _ _ _ cons _)) = case cons of
            [RecC _ _] -> nestedRecord cons
            [_]        -> defaultHandler
            _          -> multipleConType cons
        conField _ = defaultHandler

        defaultHandler = [|\fields -> case lookup nameStr fields of
            Just sv -> parseValue sv
            Nothing -> $missingError|]

        optionalField = [|\fields -> case lookup nameStr fields of
            Just sv -> parseValue sv
            Nothing -> Right Nothing|]

        nestedRecord cons = handleRecord (nameStr ++ ".") cons

        multipleConType cons = [|\fields -> case lookup nameStr fields of
            Just sv -> $(lookupConstructor cons) sv
            Nothing -> $missingError|]

        lookupConstructor cons = [|\sv -> case lookup sv $lst of
            Nothing -> Left $ InvalidValue sv
            Just c  -> Right c |] where
                lst = listE . map consItem $ cons

        nameStr = prefix ++ nameBase name
        missingError = [|Left $ MissingField nameStr|]

class Config c where
    parseConfig :: String -> Either ConfigError c
    parseConfig = undefined

(@@) :: (Config c) => c -> (c -> v) -> v
(@@) = flip ($)

infixl 5 @@

parseFields :: String -> Maybe [(String, String)]
parseFields = sequence . map parseLine . map words . lines where
    parseLine [key,"=",value] = Just (key, value)
    parseLine _               = Nothing

class ConfigValue v where
    parseValue :: String -> Either ConfigError v

instance ConfigValue String where
    parseValue = Right

instance ConfigValue Int where
    parseValue = parseReadable

instance ConfigValue Float where
    parseValue = parseReadable

instance (Read v, ConfigValue v) => ConfigValue (Maybe v) where
    parseValue = fmap Just . parseReadable

parseReadable :: Read r => String -> Either ConfigError r
parseReadable s = case reads s of
    [(v, "")] -> Right v
    _         -> Left (InvalidValue s)

