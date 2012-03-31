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
            TyConI (DataD _ _ _ cons _) -> processCons "" cons
            _ -> error "mkConfig can only be called for data types"

    processCons :: String -> [Con] -> ExpQ
    processCons prefix cons = case cons of
        [RecC cname vars] -> do
            let builders = map (buildField prefix) vars
                star     = [|(<*>)|]
                fldParam = varE (mkName "fields")
                step (e,op) bldr = (infixApp e op (appE bldr fldParam), star)
                (expr, _) = foldl' step (conE cname, [|(<$>)|]) builders

            lam1E (varP (mkName "fields"))  expr
        _ -> error "mkConfig can only be called for data types with one record constructor"

    consItem :: Con -> ExpQ
    consItem (NormalC name []) = [|(nameStr, $(conE name))|]
        where
            nameStr = map toLower . nameBase $ name
    consItem _ = error "Constructors for algebraic datatypes cannot take arguments"

    buildField :: String -> VarStrictType -> ExpQ
    buildField prefix (name, _, typ) = do
        let nameStr = prefix ++ nameBase name
            defaultResolver = [|parseValue|]
            defaultMissing  = [|\_ -> Left $ MissingField nameStr|]
            nestedResolver cons = processCons (nameStr ++ ".") cons

            resolveValue = case typ of
                ConT tnam -> do
                    info <- reify tnam
                    case info of
                        TyConI (DataD _ _ _ c@[RecC _ _] _) -> [|\sv -> Left $ InvalidValue sv|]
                        TyConI (DataD _ _ _ [_] _) -> defaultResolver
                        TyConI (DataD _ _ _ cons _) -> do
                            let lst = listE . map consItem $ cons
                            [|\sv -> case lookup sv $lst of
                                Nothing -> Left $ InvalidValue sv
                                Just c  -> Right c |]
                        _ -> defaultResolver

                _ -> defaultResolver

            resolveMissing = case typ of
                AppT a b
                    | a == (ConT ''Maybe) -> [|\_ -> Right Nothing|]
                ConT tnam -> do
                    info <- reify tnam
                    case info of
                        TyConI (DataD _ _ _ c@[RecC _ _] _) -> nestedResolver c
                        _ -> defaultMissing
                _ -> defaultMissing

        [|\fields -> case lookup nameStr fields of
            Just sv -> $resolveValue sv
            Nothing -> $resolveMissing fields|]

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

