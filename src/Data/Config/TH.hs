{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

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

data ConfigError
    = MissingField String
    | InvalidValue String
    | SyntaxError

mkConfig :: Name -> Q [Dec]
mkConfig name = [d|
    instance Config $(conT name) where
        parseConf = $(impl)
    |] where
        impl = [|\s -> case parseFields s of
            Just fields -> $constrCfg fields
            Nothing     -> Left SyntaxError|]

        constrCfg :: ExpQ
        constrCfg = do
            info <- reify name
            case info of
                TyConI (DataD _ _ _ cons _) -> processCons cons
                _ -> error "mkConfig can only be called for data types"

        processCons :: [Con] -> ExpQ
        processCons cons = case cons of
            [RecC cname vars] -> do
                let builders = map buildField vars
                    star     = [|(<*>)|]
                    fldParam = varE (mkName "fields")
                    step (e,op) bldr = (infixApp e op (appE bldr fldParam), star)
                    (expr, _) = foldl' step (conE cname, [|(<$>)|]) builders

                [|\fields -> $expr|]
            _ -> error "mkConfig can only be called for data types with one record constructor"

        buildField :: VarStrictType -> ExpQ
        buildField (name, _, typ) = do
            let nameStr = nameBase name
                resolveMissing = case typ of
                    AppT a b
                        | a == (ConT ''Maybe) -> [|Right Nothing|]
                    _ -> [|Left $ MissingField nameStr|]

            [|\fields -> case lookup nameStr fields of
                Just sv -> parseValue sv
                Nothing -> $resolveMissing|]

class Config c where
    parseConf :: String -> Either ConfigError c
    parseConf = undefined

(@@) :: (Config c, ConfigValue v) => c -> (c -> v) -> v
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

