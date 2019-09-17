{-# LANGUAGE OverloadedStrings #-}

module ParseTest where
import Data.Time
import Data.Text as T
import Data.Aeson.Types
import Data.HashMap.Strict as HM

parseTuple :: Value -> Parser (String, Bool)
parseTuple (Object o) = do
    let mbFieldA = HM.lookup "a" o
    fieldA <- case mbFieldA of
        Just x  -> return x
        Nothing -> fail "no field 'a'"
    a <- case fieldA of
        String x -> return $ T.unpack x
        _        -> fail "expected a string"
    b <- case HM.lookup "b" o of
        Just (Bool x) -> return x
        Just _        -> fail "expected a boolean"
        Nothing       -> fail "no field 'b'"
    return (a,b)
parseTuple _ = fail "expected an object"

parseJSONTime :: String -> Maybe UTCTime
parseJSONTime s = parseTimeM True defaultTimeLocale "%Y-%-m-%-dT%-T%-QZ" s