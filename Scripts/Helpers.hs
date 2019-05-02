module Helpers
  ( mkIncludeBlock
  , getDefinitions
  , getEnums
  , selectDefines
  , selectEnum
  , mkADT
  , selectEnums
  )
where

import qualified Data.Map                      as M
import           Data.Map                       ( Map )
import           Data.Char                      ( isNumber )
import           Data.Function                  ( on )
import           Data.List                      ( isPrefixOf
                                                , sortBy
                                                , nubBy
                                                , intercalate
                                                )
import           Data.Maybe                     ( mapMaybe
                                                , fromJust
                                                )
import           Control.Monad                  ( join )
import           System.Process                 ( readProcess )
import           Language.C.Analysis            ( runTrav_ )
import           Language.C.Analysis.AstAnalysis
                                                ( analyseAST )
import           Language.C.Analysis.SemRep     ( GlobalDecls(..)
                                                , TagDef(EnumDef)
                                                , EnumType(..)
                                                , Enumerator(..)
                                                )
import           Language.C.Data.Ident          ( Ident(..) )
import           Language.C.Data.InputStream    ( inputStreamFromString )
import           Language.C.Data.Position       ( position )
import           Language.C.Parser              ( parseC )
import           Language.C.Pretty              ( pretty )
import           Language.C.Syntax.Constants    ( getCInteger )
import           Language.C.Syntax.AST          ( CExpression(..)
                                                , CExpr
                                                , CConstant(CIntConst)
                                                , CBinaryOp(..)
                                                )
import           Text.Regex.PCRE                ( (=~) )

mkIncludeBlock :: [String] -> String
mkIncludeBlock = unlines . map (\e -> "#include <" ++ e ++ ">")

sortedConstants :: Map String Integer -> [(String, Integer)]
sortedConstants keyValues = sortBy (compare `on` snd) $ M.toList keyValues

mkADT :: String -> Map String Integer -> ([String], [String])
mkADT name constants = ([name], [constrs, enumClass])
 where
  constantsList    = sortedConstants constants
  typeConstr       = "data " ++ name ++ " = "
  typeConstrIndent = ("\n" ++ replicate (length typeConstr - 3) ' ')
  dataConstrs = intercalate (typeConstrIndent ++ " | ") (map fst constantsList)
  enumDeriving     = typeConstrIndent ++ " deriving (Eq, Ord, Show)"
  constrs          = typeConstr ++ dataConstrs ++ enumDeriving ++ "\n"
  enumClass        = mkEnumClass name constantsList

mkEnumClass :: String -> [(String, Integer)] -> String
mkEnumClass name constantsList =
  "instance Enum " ++ name ++ " where\n" ++ toEnums ++ fromEnums
 where
  toEnums =
    concatMap (\(k, v) -> "  toEnum " ++ show v ++ " = " ++ k ++ "\n")
      $ nubBy ((==) `on` snd) constantsList
  fromEnums =
    concatMap (\(k, v) -> "  fromEnum " ++ k ++ " = " ++ show v ++ "\n")
      $ constantsList

selectDefines :: String -> Map String Integer -> Map String Integer
selectDefines regex = M.filterWithKey (\k _ -> k =~ regex)

selectEnum :: String -> [Map String Integer] -> Map String Integer
selectEnum regex m = case selectEnums regex m of
  (x : _) -> x
  []      -> error ("Couldn't find enum for " ++ regex)

selectEnums :: String -> [Map String Integer] -> [Map String Integer]
selectEnums regex = filter (all (=~ regex) . M.keys)

getEnums :: String -> IO [Map String Integer]
getEnums source = do
  parsed <- flip parseC initPos . inputStreamFromString <$> preprocessed
  let unit  = gTags . fst . check $ runTrav_ (analyseAST $ check parsed)
      enums = mapMaybe getEnum (M.elems unit)
  return $ map cleanEnums enums
 where
  check (Left  err) = error $ show err
  check (Right a  ) = a
  preprocessed = readProcess "gcc" ["-E", "-"] source
  initPos      = position 0 "" 0 0 Nothing
  cleanEnums   = M.filterWithKey (\k _ -> not ("_" `isPrefixOf` k))

getEnum :: TagDef -> Maybe (Map String Integer)
getEnum (EnumDef (EnumType _ es _ _)) = Just $ foldl getEnumValue M.empty es
getEnum _                             = Nothing

getEnumValue :: Map String Integer -> Enumerator -> Map String Integer
getEnumValue m (Enumerator (Ident s _ _) v _ _) = M.insert s a m
  where a = evalEExpr m v

evalEExpr :: Map String Integer -> CExpr -> Integer
evalEExpr _ (CConst (CIntConst v _)) = getCInteger v
evalEExpr m (CBinary CAddOp a b _  ) = evalEExpr m a + evalEExpr m b
evalEExpr m (CBinary CSubOp a b _  ) = evalEExpr m a - evalEExpr m b
evalEExpr m (CBinary CShlOp a b _  ) = evalEExpr m a * (2 ^ evalEExpr m b)
evalEExpr m (CVar (Ident a _ _) _  ) = fromJust $ M.lookup a m
evalEExpr _ other                    = error $ "Other: " ++ show (pretty other)

sanitize :: [[String]] -> [[String]]
sanitize (["@define", n] : _ : [y] : xs) = ["@define", n, y] : sanitize xs
sanitize (["@define", x, y]        : xs) = ["@define", x, y] : sanitize xs
sanitize (_                        : xs) = sanitize xs
sanitize []                              = []

getDefinitions :: String -> IO (Map String Integer)
getDefinitions headers = do
  defines <- map words . lines <$> readDefines headers
  let isDefine (c : n : _) =
        c == "#define" && '(' `notElem` n && not ("_" `isPrefixOf` n)
      isDefine xs = error ("isDefine: Couldn't check: " ++ show xs)
      hasValue = (>= 3) . length
      names    = map (!! 1) $ filter (\d -> isDefine d && hasValue d) defines
      kludge   = map (\n -> "@define \"" ++ n ++ "\" " ++ n) names
  defines2 <- map words . lines <$> preprocess (headers ++ unlines kludge)
  let isInteresting d =
        hasValue d
          && ["@define"]
          `isPrefixOf` d
          && (all isNumber (d !! 2) || "0x" `isPrefixOf` (d !! 2) && all
               isNumber
               (drop 2 (d !! 2))
             )
      realDefines =
        map (take 2 . drop 1) $ filter isInteresting $ sanitize defines2
      clean [(_ : k), v] = (init k, read v)
      clean _            = error "Clean: got a weird list"
  return $ M.fromList (map clean realDefines)
 where
  readDefines = readProcess "gcc" ["-E", "-dM", "-"]
  preprocess  = readProcess "gcc" ["-E", "-"]
