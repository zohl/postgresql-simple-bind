{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE TupleSections              #-}

{-|
  Module:      Database.PostgreSQL.Simple.Bind.Implementation
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  TH functions that generate bindings. Examples are provided as
  pseudo-code snippets.
-}


module Database.PostgreSQL.Simple.Bind.Implementation (
    bindFunction
  , PostgresType
  ) where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.Exception (throw)
import Control.Monad (liftM2)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Debug.Trace (traceIO)
import Data.List (intersperse, intercalate)
import Data.Monoid ((<>), mconcat)
import Database.PostgreSQL.Simple (Connection, query, query_)
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGResult(..), PGColumn(..), PGIdentifier(..), PGType(..))
import Database.PostgreSQL.Simple.Bind.Common (PostgresBindException(..), PostgresBindOptions(..), ReturnType(..), unwrapRow, unwrapColumn)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..))
import Database.PostgreSQL.Simple.Types (Query(..))
import GHC.TypeLits (Symbol)
import Language.Haskell.TH.Syntax (Q, Dec(..), Exp(..), Type(..), Clause(..), Body(..), Pat(..))
import Language.Haskell.TH.Syntax (Name, mkName, newName, Lit(..), TyLit(..), TyVarBndr(..))
import Language.Haskell.TH.Syntax (Stmt(..), Lift)
import qualified Data.ByteString.Char8 as BSC8

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BS
import qualified Language.Haskell.TH.Syntax as TH

-- | Mapping from PostgreSQL types to Haskell types.
type family PostgresType (a :: Symbol)

-- | Create binding from given representation of a PostgreSQL function.
bindFunction :: PostgresBindOptions -> PGFunction -> Q [Dec]
bindFunction opt f = sequence $ ($ opt) . runReaderT . ($ f) <$> [mkFunctionT, mkFunctionE]



data Argument
  = forall a . (ToField a) => MandatoryArg {
        argName           :: String
      , argType           :: String
      , argRepresentation :: Maybe String
      , margValue         :: a
      }
  | forall a . (ToField a) => OptionalArg {
        argName           :: String
      , argType           :: String
      , argRepresentation :: Maybe String
      , morgValue         :: Maybe a
      }

instance ToField Argument where
  toField MandatoryArg {..} = toField margValue
  toField OptionalArg {..} = maybe
    (throw . DefaultValueNotFound $ argName)
    toField
    morgValue

instance Show Argument where
  show arg = concat $ [
      argName arg
    , " => "
    , maybe (throw . RepresentationNotFound $ argName arg) id (argRepresentation arg)
    , " :: "
    , argType arg
    ]


type TemplateBuilder a = ReaderT PostgresBindOptions Q a

-- | Example: "varchar" -> PostgresType "varchar"
postgresT :: PGType -> TemplateBuilder Type
postgresT t = AppT (ConT ''PostgresType) . LitT . StrTyLit <$> (formatType' t)

-- | Example: ''FromField "varchar" a -> [PostgresType "varchar" ~ a, FromField a]
mkContextT
  :: Name   -- ^ 'FromField' or 'ToField' constraint name.
  -> PGType -- ^ Variable type.
  -> Name   -- ^ Variable name.
  -> TemplateBuilder [Type]
mkContextT constraint t name = postgresT t >>= \r -> return [
    EqualityT `AppT` r `AppT` (VarT name)
  , (ConT constraint) `AppT` VarT name
  ]

mkConstraintT :: ReturnType -> Name
mkConstraintT AsRow   = ''FromRow
mkConstraintT AsField = ''FromField

mkResultColumnT :: Name -> PGType -> TemplateBuilder (Name, [Type])
mkResultColumnT c t = lift (newName "y") >>= \n -> (n,) <$> mkContextT c t n

mkReturnClauseT :: Bool -> [Type] -> TemplateBuilder Type
mkReturnClauseT isMultiple = return . (if isMultiple then (AppT ListT) else id) . \case
  [t] -> t
  ts  -> foldl AppT (TupleT (length ts)) (ts)

mkResultT'
  :: (t -> TemplateBuilder (Name, [Type]))  -- ^ Generator of a new variable with necessary constraints.
  -> ([Name] -> TemplateBuilder Type)       -- ^ Generator of a return statement.
  -> [t]                                    -- ^ Variables metadata (names+types or just types).
  -> TemplateBuilder ([Name], [Type], Type)
mkResultT' mkResultColumnT' mkReturnClauseT' ts = do
  (ns, cs) <- fmap (second concat . unzip) $ (mapM mkResultColumnT' ts)
  clause   <- mkReturnClauseT' ns
  return (ns, cs, clause)

-- | Examples:
--     (PGSingle ["varchar"]) -> (["y"], [PostgresType "varchar" ~ y, FromField y], y)
--     (PGSingle ["varchar", "bigint"]) -> (
--         ["y", "z"]
--       , [PostgresType "varchar" ~ y, FromField y, PostgresType "bigint" ~ z, FromField z]
--       , (y, z))
--     (PGSetOf ["varchar"]) -> (["y"], [PostgresType "varchar" ~ y, FromField y], [y])
--     (PGTable ["bigint", "varchar"])  -> (
--         ["y", "z"]
--       , [PostgresType "bigint" ~ y, FromField y, PostgresType "varchar" ~ z, FromField z]
--       , (y, z))
mkResultT :: PGIdentifier -> PGResult -> TemplateBuilder ([Name], [Type], Type)

mkResultT _fid (PGSingle ts) = mkResultT' mkResultColumnT' mkReturnClauseT' ts where
  mkResultColumnT' = mkResultColumnT ''FromField
  mkReturnClauseT' = mkReturnClauseT False . map VarT

mkResultT _fid (PGSetOf ts)  = mkResultT' mkResultColumnT' mkReturnClauseT' ts where
  mkResultColumnT' = (>>= uncurry (flip mkResultColumnT)) . (flip fmap) mkConstraintT' . (,)
  mkConstraintT'   = case ts of
    [t] -> mkConstraintT <$> (pboSetOfReturnType <$> ask <*> return t)
    _   -> return ''FromField
  mkReturnClauseT' = mkReturnClauseT True . map VarT

mkResultT fid (PGTable cols) = mkResultT' mkResultColumnT' mkReturnClauseT' cols where
  mkResultColumnT' = mkResultColumnT ''FromField . pgcType
  mkReturnClauseT' = (>>= mkReturnClauseT True) . sequence . zipWith wrapColumn cols
  wrapColumn c t = (($ pgcName c) . ($ fid). pboIsNullable <$> ask) >>= return . \case
    True  -> (ConT ''Maybe) `AppT` (VarT t)
    False -> VarT t


-- | Example: [
--     PGArgument { pgaName = "x", pgaType = "varchar", pgaOptional = True }
--   , PGArgument { pgaName = "y", pgaType = "bigint", pgaOptional = False }
--   ] -> (
--     ["x1", "x2"]
--   , [PostgresType "varchar" ~ x1, ToField x1, PostgresType "bigint" ~ x2, ToField x2]
--   , [x1, Maybe x2]
--   )
mkArgsT :: [PGArgument] -> TemplateBuilder ([Name], [Type], [Type])
mkArgsT cs = do
  names   <- lift . sequence $ replicate (length cs) (newName "x")
  context <- fmap concat . sequence $ zipWith (curry $ uncurry (mkContextT ''ToField) . first pgaType) cs names

  let defWrap d = case d of
        True  -> AppT (ConT ''Maybe)
        False -> id

  let clause = zipWith (\PGArgument {..} -> (defWrap pgaOptional) . VarT) cs names
  return (names, context, clause)


-- | Example: PGFunction {
--     pgfSchema    = "public"
--   , pgfName      = "foo"
--   , pgfResult    = PGSingle "varchar"
--   , pgfArguments = [
--       PGArgument { pgaName = "x", pgaType = "varchar", pgaOptional = True }
--     , PGArgument { pgaName = "y", pgaType = "bigint", pgaOptional = False }
--     ]
--   } -> {
--     foo :: forall x1 x2 x3. (
--          PostgresType "varchar" ~ x1, ToField x1
--        , PostgresType "bigint" ~ x2, ToField x2
--        , PostgresType "varchar" ~ x2, FromField x3
--        ) => Connection -> x1 -> Maybe x2 -> x3
--  }
mkFunctionT :: PGFunction -> TemplateBuilder Dec
mkFunctionT f@(PGFunction {..}) = do
  PostgresBindOptions {..}  <- ask
  (argNames, argContext, argClause) <- mkArgsT pgfArguments
  (retNames, retContext, retClause) <- mkResultT pgfIdentifier pgfResult

  let vars = map PlainTV (argNames ++ retNames)
  let context = argContext ++ retContext

  let chain x = AppT (AppT ArrowT x)
  let clause = foldr1 chain $ (ConT ''Connection):(argClause ++ [AppT (ConT ''IO) retClause])

  return $ SigD (mkName $ pboFunctionName f) $ ForallT vars context clause


unwrapE' :: ReturnType -> Exp -> Exp
unwrapE' AsRow   q = q
unwrapE' AsField q = (VarE 'fmap) `AppE` (VarE 'unwrapColumn) `AppE` q

-- | Examples:
--     (PGSingle _) q -> { unwrapRow  q }
--     (PGSetOf  _) q -> { unwrapColumn q }
--     (PGTable  _) q -> { q }
unwrapE :: PGResult -> Exp -> TemplateBuilder Exp
unwrapE (PGSingle _)  q = return $ (VarE 'fmap) `AppE` (VarE 'unwrapRow) `AppE` q
unwrapE (PGSetOf [t]) q = flip unwrapE' q . ($ t) . pboSetOfReturnType <$> ask
unwrapE (PGSetOf   _) q = return $ unwrapE' AsRow q
unwrapE (PGTable _)   q = return $ unwrapE' AsRow q


wrapArg :: PGArgument -> Name -> TemplateBuilder Exp
wrapArg PGArgument {..} argName = do
  t     <- formatType' pgaType
  value <- (pboDebugQueries <$> ask) >>= return . \case
    True  -> foldr1 AppE [ConE 'Just, VarE 'show, VarE argName]
    False -> ConE 'Nothing
  return $ foldl1 AppE $ [
      ConE $ if pgaOptional then 'OptionalArg else 'MandatoryArg
    , LitE $ StringL $ maybe "(N/A)" id pgaName
    , LitE $ StringL t
    , value
    , VarE argName]


traceE :: Exp -> Exp
traceE e = (VarE 'traceIO) `AppE` ((VarE 'show) `AppE` e)


-- | Example: PGFunction {
--     pgfSchema    = "public"
--   , pgfName      = "foo"
--   , pgfResult    = PGSingle "varchar"
--   , pgfArguments = [
--       PGArgument { pgaName = "x", pgaType = "varchar", pgaOptional = True }
--     , PGArgument { pgaName = "y", pgaType = "bigint", pgaOptional = False }
--     ]
--   } -> {
--     foo conn x1 x2 = query conn
--       (Query $ BS.pack $ concat ["select public.foo (", (formatArguments args), ")"])
--       (filterArguments [MandatoryArg "x1" x1, OptionalArg "x2" x2])
--   }
mkFunctionE :: PGFunction -> TemplateBuilder Dec
mkFunctionE f@(PGFunction {..}) = do
  PostgresBindOptions {..} <- ask
  let funcName = mkName (pboFunctionName f)

  varNames <- lift $ sequence $ replicate (length pgfArguments) (newName "x")
  connName <- lift $ newName "conn"
  let funcArgs = (VarP connName):(map VarP varNames)

  [argsName, sqlQueryName, refinedArgsName] <- lift $ mapM newName ["args", "sqlQuery", "refinedArgs"]
  specs  <- lift $ TH.lift pgfArguments

  funcDecl <- (\body -> [ValD (VarP argsName) (NormalB . ListE $ body) []])
    <$> sequence (zipWith wrapArg pgfArguments varNames)

  let traceQuery = NoBindS
        <$> if pboDebugQueries
            then (traceE $ VarE sqlQueryName):
                 (if null varNames then [] else [traceE $ VarE argsName])
            else []

  formatterOptions <- lift $ TH.lift FormatterOptions {
      foExplicitCasts   = pboExplicitCasts
    , foOlderCallSyntax = pboOlderCallSyntax
    , foDefaultSchema   = pboDefaultSchema
    }

  queryBindings <- (\prefix -> BindS
    (TupP [VarP sqlQueryName, VarP refinedArgsName])
    ((VarE 'prepareQuery)
      `AppE` formatterOptions
      `AppE` (LitE . StringL $ prefix)
      `AppE` specs
      `AppE` (VarE argsName))) <$> (queryPrefix f)

  execQuery <- NoBindS <$> unwrapE pgfResult (CondE ((VarE 'null) `AppE` (VarE refinedArgsName))
        ((VarE 'query_)
          `AppE` (VarE connName)
          `AppE` (VarE sqlQueryName))
        ((VarE 'query)
          `AppE` (VarE connName)
          `AppE` (VarE sqlQueryName)
          `AppE` (VarE refinedArgsName)))

  let funcBody = NormalB . DoE $ queryBindings:(concat [traceQuery, [execQuery]])

  return $ FunD funcName [Clause funcArgs funcBody funcDecl]



queryPrefix :: PGFunction -> TemplateBuilder String
queryPrefix PGFunction {..} = intercalate " " <$> sequence [select, formatIdentifier' pgfIdentifier] where
  select = case pgfResult of
    PGTable _     -> return $ mkSelect AsRow
    PGSetOf [t]   -> mkSelect . ($ t) . pboSetOfReturnType <$> ask
    PGSetOf _     -> return $ mkSelect AsRow
    _             -> return $ mkSelect AsField

  mkSelect AsRow   = "select * from"
  mkSelect AsField = "select"



-- untypedPlaceholder, typedPlaceholder :: String -> String
-- untypedPlaceholder = const "?"
-- typedPlaceholder atype = "(?)::" ++ atype


-- formatArgument :: String -> (String -> String) -> Argument -> Maybe String
-- formatArgument callSyntax placeholder = format where
--   format = \case
--     MandatoryArg {..} -> Just $ placeholder argType
--     OptionalArg {..} -> fmap
--       (const . concat $ [argName, callSyntax, placeholder argType])
--       morgValue
--
--
-- formatArguments :: String -> (String -> String) -> [Argument] -> String
-- formatArguments callSyntax placeholder = concat
--   . (intersperse ", ")
--   . catMaybes
--   . (map $ formatArgument callSyntax placeholder)
--
--
-- filterArguments :: [Argument] -> [Argument]
-- filterArguments = filter isPresented where
--   isPresented :: Argument -> Bool
--   isPresented (OptionalArg {..}) = maybe False (const True) $ morgValue
--   isPresented _                  = True


data FormatterOptions = FormatterOptions {
    foExplicitCasts   :: Bool
  , foOlderCallSyntax :: Bool
  , foDefaultSchema   :: Maybe String
  } deriving (Show, Eq, Lift)


formatIdentifier' :: PGIdentifier -> TemplateBuilder String
formatIdentifier' PGIdentifier {..} = (pboDefaultSchema <$> ask)
  >>= return
  . maybe pgiName (++ ("." ++ pgiName))
  . (pgiSchema <|>)

formatType' :: PGType -> TemplateBuilder String
formatType' PGType {..} = formatIdentifier' pgtIdentifier


type Formatter a = a -> ReaderT FormatterOptions Maybe Builder

formatIdentifier :: Formatter PGIdentifier
formatIdentifier PGIdentifier {..} = (foDefaultSchema <$> ask)
  >>= return
  . byteString . BSC8.pack
  . maybe pgiName (++ ("." ++ pgiName))
  . (pgiSchema <|>)

formatType :: Formatter PGType
formatType = formatIdentifier . pgtIdentifier

formatArgument :: Formatter PGArgument
formatArgument PGArgument {..} = (foExplicitCasts <$> ask) >>= \case
  True  -> fmap (byteString "(?)::" <>) (formatType pgaType)
  False -> return (char8 '?')

formatAssingment :: Formatter String
formatAssingment name = (foOlderCallSyntax <$> ask)
  >>= \ocs -> return $ (byteString . BSC8.pack $ name) <> byteString (if ocs then ":=" else "=>")

formatArgumentList
  :: Bool                      -- ^ Was optional argument omitted?
  -> [(PGArgument, Argument)]  -- ^ Arguments with metadata.
  -> ReaderT FormatterOptions Maybe [(Builder, Argument)]
formatArgumentList _ [] = return []
formatArgumentList o ((pga, arg):rest) = case arg of
  MandatoryArg {..} -> liftM2 (:)
    ((,arg) <$> formatArgument pga)
    (formatArgumentList o rest)

  OptionalArg  {..} -> case morgValue of
    Nothing  -> formatArgumentList True rest
    (Just _) -> case (pgaName pga) of
      Nothing -> if o
        then lift Nothing
        else ((char8 '?', arg):) <$> (formatArgumentList o rest)
      (Just name) -> liftM2 (:)
        ((,arg) <$> (liftM2 (<>) (formatAssingment name) (formatArgument pga)))
        (formatArgumentList o rest)


prepareQuery :: (MonadThrow m)
  => FormatterOptions
  -> Builder       -- ^ Query prefix.
  -> [PGArgument]  -- ^ Arguments to format.
  -> [Argument]
  -> m (Query, [Argument])
prepareQuery opt prefix specs args = do
  (argsString, refinedArgs) <- maybe
    (throwM (IncorrectInvocation . show $ (specs, args)))
    (return)
    (unzip <$> (runReaderT (formatArgumentList False (zip specs args)) opt))
  let sqlQuery = mconcat [
          prefix, char8 '(', mconcat . intersperse (char8 ',') $ argsString, char8 ')']
  return (Query . BS.toStrict . toLazyByteString $ sqlQuery, refinedArgs)

