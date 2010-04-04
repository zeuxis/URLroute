{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RecordWildCards,
             TypeSynonymInstances, FlexibleInstances, FlexibleContexts,
             UndecidableInstances, PatternGuards #-}

module Web.Route (
    -- * Class for adapting the library
    RouteURL(..), Args(..), NoArg(..), Arg(..),
    -- * Filter for URLs
    URLSelector, URLFilter(..), root, (==>), (=->), (<>),
    dispatch,
    -- ** filter combinators
    --   pureF, apF, idF, liftF, liftF2, liftF3, joinF,
    -- * URL filter elements
    -- ** Path filtering
    PathFilter(..), (</), string,
    CheckValue, checkValue, CheckValue_, checkValue_,
    fullPath, CheckFullPath, checkFullPath, CheckFullPath_, checkFullPath_,
    End, end,  trailing, noPath,
    -- ** Query filtering
    QueryFilter(..), ValueFilter(..), (<&), QuerySelector ((:=)),
) where

import Control.Monad
import Data.Maybe

type Path  = [String]
type Query = [(String, String)]

-- | interfacing class to underlying URL-type
class (Eq url)=> RouteURL url where
    getPath  :: url -> Path -- ^ returns the fully decoded path component of an URL
    getQuery :: url -> Query
    setPath  :: Path  -> url -> url
    setQuery :: Query -> url -> url

-- Dispatch

type URLInfo = (Path, Query)

instance RouteURL URLInfo where
   getPath  = fst
   getQuery = snd

   setPath  ps (_, qry) = (ps, qry)
   setQuery qry (ps, _) = (ps, qry)

-- | 'URLFilter's are used to construct 'URLSelector's.
data URLFilter args =
    URLFilter {
        ufSelect :: URLInfo -> Maybe (URLInfo, args),
        ufBuild  :: URLInfo -> args -> URLInfo
    }

data URLSelector url a =
    URLSelector {
        usSelect :: url -> Maybe a
    }

-- class to curry/uncurry multiple arguments


class Args args a where
    type Function args a :: *

    fromFunction :: Function args a -> args -> a
    toFunction   :: (args -> a) -> Function args a

data NoArg = NoArg
              deriving (Eq, Show)

instance Args NoArg a where
    type Function NoArg a = a

    fromFunction x _ = x
    toFunction   f   = f NoArg

data Arg a = Arg a
              deriving (Eq, Show)

instance Args (Arg b) a where
    type Function (Arg b) a = b -> a

    fromFunction f (Arg x) = f x
    toFunction   f x       = f $ Arg x

instance (Args a1 (Function a2 a), Args a2 a)=> Args (a1, a2) a where
    type Function (a1, a2) a = Function a1 (Function a2 a)

    fromFunction f (x, y) = fromFunction (fromFunction f x) y
    toFunction f = toFunction (\x -> toFunction (\y -> f (x,y)))


infix  2 ==>
infix  2 =->
infixl 3 <>
infixl 3 </
infixl 3 <&
infix  4 :=

-- | Combines 'URLFilter's
(<>) :: URLFilter args1 -> URLFilter args2 -> URLFilter (args1, args2)
uf1 <> uf2 =
    URLFilter {
      ufSelect = \ui -> do
                    (ui1, a1) <- ufSelect uf1 ui
                    (ui2, a2) <- ufSelect uf2 ui1
                    return (ui2, (a1,a2)),
      ufBuild  = \ui (a1, a2) -> let
                    ui1 = ufBuild uf2 ui  a2
                    in    ufBuild uf1 ui1 a1
    }

-- | Binds a value to an 'URLFilter'.
-- (==>) :: (Monad m)=> URLFilter m b a -> b -> URLSelector (m a)
-- (URLFilter filt) ==> value = filt . top $ return value

(==>) :: (RouteURL url, Args args a, Args args url)=>
         URLFilter args -> Function args a ->
            (url -> Function args url,
             URLSelector url a)
uf ==> f =
    (
      \url -> toFunction $ \args ->
          let (path, query) = ufBuild uf (getPath url, getQuery url) args
          in setPath path . setQuery query $ url,

      URLSelector {
        usSelect = \url -> do
                      (_, args) <- ufSelect uf (getPath url, getQuery url)
                      return $ fromFunction f args
      }
    )

-- | like '(==>)', but does not return a builder
(=->) :: (RouteURL url, Args args a, Args args url)=>
         URLFilter args -> Function args a ->
         URLSelector url a
uf =-> f = snd $ uf ==> f

-- | Performs a dispatch, returns a list of all matches
dispatch :: (RouteURL url)=> [URLSelector url a] -> url -> [a]
dispatch sel url =
    catMaybes $ map (($ url) . usSelect) sel


------------
-- Filters
------------


idF, root :: URLFilter NoArg
idF  = URLFilter {
         ufSelect = \ui -> Just (ui, NoArg),
         ufBuild  = \ui _ -> ui
       }
root = idF

-- -- | Supply a value function with a constant value
-- pureF :: a -> URLFilter (a -> r) r
-- pureF x = URLFilter (\(ui, f) -> Just (ui, f x))
--                     (\(ui, x) -> (ui, const x))

-- -- | Apply the function present in the filter parameters
-- apF :: (Monad m)=> URLFilter m (b -> r) ((a -> b) -> a -> r)
-- apF = fmap curry . liftF $ uncurry ($)

-- -- | Lift a function to the parameter of filter functions
-- liftF :: (Monad m)=> (a -> b) -> URLFilter m (b -> r) (a -> r)
-- liftF f = URLFilter $ fmap (liftM (. f))

-- -- | Lift a binary function to the parameter of filter functions
-- liftF2 :: (Monad m)=> (a -> b -> c)
--                         -> URLFilter m (c -> r) (a -> b -> r)
-- liftF2 op = apF <> liftF op

-- -- | Lift a ternary function to the parameter of filter functions
-- liftF3 :: (Monad m)=> (a -> b -> c -> d)
--                         -> URLFilter m (d -> r) (a -> b -> c -> r)
-- liftF3 op = apF <> apF <> liftF op

-- -- | Joins two 'URLFilter's
-- joinF :: (Monad m)=> (a -> a -> a)
--                      -> URLFilter m b a -> URLFilter m b a -> URLFilter m b a
-- joinF op (URLFilter f1) (URLFilter f2) =
--     URLFilter $ \sel -> join (liftOp op) (f1 sel) (f2 sel)

-----------------------
-- URL Path selectors
-----------------------

class PathFilter p where
    type PathFilterArgs p
    pathFilter :: p -> URLFilter (PathFilterArgs p)

(</) :: (PathFilter p)=> URLFilter a
                         -> p -> URLFilter (a, PathFilterArgs p)
filt </ p = filt <> pathFilter p

-- extract path elements
data CheckValue  a = CheckValue  String (String -> Maybe a) (a -> String)
data CheckValue_   = CheckValue_ String (String -> Bool)    String

instance PathFilter (CheckValue p) where
    type PathFilterArgs (CheckValue p) = Arg p
    pathFilter (CheckValue desc parse show) = URLFilter{..}
      where
        ufSelect ([], _)      = Nothing
        ufSelect (p:ps, qry)
          | Just x <- parse p = Just ((ps, qry), Arg x)
          | otherwise         = Nothing

        ufBuild (ps,qry) (Arg x) = (show x:ps,qry)

instance PathFilter CheckValue_ where
    type PathFilterArgs CheckValue_ = NoArg
    pathFilter (CheckValue_ desc parse path) = URLFilter{..}
      where
        ufSelect ([],_) = Nothing
        ufSelect ((p:ps), qry)
          | parse p     = Just ((ps, qry), NoArg)
          | otherwise   = Nothing

        ufBuild (ps, qry) _ = (path:ps, qry)

instance PathFilter String where
    type PathFilterArgs String = NoArg
    pathFilter str = pathFilter $ CheckValue_ str (==str) str

string :: String -> CheckValue String
string name = CheckValue name Just id

checkValue :: String -> (String -> Maybe a) -> (a -> String) -> CheckValue a
checkValue = CheckValue

checkValue_ :: String -> (String -> Bool) -> String -> CheckValue_
checkValue_ = CheckValue_

data End = End

instance PathFilter End where
    type PathFilterArgs End = NoArg
    pathFilter End = URLFilter{..}
      where
        ufSelect ([], qry) = Just (([], qry), NoArg)
        ufSelect _         = Nothing

        ufBuild (_,qry) _ = ([],qry)

-- | Matches when the remaining path is empty
end :: End
end = End

-- extract the full path

data CheckFullPath a = CheckFullPath  String ([String] -> Maybe a) (a -> [String])
data CheckFullPath_  = CheckFullPath_ String ([String] -> Bool)    [String]

instance PathFilter (CheckFullPath p) where
    type PathFilterArgs (CheckFullPath p) = Arg p
    pathFilter (CheckFullPath desc parse show) = URLFilter{..}
      where
        ufSelect (ps, qry)
          | Just x <- parse ps = Just (([], qry), Arg x)
          | otherwise          = Nothing

        ufBuild (_,qry) (Arg x) = (show x, qry)

instance PathFilter CheckFullPath_ where
    type PathFilterArgs CheckFullPath_ = NoArg
    pathFilter (CheckFullPath_ desc parse defaultPath) = URLFilter{..}
      where
        ufSelect (ps, qry)
          | parse ps  = Just (([], qry), NoArg)
          | otherwise = Nothing

        ufBuild (_,qry) _ = (defaultPath, qry)

fullPath :: CheckFullPath [String]
fullPath = CheckFullPath ".." Just id

checkFullPath :: String -> ([String] -> Maybe a) -> (a -> [String])
                 -> CheckFullPath a
checkFullPath = CheckFullPath

checkFullPath_ :: String -> ([String] -> Bool) -> [String] -> CheckFullPath_
checkFullPath_ = CheckFullPath_

-- | Matches on a trailing slash
trailing :: CheckFullPath_
trailing = checkFullPath_ "/" (==[""]) [""]

-- | Matches, of the path is empty
noPath :: CheckFullPath_
noPath = checkFullPath_ "[/]" (\p -> null p || p==[""]) [""]

-- -----------------
-- Query selectors
-- -----------------

class QueryFilter q where
    type QueryFilterArgs q
    queryFilter :: q -> URLFilter (QueryFilterArgs q)

(<&) :: (QueryFilter q)=> URLFilter args
                          -> q -> URLFilter (args, QueryFilterArgs q)
filt <& q = filt <> queryFilter q

data QuerySelector v = String :=  v

-- filters for values of query-parameter
class ValueFilter v where
    type ValueFilterArgs v
    valueFilter :: String -> v -> URLFilter (ValueFilterArgs v)

instance (ValueFilter v)=> QueryFilter (QuerySelector v) where
    type QueryFilterArgs (QuerySelector v) = ValueFilterArgs v
    queryFilter (k := v) = valueFilter k v

instance ValueFilter (CheckValue v) where
    type ValueFilterArgs (CheckValue v) = Arg v
    valueFilter k (CheckValue name parse show) = URLFilter{..}
      where
        ufSelect (ps, qry) = do
          val <- lookup k qry
          x   <- parse val
          return ((ps, filter ((/= k) . fst) qry), Arg x)

        ufBuild (ps, qry) (Arg x) = (ps, (k, show x):qry)

instance ValueFilter CheckValue_ where
    type ValueFilterArgs CheckValue_ = NoArg
    valueFilter k (CheckValue_ name parse defaultValue) = URLFilter{..}
      where
        ufSelect (ps, qry) = do
          val <- lookup k qry
          if parse val
            then return ((ps, filter ((/= k) . fst) qry), NoArg)
            else fail "no parse"

        ufBuild (ps, qry) _ = (ps, (k, defaultValue):qry)

instance ValueFilter String where
    type ValueFilterArgs String = NoArg
    valueFilter k str = valueFilter k (CheckValue_ str (==str) str)

instance QueryFilter End where
    type QueryFilterArgs End = NoArg
    queryFilter End = URLFilter{..}
      where
        ufSelect (ps, []) = Just ((ps, []), NoArg)
        ufSelect _        = Nothing

        ufBuild (ps, _) (NoArg) = (ps, [])

