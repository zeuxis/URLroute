{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
import Test.HUnit hiding (path)

import Data.List
import Data.Maybe
import Network.URI

import Web.Route hiding ((==>))
import qualified Web.Route as R ((==>))


-- use a super simple implementation (doesn't handle [un]escaping)
instance RouteURL URI where
    getPath  uri = split '/' $ uriPath uri
    getQuery uri = map extractQry . split '&' $ uriQuery uri

    setPath []   uri = uri {uriPath  = ""}
    setPath path uri = uri {uriPath  = concatMap ('/':) path}
    setQuery []  uri = uri {uriQuery = ""}
    setQuery qry uri = uri {uriQuery = ('?':) . concat . intersperse "&" $
                                       [key ++ "=" ++ val | (key,val) <- qry]
                           }

-- helper
split :: Char -> String -> [String]
split c "" = []
split c str = split' $ drop 1 str
  where
    split' str =
      case break (== c) str of
        (xs, [])     -> [xs]
        (xs, [_])    -> [xs,""]
        (xs, (_:ys)) -> xs : split' ys

extractQry :: String -> (String, String)
extractQry qry = let (key, val) = break (== '=') qry
                 in (key, drop 1 val)

readMaybe :: (Read a)=> String -> Maybe a
readMaybe x = case reads x of
                [(x,"")] -> Just x
                _        -> Nothing

infix 2 ==>
(==>) :: (Args args a, Args args URI)=>
         URLFilter args -> Function args a ->
         (Function args URI, URLSelector URI a)
uf ==> f = let (builder, disp) = uf R.==> f
           in (builder nullURI, disp)

-- URL combinators
int :: CheckValue Int
int = checkValue "<int>" readMaybe show

checkEven :: CheckValue_
checkEven = checkValue_ "<even>" (maybe False even . readMaybe) (show 0)

-- test routes
(bAB, dAB)  = root </ "a" </ "b"
                  ==> "a/b"
(bBA, dBA)  = root </ "b" </ "a"
                  ==> "b/a"
(bC,  dC)   = root </ "c" </ string "<c>"
                  ==> ("c/"++)
(bD1, dD1)  = root </ "d1" </ int
                  ==> \i -> "d1/" ++ show i
(bD2, dD2)  = root </ "d2" </ int </ int
                  ==> \i j -> "d2/" ++ show i ++ "/" ++ show j
(bD3, dD3)  = root </ "d3" </ checkEven
                  ==> "d3/even"
(bE1, dE1)  = root </ "e" </ end
                  ==> "end"
(bE2, dE2)  = root </ end
                  ==> "empty"
(bF1, dF1)  = root </ "f1" </ fullPath
                  ==> ("f1 " ++) . intercalate "."
(_  , dF1') = root </ "f1" </ fullPath </ "c"
                  ==> const "never matches"
(bF2, dF2)  = root </ "f2" </ checkFullPath ".." (Just . length)
                                                 (\i -> replicate i "x")
                  ==> ("f2 "++) . show
(bF3, dF3)  = root </ "f3" </ checkFullPath_ "/*/*" (\p -> length p == 2)
                                                    ["x","x"]
                  ==> "f3"
(bG,  dG)   = root </ "g" </ trailing
                  ==> "trailing"
(bH,  dH)   = root </ "h" </ noPath
                  ==> "no path"

(bP1, dP1)  = root </ "p" <& "a" := "a" <& "b" := "b"
                  ==> "a=a"
(bP2, dP2)  = root </ "p" <& "a" := string "<a>" <& "b" := int
                  ==> \val i -> "a:" ++ val ++ " b:" ++ show i
(bQ,  dQ)   = root </ "q" <& end
                  ==> "q end"

-- test dispatcher
dispatcher = [
    dAB, dBA, dC, dD1, dD2, dD3, dE1, dE2, dF1, dF1', dF2, dF3, dG, dH,
    dP1, dP2, dQ,

--     </ "p" <> coord <& end
--         ==> \(x,y) -> "coord:" ++ (show $ x+y),

    root =-> "no match"
  ]

-- coord :: (Monad m)=> URLFilter m ((Int, Int) -> a) a
-- coord = liftF2 (,) <& "x" := checkValue "<x>" readMaybe
--                    <& "y" := checkValue "<y>" readMaybe

disp url pathLen res = TestCase $ do
      let (Just url') = parseURI url
      res @=? head (dispatch dispatcher url')

testDispatch = TestLabel "dispatching" $ TestList [
      TestLabel "path" $ TestList [
        disp "http://local/b/a"           2 "b/a",
        disp "http://local/a/b"           2 "a/b",
        disp "http://local/a/b/"          2 "a/b",
        disp "http://local/a/b/c"         2 "a/b",
        disp "http://local/c"             0 "no match",
        disp "http://local/c/"            2 "c/",
        disp "http://local/c/a"           2 "c/a",
        disp "http://local/c/a/b"         2 "c/a",
        disp "http://local/d1/2"          2 "d1/2",
        disp "http://local/d2/1/2"        3 "d2/1/2",
        disp "http://local/d3/4"          2 "d3/even",
        disp "http://local/d3/1"          0 "no match",
        disp "http://local/e"             1 "end",
        disp "http://local/e/"            0 "no match",
        disp "http://local/e/a"           0 "no match",
        disp "http://local"               0 "empty",
        disp "http://local/"              0 "no match",
        disp "http://local/f1/a/b"        3 "f1 a.b",
        disp "http://local/f1/a/b/"       4 "f1 a.b.",
        disp "http://local/f1/c/c"        3 "f1 c.c",
        disp "http://local/f2/c/c"        3 "f2 2",
        disp "http://local/f2/c/c/"       4 "f2 3",
        disp "http://local/f3/c/c"        3 "f3",
        disp "http://local/f3/c"          0 "no match",
        disp "http://local/g"             0 "no match",
        disp "http://local/g/"            2 "trailing",
        disp "http://local/g/a"           0 "no match",
        disp "http://local/h"             1 "no path",
        disp "http://local/h/"            2 "no path",
        disp "http://local/h/a"           0 "no match"
      ],
      TestLabel "query" $ TestList [
        disp "http://local/p?a=a"         0 "no match",
        disp "http://local/p?a=a&b=b"     1 "a=a",
        disp "http://local/p?a=b&b=x"     0 "no match",
        disp "http://local/p?b=b&a=a"     0 "a=a",
        disp "http://local/p?a=b&b=1"     1 "a:b b:1",
        disp "http://local/p?a=b&b=2&c=2" 1 "a:b b:2",
  --           disp "http://local/p?x=1&y=2"     1 "coord:3",
  --           disp "http://local/p?x=1&y=2&a="  0 "no match",

        disp "http://local/q"             1 "q end",
        disp "http://local/q?x=1"         0 "no match"
      ]
    ]


build :: String -> URI -> Test
build url built = TestCase $ do
    let (Just url') = parseRelativeReference url
    url' @=? built

testBuild = TestLabel "building" $ TestList [
      TestLabel "path" $ TestList [
        build "/a/b"    $ bAB,
        build "/b/a"    $ bBA,
        build "/c/a"    $ bC  "a",
        build "/d1/1"   $ bD1 1,
        build "/d2/1/2" $ bD2 1 2,
        build "/d3/0"   $ bD3,
        build "/e"      $ bE1,
        build ""        $ bE2,
        build "/f1/a/b" $ bF1 ["a","b"],
        build "/f2/x/x" $ bF2 2,
        build "/f3/x/x" $ bF3,
        build "/g/"     $ bG,
        build "/h/"     $ bH
      ],
      TestLabel "query" $ TestList [
        build "/p?a=a&b=b" $ bP1,
        build "/p?a=a&b=1" $ bP2 "a" 1,
        build "/q"         $ bQ
      ]
    ]

main = runTestTT $ TestList [testDispatch, testBuild]