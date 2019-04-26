-- Copyright 2019 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.



{-# LANGUAGE CPP, LambdaCase, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Exception
import Control.Monad
import Data.List (intercalate)
import System.Console.GetOpt (ArgDescr(ReqArg, NoArg)
                             , ArgOrder(Permute), OptDescr(Option)
                             , getOpt, usageInfo
                             )
import System.Environment (getArgs)
import System.Exit
import System.IO (Handle, hPutStr, hPutStrLn, stderr, stdout)
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import System.Timeout
import Text.Printf

import qualified NinetyNineSolutions as C

-- term color

type TermColor = String

kR = "\x1b[31;1m"
kG = "\x1b[32;1m"

putTag :: String -> TermColor -> IO ()
putTag = hPutTag stdout

hPutTag :: Handle -> String -> TermColor -> IO ()
hPutTag h tag color = do
  isTTY <- queryTerminal stdOutput
  if isTTY
    then hPutStr h $ "[" ++ color ++ tag ++ "\x1b[0m]"
    else hPutStr h $ "[" ++ tag ++ "]"


-- options

newtype Section = Section Int
  deriving Eq

instance Bounded Section where
  minBound = Section 1
  maxBound = Section 6

instance Ord Section where
  compare (Section s1) (Section s2) = compare s1 s2

instance Enum Section where
  toEnum = Section
  fromEnum (Section i) = i

validSection :: Section -> Bool
validSection s = s >= minBound && s <= maxBound

newtype Options = Options
    { optSections :: [Section]
    }

options :: [OptDescr (Options -> IO Options)]
options =
  [
    Option ['s'] ["section"]
      (ReqArg (\v opts ->
                 do let i = read v
                    unless (validSection (Section i)) $ do
                      hPutTag stderr "Error" kR
                      hPutStrLn stderr $ " Invalid section number: " ++ v
                      let (Section from, Section to) = (minBound, maxBound)
                      hPrintf stderr "Valid section numbers: %d..%d\n" from to
                      exitFailure
                    return $ opts { optSections = optSections opts ++ [Section i] }
              )
        "<id>"
      )
      "Include section <id> in the run",
    Option ['h'] ["help"]
      (NoArg (\_ ->
                do hPutStrLn stderr (usageInfo usageHeader options)
                   exitSuccess
             ))
      "Show this help message"
  ]

usageHeader :: String
usageHeader = "Usage: ./main [-h] [-s|--section <id>] [--help]"


-- tests

ifError :: (String -> a) -> ErrorCall -> a
#if __GLASGOW_HASKELL__ < 800
ifError f (ErrorCall s) = f s
#else
ifError f (ErrorCallWithLocation s _) = f s
#endif

timeLimit :: Int
timeLimit = 1000000 -- 10^6 microseconds = 1 second

test :: (Show a, Eq a) => String -> a -> a -> IO Bool
test name expected actual = do
  let checkEq = if expected == actual
                then True  <$ onSuccess
                else False <$ onError
      runTest = timeout timeLimit $ catch checkEq $ ifError $ (False <$) . onFailure
  void $ printf "%-42s" name
  result <- runTest >>= maybe (False <$ onTimeout) return
  putStrLn ""
  return result
  where onSuccess   = putTag "OK" kG >> printf " got: %s" (show actual)
        onError     = putTag "KO" kR >> printf " want: %s; got: %s" (show expected) (show actual)
        onFailure s = putTag "!!" kR >> printf " error: %s" s
        onTimeout   = putTag "??" kR >> putStr " (timeout)"

-- main

tests :: [Section] -> [IO Bool]
tests sections =
  let display s = True <$ putStrLn s
  in intercalate [display ""]
     $ flip map sections
     $ \case Section 1 ->
               [ display "#### Problems 1 to 10"
               , test "myLast [1,2,3,4]" 4               $ C.myLast [1,2,3,4]
               , test "myLast ['x','y','z']" 'z'         $ C.myLast ['x','y','z']
               , test "myButLast [1,2,3,4]" 3            $ C.myButLast [1,2,3,4]
               , test "myButLast ['a'..'z']" 'y'         $ C.myButLast ['a'..'z']
               , test "elementAt [1,2,3] 2" 2            $ C.elementAt [1,2,3] 2
               , test "elementAt \"haskell\" 5" 'e'      $ C.elementAt "haskell" 5
               , test "myLength [123, 456, 789]" 3       $ C.myLength [123, 456, 789]
               , test "myLength \"Hello, world!\"" 13    $ C.myLength "Hello, world!"
               , test "myReverse \"A man, a plan, a canal, panama!\""
                      "!amanap ,lanac a ,nalp a ,nam A"  $ C.myReverse "A man, a plan, a canal, panama!"
               , test "myReverse [1,2,3,4]" [4,3,2,1]    $ C.myReverse [1,2,3,4]
               , test "isPalindrome [1,2,3]" False       $ C.isPalindrome [1,2,3]
               , test "isPalindrome \"madamimadam\"" True
                                                         $ C.isPalindrome "madamimadam"
               , test "isPalindrome [1,2,4,8,16,8,4,2,1]" True
                                                         $ C.isPalindrome [1,2,4,8,16,8,4,2,1] 
               , test "flatten (Elem 5)" [5]             $ C.flatten (C.Elem 5)
               , test "flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])"
                      [1,2,3,4,5]                        $ C.flatten (C.List [C.Elem 1, C.List [C.Elem 2, C.List [C.Elem 3, C.Elem 4], C.Elem 5]])
               , test "compress \"aaaabccaadeeee\"" "abcade"
                                                         $ C.compress "aaaabccaadeeee"
               , test "compress [1,2,2,2,3,3,4,5,5,5]"  [1,2,3,4,5] 
                                                         $ C.compress [1,2,2,2,3,3,4,5,5,5]
               , test "pack \"aaaabccaadeeee\"" ["aaaa","b","cc","aa","d","eeee"] 
                                                         $ C.pack "aaaabccaadeeee"
               , test "pack [1,2,2,2,3,3,4,5,5,5]" [[1],[2,2,2],[3,3],[4],[5,5,5]] 
                                                         $ C.pack [1,2,2,2,3,3,4,5,5,5]
               , test "encode \"aaaabccaadeeee\"" [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
                                                         $ C.encode "aaaabccaadeeee"   
               , test "encode [1,2,2,2,3,3,4,5,5,5]" [(1,1),(3,2),(2,3),(1,4),(3,5)] 
                                                         $ C.encode [1,2,2,2,3,3,4,5,5,5]
               ]
             Section 2 ->
               [ display "#### Problems 11 to 20"
               , test "encodeModified \"aaaabccaadeeee\""
                      [C.Multiple 4 'a', C.Single 'b', C.Multiple 2 'c', C.Multiple 2 'a', C.Single 'd', C.Multiple 4 'e']
                                                         $ C.encodeModified "aaaabccaadeeee"
               , test "encodeModified [1,2,2,2,3,3,4,5,5,5]"  [C.Single (1::Int), C.Multiple 3 2, C.Multiple 2 3, C.Single 4, C.Multiple 3 5] 
                                                         $ C.encodeModified [1,2,2,2,3,3,4,5,5,5]
               , test "decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']"
                       "aaaabccaadeeee"                  $ C.decodeModified [C.Multiple 4 'a', C.Single 'b', C.Multiple 2 'c', C.Multiple 2 'a', C.Single 'd', C.Multiple 4 'e']
               , test "decodeModified [Single (1::Int), Multiple 3 2, Multiple 2 3, Single 4, Multiple 3 5]"      
                       [1,2,2,2,3,3,4,5,5,5]             $ C.decodeModified [C.Single (1::Int), C.Multiple 3 2, C.Multiple 2 3, C.Single 4, C.Multiple 3 5]
               , test "encodeDirect \"aaaabccaadeeee\"" [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
                                                         $ C.encodeDirect "aaaabccaadeeee"
               , test "encodeDirect [1,2,2,2,3,3,4,5,5,5]" [(1,1),(3,2),(2,3),(1,4),(3,5)]
                                                         $ C.encodeDirect [1,2,2,2,3,3,4,5,5,5]
               , test "dupli [1,2,3]" [1,1,2,2,3,3]      $ C.dupli [1,2,3] 
               , test "dupli \"abbacddc\"" "aabbbbaaccddddcc"
                                                         $ C.dupli "abbacddc"
               , test "repli [1,2,3] 3" [1,1,1,2,2,2,3,3,3]
                                                         $ C.repli [1,2,3] 3
               , test "repli [1,2,3] 5" [1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5]
                                                         $ C.repli [1,2,3] 5    
               , test "dropEvery \"abcdefghij\" 3" "abdeghj"
                                                         $ C.dropEvery "abcdefghij" 3
               , test "dropEvery [1,2,3,4,5,6,7] 2" [1,3,5,7] 
                                                         $ C.dropEvery [1,2,3,4,5,6,7] 2
               , test "split \"abcdefghij\" 3" ("abc", "defghij") 
                                                         $ C.split "abcdefghij" 3
               , test "split [1,2,3,4,5] 2" ([1,2],[3,4,5])
                                                         $ C.split [1,2,3,4,5] 2
               , test "slice \"abcdefghij\" 5 8" "efg"   $ C.slice "abcdefghij" 5 8 
               , test "slice [1,2,3,4,5] 2 4" [2,3,4]    $ C.slice [1,2,3,4,5] 2 4
               , test "rotate ['a','b','c','d','e','f','g','h'] 3" "defghabc"
                                                         $ C.rotate ['a','b','c','d','e','f','g','h'] 3
               , test "rotate ['a','b','c','d','e','f','g','h'] (-2)" "ghabcdef"
                                                         $ C.rotate ['a','b','c','d','e','f','g','h'] (-2)
               , test "removeAt \"abcd\" 2" ('b', "acd") $ C.removeAt "abcd" 2
               ]

             Section unexpected ->
               [ display $ "Unexpected section requested: " ++ show unexpected
               ]

parseOpts :: IO Options
parseOpts = do
  args <- getArgs
  case getOpt Permute options args of
    (o, [], []) -> do
      let defaultOptions = Options { optSections = [] }
      opts <- foldM (flip id) defaultOptions o
      let opts' = opts { optSections = if null $ optSections opts
                                       then [minBound .. maxBound]
                                       else optSections opts }
      return opts'
    (_, nonOptions@(_:_), _) -> do
      hPrintf stderr " Unexpected non-option argument(s): %s\n"
        (intercalate ", " $ map show nonOptions)
      hPutStrLn stderr $ usageInfo usageHeader options
      exitFailure
    (_, _, errs@(_:_)) -> do
      hPutStrLn stderr (concat errs ++ usageInfo usageHeader options)
      exitFailure

main :: IO ()
main = do
  opts <- parseOpts
  results <- sequence $ tests (optSections opts)
  let failing = length . filter not $ results
  when (failing > 0) exitFailure
