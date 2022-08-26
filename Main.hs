module Main where
import System.CPUTime
import Testing
import Data.Tuple (swap)
import System.Environment
import System.Console.GetOpt
import Framework
import DigitRecognition
import Control.Monad
import System.Console.ANSI
import Test.Grader.Timing


timingChunk = 5

-- Options record
data Options = Options {
    optHelp              :: Bool
  , optTest              :: Bool
  , timeout              :: Double
  , optRanks             :: Bool
  , optForceTests        :: Bool
  , count                :: Int
  , verbocity            :: Int
  , trainingImagesPath   :: FilePath
  , trainingLabelsPath   :: FilePath
  , validationImagesPath :: FilePath
  , validationLabelsPath :: FilePath
} deriving Show

defaultOptions :: Options
defaultOptions = Options {
     optHelp = False
   , optTest = False
   , optRanks = False
   , optForceTests = False
   , timeout = 0
   , count = -1
   , verbocity = 1
   , trainingImagesPath = "digitdata/trainingimages"
   , trainingLabelsPath = "digitdata/traininglabels"
   , validationImagesPath = "digitdata/validationimages"
   , validationLabelsPath = "digitdata/validationlabels"
 }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['h'] ["help"] (NoArg (\opts -> opts { optHelp = True })) "Print a help message and exit.",
  Option [] ["test"] (NoArg (\opts -> opts { optTest = True })) "Runs a series of tests on your code",
  Option ['q'] ["quick"] (NoArg (\opts -> opts { trainingImagesPath = "digitdata/testimages", trainingLabelsPath = "digitdata/testlabels" })) "Uses test images and labels for faster execution",
  Option [] ["quiet"] (NoArg (\opts -> opts {verbocity = 0})) "Only print error messages on tests, or minimal output when solving.",
  Option [] ["timeout"] (ReqArg (\n opts -> opts { timeout = read n }) "N") "Timeout after N minutes",
  Option ['c'] ["count"] (ReqArg (\n opts -> opts { count = read n }) "N") "Only test the first N images",
  Option ['v'] ["verbocity"] (ReqArg (\n opts -> opts { verbocity = read n }) "N") "Set the verbocity of the output. 0-2, 2 being the most verbose.",
  Option []    ["ranking"] (NoArg (\opts -> opts { optRanks = True, verbocity = 2})) "Output the ranks for each digit instead of the most likely digit. Set verbocity to 2.",
  Option [] ["forceTests"]       (NoArg  (\opts -> opts { optTest=True, optForceTests = True})) "Force evaluation of all test cases, regardless of early failures.",
  Option []    ["train-image"] (ReqArg (\path opts -> opts { trainingImagesPath = path }) "DIR") "Override the path for training images",
  Option []    ["train-label"] (ReqArg (\path opts -> opts { trainingLabelsPath = path }) "DIR") "Override the path for training labels",
  Option []    ["valid-image"] (ReqArg (\path opts -> opts { validationImagesPath = path }) "DIR") "Override the path for validation images",
  Option []    ["valid-label"] (ReqArg (\path opts -> opts { validationLabelsPath = path }) "DIR") "Override the path for validation labels"
  ]

-- Main IO function
main :: IO()
main = do
  args <- getArgs
  (opts, errs) <- compilerOpts args
  if not (null errs) 
  then do
         mapM putStrLn errs
         return ()
  else if optHelp opts 
       then helpIO
       else if optTest opts
            then if trainingImagesPath opts == "digitdata/testimages"
                 then putStrLn "Tests are not compatible with the --quick flag"
                 else do
                    trainingImages <- labeledImagesFromFiles (trainingImagesPath opts) (trainingLabelsPath opts)
                    allValidationImages <- pixelImagesFromFile (validationImagesPath opts)
                    allValidationLabels <- labelsFromFile (validationLabelsPath opts)
                    runTests trainingImages allValidationImages allValidationLabels (verbocity opts) (optForceTests opts)
            else readAndClassify opts

readAndClassify :: Options -> IO()
readAndClassify opts = do
    trainingImages <- labeledImagesFromFiles (trainingImagesPath opts) (trainingLabelsPath opts)
    allValidationImages <- pixelImagesFromFile (validationImagesPath opts)
    allValidationLabels <- labelsFromFile (validationLabelsPath opts)
    when (length allValidationImages /= length allValidationLabels) $ error "Validation Images and Validation Labels are different lengths!"
    start <- getCPUTime
    corpus <- eval $ buildCorpus trainingImages
    end <- getCPUTime
    when (verbocity opts > 1) $ 
            putStrLn $ "Time Elapsed Building Corpus:\t" ++ (show $ (fromIntegral (end - start)) / (10^12)) ++ "s"
    if optRanks opts 
    then let ranks = [rankImage corpus image | image <- allValidationImages]
             bestRank ranking = snd $ maximum $ [swap x | x <- ranking]
         in displayResults opts bestRank allValidationImages allValidationLabels ranks
    else let guesses = [classifyImage corpus image | image <- allValidationImages]
         in displayResults opts id           allValidationImages allValidationLabels guesses

displayResults :: Show a => Options -> (a -> Digit) -> [PixelImage] -> [Digit] -> [a] -> IO()
displayResults opts toDigit vImgs vLbls guesses = do
      let protoResults = zip guesses vLbls
      start <- getCPUTime
      results <- evalResults opts toDigit protoResults
      end <- getCPUTime
      when (verbocity opts > 1) $ 
            putStrLn $ "Time Elapsed:\t" ++ (show $ (fromIntegral (end - start)) / (10^12)) ++ "s"
      let total = length results
      let correct = length $ filter (\(guess, answer) -> (toDigit guess) == answer) results
      putStrLn $ "Correct:\t" ++ show ((correct * 100) `div` total)  ++ "%"

putResult :: Show a => Bool -> (a -> Digit) -> (a, Digit) -> IO ()
putResult verb toDigit (elem, correct) 
    | (guess == correct) = when verb $ do
                            putStr $ elemDisp  ++ "\t" ++ (show correct) ++ "\t"
                            setSGR [SetColor Foreground Vivid Green]
                            putStrLn "Correct!"
                            setSGR [Reset]
    | otherwise          = when verb $ do
                            putStr $ elemDisp ++ "\t" ++ (show correct) ++ "\t"
                            setSGR [SetColor Foreground Vivid Red]
                            putStrLn "Incorrect!"
                            setSGR [Reset]
    where 
        guess = toDigit elem
        elemStr = show elem
        elemDisp = if length elemStr > 8 then take 85 $ elemStr ++ (repeat ' ') else elemStr

evalResults :: Show a => Options -> (a -> Digit) -> [(a, Digit)] -> IO [(a, Digit)]
evalResults opts toDigit protoResults =  do
    let results = if count opts == -1 then protoResults else take (count opts) protoResults
    let verb = verbocity opts > 0
    when verb $ putStrLn "Guess\tLabel\tResult"
    let remaining = timeout opts * 60
    if timeout opts == 0 
    then do mapM_ (putResult verb toDigit) $ results
            return results
    else putWithTimeout verb toDigit remaining results

time :: IO t -> IO Double 
time a = do 
  start <- getCPUTime 
  v <- a 
  end <- getCPUTime 
  let diff = (fromIntegral (end - start)) / (10^12) 
  return (diff :: Double) 
    
-- Chunk the computations into groups of ten, then check if a timeout has been exceeded
putWithTimeout :: Show a => Bool -> (a -> Digit) ->  Double -> [(a, Digit)] -> IO [(a, Digit)]
putWithTimeout verb toDigit timeout results = do
    let batch = take timingChunk results
    elapsed <- time $ mapM_ (putResult verb toDigit) $ batch
    let remaining = timeout - elapsed
    if remaining > 0
    then fmap (batch++) $ putWithTimeout verb toDigit remaining (drop timingChunk results)
    else do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn "Timeout reached."
      setSGR [Reset]
      return batch

-- Return the list of flags
compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
     (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: classifier [OPTION...]"

-- Print help
helpIO :: IO()
helpIO = putStrLn $ usageInfo usage options
    where usage = "Usage: classifier [OPTION...]"
