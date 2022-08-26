{-# LANGUAGE FlexibleInstances #-}

module Testing where
import Numeric
import Debug.Trace
import Data.Maybe
import DigitRecognition
import Framework
import System.CPUTime
import Data.Semigroup
import Control.Monad
import Control.Monad.Extra
import Test.Grader.Core
import Test.Grader.Rubric
import Test.Grader.Eval
import Test.Grader.Tests
import Test.Grader.Parsing.Util
import Test.Grader.Parsing
import System.Console.ANSI
import Control.Exception
import Data.List
import Data.Either
import Data.Dynamic
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS
import System.IO.Unsafe

class Testable a where
    makeLens :: a -> [(Digit, Int)]
    getInfo :: Digit -> a -> Dynamic

    
instance Testable ([(Digit, Int)], [(Digit, [[(Int, Int)]])]) where
    makeLens (dc, ds) = dc
    getInfo digit (ds, dc) = toDyn $ lookupVal digit dc
    
instance Testable [(Digit, [PixelImage])] where
    makeLens corpus = [(d, length i) | (d,i) <- corpus]
    getInfo digit corpus = toDyn $ lookupVal digit corpus

testAllFeatures :: Grader String
testAllFeatures = assess "allFeatures" 2 $ do
                check "that the length of allFeatures is 784" $ (length allFeatures) `shouldBe` 784
                check "that allFeatures contains 0" $ 0 `shouldBeIn` allFeatures
                check "that allFeatures contains 700" $ 700 `shouldBeIn` allFeatures
                check "allFeatures is correct" $ quiet (allFeatures `shouldBe` (take . fst . head . readHex) "310" [0..])
    
testAllDigits :: Grader String
testAllDigits = assess "allDigits" 2 $ quiet $ (sort allDigits) `shouldBe` (map read $ map (\x -> [x]) "0123456789")

imageOutput :: String
imageOutput = unlines ["                            ",
                       "                            ",
                       "                            ",
                       "                            ",
                       "                            ",
                       "                            ",
                       "                            ",
                       "      ######                ",
                       "      ################      ",
                       "      ################      ",
                       "            # ########      ",
                       "                  ###       ",
                       "                 ####       ",
                       "                 ####       ",
                       "                ####        ",
                       "                ###         ",
                       "                ###         ",
                       "               ###          ",
                       "              ####          ",
                       "              ###           ",
                       "             ####           ",
                       "            ####            ",
                       "           ####             ",
                       "           ####             ",
                       "          #####             ",
                       "          #####             ",
                       "          ###               ",
                       "                            "]

testShowPixelImage :: PixelImage -> Grader String
testShowPixelImage img = assess "showPixelImage" 8 $ do
                       check "that a small image displays properly" $ (showPixelImage [[True, True], [True, False]]) `shouldBe` ("##\n# \n")
                       check "that a full-sized image displays properly" $ quiet $ (showPixelImage img) `shouldBe` (imageOutput)

testLookupVal :: Grader String
testLookupVal = assess "lookupVal" 7 $ do
              check "that 'b' looks up in the given example" $ (lookupVal 'b' [('a', 8), ('b', 7), ('c', 9)]) `shouldBe` 7 
              check "that j is the 10th letter" $ (lookupVal 10 (zip [1..10] ['a'..'z'])) `shouldBe` 'j'
              check "that c is the 3rd letter" $ (lookupVal 'c' (zip ['a'..'z'] [1..10])) `shouldBe` 3              


testBuildCorpus :: Corpus -> Grader String
testBuildCorpus corpus = 
    let lens = makeLens corpus
        lc v = (v,lookupVal v lens)
    in assess "buildCorpus" 10 $ do
        check "That buildCorpus is defined" $ shouldBeDefined "buildCorpus"
        checkpoint
        check "that buildCorpus produces 10 entries" $ (length lens) `shouldBe` 10
        check "that buildCorpus groups digits with the right number of images" $ (map (\v -> (v, lookupVal v (makeLens corpus))) [0..9]) `shouldBe` [(0,97),(1,116),(2,99),(3,93),(4,105),(5,92),(6,94),(7,117),(8,87),(9,100)]


testProbOfDigit :: Corpus -> Grader String
testProbOfDigit corpus = 
    let testDigit k = (k,probOfDigit corpus k)
    in assess "probOfDigit" 8 $ do
          check "probOfDigit for 0..3" $ (map testDigit [0..3]) `shouldBe` [(0, 97 `outOf` 1000), (1, 116 `outOf` 1000), (2, 99 `outOf` 1000), (3, 93 `outOf` 1000)]
          check "probOfDigit for 4..9" $ (map testDigit [4..9]) `shouldBe` [(4, 105 `outOf` 1000), (5, 92 `outOf` 1000), (6, 94 `outOf` 1000), (7, 117  `outOf`  1000), (8, 87 `outOf` 1000), (9, 100  `outOf` 1000)]
          check "probabilites of all digits sums to 1" $ (sum $ [ probOfDigit corpus x | x <- [0..9] ]) `shouldApprox` 1


testProbOfFeature zeroImages = 
    assess"probOfFeature" 6 $ do
            check "is near 0 when no images have that feature (10)" $ (probOfFeature zeroImages 10) `shouldApprox` 0 
            check "feature 300 on the zeroImages" $ (probOfFeature zeroImages 300) `shouldApprox` (17 `outOf` 20)

testProbOfNoFeature zeroImages = 
    assess "probOfNoFeature" 6 $ do
            check "is near 1 when no images have that feature (10)" $  (probOfNoFeature zeroImages 10) `shouldApprox` 1 
            check "feature 300 on the zeroImages" $ (probOfNoFeature zeroImages 300) `shouldApprox` (3 `outOf` 20)

testRankOfDigit :: Corpus  -> [PixelImage] -> Grader String
testRankOfDigit corpus validImages = 
    let img1 = validImages!!5
        img2 = validImages!!10
        img3 = validImages!!20
    in assess "rankOfDigit" 12 $ do
            check "1 more likely than 8 for 6th image" $ (((rankOfDigit corpus 1 img1) > (rankOfDigit corpus 8 img1))) `shouldBe` True
            check "0 more likely than 4 for 11th image" $ (((rankOfDigit corpus 0 img2) > (rankOfDigit corpus 4 img2))) `shouldBe` True
            check "0 more likely than 5 for 11th image" $ (((rankOfDigit corpus 0 img2) > (rankOfDigit corpus 5 img2))) `shouldBe` True
            check "9 more likely than 1 for 21st image" $ (((rankOfDigit corpus 9 img3) > (rankOfDigit corpus 1 img3)))  `shouldBe` True
            

testClassifyImage :: Corpus -> [PixelImage] -> Grader String
testClassifyImage corpus validImages = 
    assess "classifyImage" 14 $ do
            check "the 3rd image is 1" $ (classifyImage corpus (validImages!!2)) `shouldBe` 1  
            check "the 11th image is 0" $ (classifyImage corpus (validImages!!10)) `shouldBe` 0  
            check "the 21rd image is 9" $ (classifyImage corpus (validImages!!20)) `shouldBe` 9  


testLookupValFC :: Grader String
testLookupValFC = assess "lookupVal implementation details" 4 $ do
              duplGen <- check "for any error when more than 1 match" $ shouldErrGenerous (lookupVal 1 [(1, "lookup"), (1, "conflicts"), (1, "are"), (1, "fun")]) 
              dupl <- check "for a non-Prelude error when more than 1 match" $ shouldErr (lookupVal 1 [(1, "lookup"), (1, "conflicts"), (1, "are"), (1, "fun")]) 
              missing <- check "for a non-Prelude error when 0 the key is missing" $ shouldErr ((lookupVal 1 []) :: String)
              return $ do 
                  whenM (passed duplGen) (addPoints 1) 
                  whenM (passed dupl) (addPoints 1)
                  whenM (passed missing) (addPoints 2) 
                  passWhen =<< allPassed 
            
testBuildCorpusFC :: [(PixelImage, Digit)] -> Grader String
testBuildCorpusFC trainingImages =
    let smallCorpus = buildCorpus (take 10 trainingImages)
        slens = makeLens smallCorpus
        ls v = (v,lookupVal v slens)
    in assess "buildCorpus implementation details" 4 $ do
        small <- check "small inputs" $ (map ls [0,1,2,3,4,5,9]) `shouldBe` [(0,1),(1,3),(2,1),(3,1),(4,2),(5,1),(9,1)]
        blanks <- check "that you don't generate empty associations" $ (length slens) `shouldBe` 7
        shlem <- check "for excessive duplication of work" $ shouldNotRepeatWork "buildCorpus"
        lin <- check "that buildCorpus is linear" $ buildCorpus `shouldBeLinearIn` (`take` trainingImages)
        return $ do
            whenM (passed small <&&> passed blanks) $ addPoints 2
            whenM (passed shlem <&&> passed lin) $ addPoints 2
            passWhen =<< allPassed

testSmoothing :: Grader String
testSmoothing =
    let summary = fd $ getInfo 0 $ buildCorpus smoothingImgs
        features = [probOfFeature summary f | f <- [0..4]]
        complements = [probOfNoFeature summary f | f <- [0..4]] -- can this crash if probOfNoFeature is undefined?
        results = features ++ complements
        testGaps f = let gaps = zipWith (-) f (tail f) 
                     in and $ zipWith (==) gaps (tail gaps)
        unique s = (sort s) == (sort $ nub s)
    in assess "Tests for smoothing" 7 $ do
        nZero <- checkLoud "that no probabilities = 0" $ results `shouldNotContain` 0
        nGtOne <- checkLoud "that no probabilities > 1" $ results `shouldSatisfy` (not.any (>1))
        nOne <- checkLoud "that no one probabilities = 1" $  results `shouldNotContain` 1
        sumOne <- checkLoud "that probabilities sum to one" $ zipWith (+) features complements `shouldBe` [1,1,1,1,1]
        noColl <- checkLoud "that there are no collisions" $
             (features `shouldSatisfy` unique) <> (complements `shouldSatisfy` unique)
        evenGap <- checkLoud "that gaps between probabilities are even" $ 
         (features `shouldSatisfy` testGaps) <> (complements `shouldSatisfy` testGaps)
        return $ do
            whenM (passed nZero <&&> failedOne [nGtOne,nOne]) $ passWith 1
            whenM (passedAll [nZero, nGtOne, nOne] <&&> failed sumOne) $ passWith 4
            whenM (passedAll [nZero, nGtOne, nOne, sumOne]) $ passWith 7
            whenM (failed evenGap <||> failed noColl) $ dockPoints 2
            
smoothingImgs :: [([[Bool]], Digit)]
smoothingImgs =
            let bottom = replicate 27 (replicate 28 True) 
                footer = replicate 24 True
                a = ([False, False, False, False] ++ footer):bottom
                b = ([False, False, False, True] ++ footer):bottom
                c = ([False, False, True , True] ++ footer):bottom
                d = ([False, True , True , True] ++ footer):bottom
            in [(img, 0) | img <- [a,b,c,d]] 

testAvoidWork :: [(PixelImage, Digit)] -> [PixelImage] -> Grader String
testAvoidWork trainingData images = 
  let findImage d = head [image | (image, digit) <- trainingData, digit == d]
      safeData = [(findImage d, d) | d <- allDigits] ++ trainingData
      numTrainingImages x = buildCorpus $ take (max 10 (x+1)) safeData
      classifyImages corpus = map (classifyImage corpus) (take 20 images)
  in assess "that you avoid unnecessary work" 10 $ classifyImages `shouldBeConstantIn` numTrainingImages
 
testReqStyle :: Grader String
testReqStyle = assess "required code style" 0 $ do
    nr <- check "that you do not use recursion" $ noneShouldBeRecursive
    nhof <- check "that you do not use higher order functions" $ noneShouldBeHigherOrder
    nind <- check "that you do not use (!!)" $ shouldNotBeCalled "!!"
    return $ do  whenM (failed nr) (addPoints (-5))
                 whenM (failed nhof) (addPoints (-5))
                 whenM (failed nind) (addPoints (-5))
                 passWhen =<< allPassed 



testStyle = assess "common style mistakes (optional)" 0 $ do
    check "that you do not use nested list comprehensions" $ noneShouldUseNestedLC
    check "that you use lookupVal" $ shouldBeCalled "lookupVal"
    check "that you don't have unnecessary parens" $ noneShouldHaveUnneededParens 
    check "that you're pattern matching tuples instead of fst/snd" $ noneCouldMatchTuple 
    return passAssessment

--these need to be tested and so are worth points
testClassifyImageFC :: Corpus -> [PixelImage] -> Grader String
testClassifyImageFC corpus validImages = 
    assess "classifyImage implementation details (optional)" 0 $ do
            check "the 9th image is not 5" $ (((classifyImage corpus (validImages!!8)) /= 5)) `shouldBe` True 
            check "detect maximum rank of 0 on 55th image" $ shouldErr (classifyImage (buildCorpus [(validImages!!0, 10)]) (validImages!!54)) -- THIS CASE NEEDS TO BE TESTED!
            return passAssessment

testRankImage :: Corpus  -> [PixelImage] ->  Grader String
testRankImage corpus validImages =  
    assess "rankImage (completely optional)" 0 $ do
            check "rankImage" $ (length $ rankImage corpus (validImages!!2)) `shouldBe` 10
            check "that rankImage is sorted" $ (let ranks = map snd $ rankImage corpus (validImages!!2) in reverse ranks == sort ranks) `shouldBe` True  

tree :: [(PixelImage, Digit)] -> [PixelImage] -> [Digit] -> Grader String
tree trainingImages validImages validLabels = 
  let zeroImages = fd $ getInfo 0 $ mbc trainingImages 0 20
      medCorpus = buildCorpus $ take 1000 trainingImages
  in describe "Project 1" $ do
        testReqStyle 
        describe "milestone one" $ do
          testAllFeatures
          testAllDigits
          testShowPixelImage (validImages !! 0)
          testLookupVal
        checkpoint
        describe "milestone two" $ do
          testBuildCorpus medCorpus 
        checkpoint
        describe "core project" $ do
          testProbOfDigit medCorpus
          testProbOfFeature zeroImages
          testProbOfNoFeature zeroImages
          testRankOfDigit medCorpus validImages
          testClassifyImage medCorpus validImages
        checkpoint
        describe "full credit" $ do
          testLookupValFC
          testBuildCorpusFC trainingImages
          testSmoothing
          testAvoidWork trainingImages validImages 
        describe "optional requirements" $ do
          testStyle 
          testClassifyImageFC medCorpus validImages

runTests :: [(PixelImage, Digit)] -> [PixelImage] -> [Digit] -> Int -> Bool -> IO ()
runTests trainings valids labels verb force = do
    let a = runGrader $ tree trainings valids labels
    format <- makeFormat verb force "projectDesc.yaml"
    runRWST a () format
    return ()

mbc :: Eq a => [(PixelImage, a)] -> a -> Int -> Corpus
mbc xs d n = 
    let y = nub $ map snd $ xs
        i = take n $ map fst $ filter (\k -> snd k == d) xs
    in buildCorpus $ map (\x -> (x,0)) i

fd = (fromJust . fromDynamic)
