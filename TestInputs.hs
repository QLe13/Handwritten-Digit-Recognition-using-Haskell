module TestInputs where
import System.IO.Unsafe
import Framework
import Data.List
import DigitRecognition

-- Test values for showPixelImage
smallImage :: PixelImage
smallImage= [[True, True], [True, False]]

shownImage = validImages!!0

showPixelImage1Res :: String
showPixelImage1Res = "##\n# \n"

showPixelImage2 :: PixelImage 
showPixelImage2 = validImages !! 0

-- Test values for lookupVal
assocs1 = [('a', 8), ('b', 7), ('c', 9)]
assocs2 = zip [1..10] ['a'..'z']
assocs3 = [(1, "lookup"), (1, "conflicts"), (1, "are"), (1, "fun")] 
assocs4 = []

-- The "toy" example test values for buildCorpus from the webpage/in-class activity
img1a = [[False,True],[False,True]]
img1b = [[True,False],[True,False]]
img1c = [[True,True],[True,True]]
img2a = [[True,True],[False,True]]
img2b = [[True,False],[False,True]]

images = [(img1a, 1), (img1b, 1), (img1c, 1), (img2a, 2), (img2b, 2)] 

toyCorpusOutput = [(1, [img1a, img1b, img1c]), (2, [img2a, img2b])]

--this will be true if your buildCorpus is working correct for this example.
yayBuildCorpus = (sortCorpus (buildCorpus images)) == (sortCorpus toyCorpusOutput)
--
-- You can use the toyCorpusOutput on further functions if you modify allFeatures.
-- See if you can classify the newIMg below.
newImg= [[False,True],[False,False]]


--Test values for buildCorpus, and other functions that need a corpus



-- 
commonCorpusInput :: [(PixelImage, Digit)]
commonCorpusInput = take 1000 trainingData

commonCorpus = buildCorpus commonCorpusInput
corpusLengths = [(d, length i) | (d,i) <- commonCorpus]
corpusLengthsAnswer = [(0,97),(1,116),(2,99),(3,93),(4,105),(5,92),(6,94),(7,117),(8,87),(9,100)]


-- Results for the probOfDigit tests, which are run on the commonCorpus
probOfDigit1Res = [(0, 97 `outOf` 1000), (1, 116 `outOf` 1000), (2, 99 `outOf` 1000), (3, 93 `outOf` 1000)]
probOfDigit2Res = [(4, 105 `outOf` 1000), (5, 92 `outOf` 1000), (6, 94 `outOf` 1000), (7, 117  `outOf`  1000), (8, 87 `outOf` 1000), (9, 100  `outOf` 1000)]
probOfDigitTest3 = (sum $ [ probOfDigit (buildCorpus commonCorpusInput) x | x <- [0..9] ])

--Images for digit 0, used in testing probOfFeature/probOfNoFeature
probFeaturesImages = take 20 $ [ dig | (img, dig) <- trainingData, dig == 0 ]

--Images used in various tests, particularly rankOfDigit and classifyImage with the commonCorpus
--below
img3 = validImages!!2
img6 = validImages!!5
img9 = validImages!!8
img11 = validImages!!10
img21 = validImages!!20
img55 = validImages!!54

--Cornercases for lookupVal. Try looking up 1, and 2. 
lookupValFCTest1 :: [(Integer, [Char])]
lookupValFCTest1 = [(1, "lookup"), (1, "conflicts"), (1, "are"), (1, "fun")]

-- Used to test buildCorpus for full credit
smallCorpusInput :: [(PixelImage, Digit)]
smallCorpusInput = take 10 trainingData
smallCorpus = buildCorpus smallCorpusInput
smallCorpusLengths = [(d, length i) | (d,i) <- smallCorpus]

buildCorpusFullCredit1Res :: [(Integer, Integer)]
buildCorpusFullCredit1Res = [(0,1),(1,3),(2,1),(3,1),(4,2),(5,1),(9,1)]

--Some particularly simple images to check for smoothing, see the examples in the project
--description
smoothingImgs :: [([[Bool]], Digit)]
smoothingImgs =
            let bottom = replicate 27 (replicate 28 True) 
                footer = replicate 24 True
                a = ([False, False, False, False] ++ footer):bottom
                b = ([False, False, False, True] ++ footer):bottom
                c = ([False, False, True , True] ++ footer):bottom
                d = ([False, True , True , True] ++ footer):bottom
            in [(img, 0) | img <- [a,b,c,d]] 

--Ignore these!
trainingData :: [(PixelImage, Digit)]
trainingData = unsafePerformIO $ labeledImagesFromFiles  "digitdata/trainingimages"  "digitdata/traininglabels"
validImages :: [PixelImage]
validImages = unsafePerformIO $  pixelImagesFromFile "digitdata/validationimages"
validLabels :: [Digit]
validLabels = unsafePerformIO $ labelsFromFile "digitdata/validationlabels"
