module DigitRecognition where
import Data.List (nub, sort)
import Data.List.Split (chunksOf)
import Data.Tuple (swap)
import Data.Ratio (numerator, denominator, (%))
import Debug.Trace
--                                          Type Aliases
type PixelImage = [[Bool]] 
-- A pixel image is a two-dimensional list of booleans.
-- False represents an empty pixel, and True a grey or black pixel. Each pixel image will be 28x28.
type Feature = Int
-- Each pixel location is considered a separate feature for classification. Because the image is
-- 28x28, there are 784 total features.
type Digit = Integer
-- Each image is of a specific digit, 0 through 9. To distinguish the labels or guesses from
-- other numbers, we use a type alias.


allDigits :: [Digit]
allDigits = [0,1,2,3,4,5,6,7,8,9]

allFeatures :: [Feature]
allFeatures = [0..783]

showPixelImage :: PixelImage -> String
helper :: [Bool] -> [Char]
helper row = [ if x == True then '#' else ' ' | x <- row]
showPixelImage img = unlines [ helper x | x <- img]

lookupVal :: Eq a => a -> [(a, b)] -> b
lookupVal key lst 
    | (length[snd tup| tup <- lst, fst tup==key ]) == 1 = head[snd tup| tup <- lst, fst tup==key ]
    | (length[snd tup| tup <- lst, fst tup==key ]) < 1 = error "Digit not available"
    | (length[snd tup| tup <- lst, fst tup==key ]) > 1 = error "More than one digit"


helper2 key imgLbls = [fst x | x <- imgLbls, snd x == key] 

setList imgLbls = nub [snd x | x <- imgLbls]

type DigitSummary = [(Digit, Summary)]
type DigitCount = [(Digit, Int)]
type Summary = [[(Int, Int)]]
type Corpus = (DigitCount, DigitSummary)

countDigit imgLbls = [(x, length (helper2 x imgLbls)) | x<-(setList imgLbls)]


helperTuple imgLbls digit ftr = 
                                let y = helper2 digit imgLbls
                                    a = length y
                                    b = length [1 | x<-y, hasFeature x ftr == True ]
                                in (b, a-b) 

summary :: [(PixelImage, Digit)] -> Digit -> Summary
featureGrid :: [[Feature]]
featureGrid = chunksOf 28 allFeatures
summary imgLbls digit = [[helperTuple imgLbls digit ftr | ftr <- line] | line <- featureGrid]

buildCorpus :: [(PixelImage, Digit)] -> Corpus
buildCorpus imgLbls = 
                     let list = setList imgLbls
                     in ( [(x, length (helper2 x imgLbls)) | x<-list], [(x, summary imgLbls x) | x<- list])


outOf :: Int -> Int -> Rational
outOf a b =  (fromIntegral a) % (fromIntegral b)
--Example:      2 `outOf` 10
--              (length [1..3]) `outOf` (length [1..10])
--hasFeature checks if an image has a specific feature: i.e. if that pixel is white or blank.

hasFeature :: PixelImage -> Feature -> Bool
hasFeature img ftr = 
    let dim = length img
        row = img !! (ftr `div` dim)
        pixel = row !! (ftr `mod` dim)
    in pixel
-- Example:    img `hasFeature` ftr
-- Given a corpus and a specific digit Y, probOfDigit estimates P(Y). This is the fraction
-- of the images in the corpus that are labeled with Y.  You will need to use `outOf` to create
-- the fraction.
-- Example: probOfDigit corpus 9
--         2 % 3
probOfDigit :: Corpus -> Digit -> Rational

probOfDigit corpus digit = outOf (sum [snd x | x <- fst corpus, fst x == digit]) (sum [snd x | x <- fst corpus])


-- Given the list of images (imgs) labeled for a given digit Y, and a feature F (ftr),
-- probOfFeature imgs ftr estimates the probability P(ftr=Black | Y).
getFeature :: Summary -> Feature -> (Int, Int)
getFeature sum ind = 
    let row = sum !! (ind `div` 28)
        val = row !! (ind `mod` 28)
    in val

probOfFeature :: Summary -> Feature -> Rational
probOfFeature summary ftr = outOf (1+ fst (getFeature summary ftr)) (fst (getFeature summary ftr) + snd(getFeature summary ftr) + 2)
-- Given the list of images (imgs) labeled for a given digit Y, and a feature F (ftr),
-- probOfNoFeature imgs ftr estimates the probability P(ftr=White | Y). See the assignment page
-- for details.
probOfNoFeature :: Summary -> Feature -> Rational

probOfNoFeature summary ftr = outOf (1+ snd (getFeature summary ftr)) (fst (getFeature summary ftr) + snd(getFeature summary ftr) + 2)
-- rankOfDigit estimate the rank of a given digit for a given instance

rankOfDigit :: Corpus -> Digit -> PixelImage -> Rational
prodOfPos :: Corpus -> Digit -> PixelImage -> Rational
prodOfPos corpus digit newImg = product[probOfFeature ((snd (head [x | x <- snd corpus , fst x == digit]))) y | y <- allFeatures, hasFeature newImg y == True]
prodOfNeg corpus digit newImg = product[probOfNoFeature ((snd (head [x | x <- snd corpus , fst x == digit]))) y | y <- allFeatures, hasFeature newImg y == False]
rankOfDigit corpus digit newImg = product[(probOfDigit corpus digit), (prodOfNeg corpus digit newImg), (prodOfPos corpus digit newImg) ]

-- classifyImage return the most likely digit, based on the rank computed by rankOfDigit.

classifyImage :: Corpus -> PixelImage -> Digit
rankTup :: Corpus -> PixelImage -> [(Rational,Digit)]
rankTup corpus newImg = [ (rankOfDigit corpus x newImg,x) | x <- allDigits ]
classifyImage corpus newImg = snd (maximum (rankTup corpus newImg))




-- valueOfRank takes a rank and turns it into a somewhat reasonable integer, suitable for
-- printing. The ranks may be negative
valueOfRank :: Rational -> Int
valueOfRank r = 350 + ratLog r 
    where numDigits x = length $ show x
          ratLog r = (numDigits $ numerator r) - (numDigits $ denominator r)



rankImage :: Corpus -> PixelImage -> [(Digit, Int)]
rankImage corpus newImg = 
    undefined