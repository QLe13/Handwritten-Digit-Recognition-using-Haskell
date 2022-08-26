module Framework where
import Data.List.Split (chunksOf)
import Debug.Trace
import Data.List (sort)
import DigitRecognition


--                                  Image Reading Functions
-- These functions are used to turn strings into PixelImages and Digits. You will not need to
-- call these functions, but understanding how they work may help with showPixelImage in
-- DigitRecognition.hs.
--
isGoodImage :: [String] -> Bool
-- isGoodImage ensures that the input list is 28 lines long, and that
-- each line is 28 characters that are '#', '+', or ' '. 
-- The (and) function takes a list of Booleans, returning True when all of them are True.
isGoodImage strs = length(strs) == 28 
                   && (and [isGoodLine str | str <- strs])
    where isGoodLine str = length str == 28 && and [c `elem` "#+ " | c <- str]

readPixelImage :: [String] -> PixelImage
--readPixelImage turns a 28x28 list of characters into a 28x28 list of booleans.
--Both '+' and '#' are taken to be True, while ' ' becomes False. Thus, we lose some
--information.
readPixelImage strs = if isGoodImage strs 
                      then [readLine str | str <- strs] 
                      else error "Improper pixel image!"
    where readPixel x = not (x == ' ')
          readLine str = [readPixel x | x <- str]

readPixelImages :: String -> [PixelImage]
--readPixelImages takes the contents of a file storing images and turns it into a list of
--PixelImages. The (lines) function breaks the string into a list of string, separating at '\n'
--newlines. The (chunksOf) function, imported from Data.List.Split, breaks the list of lines
--into 28-line chunks. Thus, we end up with a list of list of strings.
readPixelImages imgStr =  
    let imageChunks = chunksOf 28 (lines imgStr)
    in [readPixelImage strs | strs <- imageChunks]

readLabels :: String -> [Digit]
--readLabels takes the contents of a file storing labels and turns it into a list of digits.
--Since each label is on one line, reading a label is simply a matter of ensuring it is
--a string "0" through "9", and calling the built-in (read) function.
readLabels lblStr = [readLabel str | str <- lines lblStr]
    where readLabel str = if str `elem` validDigits
                          then read str
                          else error $ "Invalid label!" ++ str
          validDigits = [show x | x <- [0..9]]

readLabeledImages :: String -> String -> [(PixelImage, Digit)]
--readLabeledImages takes the contents of two files, one holding images and the other holding
--the corresponding labels, and pairs them up. Recall that (zip) takes two lists and returns the
--list of tuples.
readLabeledImages imgStr lblStr =
    let images = readPixelImages imgStr
        labels = readLabels lblStr
    in if length images == length labels
       then zip images labels
       else error "Number of labels and images are different!"


--Black magic functions
-- sortCorpus is used to keeps everything nice and sorted, so equality can be checked. 
-- You should never need to use this function.
sortCorpus corpus = sort [(label, sort images) | (label, images) <- corpus]

pixelImagesFromFile :: String -> IO [PixelImage]
pixelImagesFromFile fname = do
    contents <- readFile fname
    return $ readPixelImages contents

labelsFromFile :: String -> IO [Digit]
labelsFromFile lblFile = do
    lblStr <- readFile lblFile
    return $ readLabels lblStr

labeledImagesFromFiles :: String -> String -> IO [(PixelImage, Digit)]
labeledImagesFromFiles imgFile lblFile = do
    imgStr <- readFile imgFile
    lblStr <- readFile lblFile
    return $ readLabeledImages imgStr lblStr
