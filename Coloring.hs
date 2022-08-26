module Coloring where
import System.Console.ANSI

-- Template functions for coloring
putColor :: String -> Color -> IO ()
putColor str c = do
    setSGR [SetColor Foreground Vivid c]
    putStr str
    resetColors 

putColorLn :: String -> Color -> IO ()
putColorLn str c = do
    setSGR [SetColor Foreground Vivid c]
    putStrLn str
    resetColors

setItalicized :: IO ()
setItalicized = setSGR [SetItalicized True]

setLayerColor :: ConsoleLayer -> Color -> IO ()
setLayerColor layer c = setSGR [SetColor layer Vivid c]

resetColors :: IO ()
resetColors = setSGR [Reset]
 
setBlink :: IO ()
setBlink = setSGR [SetBlinkSpeed RapidBlink]

-- Functions to print text in different colors
putRed :: String -> IO ()
putRed str = putColor str Red

putRedLn :: String -> IO ()
putRedLn str = putColorLn str Red

putGreen :: String -> IO ()
putGreen str = putColor str Green

putGreenLn :: String -> IO ()
putGreenLn str = putColorLn str Green

putYellow :: String -> IO ()
putYellow str = putColor str Yellow

putYellowLn :: String -> IO ()
putYellowLn str = putColorLn str Yellow

putBlue :: String -> IO ()
putBlue str = putColor str Blue

putBlueLn :: String -> IO ()
putBlueLn str = putColorLn str Blue

putMagenta :: String -> IO ()
putMagenta str = putColor str Magenta

putMagentaLn :: String -> IO ()
putMagentaLn str = putColorLn str Magenta

putCyan :: String -> IO ()
putCyan str = putColor str Cyan

putCyanLn :: String -> IO ()
putCyanLn str = putColorLn str Cyan

putWhite :: String -> IO ()
putWhite str = putColor str White

putWhiteLn :: String -> IO ()
putWhiteLn str = putColorLn str White

putBlack :: String -> IO ()
putBlack str = putColor str Black

putBlackLn :: String -> IO ()
putBlackLn str = putColorLn str Black

-- Functions to print text in different colors and italicized
putIRed :: String -> IO ()
putIRed str = do
    setItalicized
    putRed str
    resetColors

putIRedLn :: String -> IO ()
putIRedLn str = do
    setItalicized
    putRedLn str
    resetColors

putIGreen :: String -> IO ()
putIGreen str = do
    setItalicized
    putGreen str
    resetColors

putIGreenLn :: String -> IO ()
putIGreenLn str = do
    setItalicized
    putGreenLn str
    resetColors

putIYellow :: String -> IO ()
putIYellow str = do
    setItalicized
    putYellow str
    resetColors

putIYellowLn :: String -> IO ()
putIYellowLn str = do
    setItalicized
    putYellowLn str
    resetColors

putIBlue :: String -> IO ()
putIBlue str = do
    setItalicized
    putBlue str
    resetColors

putIBlueLn :: String -> IO ()
putIBlueLn str = do
    setItalicized
    putBlueLn str
    resetColors

putIMagenta :: String -> IO ()
putIMagenta str = do
    setItalicized
    putMagenta str
    resetColors

putIMagentaLn :: String -> IO ()
putIMagentaLn str = do
    setItalicized
    putMagentaLn str
    resetColors

putICyan :: String -> IO ()
putICyan str = do
    setItalicized
    putCyan str
    resetColors

putICyanLn :: String -> IO ()
putICyanLn str = do
    setItalicized
    putCyanLn str
    resetColors

putIWhite :: String -> IO ()
putIWhite str = do
    setItalicized
    putWhite str
    resetColors

putIWhiteLn :: String -> IO ()
putIWhiteLn str = do
    setItalicized
    putWhiteLn str
    resetColors

putIBlack :: String -> IO ()
putIBlack str = do
    setItalicized
    putBlack str
    resetColors

putIBlackLn :: String -> IO ()
putIBlackLn str = do
    setItalicized
    putBlackLn str
    resetColors

-- Setting the background color of the terminal
--
-- These functions will permanenty set the background color
-- until a reset is performed. 
setBackgroundRed :: IO ()
setBackgroundRed = setLayerColor Background Red

setBackgroundGreen :: IO ()
setBackgroundGreen = setLayerColor Background Green

setBackgroundYellow :: IO ()
setBackgroundYellow = setLayerColor Background Yellow

setBackgroundBlue :: IO ()
setBackgroundBlue = setLayerColor Background Blue

setBackgroundMagenta :: IO ()
setBackgroundMagenta = setLayerColor Background Magenta

setBackgroundCyan :: IO ()
setBackgroundCyan = setLayerColor Background Cyan

setBackgroundWhite :: IO ()
setBackgroundWhite = setLayerColor Background White

setBackgroundBlack :: IO ()
setBackgroundBlack = setLayerColor Background Black

-- Testing 
--
-- Most terminals should support the functions defined above,
-- and testColors will help identify if that is not the case.
testColors :: IO ()
testColors = do
    clearScreen
    setCursorPosition 0 0
    testTextColor
    testItalicTextColor
    testTextLnColor
    testItalicTextLnColor
    testBackgroundColor

testTextLnColor :: IO ()
testTextLnColor = do
    putRedLn "This should be red"
    putGreenLn "This should be green"
    putYellowLn "This should be yellow"
    putBlueLn "This should be blue"
    putMagentaLn "This should be magenta"
    putCyanLn "This should be cyan"
    putWhiteLn "This should be white"
    putBlackLn "This should be black"

testTextColor :: IO ()
testTextColor = do
    putStr "colors: "
    putRed "red "
    putGreen "green "
    putYellow "yellow "
    putBlue "blue "
    putMagenta "magenta "
    putCyan "cyan "
    putWhite "white "
    putBlack "black\n"

testItalicTextColor :: IO ()
testItalicTextColor = do
    putStr "italics: "
    putIRed "red "
    putIGreen "green "
    putIYellow "yellow "
    putIBlue "blue "
    putIMagenta "magenta "
    putICyan "cyan "
    putIWhite "white "
    putIBlack "black\n"

testItalicTextLnColor :: IO ()
testItalicTextLnColor = do
    putIRedLn "This should be italicized red"
    putIGreenLn "This should be italicized green"
    putIYellowLn "This should be italicized yellow"
    putIBlueLn "This should be italicized blue"
    putIMagentaLn "This should be italicized magenta"
    putICyanLn "This should be italicized cyan"
    putIWhiteLn "This should be italicized white"
    putIBlackLn "This should be italicized black"

testBackgroundColor :: IO ()
testBackgroundColor = do
    setBackgroundRed
    putWhiteLn "This should be white on red"
    resetColors
    setBackgroundGreen
    putWhiteLn "This should be white on green"
    resetColors
    setBackgroundYellow
    putBlackLn "This should be black on yellow"
    resetColors
    setBackgroundBlue
    putWhiteLn "This should be white on blue"
    resetColors
    setBackgroundMagenta
    putWhiteLn "This should be white on magenta"
    resetColors
    setBackgroundCyan
    putBlackLn "This should be black on cyan"
    resetColors
    setBackgroundWhite
    putBlackLn "This should be black on white"
    resetColors
    setBackgroundBlack
    putWhiteLn "This should be white on black"
    resetColors
