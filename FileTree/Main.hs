module Main where

import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.Posix.Files

data Args = Args
    { hidden :: Bool
    , depth :: Int
    , icons :: Bool
    , size :: Bool
    , path :: String
    }

data FileTree
    = Link String String
    | File String String
    | Dir String [FileTree]
    deriving (Show)

printTree :: Int -> FileTree -> String
printTree n (Link name link) =
    concat
        [ replicate (n*2) ' '
        , "↳ "
        , name
        , " \60063 "
        , "(link)"
        ]
printTree n (File "" []) = ""
printTree n (File name st) =
    concat
        [ replicate (n*2) ' '
        , "↳ "
        , name
        , " "
        , st
        ]
printTree n (Dir str f) = replicate (n*2) ' ' ++ colorize str ++ "\n" ++ intercalate "\n" (filter (not . null) $ map (printTree (n + 1)) f)
  where
    colorize str = "\x1b[38;2;29;31;33;48;2;110;54;201m δ " ++ str ++ " \x1b[0m"

split :: FilePath -> [FilePath] -> Bool -> IO ([FilePath], [(String, FilePath)], [FilePath])
split path s si = do
    d <- filterM isDirectory s
    l <- filterM isLink s
    f' <- (\\ l) <$> filterM isFile s
    f <- if si then mapM (\x -> size x >>= \z -> return (show z ++ "M", x)) f' else mapM (return . (,) "") f'
    return (d, f, l)
  where
    isLink = pathIsSymbolicLink . (path </>)
    size x = getFileSize (path </> x) >>= \x -> return $ truncate' (fromIntegral x / 1024 ^ 2) 2
    truncate' :: Double -> Int -> Double
    truncate' x n = fromIntegral (floor (x * 10 ^ n)) / 10 ^ n
    isFile = doesFileExist . (path </>)
    isDirectory = doesDirectoryExist . (path </>)

buildTree args = buildTree' "" (depth args) (hidden args) (path args)
  where
    buildTree' :: FilePath -> Int -> Bool -> FilePath -> IO FileTree
    buildTree' _ 0 _ _ = return (File "" [])
    buildTree' subdir depth hidden path = do
        let sf = subdir </> path
        currentDir <- filterH <$> listDirectory sf
        (d, f, l) <- split sf currentDir (size args)
        ds <- traverse (buildTree' sf (depth - 1) hidden) d
        return (Dir path (map (\(x, q) -> q `File` x) f ++ map (`Link` "") l ++ ds))
      where
        filterH = filter ((|| hidden) . (/= '.') . head)

parseArgs :: Args -> [String] -> Args
parseArgs m [] = m
parseArgs (Args h d i s p) (n : ns) = case n of
    "-" -> error "Expected Argument"
    "-d" -> error "-d Expects a +ve integer"
    [p] -> error "Undefined Argument given"
    ('-' : 'd' : depth) -> parseArgs (Args h (read depth) i s p) ns
    "-h" -> parseArgs (Args True d i s p) ns
    "-i" -> parseArgs (Args h d True s p) ns
    "-s" -> parseArgs (Args h d i True p) ns
    p -> parseArgs (Args h d i s p) ns

main = do
    args <- getArgs
    let options = parseArgs (Args False (-1) False False ".") args
    tree <- buildTree options
    putStrLn . printTree 0 $ tree
