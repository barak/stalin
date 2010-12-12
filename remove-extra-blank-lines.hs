import Data.List
import System.IO

main =
  do
    f <- hGetContents stdin
    hPutStr stdout $
      concatMap (++"\n") $
        do
          ls <- tails (lines f)
          if take 2 ls == ["",""]
            then []
            else take 1 ls
