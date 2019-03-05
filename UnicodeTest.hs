import qualified Data.ByteString as BS
import qualified Data.Text.ICU.Convert as ICU

-- https://stackoverflow.com/a/34934953

import System.IO

main = do
    -- dunno what the Nothing argument is for, read the docs!
    conv <- ICU.open "utf-8" Nothing
    h    <- openFile "tests/repl/repl_list1.txt" System.IO.ReadMode
    bs   <- BS.hGetContents h
    print (ICU.toUnicode conv bs)
