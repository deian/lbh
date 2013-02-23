module ActiveCode.Languages (langMap, languages) where

import qualified Data.ByteString.Lazy.Char8 as L8
import           ActiveCode.Haskell    (haskell)
import           ActiveCode.JavaScript (js)
import           ActiveCode.C          (c, cpp)

langMap :: [(String, L8.ByteString -> IO ())]
langMap =
  [ ("haskell"   , haskell)
  , ("javascript", js     )
  , ("c"         , c      )
  , ("cpp"       , cpp    )
  ]

languages :: [String]
languages = map fst langMap