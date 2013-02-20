module LBH.Utils (extractHaskellCodeBlocks) where

import Text.Pandoc
import Control.Monad.State

type M = State Int

extractHaskellCodeBlocks :: Pandoc -> Pandoc
extractHaskellCodeBlocks (Pandoc i bs) = (flip evalState) 0 $ do
  bs' <- concat `liftM` forM bs extractFromBlock
  return $ Pandoc i bs'

-- | Currently only handle top-level blocks
extractFromBlock :: Block -> M [Block]
extractFromBlock (CodeBlock attrs blk) | isActiveHaskell attrs = do
  x <- freshVar
  let _id = "active-haskell-" ++ show x
  return $ [(CodeBlock (_id , ["haskell","active-haskell"] ,[]) blk)
           ,(RawBlock "html" $
              "<div id=\"raw-"++_id++"\" class=\"raw-active-haskell\">"
              ++ blk ++ "</div>")
            ]
 where isActiveHaskell = (== ("",["active-haskell"],[]))
extractFromBlock b = return [b]

-- | Generate a fresh variable
freshVar :: M Int
freshVar = do
  i <- get
  put (i+1)
  return i