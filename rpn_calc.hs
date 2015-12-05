import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFn [] . words
    where foldingFn (x:y:ys) "+" = (y + x) : ys
          foldingFn (x:y:ys) "-" = (y - x) : ys
          foldingFn (x:y:ys) "*" = (y * x) : ys
          foldingFn (x:y:ys) "/" = (y / x) : ys
          foldingFn xs numberStr = read numberStr:xs

