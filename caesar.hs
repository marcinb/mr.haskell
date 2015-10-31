import Data.Char

encode :: Int -> String -> String
encode shift message = 
    let ords = map ord message
        shifted = map (+ shift) ords
    in map chr shifted

decode :: Int -> String -> String
decode shift = encode (negate shift)
