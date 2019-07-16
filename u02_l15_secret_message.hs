data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where halfAlphabet = alphabetSize `div` 2 
          offset = fromEnum c + halfAlphabet
          rotation = offset `mod` alphabetSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder alphaSize c = toEnum rotation
    where halfAlphabet = alphaSize `div` 2 
          offset = if even alphaSize
                   then fromEnum c + halfAlphabet
                   else 1 + fromEnum c + halfAlphabet
          rotation = offset `mod` alphaSize

message  :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot4l vals
    where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
          rot4l = rotN alphaSize

rotEncoder :: String -> String
rotEncoder text = map rotChar text
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotCharDecoder = rotNdecoder alphaSize

---------------------

xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && not (v1 && v2)

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)


---------------------

type Bits = [Bool]

int2Bits' :: Int -> Bits
int2Bits' 0 = [False]
int2Bits' 1 = [True]
int2Bits' n = if remainder == 0
             then False : int2Bits' nextval
             else True : int2Bits' nextval
    where remainder = n `mod` 2
          nextval = n `div` 2

maxBits :: Int
maxBits = length (int2Bits' maxBound)

int2Bits :: Int -> Bits
int2Bits n = leadingFalses ++ reversedBits
    where reversedBits = reverse (int2Bits' n)
          missingBits = maxBits - length reversedBits
          leadingFalses = take missingBits (cycle [False])

char2Bit :: Char -> Bits
char2Bit c = int2Bits (fromEnum c)

bits2Int :: Bits -> Int
bits2Int bits = sum (map (\x -> 2^(snd x)) trueLocations)
    where size = length bits
          indices = [size-1, size-2 .. 0]
          trueLocations = filter (\x -> fst x == True) (zip bits indices)

bits2Char :: Bits -> Char
bits2Char bits = toEnum (bits2Int bits)
