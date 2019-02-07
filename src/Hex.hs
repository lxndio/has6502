module Hex where

data HexDigit = H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | HA | HB | HC | HD | HE | HF deriving (Show)
type HexNumber = [HexDigit]

data HexParseError = HexParseError String deriving (Show)

hexValues = [ (H0, 0), (H1, 1) , (H2, 2) , (H3, 3) , (H4, 4) , (H5, 5) , (H6, 6), (H7, 7)
            , (H8, 8), (H9, 9), (HA, 10), (HB, 11), (HC, 12), (HD, 13), (HE, 14), (HF, 15) ]

instance Eq HexDigit where
  (==) H0 H0 = True
  (==) H1 H1 = True
  (==) H2 H2 = True
  (==) H3 H3 = True
  (==) H4 H4 = True
  (==) H5 H5 = True
  (==) H6 H6 = True
  (==) H7 H7 = True
  (==) H8 H8 = True
  (==) H9 H9 = True
  (==) HA HA = True
  (==) HB HB = True
  (==) HC HC = True
  (==) HD HD = True
  (==) HE HE = True
  (==) HF HF = True
  (==) _  _ = False

-- unsafe
getHexValue :: HexDigit -> Int
getHexValue h = iter h hexValues where
  iter h ((hv, v):vs) = if h == hv then v else iter h vs
  iter _ []           = 0

-- unsafe
hexToInt :: HexNumber -> Int
hexToInt h = iter h (length h - 1) where
  iter (h:hs) pos = (getHexValue h) * 16^pos + iter hs (pos-1)
  iter []     _   = 0