module Ngrams
    ( Grams,
      Penalties (..),
      SortedGramBags,
      aksaraGrams, allGrams, reduceGrams,
      sortGrams, alignGrams, printGrams,
      printNexLabels, printNexGrams
    ) where

import Data.Maybe
import Data.List
import qualified Data.List.Unique as U
import Data.Align
import Transcribe
import qualified Data.Vector as V
import qualified Data.Map as MM

type Gram = V.Vector Char
type Grams = [Gram]

{-
data GramBag = GramBag {
    gSiglum :: String,
    gGrams :: [(Grams,String)]
    --gGrams :: [MM.Map Grams String]
    } deriving (Show, Eq)
-}

type GramBags = MM.Map String [(Grams,String)]
-- Map "AEd" [([V.fromList "a", V.fromList "kṣa", V.fromList "rāḥ"],"akṣarāḥ")]

type SortedGramBags = MM.Map String (MM.Map Grams String)

data Penalties = Penalties {
    p1Gram :: Double,
    pNgram :: Double,
    pMatch :: Double,
    pMismatch :: Double,
    pGap :: Double,
    pGapX :: Double,
    pVowelVowel :: Double,
    pConsonantConsonant :: Double
    } deriving Show

aksaraGrams :: Int -> [(String,([String],[String]))] -> GramBags
aksaraGrams n xs = MM.fromListWith (++) $ reverse $ map go xs
    where
    go :: (String,([String],[String])) -> (String,[(Grams,String)])
    go (siglum,(sics,norms)) = (siglum, ngram'' n (zip norms sics))


allGrams :: SortedGramBags -> Bool -> [(Grams, Int)]
allGrams xs singles
    | singles == True = go xs
    | otherwise       = filter (\x -> (snd x) > 1) $ go xs -- ^ remove singletons
    where 
        go xs = U.count_ $  -- ^ return [(Grams, Int)] where Int is the number of occurences
                foldl (++) [] $ -- ^ concatenate them all
                map (U.uniq . MM.keys) (MM.elems xs) -- ^ get unique ngrams from each witness


reduceGrams :: [(Grams,Int)] -> Penalties -> Bool -> [[Grams]]
reduceGrams as p singles = go [] $ map (\(x,y) -> ([x],y)) as
    where
    go:: [([Grams],Int)] -> [([Grams],Int)] -> [[Grams]]
    go done []
        | singles == True = map fst $ reverse $ sortOn snd done
        | otherwise       = map fst $ reverse $ filter (\x -> (snd x) > 1) $ sortOn snd done
    go done (s:ss)
        -- | found == Nothing = go (done ++ [s]) ss
        | found == Nothing = go (s:done) ss
        | otherwise        = go done (fromJust found)
        where
        found = findSimilar s ss p

findSimilar :: ([Grams],Int) -> [([Grams],Int)] -> Penalties -> Maybe [([Grams],Int)]
findSimilar a bs p = go [] a bs
    where
    go :: [([Grams],Int)] -> ([Grams],Int) -> [([Grams],Int)] -> Maybe [([Grams],Int)]
    go _ _ [] = Nothing
    go done x (y:ys)
        | scoreGrams (fst x) (fst y) p = 
            Just (reverse done ++ ((fst x ++ fst y, snd x + snd y)):ys)
        | otherwise        = go (y:done) x ys

scoreGrams :: [Grams] -> [Grams] -> Penalties -> Bool
scoreGrams xs ys p = scoreall $ zip <$> xs <*> ys
    where
    -- e.g., zip <$> [["liṅ","ga"]] <*> [["liṃ","ge"],["liṃ","gye"]] ==
    --       [[("liṅ","liṃ"),("ga","ge")],[("liṅ","liṃ"),("ga","gye")]]
    scoreall :: [[(Gram,Gram)]] -> Bool
    scoreall [] = True
    scoreall (m:ms)
        | score 0 m == False = False
        | otherwise          = scoreall ms
        where
        -- e.g., [("liṅ","liṃ"),("ga","ge")]
        --       fst n == "liṅ", snd n == "liṃ"
        score :: Double -> [(Gram,Gram)] -> Bool
        score _ [] = True
        score mem (n:ns)
            | scoreone       < p1Gram p = False
            | cumulative     < pNgram p = False
            | otherwise                 = score cumulative ns
            where
            scoreone = traceScore $ align (gramConfig p) (fst n) (snd n)
            cumulative = mem + scoreone
        
gramConfig p = alignConfig lookup (pGap p) (pGapX p)
    where
    lookup :: Char -> Char -> Double
    lookup a b
        | a == b = pMatch p
        | isV a && isV b = pVowelVowel p
        | isC a && isC b = pConsonantConsonant p
        | otherwise      = pMismatch p
        where
        isV x = x `elem` "aAiIuUfFxXeEoO"
        isC x = x `elem` "kKgGNcCjJYwWqQRtTdDnpPbBmyrlvSzshL"

sortGrams :: GramBags -> SortedGramBags
sortGrams xs = MM.map sortGram xs
    where
    sortGram :: [(Grams,String)] -> MM.Map Grams String
    sortGram x = MM.fromListWith (\a b -> a ++ ';':b) x

alignGrams :: SortedGramBags -> [[Grams]] -> MM.Map String [String]
alignGrams xs headers = MM.map go xs
    where
    go :: MM.Map Grams String -> [String]
    go grams = map gogo headers
        where
        gogo :: [Grams] -> String
        gogo header = maybeIntercalate mapped
            where
            mapped = map (\h -> fromMaybe "" $ MM.lookup h grams) header
            maybeIntercalate xs
                | foldl (&&) True $ map null xs = ""
                | otherwise                     = intercalate ";;" xs

printGrams :: MM.Map String [String] -> [String]
printGrams gs = map printGram (MM.toList gs)

printGram :: (String,[String]) -> String
printGram (sig,grams) = sig ++ ',' : (intercalate "," $ map go $ reverse grams)
    where
    go :: String -> String
    go x = quothy $ transliterateString slp1' iast x

quoth :: String -> String
quoth s = '\'':s ++ "'"

quothy :: String -> String
quothy s = '"':s ++ "\""

printNexLabels :: [[Grams]] -> String
printNexLabels gs = intercalate ",\n" zipped
    where
    labels = map (quoth . (intercalate ";;") . 
                map (concat . map (V.toList))) gs
    zipped = map (\(a,b) -> (show a) ++ " / '[]' " ++ b) (zip [1..] labels)

printNexGrams gs = map printNexGram (MM.toList gs)

printNexGram :: (String,[String]) -> String
printNexGram (sig,grams) = (quoth sig) ++ ' ' : (foldl (++) [] $ map go grams)
    where
    go :: String -> String
    go x
        | x == ""   = "A"
        | otherwise = "B" 
{-
ngram :: Int -> [a] -> [[a]]
ngram n xs
    | n <= length xs = take n xs : ngram n (drop 1 xs)
    | otherwise      = []

ngram' :: Int -> [(String,String)] -> [(String,String)]
ngram' n xs
    | n <= length xs = go (take n xs) : ngram' n (drop 1 xs)
    | otherwise      = []
    where
    go :: [(String,String)] -> (String,String)
    go = foldl (\(a,b) (c,d) -> (a++c,b++d)) ("","")
-}

ngram'' :: Int -> [(String,String)] -> [(Grams,String)]
ngram'' n xs
    | n <= length xs = (rev . collect $ take n xs) : ngram'' n (drop 1 xs)
    | otherwise      = []
    where
    rev :: (Grams,String) -> (Grams,String)
    rev (m,n) = (reverse m,n)

    collect :: [(String,String)] -> (Grams,String)
    collect = foldl (\(a,b) (c,d) -> (V.fromList c:a,b++d)) ([],"")

{-
aksaraGrams' :: Int -> [(String,([String],[String]))] -> [GramBag]
aksaraGrams' n xs = map go xs
    where
    go :: (String,([String],[String])) -> GramBag
    go (siglum,(sics,norms)) = GramBag {
            gSiglum = siglum,
            gGrams = ngram'' n (zip norms sics)
        }

uniqSigla :: [GramBag] -> [GramBag]
uniqSigla gs = go [] gs
    where 
        go:: [GramBag] -> [GramBag] -> [GramBag]
        go outs [] = outs
        go outs (i:ins)
            |found == Nothing = go (i:outs) ins
            | otherwise       = go outs (fromJust found)
            where found = matchSiglum i ins

matchSiglum :: GramBag -> [GramBag] -> Maybe [GramBag]
matchSiglum i is = go [] is
    where
    go:: [GramBag] -> [GramBag] -> Maybe [GramBag]
    go _ [] = Nothing
    go done (y:ys)
        | (gSiglum i) == (gSiglum y) = Just (reverse done ++ newBag:ys)
        | otherwise        = go (y:done) ys
        where newBag = GramBag {gSiglum = gSiglum i, gGrams = gGrams i ++ gGrams y}

allGrams :: SortedGramBags -> [Grams]
allGrams xs = sort . nub $ foldl (++) [] $ map (MM.keys) (MM.elems xs)

allGrams' :: [GramBag] -> [Grams]
allGrams' xs = nub $ foldl (++) [] $ map go xs
    where
    go x = map fst $ gGrams x

reduceGrams:: [Grams] -> Penalties -> [[Grams]]
reduceGrams aa p = go [] $ map (\x -> [x]) aa
    where
    go:: [[Grams]] -> [[Grams]] -> [[Grams]]
    go done [] = reverse done
    go done (s:ss)
        -- | found == Nothing = go (done ++ [s]) ss
        | found == Nothing = go (s:done) ss
        | otherwise        = go done (fromJust found)
        where
        found = findSimilar s ss p

findSimilar:: [Grams] -> [[Grams]] -> Penalties -> Maybe [[Grams]]
findSimilar a bs p = go [] a bs
    where
    go:: [[Grams]] -> [Grams] -> [[Grams]] -> Maybe [[Grams]]
    go _ _ [] = Nothing
    go done x (y:ys)
        | scoreGrams x y p = Just (reverse done ++ (x ++ y):ys)
        | otherwise        = go (y:done) x ys

sortGrams' :: [GramBag] -> [(String,MM.Map Grams String)]
sortGrams' xs = map sortGram xs
    where
    sortGram :: GramBag -> (String,MM.Map Grams String)
    sortGram x = (gSiglum x, MM.fromListWith (\a b -> a ++ ';':b) $ gGrams x)
-}
