import System.IO
import Data.Char (isSpace, isDigit)
import Data.List (sortBy)
import Data.Ord (comparing)

-- Полином как список (коэффициент, степень)
type Term = (Int, Int)
type Poly = [Term]

-- Удаляем пробелы
stripSpaces :: String -> String
stripSpaces = filter (not . isSpace)

-- Разбиваем строку на слагаемые с явным знаком: "+3x^2", "-x", "+5"
tokenize :: String -> [String]
tokenize s0 =
  let s = stripSpaces s0
      s' = case s of
              [] -> []
              (c:cs) -> if c == '+' || c == '-' then c:cs else '+' : c : cs
  in splitTerms s'
  where
    splitTerms :: String -> [String]
    splitTerms [] = []
    splitTerms (c:cs) =
      let (termBody, rest) = span (\ch -> ch /= '+' && ch /= '-') cs
          term = c : termBody
      in term : case rest of
                  []       -> []
                  (c':cs') -> splitTerms (c':cs')

-- Парсинг одного слагаемого вида "+3x^2", "-x", "+5" и т.п.
parseTerm :: String -> Term
parseTerm s =
  let (sign, body) =
        case s of
          ('+':rest) -> (1, rest)
          ('-':rest) -> (-1, rest)
          _          -> (1, s)
  in
  if 'x' `elem` body
    then
      let (coeffPart, rest) = span (/= 'x') body
          baseCoeff = if null coeffPart then 1 else read coeffPart
          coeff = sign * baseCoeff
          rest1 = drop 1 rest  -- пропускаем 'x'
      in case rest1 of
          ""      -> (coeff, 1)
          ('^':p) -> (coeff, read p)
          _       -> error ("Bad term: " ++ s)
    else
      let coeff = sign * (if null body then 0 else read body)
      in (coeff, 0)

-- Нормализация полинома: собираем одинаковые степени, убираем нули, сортируем
normalize :: Poly -> Poly
normalize ts =
  let grouped = foldr insertTerm [] ts
      insertTerm (c, d) acc =
        case break ((== d) . snd) acc of
          (before, (c',d'):after) -> (c + c', d) : before ++ after
          _                       -> (c, d) : acc
      nonZero = filter (\(c,_) -> c /= 0) grouped
  in sortBy (flip (comparing snd)) nonZero

-- Парсинг строки в полином
parsePoly :: String -> Poly
parsePoly s = normalize (map parseTerm (tokenize s))

-- Сложение двух полиномов
addPoly :: Poly -> Poly -> Poly
addPoly p q = normalize (p ++ q)

-- Умножение полиномов
mulPoly :: Poly -> Poly -> Poly
mulPoly p q = normalize [ (c1*c2, d1+d2) | (c1,d1) <- p, (c2,d2) <- q ]

-- Производная
derivative :: Poly -> Poly
derivative p = normalize [ (c*d, d-1) | (c,d) <- p, d > 0 ]

-- Возврат x^n
pow :: Int -> Int -> Int
pow _ 0 = 1
pow x n
  | n < 0     = error "Negative power not supported for Int"
  | otherwise = x * pow x (n-1)

-- Значение полинома в точке
evalPoly :: Poly -> Int -> Int
evalPoly p x = sum [ c * pow x d | (c,d) <- p ]

-- Печать полинома в "красивом" виде
showPoly :: Poly -> String
showPoly [] = "0"
showPoly ((c,d):ts) =
  let firstTerm = showTerm True (c,d)
      restTerms = concatMap (\t -> showTerm False t) ts
  in firstTerm ++ restTerms

showTerm :: Bool -> Term -> String
showTerm isFirst (c,d)
  | c == 0 = ""
  | d == 0 =
      let absC = abs c
      in if isFirst
           then show c
           else if c > 0 then " + " ++ show absC
                         else " - " ++ show absC
  | d == 1 =
      let absC = abs c
      in if isFirst
           then case c of
                  1  -> "x"
                  -1 -> "-x"
                  _  -> show c ++ "x"
           else if c > 0
                  then case c of
                         1 -> " + x"
                         _ -> " + " ++ show absC ++ "x"
                  else case c of
                         -1 -> " - x"
                         _  -> " - " ++ show absC ++ "x"
  | otherwise =
      let absC = abs c
      in if isFirst
           then case c of
                  1  -> "x^" ++ show d
                  -1 -> "-x^" ++ show d
                  _  -> show c ++ "x^" ++ show d
           else if c > 0
                  then case c of
                         1 -> " + x^" ++ show d
                         _ -> " + " ++ show absC ++ "x^" ++ show d
                  else case c of
                         -1 -> " - x^" ++ show d
                         _  -> " - " ++ show absC ++ "x^" ++ show d

-- Главная функция
main :: IO ()
main = do
  content <- readFile "/uploads/input.txt"
  let ls = lines content
  case ls of
    (pLine:qLine:xLine:_) -> do
      let p = parsePoly pLine
          q = parsePoly qLine
          x0 = read xLine :: Int
          s  = addPoly p q
          m  = mulPoly p q
          dp = derivative p
          dq = derivative q
      putStrLn "P(x) ="
      putStrLn ("  " ++ showPoly p)
      putStrLn "Q(x) ="
      putStrLn ("  " ++ showPoly q)
      putStrLn "P(x) + Q(x) ="
      putStrLn ("  " ++ showPoly s)
      putStrLn "P(x) * Q(x) ="
      putStrLn ("  " ++ showPoly m)
      putStrLn "P'(x) ="
      putStrLn ("  " ++ showPoly dp)
      putStrLn "Q'(x) ="
      putStrLn ("  " ++ showPoly dq)
      putStrLn ("P(" ++ show x0 ++ ") = " ++ show (evalPoly p x0))
      putStrLn ("Q(" ++ show x0 ++ ") = " ++ show (evalPoly q x0))
      putStrLn ("(P+Q)(" ++ show x0 ++ ") = " ++ show (evalPoly s x0))
    _ -> putStrLn "input.txt must contain at least 3 lines"
