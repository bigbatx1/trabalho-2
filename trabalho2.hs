--Trabalho 2 PF
--Hendrik Abdalla Hermann 11911BCC034 e Marcelo Junio de Oliveira Teixeira 11911BCC024

--Listas:

l1=[1..1000]
l2=[1000,999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7=[20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]

--Ex 1:

--A
selection::(Ord t)=>[t]->[t]
selection [] = []
selection [a] = [a]
selection (head:tail) =
  let least = foldr1 (min) (head:tail)
      remove _ [] = []
      remove n (head:tail) = if n == head then tail else head:(remove n tail)
   in [least] ++ selection (remove least (head:tail))

--B
insertion::(Ord t)=>[t]->[t]
insertion lista = foldr (insere) [] lista
  where
    insere a [] = [a]
    insere x (head:tail) = if x <= head then (x:head:tail) else head:(insere x tail)

--C
quickSort::(Ord t)=>[t]->[t]
quickSort [] = []
quickSort (pivo:tail) = quickSort (filter (< pivo) tail) ++ [pivo] ++ quickSort (filter (>= pivo) tail)


--Ex 2

troca :: (Ord a) => ([a], Int) -> ([a], Int)
troca ([head1], flag) = ([head1], flag)
troca ((head1 : head2 : tail), flag) =
  if head1 > head2
    then add (troca ((head1 : tail), 1)) head2
    else add (troca ((head2 : tail), flag)) head1
  where
    add (l, f) elemen = (elemen : l, f)

bubbleAux :: (Ord a) => ([a], Int) -> Int -> ([a], Int)
bubbleAux (l, flag) 0 = (l, flag)
bubbleAux (l, flag) n
  | flag == 0 = (l, flag)
  | otherwise = bubbleAux (troca (l, 0)) (n -1)

bubbleSort1 :: (Ord a) => [a] -> [a]
bubbleSort1 [] = []
bubbleSort1 lista = fst (bubbleAux (lista, -1) (length lista))

bubbleSort2 :: (Ord a) => [a] -> [a]
bubbleSort2 [] = []
bubbleSort2 lista =
  let troca [x] = [x]
      troca (x : y : tail) =
        if x > y
          then y : troca (x : tail)
          else x : troca (y : tail)

      split lista = (take (length lista - 1) lista, drop (length lista - 1) lista)

      bubble [x] = [x]
      bubble l = (bubble haTrocar) ++ ultimoElm
        where
          listaMod = troca l
          (haTrocar, ultimoElm) = split listaMod
   in bubble lista

bubbleSort3 :: (Ord a) => [a] -> [a]
bubbleSort3 [] = []
bubbleSort3 l =
  let add (l, f) y = (y : l, f)
      split lista = (take (length lista - 1) lista, drop (length lista - 1) lista)

      troca ([head1], flag) = ([head1], flag)
      troca ((head1 : head2 : tail), flag)=
        if head1 > head2
          then add (troca ((head1 : tail), 1)) head2
          else add (troca ((head2 : tail), flag)) head1

      bubble ([head1], flag) = ([head1], flag)
      bubble (lista, flag)
        | n_flag == 0 = (lista, flag)
        | otherwise = (fst (bubble (parte_a_trocar, 0)) ++ ultimo_elem, 0)
        where
          (lista_trocada, n_flag) = troca (lista, flag)
          (parte_a_trocar, ultimo_elem) = split lista_trocada
   in fst (bubble (l, -1))

bubbleSort1Cont :: (Ord a) => [a] -> ([a], Int)
bubbleSort1Cont [] = ([], 0)
bubbleSort1Cont lista = format (bubleAuxCont (lista, -1, 0) (length lista))
  where
    format (l, _, c) = (l, c)

trocaCont :: (Ord a) => ([a], Int, Int) -> ([a], Int, Int)
trocaCont ([head1], flag, n) = ([head1], flag, n)
trocaCont ((head1 : head2 : tail), flag, n) =
  if head1 > head2
    then add (trocaCont ((head1 : tail), 1, n + 1)) head2
    else add (trocaCont ((head2 : tail), flag, n + 1)) head1
  where
    add (l, f, c) e = (e : l, f, c)

bubleAuxCont :: (Ord a) => ([a], Int, Int) -> Int -> ([a], Int, Int)
bubleAuxCont (l, flag, c) 0 = (l, flag, c)
bubleAuxCont (l, flag, c) n
  | flag == 0 = (l, flag, c)
  | otherwise = bubleAuxCont (trocaCont (l, 0, c)) (n -1)

bubbleSort2Cont :: (Ord a) => [a] -> ([a], Int)
bubbleSort2Cont [] = ([], 0)
bubbleSort2Cont lista =
  let add (l, c) e = (e : l, c)

      troca ([x], c) = ([x], c)
      troca ((x : y : tail), c) =
        if x > y
          then add (troca (x : tail, c + 1)) y
          else add (troca (y : tail, c + 1)) x

      split lista = (take (length lista - 1) lista, drop (length lista - 1) lista)

      bubble :: (Ord a) => ([a], Int) -> ([a], Int)
      bubble ([x], c) = ([x], c)
      bubble (l, c) = (proxima_etapa ++ ultimo_elem, rec_c)
        where
          (lista_trocada, c1) = (troca (l, c))
          (parte_a_trocar, ultimo_elem) = split lista_trocada
          (proxima_etapa, rec_c) = bubble (parte_a_trocar, c1)
   in bubble (lista, 0)

bubbleSort3Cont :: (Ord a) => [a] -> ([a], Int)
bubbleSort3Cont [] = ([], 0)
bubbleSort3Cont l =
  let add (l, f, c) y = (y : l, f, c)
      split lista = (take (length lista - 1) lista, drop (length lista - 1) lista)
      format (l, _, c) = (l, c)

      troca ([x], flag, c) = ([x], flag, c)
      troca ((x : y : tail), flag, c) =
        if x > y
          then add (troca ((x : tail), 1, c + 1)) y
          else add (troca ((y : tail), flag, c + 1)) x

      bubble ([x], flag, c) = ([x], flag, c)
      bubble (lista, flag, c)
        | n_flag == 0 = (lista, flag, c)
        | otherwise = (proxima_etapa ++ ultimo_elem, 0, rec_c)
        where
          (lista_trocada, n_flag, c1) = troca (lista, flag, c)
          (parte_a_trocar, ultimo_elem) = split lista_trocada
          (proxima_etapa, _, rec_c) = bubble (parte_a_trocar, 0, c1)
   in format (bubble (l, -1, 0))

--Melhor variacao:

--As diferencas de tempo de execucao nos diferentes algoritmos bubble sao baixas , entretanto as interações realizadas pela 
--variação 1 em relação a variação 3 é muito maior, quando comparado a variação 2 a
--diferença na quantidade de interações é mínima entretanto a variacao 3 ainda é menor. 
--Assim sendo, conclue-se que a Variação 3 é a melhor em relação as outras.

--Ex 3

selectionSortEx3 :: (Ord a) => [a] -> [a]
selectionSortEx3 [] = []
selectionSortEx3 [x] = [x]
selectionSortEx3 (x : tail) =
  let least = foldr1 (min) (x : tail)

      remove _ [] = []
      remove n (h : t) =
        if n == h
          then t
          else h : (remove n t)
   in least : selectionSortEx3 (remove least (x : tail))

removeMenor :: (Ord a) => (a, [a]) -> (a, [a])
removeMenor (m, [x]) = if x < m then (x, [m]) else (m, [x])
removeMenor (menor, (x : tail))
  | x < menor = add menor (removeMenor (x, xs))
  | otherwise = add x (removeMenor (menor, xs))
  where
    add a (n, l) = (n, a : l)

selection2 :: (Ord a) => [a] -> [a]
selection2 [] = []
selection2 [x] = [x]
selection2 lst =
  let (least, novoUlt) = removeMenor (head lst, tail lst)
   in least : (selection2 novoUlt)

removeMenorCont :: (Ord a) => (a, [a], Int) -> (a, [a], Int)
removeMenorCont (m, [x], c) = if x < m then (x, [m], c + 1) else (m, [x], c + 1)
removeMenorCont (menor, (x : tail), c1)
  | x < menor = add menor (removeMenorCont (x, xs, c1 + 1))
  | otherwise = add x (removeMenorCont (menor, xs, c1 + 1))
  where
    add a (n, l, c) = (n, a : l, c)

selection2Cont :: (Ord a) => [a] -> ([a], Int)
selection2Cont [] = ([], 0)
selection2Cont [x] = ([x], 0)
selection2Cont (x : xs) =
  let (least, novoUlt, cont) = removeMenorCont (x, xs, 0)

      (proxima_etapa, nCont) = selection2Cont novoUlt
   in (least : proxima_etapa, cont + nCont)

--Melhor variacao:

--A variação 2 parece possuir um desempenho melhor devido a sua implementação. 
--achar o menor elemento e remove-ló da lista sem percorrer duas vezes utiliza mais processamento por manter o 
--valor fora da lista e fazer a sua inserção de volta quando um valor menor que ele for encontrado. Assim,sendo a
--Variação 1 parece ser a melhor em relação as outras. 

--Ex 4

divide :: (Ord a) => a -> [a] -> ([a], [a])
divide _ [] = ([], [])
divide x [e] = if e < x then ([e], []) else ([], [e])
divide x (e : es)
  | e < x = addEsq e (divide x es)
  | otherwise = addDir e (divide x es)
  where
    addEsq a (l, r) = (a : l, r)
    addDir a (l, r) = (l, a : r)

quickEx4 :: (Ord a) => [a] -> [a]
quickEx4 [] = []
quickEx4 (piv : tail) =
  let (left, right) = divide piv tail
   in (quickEx4 left) ++ [piv] ++ (quickEx4 right)

quickSort2 :: (Ord a) => [a] -> [a]
quickSort2 [] = []
quickSort2 lst =
  let firstThree = take 3 lst
      piv =
        if length (firstThree) < 3
          then firstThree !! 0
          else foldr1 (min) (firstThree)

      deletaPrimOcorrencia _ [] = []
      deletaPrimOcorrencia x (y : ys)
        | x == y = ys
        | otherwise = y : deletaPrimOcorrencia x ys

      (left, right) = divide piv (deletaPrimOcorrencia piv lst)
   in (quickSort2 left) ++ [piv] ++ (quickSort2 right)

divideCont :: (Ord a) => a -> [a] -> Int -> ([a], [a], Int)
divideCont _ [] n = ([], [], n)
divideCont x [e] n =
  if e < x
    then ([e], [], n + 1)
    else ([], [e], n + 1)
divideCont x (e : es) n
  | e < x = addEsq e (divideCont x es (n + 1))
  | otherwise = addDir e (divideCont x es (n + 1))
  where
    addEsq a (l, r, c) = (a : l, r, c)
    addDir a (l, r, c) = (l, a : r, c)

quickSortCont :: (Ord a) => [a] -> ([a], Int)
quickSortCont [] = ([], 0)
quickSortCont (piv : tail) =
  let (left, right, n) = divideCont piv tail 0

      (sortedL, n_L) = quickSortCont left
      (sortedR, n_R) = quickSortCont right
   in (sortedL ++ [piv] ++ sortedR, n + n_L + n_R)

quickSortCount2 :: (Ord a) => [a] -> ([a], Int)
quickSortCount2 [] = ([], 0)
quickSortCount2 lst =
  let piv = foldr1 (min) (take 3 lst)

      deleteFrstOc :: (Ord a) => a -> [a] -> Int -> ([a], Int)
      deleteFrstOc _ [] n = ([], n)
      deleteFrstOc x (y : ys) n
        | x == y = (ys, n + 1)
        | otherwise = add y (deleteFrstOc x ys (n + 1))
        where
          add e (l, c) = (e : l, c)

      (novoUlt, checks) = deleteFrstOc piv lst 0

      (left, right, n1) = divideCont piv novoUlt 0
      (sortedL, n_L) = quickSortCount2 left
      (sortedR, n_R) = quickSortCount2 right
   in (sortedL ++ [piv] ++ sortedR, n1 + n_L + n_R + checks + 3)

--Melhor variacao:

--Analisando as interações a variação 2 é superior pois  realiza menos testes comparadas as outras,
--isso se deve a maior divergência na escolha do pivô entre elas. 
--Dependendo da complexidade da lista a ser ordenada a Variação 1 tem vantagem. 
--Considerando os testes realizados a Variação 2 parece ser a melhor opcao.

-- Ex 5

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge l1 [] = l1
merge [] l2 = l2
merge (a : as) (b : bs)
  | a > b = b : (merge (a : as) bs)
  | otherwise = a : (merge as (b : bs))

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort lst =
  let left = mergeSort (take ((length lst) `div` 2) lst)
      right = mergeSort (drop ((length lst) `div` 2) lst)
   in merge left right

sortIntoBuckets :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
sortIntoBuckets num k m n [bucket] =
  if ((num * k) `div` m) <= n
    then [num : bucket]
    else [bucket]
sortIntoBuckets num k m n (bucket : buckets)
  | ((num * k) `div` m) <= n = (num : bucket) : buckets
  | otherwise = bucket : (sortIntoBuckets num k m (n + 1) buckets)

bucketSort :: [Int] -> [Int]
bucketSort [] = []
bucketSort [x] = [x]
bucketSort l1 =
  let k = length l1

      m = foldr1 (max) l1

      buckets = [[] | _ <- [1 .. k]]

      newBuckets = foldr (\x -> sortIntoBuckets x k m 1) buckets l1

      sortedBuckets = map (mergeSort) newBuckets

      finalList = foldr1 (++) sortedBuckets
   in finalList

--Ex 6

data Exp a
  = Val a
  | Add (Exp a) (Exp a)
  | Sub (Exp a) (Exp a)
  | Mult (Exp a) (Exp a)
  | Pot (Exp a) (Exp a)

avalia :: Floating a => Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mult exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Pot exp1 exp2) = (avalia exp1) ** (avalia exp2)

expre1 :: Exp Integer
expre1 = (Mult (Add (Val 3) (Val 12)) (Pot (Sub (Val 15) (Val 5)) (Mult (Val 1) (Val 3))))

expre2 :: Exp Integer
expre2 = (Sub (Val 0) (Mult (Add (Add (Val 6) (Val 8)) (Sub (Val 1) (Val 5))) (Add (Val 2) (Pot (Val 6) (Val 2)))))

--Ex 7

data Hora
  = PM Int Int
  | AM Int Int
  deriving (Eq, Show, Ord)

validaHora :: Int -> Bool
validaHora h
  | h > 0 && h <= 11 = True
  | otherwise = False

validaMin :: Int -> Bool
validaMin m
  | m >= 0 && m <= 59 = True
  | otherwise = False

horasDecorridas :: Hora -> Int
horasDecorridas (AM hora min)
  | validaHora (hora) == True && validaMin (min) == True = hora
  | otherwise = undefined
horasDecorridas (PM hora min)
  | validaHora (hora) == True && validaMin (min) == True = 12 + hora
  | otherwise = undefined

minutosDecorridos :: Hora -> Int
minutosDecorridos (AM hora min)
  | validaHora (hora) == True && validaMin (min) == True = hora * 60 + min
  | otherwise = undefined
minutosDecorridos (PM hora min)
  | validaHora (hora) == True && validaMin (min) == True = ((12 + hora) * 60) + min
  | otherwise = undefined

segundosDecorridos :: Hora -> Int
segundosDecorridos (AM hora min)
  | validaHora (hora) == True && validaMin (min) == True = (hora * 60 + min) * 60
  | otherwise = undefined
segundosDecorridos (PM hora min)
  | validaHora (hora) == True && validaMin (min) == True = (((12 + hora) * 60) + min) * 60
  | otherwise = undefined

--Ex 8

data Contato
  = Nome String
  | Fone String

type Texto = String

type Data = (Int, Int, Int)

data Mensagem
  = WhatsApp Contato Texto Hora Data
  | LinkedIn Contato Texto Hora Data
  | Facebook Contato Texto Hora Data

--A
msgRecebidas :: [Mensagem]
msgRecebidas =
  [ (WhatsApp (Nome "dalton") "fghfdgh" (AM 10 30) (13, 08, 20)),
    (LinkedIn (Fone "464563") "dfghdfgh" (AM 10 31) (13, 08, 20)),
    (Facebook (Nome "faskdflans") "ertyerty" (AM 10 32) (13, 08, 20)),
    (WhatsApp (Nome "faskdflans") "cvbxcbv" (AM 10 33) (13, 08, 20)),
    (WhatsApp (Nome "dalton") "vcnmghjm" (AM 10 37) (13, 08, 20)),
    (Facebook (Nome "faskdflans") "qwerqwer" (AM 11 30) (13, 08, 20)),
    (WhatsApp (Nome "faskdflans") "sdfasdf" (AM 11 35) (13, 08, 20)),
    (Facebook (Fone "464563") "gbdfgsdfg" (AM 11 37) (13, 08, 20)),
    (LinkedIn (Nome "dalton") "ertert" (AM 11 39) (13, 08, 20)),
    (WhatsApp (Nome "dalton") "yukuiol" (AM 11 42) (13, 08, 20)),
    (LinkedIn (Nome "faskdflans") "uidfghjgfh" (AM 11 42) (13, 08, 20)),
    (Facebook (Fone "464563") "tiyuityui" (AM 11 53) (13, 08, 20)),
    (WhatsApp (Nome "dalton") "hjkghjk" (AM 11 53) (13, 08, 20)),
    (WhatsApp (Nome "faskdflans") "adfgsdfg" (AM 11 54) (13, 08, 20)),
    (LinkedIn (Nome "faskdflans") "sdfgeryt" (AM 11 54) (13, 08, 20)),
    -- ======================================================
    (Facebook (Nome "dalton") "wertygdfhfg" (PM 3 25) (14, 08, 20)),
    (LinkedIn (Fone "dalton") "dfghrty" (PM 3 25) (14, 08, 20)),
    (WhatsApp (Nome "dalton") "retyfghdfg" (PM 3 24) (14, 08, 20)),
    (LinkedIn (Nome "faskdflans") "nfdghfgh" (PM 3 27) (14, 08, 20)),
    (LinkedIn (Nome "dalton") "fdghfgn" (PM 3 30) (14, 08, 20)),
    (WhatsApp (Nome "dalton") "cvbnvcbn" (PM 3 33) (14, 08, 20)),
    (Facebook (Nome "faskdflans") "rtyerty" (PM 3 49) (14, 08, 20)),
    (WhatsApp (Fone "464563") "ertygfhdfg" (PM 4 50) (14, 08, 20)),
    (WhatsApp (Nome "dalton") "sdrtert" (PM 4 57) (14, 08, 20)),
    (LinkedIn (Nome "faskdflans") "cvbncvbn" (PM 4 30) (14, 08, 20)),
    (WhatsApp (Nome "dalton") "wertwert" (PM 4 30) (14, 08, 20)),
    (Facebook (Fone "464563") "fnmdhnjm" (PM 4 30) (14, 08, 20)),
    (LinkedIn (Nome "faskdflans") "wertyey" (PM 4 30) (14, 08, 20)),
    (LinkedIn (Fone "464563") "wertyrty" (PM 4 30) (14, 08, 20)),
    (Facebook (Nome "dalton") "weyrwty" (PM 4 30) (14, 08, 20))
  ]

--B
myBubblesort [] = []
myBubblesort lista = bolhaOrd lista (length lista)

bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (catch lista) (n -1)

catch [t] = [t]
catch (x:y:zs)
  | x > y = y : catch (x : zs)
  | otherwise = x : catch (y : zs)
  
--C
msg::Mensagem->Mensagem->Bool
msg (Msg _ _ d1 h1 _) (Msg _ _ d2 h2 _)
  | d1 == d2 = (minpass h1) < (minpass h2)
  | otherwise = precede d1 d2

ordenaDataHora :: [Mensagem] -> [Mensagem]
ordenaDataHora [] = []
ordenaDataHora (piv:xs) =
  (ordenaDataHora [x | x <- xs, (msg x piv) == False])
    ++ [piv]
    ++ (ordenaDataHora [x | x <- xs, (msg x piv) == True])
	
--D
last::Contato->[Mensagem]->[Mensagem]
last cont msgs = take 2 [(Msg c m d h a) | (Msg c m d h a) <- msgOrd, c == cont]
  where
    msgOrd = ordenaDataHora msgs
	
--Ex 9

data ArvBinInt
  = Nulo
  | No Int ArvBinInt ArvBinInt
  deriving (Show)

arvDados :: ArvBinInt
arvDados =
  No
    4
    (No 2 Nulo Nulo)
    ( No
        10
        (No 5 Nulo Nulo)
        (No 15 Nulo Nulo)
    )

--A
internos :: ArvBinInt -> [Int]
internos Nulo = []
internos (No _ Nulo Nulo) = []
internos (No n esq dir) = [n] ++ internos esq ++ internos dir

--B
somaNos :: ArvBinInt -> Int
somaNos Nulo = 0
somaNos (No n Nulo Nulo) = n
somaNos (No n esq dir) = n + somaNos esq + somaNos dir

--C
pertenceArv :: Int -> ArvBinInt -> Bool
pertenceArv _ Nulo = False
pertenceArv x (No v esq dir) =
  x == v
    || if x < v
      then (pertenceArv x esq)
      else (pertenceArv x dir)

--Ex 10

data ArvBinEA a
  = Vazia
  | Folha a
  | NoEA (Char, ArvBinEA a, ArvBinEA a)
  deriving (Show)

arvEA :: ArvBinEA Float
arvEA = NoEA ('+', NoEA ('*', Folha 10, Folha 5), Folha 7)

calculaArv :: (Fractional a, Integral a) => ArvBinEA a -> a
calculaArv Vazia = 0
calculaArv (Folha valor) = valor
calculaArv (NoEA (operacao, esq, dir))
  | (operacao == '+') = (calculaArv esq) + (calculaArv dir)
  | (operacao == '*') = (calculaArv esq) * (calculaArv dir)
  | (operacao == '/') = (calculaArv esq) / (calculaArv dir)
  | (operacao == '^') = (calculaArv esq) ^ (calculaArv dir)
  | (operacao == '-') = (calculaArv esq) - (calculaArv dir)









