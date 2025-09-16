{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use (,)" #-}
{-# HLINT ignore "Use const" #-}
module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma
import Histograma (casilleros, histograma)

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)



recrExpr :: (Float-> b) -> (Float-> Float -> b) ->
            (Expr -> Expr -> b -> b -> b) -> (Expr -> Expr -> b -> b -> b) ->
            (Expr -> Expr -> b -> b -> b) -> (Expr -> Expr -> b -> b -> b) -> Expr -> b
recrExpr fCon fRang fSum fRest fMult fDiv expr = case expr of
                Const x     -> fCon x
                Rango x y   -> fRang x y
                Suma e1 e2  -> fSum e1 e2 (rec e1) (rec e2)
                Resta e1 e2 -> fRest e1 e2 (rec e1) (rec e2)
                Mult e1 e2  -> fMult e1 e2 (rec e1) (rec e2)
                Div e1 e2   -> fDiv e1 e2 (rec e1) (rec e2)
  where rec = recrExpr fCon fRang fSum fRest fMult fDiv


-- Los esquemas de recursión estructural (foldr) y explícita (recr) del tipo de dato definido -> Expr.
-- La idea es que foldExpr en cada constructor reemplaza los subárboles por los resultados ya procesados,
-- entonces las funciones que se definen usando foldExpr solo dependen de los valores reducidos de los hijos.
-- Mientras que en recrExpr, además del resultado recursivo, también recibe los subárboles originales, lo cual lo hace más general.


foldExpr :: (Float -> b) -> (Float -> Float -> b) -> 
            (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Expr -> b
foldExpr fCon fRang fSum fRest fMult fDiv expr = case expr of
    Const x     -> fCon x
    Rango x y   -> fRang x y
    Suma e1 e2  -> fSum (rec e1) (rec e2)
    Resta e1 e2 -> fRest (rec e1) (rec e2)
    Mult e1 e2  -> fMult (rec e1) (rec e2)
    Div e1 e2   -> fDiv (rec e1) (rec e2)
  where
    rec = foldExpr fCon fRang fSum fRest fMult fDiv




-- Cada operación (suma, resta, etc.) debe devolver un par (Float, Gen):
-- el resultado numérico y el generador actualizado. 
-- Primero se evalúa el subárbol izquierdo con f1 g, obteniendo (v1, g1). 
-- Luego se evalúa el derecho con f2 g1 = (v2, g2), usando el generador ya avanzado. 
-- Con v1 y v2 se aplica la operación aritmética y se obtiene vf, 
-- que junto con g2 se devuelve como (vf, g2).


eval :: Expr -> G Float
eval = foldExpr fCon fRang fSum fRest fMult fDiv
      where fCon x g = (x,g)
            fRang x y g = dameUno (x,y) g
            fSum f1 f2 g = (fst (f1 g) + fst (f2 (snd (f1 g))), snd (f2 (snd (f1 g))))
            fRest f1 f2 g = (fst (f1 g) - fst (f2 (snd (f1 g))), snd (f2 (snd (f1 g))))
            fMult f1 f2 g = (fst (f1 g) * fst (f2 (snd (f1 g))), snd (f2 (snd (f1 g))))
            fDiv f1 f2 g = (fst (f1 g) / fst (f2 (snd (f1 g))), snd (f2 (snd (f1 g))))

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.

-- Se calcula la lista de números reales (xs) y el generador (gen') con la función "muestra"
-- con la función (f), el número entero (cantDeMuestras) y el generador (g).
-- La lista (xs) son todos los resultados de aplicar (cantDeMuestras) veces la función (f) al generador (g).
-- El generador (gen') es el generador (g) luego de aplicarle (cantDeMuestras) veces la función (f).
-- Se calcula el rango (piso,techo) de la lista (xs) con la función "rango95".
-- Devuelve la tupla con el histograma generado con la función (histograma) ya implementada,
-- y el generador (gen') ya mencionado

armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma casilleros cantDeMuestras f g =  (histograma casilleros (piso, techo) xs, gen')
  where
    (xs, gen') = muestra f cantDeMuestras g -- f retorna valores, g
    (piso, techo) = rango95 xs 


-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.

-- Se evalúa la expresión (expr) utilizando la función "eval", que devuelve una función de tipo G Float.
-- Se arma el histograma utilizando la función "armarHistograma" con el número entero (casilleros) del input,
-- el número entero (cantDeEvaluaciones) del input, y la expresión evaluada anteriormente mencionada.
-- Devuelve una función que recibe un generador, evalúa la expresión usando ese generador, y arma el histograma
-- de la expresión evaluada.

evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma casilleros cantDeEvaluacinoes expr = armarHistograma casilleros cantDeEvaluacinoes expresionEvaluada
  where
    expresionEvaluada = eval expr

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- Toma una expresión y la recorre recursivamente usando la función "recrExpr", construyendo el
-- String que represente correctamente las operaciones de la expresión.

-- Las dos primeras funciones de "recrExpr" son los casos base de una constante Const y un rango Rango.
-- Para las constantes, simplemente se le aplica "show", que dado un Float, lo devuelve como String.
-- Para los rangos, también se aplica "show" a ambos números, y se los concatena con un '~' en el medio.  

-- Para evitar usar el uso de paréntesis innecesarios en el resto de expresiones, cada función de "recrExpr"
-- utiliza "maybeParen" y "constructor".
-- La función "maybeParen" recibe un Bool, que si es True coloca paréntesis, y si es False no lo hace.
-- La función "constructor" recibe una expresión y devuelve su contructor.
-- Este se usa para saber si las expresiones de cada lado de las operaciones
-- son un tipo de operación que debería llevar paréntesis o no.
-- En el caso de la suma, resta, y multiplicación, se aplica paréntesis si hay una resta, división, o multiplicación.
-- En el caso de la división, se aplica paréntesis para todas las operaciones, pero no para las constantes o los rangos.

mostrar :: Expr -> String
mostrar = recrExpr (\x -> show x) (\x y -> show x ++ "~" ++ show y)
                    (\con1 con2 ac1 ac2 -> maybeParen (constructor con1 `elem` [CEResta, CEDiv, CEMult]) ac1 ++ " + " ++ maybeParen (constructor con2 `elem` [CEResta, CEDiv, CEMult]) ac2)                   
                    (\con1 con2 ac1 ac2 -> maybeParen (constructor con1 `elem` [CEResta, CEDiv, CEMult]) ac1 ++ " - " ++ maybeParen (constructor con2 `elem` [CEResta, CEDiv, CEMult]) ac2)                   
                    (\con1 con2 ac1 ac2 -> maybeParen (constructor con1 `elem` [CEResta, CEDiv, CEMult]) ac1 ++ " * " ++ maybeParen (constructor con2 `elem` [CEResta, CEDiv, CEMult]) ac2)                   
                    (\con1 con2 ac1 ac2 -> maybeParen (constructor con1 `elem` [CESuma, CEResta, CEDiv, CEMult]) ac1 ++ " / " ++ maybeParen (constructor con2 `elem` [CESuma, CEResta, CEDiv, CEMult]) ac2)








data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s


 
