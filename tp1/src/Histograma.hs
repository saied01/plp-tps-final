{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Util

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
-- vacio :: Int -> (Float, Float) -> Histograma
vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u)
  | n >= 1 && l < u = Histograma l u (replicate (n + 2) 0)
  | otherwise = error "vacio: requiere n >= 1 y l < u"

--                         x       piso     salto      indice mas alto       resultado
-- averiguarIndiceDeX :: Float -> Float ->  Float ->        Int      ->      Int
averiguarIndiceDeX :: Float -> Float -> Float -> Int -> Int
averiguarIndiceDeX x piso salto indiceActual
  | x >= piso + (salto * (fromIntegral indiceActual - 1)) = indiceActual
  | indiceActual == 0 = 0
  | otherwise = averiguarIndiceDeX x piso salto (indiceActual-1)



-- | Agrega un valor al histograma.
agregar :: Float -> Histograma -> Histograma
agregar x (Histograma i t xs)  = (Histograma i t (actualizarElem indice f xs))
    where
      f x = x + 1
      indice = averiguarIndiceDeX x i t ((length xs) - 1)

-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma n (i, t) xs = foldr agregar obj_histograma xs
  where
    obj_histograma = vacio n (i, t)

-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
--               Casillero piso  techo cantidad porcentaje
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p

-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
casilleros :: Histograma -> [Casillero]
casilleros (Histograma piso saltos xs) = zipWith f [0..] xs
  where
    total = fromIntegral (sum xs)
    f indice x
      | indice == 0 = Casillero infinitoNegativo  piso  x  (fromIntegral x * ( 100 / total))
      | indice == (length xs-1) =  Casillero (piso + saltos*(fromIntegral indice-1)) infinitoPositivo x (fromIntegral x * ( 100 / total))
      | otherwise = Casillero (piso + saltos*(fromIntegral indice-1)) (piso + saltos*fromIntegral indice) x (fromIntegral x * ( 100 / total))


