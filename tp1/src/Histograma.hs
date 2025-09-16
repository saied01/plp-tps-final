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

-- | Construye un histograma vacío.
-- 
-- Precondiciones:
--   cantidadDeElem >= 1@
--   piso < techo
--
-- El histograma resultante tiene:
--   cantidadDeElem casilleros y  2 casilleros adicionales 
--   para valores fuera de rango (con - y + infinito).
--   Todos los casilleros inicializados en 0.
--
-- El tamaño del salto entre casilleros se calcula como:
--   (techo - piso) / cantidadDeElem

vacio :: Int -> (Float, Float) -> Histograma
vacio cantidadDeElem (piso, techo)
  | cantidadDeElem >= 1 && piso < techo = Histograma piso salto (replicate (cantidadDeElem + 2) 0)
  | otherwise = error "vacio: requiere cantidadDeElem >= 1 y l < u"
  where
    salto = (techo - piso) / fromIntegral cantidadDeElem


-- | Dado un valor x, un piso, el salto entre casilleros 
-- y el indice más alto disponible, calcula el indice del casillero
-- en el que debería caer x.
--
-- Si x es menor al piso, devuelve 0.
-- Si x es mayor o igual al limite superior, devuelve el indice más alto. 

averiguarIndiceDeX :: Float -> Float -> Float -> Int -> Int
averiguarIndiceDeX x piso salto indiceActual
  | x >= piso + (salto * (fromIntegral indiceActual - 1)) = indiceActual
  | indiceActual == 0 = 0
  | otherwise = averiguarIndiceDeX x piso salto (indiceActual-1)



-- | Incrementa en 1 el casillero correspondiente al valor dado.
-- Usa 'averiguarIndiceDeX' para determinar el índice en el histograma
-- donde cae el valor, y luego actualiza ese casillero. 
agregar :: Float -> Histograma -> Histograma
agregar x (Histograma i t xs)  = (Histograma i t (actualizarElem indice f xs))
    where
      f x = x + 1
      indice = averiguarIndiceDeX x i t ((length xs) - 1)

-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
-- Como ya tenemos definidas las funciones para armar un histograma vacío, y una para agregar un valor a un histograma dado,
-- simplemente se crea un histograma vacío y se agregan todos los valores usando foldr junto con la función agregar.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma casilleros (piso, techo) xs = foldr agregar obj_histograma xs
  where
    obj_histograma = vacio casilleros (piso, techo)

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
-- Se construye la lista de casilleros a partir de la representación interna del histograma.
-- Para cada posición de la lista de xs, `zipWith` combina el índice con la cantidad,
-- y genera el casillero correspondiente con sus límites y porcentaje.
-- Tenemos tres casos ya que tenemos:
-- 1- El primer casillero que va de -inf al primer valor.
-- 2- El ultimo casillero que va del ultimo valor a +inf
-- 3- El resto de casilleros del medio, que van de un valor al siguiente dependiendo del salto.
casilleros :: Histograma -> [Casillero]
casilleros (Histograma piso saltos xs) = zipWith f [0..] xs
  where
    porcentaje = if sum xs == 0 then 0 else 100 / fromIntegral (sum xs)
    f indice x
      | indice == 0 = Casillero infinitoNegativo  piso  x  (fromIntegral x * porcentaje)
      | indice == (length xs-1) =  Casillero (piso + saltos*(fromIntegral indice-1)) infinitoPositivo x (fromIntegral x * porcentaje)
      | otherwise = Casillero (piso + saltos*(fromIntegral indice-1)) (piso + saltos*fromIntegral indice) x (fromIntegral x * porcentaje)


