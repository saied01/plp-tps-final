module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
    "Ej 7 - Expr.recrExpr" ~: testsRecr,
    "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      alinearDerecha 1 "test" ~?= "test",
      alinearDerecha 8 "abc" ~?= "     abc",
      alinearDerecha 0 "x" ~?= "x"
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      actualizarElem 2 (* 2) [5, 10, 15] ~?= [5, 10, 30],
      actualizarElem 5 (+ 1) [1, 2, 3] ~?= [1, 2, 3],
      actualizarElem (-1) (+ 10) [1, 2, 3] ~?= [1, 2, 3]
    ]

testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],
      casilleros (vacio 2 (10, 20))
        ~?= [ Casillero infinitoNegativo 10 0 0,
              Casillero 10 15 0 0,
              Casillero 15 20 0 0,
              Casillero 20 infinitoPositivo 0 0
            ],
      casilleros (vacio 4 (-2, 2))
        ~?= [ Casillero infinitoNegativo (-2) 0 0,
              Casillero (-2) (-1) 0 0,
              Casillero (-1) 0 0 0,
              Casillero 0 1 0 0,
              Casillero 1 2 0 0,
              Casillero 2 infinitoPositivo 0 0
            ]
    ]

testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6)
   in test
        [ casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores están acá
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 5 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 1 100,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 10 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 1 100
                ]
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ histograma 4 (1, 5) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),
      histograma 2 (0, 10) [5] ~?= agregar 5 (vacio 2 (0, 10)),
      histograma 3 (0, 6) [1, 2, 3, 4, 5] 
        ~?= agregar 5 (agregar 4 (agregar 3 (agregar 2 (agregar 1 (vacio 3 (0, 6)))))),
      -- Histograma con valores fuera de rango
      histograma 2 (0, 10) [-1, 5, 15] 
        ~?= agregar 15 (agregar 5 (agregar (-1) (vacio 2 (0, 10))))
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      -- Dos valores en diferentes casilleros
      casilleros (agregar 5 (agregar 1 (vacio 3 (0, 6))))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 1 50.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 1 50.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      -- Múltiples valores en un casillero
      casilleros (agregar 3 (agregar 2.5 (agregar 2 (vacio 3 (0, 6)))))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 3 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ]
    ]

testsRecr :: Test
testsRecr =
  test
    [ -- Prueba de que recrExpr existe y funciona básicamente
      recrExpr id (\x y -> x + y) (\_ _ r1 r2 -> r1 + r2) (\_ _ r1 r2 -> r1 - r2) 
               (\_ _ r1 r2 -> r1 * r2) (\_ _ r1 r2 -> r1 / r2) (Const 5.0) ~?= 5.0,
      recrExpr id (\x y -> x + y) (\_ _ r1 r2 -> r1 + r2) (\_ _ r1 r2 -> r1 - r2) 
               (\_ _ r1 r2 -> r1 * r2) (\_ _ r1 r2 -> r1 / r2) (Rango 1 5) ~?= 6.0,
      recrExpr id (\x y -> x + y) (\_ _ r1 r2 -> r1 + r2) (\_ _ r1 r2 -> r1 - r2) 
               (\_ _ r1 r2 -> r1 * r2) (\_ _ r1 r2 -> r1 / r2) (Suma (Const 2) (Const 3)) ~?= 5.0,
      -- Prueba que recibe los subárboles originales
      recrExpr (const 1) (\_ _ -> 1) (\e1 _ _ _ -> if e1 == Const 1 then 100 else 0) 
               (\_ _ _ _ -> 0) (\_ _ _ _ -> 0) (\_ _ _ _ -> 0) (Suma (Const 1) (Const 2)) ~?= 100
    ]

testsFold :: Test
testsFold =
  test
    [ -- Evaluación simple con foldExpr
      foldExpr id (\x y -> x + y) (+) (-) (*) (/) (Const 5.0) ~?= 5.0,
      foldExpr id (\x y -> x + y) (+) (-) (*) (/) (Rango 1 5) ~?= 6.0,
      foldExpr id (\x y -> x + y) (+) (-) (*) (/) (Suma (Const 2) (Const 3)) ~?= 5.0,
      foldExpr id (\x y -> x + y) (+) (-) (*) (/) (Mult (Const 4) (Const 5)) ~?= 20.0,
      foldExpr id (\x y -> x + y) (+) (-) (*) (/) (Div (Const 10) (Const 2)) ~?= 5.0
    ]

testsEval :: Test
testsEval =
  test
    [ fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
      -- Constante sola
      fst (eval (Const 10) genFijo) ~?= 10.0,
      -- Multiplicación
      fst (eval (Mult (Const 3) (Const 4)) genFijo) ~?= 12.0,
      -- División
      fst (eval (Div (Const 10) (Const 2)) (genNormalConSemilla 0)) ~?= 5.0
   ]

testsArmarHistograma :: Test
testsArmarHistograma =
  test
    [ -- Histograma simple con función constante
      let (h, _) = armarHistograma 3 10 (\g -> (5.0, g)) genFijo
          cs = casilleros h
      in casCantidad (cs !! 2) ~?= 10, -- Todos los valores deben estar en un casillero
      
      -- Histograma con generador de rangos
      let (h, _) = armarHistograma 5 100 (dameUno (1, 5)) genFijo
          cs = casilleros h
      in sum (map casCantidad cs) ~?= 100, -- Debe haber 100 valores en total
      
      -- Verificar que el rango cubre el 95% con genNormalConSemilla
      let (h, _) = armarHistograma 11 1000 (dameUno (1, 5)) (genNormalConSemilla 0)
          cs = casilleros h
      in sum (map casCantidad cs) ~?= 1000,
      
      -- Histograma con más casilleros
      let (h, _) = armarHistograma 10 500 (dameUno (0, 100)) (genNormalConSemilla 1)
          cs = casilleros h
      in length cs ~?= 12 -- 10 casilleros finitos + 2 infinitos
    ]

testsEvalHistograma :: Test
testsEvalHistograma =
  test
    [ -- Verificar que evalHistograma genera el número correcto de muestras
      let (h, _) = evalHistograma 11 100 (Suma (Const 1) (Const 2)) genFijo
          cs = casilleros h
      in sum (map casCantidad cs) ~?= 100,
      
      -- Expresión simple con constante
      let (h, _) = evalHistograma 5 50 (Const 10) genFijo
          cs = casilleros h
      in sum (map casCantidad cs) ~?= 50,
      
      -- Expresión con rango
      let (h, _) = evalHistograma 11 200 (Rango 1 5) (genNormalConSemilla 0)
          cs = casilleros h
      in sum (map casCantidad cs) ~?= 200,
      
      -- Expresión compleja
      let (h, _) = evalHistograma 11 1000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
          cs = casilleros h
      in sum (map casCantidad cs) ~?= 1000
    ]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar = test
  [ mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
      ~?= "1.0 + 2.0 + 3.0 + 4.0"
  , mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
      ~?= "1.0 + 2.0 + 3.0 + 4.0"
  , mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
      ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0"
  , mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
      ~?= "(1.0 - 2.0) - (3.0 - 4.0)"
  , mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
      ~?= "((1.0 - 2.0) - 3.0) - 4.0"
  ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
