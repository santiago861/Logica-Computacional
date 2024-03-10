{-
- Lógica Computacional 2024-2

- Integrantes:
- Reyes Medina Santiago Iván
- López Espinoza Ashley Yael
- Islas Espino Julio Cesar
-}

module FormulasProposicionales where

import Operadores
import Data.List

-- Representa variables p, q, r, s...
type Atom = String

-- Representa las variables que se evalúan a True.
type State = [Atom]

-- data Prop
data Prop = Var Atom        -- Representa una variable proposicional
          | Neg Prop        -- Representa la negación de una proposición
          | Conj Prop Prop  -- Representa la conjunción de dos proposiciones
          | Disy Prop Prop  -- Representa la disyunción de dos proposiciones
          | Impl Prop Prop  -- Representa la implicación entre dos proposiciones
          | Syss Prop Prop  -- Representa la equivalencia entre dos proposiciones

-- instance Show
instance Show Prop where
  show (Var p) = show p
  show (Neg p) = "¬" ++ show p
  show (Conj p q) = "(" ++ show p ++ " ^ " ++ show q ++ ")"
  show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
  show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
  show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Definición de los operadores para el tipo Prop
instance Operadores Prop where
  (¬) p = Neg p
  (\/) p q = Disy p q
  (/\) p q = Conj p q
  (-->) p q = Impl p q
  (<-->) p q = Syss p q


---------------------------------------------------------------------------------
--------                             FUNCIONES                           --------
---------------------------------------------------------------------------------

-- Funcion que dada una formula, regresa el conjunto de todos los  
--símbolos que aparecen en ella.
vars :: Prop -> [Atom]
vars (Var p) = [p]
vars (Neg p) = vars p
vars (Conj p q) = nub (vars p ++ vars q)
vars (Disy p q) = nub (vars p ++ vars q)
vars (Impl p q) = nub (vars p ++ vars q)
vars (Syss p q) = nub (vars p ++ vars q)

-- Funcion que evalua una proposicion dado un estado.
interp :: State -> Prop -> Bool
interp state (Var p) = p `elem` state
interp state (Neg p) = not (interp state p)
interp state (Conj p q) = interp state p && interp state q
interp state (Disy p q) = interp state p || interp state q
interp state (Impl p q) = not (interp state p) || interp state q
interp state (Syss p q) = interp state p == interp state q

{-
State = ["p"]
Prop  = Conj (Var "p") (Var "q")
-}

-- Funcion que elimina las equivalencias (<->).
elimEquiv :: Prop -> Prop
elimEquiv (Var p) = Var p
elimEquiv (Neg p) = Neg (elimEquiv p)
elimEquiv (Conj p q) = Conj (elimEquiv p) (elimEquiv q)
elimEquiv (Disy p q) = Disy (elimEquiv p) (elimEquiv q)
elimEquiv (Impl p q) = Disy (Neg (elimEquiv p)) (elimEquiv q)
elimEquiv (Syss p q) = Conj (Disy (Neg (elimEquiv p)) (elimEquiv q)) (Disy (Neg (elimEquiv q)) (elimEquiv p))


-- Funcion que elimina las implicaciones, puedes suponer que no hay
-- equivalencias.
elimImpl :: Prop -> Prop
elimImpl (Var p) = Var p
elimImpl (Neg p) = Neg (elimImpl p)
elimImpl (Conj p q) = Conj (elimImpl p) (elimImpl q)
elimImpl (Disy p q) = Disy (elimImpl p) (elimImpl q)
elimImpl (Impl p q) = Disy (Neg (elimImpl p)) (elimImpl q)
elimImpl (Syss p q) = Conj (Disy (Neg (elimImpl p)) (elimImpl q)) (Disy (Neg (elimImpl q)) (elimImpl p))


{-
P -> (Q -> R) => ¬P v (Q -> R) => ¬P v (¬Q v R)
-}

-- Funcion que da TODAS las posibles interpretaciones que podria tomar
-- una formula.
posiblesInterp :: Prop -> [State]
posiblesInterp prop = potencia (vars prop)

-- Funicion que nos dice si un estado es modelo de una proposicion.
esModelo :: Prop -> State -> Bool
esModelo prop state = interp state prop

-- Funcion que nos da TODOS los modelos de una proposicion.
todosModelos :: Prop -> [State]
todosModelos prop = filter (\state -> esModelo prop state) (posiblesInterp prop)

-- Funcion que nos dice si una proposicion es satifacible.
esSatisfacible :: Prop -> Bool
esSatisfacible prop = not (null (todosModelos prop))

-- Funcion que nos dice si una proposicion es instisfacible.
esInsatisfacible :: Prop -> Bool
esInsatisfacible prop = not (esSatisfacible prop)

-- Funcion que nos dice si una proposicion es una tautologia.
esTautologia :: Prop -> Bool
esTautologia prop = esInsatisfacible (Neg prop)

-- Funcion que nos dice si una proposicion es una contradiccion.
esContradiccion :: Prop -> Bool
esContradiccion prop = esTautologia (Neg prop)

---------------------------------------------------------------------------------
--------                           AUXILIARES                            --------
---------------------------------------------------------------------------------

-- Función auxiliar que calcula la potencia de un conjunto.
potencia :: [a] -> [[a]]
potencia [] = [[]]
potencia (x:xs) = [ x:ys | ys <- xss] ++ xss
  where
    xss = potencia xs

---------------------------------------------------------------------------------
--------                             EJEMPLOS                            --------
---------------------------------------------------------------------------------

i  = Conj (Var "p") (Var "q")
i' = (Var "p") /\ (Var "q")

p = Var "p"
q = Var "q"
r = Var "r"
varsrp = ["r", "p"]
form1 = ((p \/ q) /\ (((¬) q) \/ r))
interp1 = interp varsrp form1

taut1 = (p \/ (¬) p)
taut2 = ((p \/ q) \/ ((¬)p /\ (¬) q))

cont1 = ((p \/ q) /\ ((¬)p /\ (¬) q))

potencia1 = potencia [1,2,3]
