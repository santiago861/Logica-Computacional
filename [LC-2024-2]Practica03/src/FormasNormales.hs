{-
- Lógica Computacional 2024-2

- Integrantes:
- Reyes Medina Santiago Iván
- López Espinoza Ashley Yael
- Islas Espino Julio Cesar
-}

module FormasNormales where

import FormProp
import Operadores

--------------------------------------------------------------------------------
-- Función que dada una fórmula f, regresa una fórmula equivalente a f
-- en su Forma Normal Negativa.
fnn :: Prop -> Prop
fnn = fnnAux . elimImpl

-- Función auxiliar que transforma una fórmula en su Forma Normal Negativa.
fnnAux :: Prop -> Prop
fnnAux (Var p) = Var p
fnnAux (Neg (Var p)) = Neg (Var p)
fnnAux (Neg (Neg p)) = fnnAux p -- Doble negación
fnnAux (Neg (Conj p q)) = Disy (fnnAux (Neg p)) (fnnAux (Neg q)) -- De Morgan
fnnAux (Neg (Disy p q)) = Conj (fnnAux (Neg p)) (fnnAux (Neg q)) -- De Morgan
fnnAux (Conj p q) = Conj (fnnAux p) (fnnAux q)
fnnAux (Disy p q) = Disy (fnnAux p) (fnnAux q)

--------------------------------------------------------------------------------

-- Función que dada una fórmula f, regresa una fórmula equivalente a f
-- en su Forma Normal Conjuntiva.
fnc :: Prop -> Prop
fnc = fncAux . fnn

-- Función auxiliar que transforma una fórmula en su Forma Normal Conjuntiva.
fncAux :: Prop -> Prop
fncAux (Conj p q) = Conj (fncAux p) (fncAux q)
fncAux (Disy p q) = distribuir (fncAux p) (fncAux q) -- Distribuir disyunciones sobre conjunciones
fncAux p = p

-- Función que distribuye disyunciones sobre conjunciones.
distribuir :: Prop -> Prop -> Prop
distribuir (Conj p q) r = Conj (distribuir p r) (distribuir q r)
distribuir p (Conj q r) = Conj (distribuir p q) (distribuir p r)
distribuir p q = Disy p q

--------------------------------------------------------------------------------

-- Función que dada una fórmula f, regresa una fórmula equivalente a f
-- en su Forma Normal Disyuntiva.
fnd :: Prop -> Prop
fnd = fndAux . fnn

-- Función auxiliar que transforma una fórmula en su Forma Normal Disyuntiva.
fndAux :: Prop -> Prop
fndAux (Disy p q) = Disy (fndAux p) (fndAux q)
fndAux (Conj p q) = distribuirDisy (fndAux p) (fndAux q) -- Distribuir conjunciones sobre disyunciones
fndAux p = p

-- Función que distribuye conjunciones sobre disyunciones.
distribuirDisy :: Prop -> Prop -> Prop
distribuirDisy (Disy p q) r = Disy (distribuirDisy p r) (distribuirDisy q r)
distribuirDisy p (Disy q r) = Disy (distribuirDisy p q) (distribuirDisy p r)
distribuirDisy p q = Conj p q

--------------------------------------------------------------------------------

-- ejemplo 1 para forma normal negativa
fnn1 = fnn ((Var "p") /\ ((Neg (Var "q")) \/ (Var "r")))

-- ejemplo 2 para forma normal negativa
fnn2 = fnn ((Neg ((Neg (Var "p")) /\ (Var "q"))) \/ ((Var "r") /\ (Neg (Var "s"))))

-- ejemplo 1 para forma normal conjuntiva
fnc1 = fnc ((Var "p") \/ ((Var "q") /\ (Var "r")))

-- ejemplo 2 para forma normal conjuntiva
fnc2 = fnc (((Var "p") /\ (Var "q")) \/ ((Var "r") /\ (Var "s")))

-- ejemplo 1 para forma normal disyuntiva
fnd1 = fnd ((Var "p") /\ ((Var "q") \/ (Var "r")))

-- ejemplo 2 para forma normal disyuntiva
fnd2 = fnd (((Var "p") \/ (Var "q")) /\ ((Var "r") \/ (Var "s")))
