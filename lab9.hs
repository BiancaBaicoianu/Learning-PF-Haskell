import Data.List (nub)
import Data.Maybe (fromJust)


type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving (Eq, Read)
infixr 2 :|:
infixr 3 :&:
infixr 4 :->:     -- ex 9.
infixr 5 :<->:


--1. Scrieti următoarele formule ca expresii de tip Prop, denumindu-le p1, p2, p3.
--a) (P ∨ Q) ∧ (P ∧ Q)
p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

--b) (P ∨ Q) ∧ (¬P ∧ ¬Q)
p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

--c)  (P ∧ (Q ∨ R)) ∧ ((¬P ∨ ¬Q) ∧ (¬P ∨ ¬R))
p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))


--2. Faceti tipul Prop instantă a clasei de tipuri Show, înlocuind conectivele Not, :|: si :&:
--cu ~, | si & si folosind direct numele variabilelor în loc de constructia Var nume.

instance Show Prop where
  show (Var nume) = nume
  show F = "F"
  show T = "T"
  show (Not prop) = "(~" ++ show prop ++ ")"
  show (prop1 :|:prop2) = "(" ++ show prop1 ++ "|" ++ show prop2 ++ ")"
  show (prop1 :&:prop2) = "(" ++ show prop1 ++ "&" ++ show prop2 ++ ")"
  --ex 9.
  show (prop1 :->: prop2) = "(" ++ show prop1 ++ "->" ++ show prop2 ++ ")"
  show (prop1 :<->: prop2) = "(" ++ show prop1 ++ "<->" ++ show prop2 ++ ")"

test_ShowProp :: Bool
test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"


-- Tipul Env este o listă de atribuiri de valori de adevăr pentru (numele) variabilelor propozitionale.
type Env = [(Nume, Bool)]

-- generează o eroare dacă valoarea nu este găsită.
impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

-- 3. Definiti o functie eval care dat fiind o expresie logică si un mediu de evaluare,
--calculează valoarea de adevăr a expresiei.
eval :: Prop -> Env -> Bool
eval (Var nume) env = impureLookup nume env
eval F env = False
eval T env = True
eval (Not prop) env = not (eval prop env)
eval (prop1 :&: prop2) env = eval prop1 env && eval prop2 env
eval (prop1 :|: prop2) env = eval prop1 env || eval prop2 env
--9. 
eval (prop1 :->: prop2) env = eval prop1 env < eval prop2 env
eval (prop1 :<->: prop2) env = eval prop1 env == eval prop2 env

test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

--4. Definiti o functie variabile care colectează lista tuturor variabilelor dintr-o formulă.
--Indicatie: folositi functia nub.

variabile :: Prop -> [Nume]
variabile (Var nume) = [nume]
variabile F = []
variabile T = []
variabile (Not prop) = variabile prop
variabile (prop1 :&: prop2) = nub $ variabile prop1 ++ variabile prop2
variabile (prop1 :|: prop2) = nub $ variabile prop1 ++ variabile prop2
--9.
variabile (prop1 :->: prop2) = nub $ variabile prop1 ++ variabile prop2
variabile (prop1 :<->: prop2) = nub $ variabile prop1 ++ variabile prop2

test_variabile =
  variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]


--5. Dată fiind o listă de nume, definiti toate atribuirile de valori de adevăr posibile pentru ea.

envs :: [Nume] -> [Env]
envs [] = []
envs [x] = [[(x, False)], [(x, True)]]
envs (x : xs) =
    map ((x, False) :) (envs xs) ++ map ((x, True) :) (envs xs)

test_envs = 
    envs ["P", "Q"]
    ==
    [ [ ("P",False)
      , ("Q",False)
      ]
    , [ ("P",False)
      , ("Q",True)
      ]
    , [ ("P",True)
      , ("Q",False)
      ]
    , [ ("P",True)
      , ("Q",True)
      ]
    ]


-- 6. Definiti o functie satisfiabila care dată fiind o Propozitie verifică dacă aceasta este satisfiabilă. Puteti folosi rezultatele de la exercitiile 4 si 5.
satisfiabila :: Prop -> Bool
satisfiabila p = foldr ((||) . eval p) False (envs (variabile p))
 
test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

--7. O propozitie este validă dacă se evaluează la True pentru orice interpretare a varibilelor.
--O forumare echivalenta este aceea că o propozitie este validă dacă negatia ei este nesatisfiabilă. 
--Definiti o functie valida care verifică dacă o propozitie este validă.

valida :: Prop -> Bool
valida p = not $ satisfiabila (Not p)

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True

--9. Extindeti tipul de date Prop si functiile definite până acum pentru a include conectivele logice -> (implicatia) si <-> (echivalent,a), 
-- folosind constructorii :->: si :<->:.

--10. Două propozitii sunt echivalente dacă au mereu aceeasi valoare de adevăr, indiferent de valorile variabilelor propozitionale. 
--Scrieti o functie care verifică dacă două propozitii sunt echivalente.

echivalenta :: Prop -> Prop -> Bool
echivalenta p q = and [eval(p:<->:q)(x++y)|x<-envs(variabile p), y<-envs(variabile q)]
 
test_echivalenta1 =
  True
  ==
  (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 =
  False
  ==
  (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 =
  True
  ==
  (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))

