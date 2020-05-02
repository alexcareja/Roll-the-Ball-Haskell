{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = NewNode {state :: s,
                parent :: Maybe (Node s a),
                depth :: Int,
                action :: Maybe a,
                children :: [Node s a]} deriving (Eq)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState = state 

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent = parent

nodeDepth :: Node s a -> Int
nodeDepth = depth

nodeAction :: Node s a -> Maybe a
nodeAction = action

nodeChildren :: Node s a -> [Node s a]
nodeChildren = children

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

createNewNode :: (ProblemState s a, Eq s) => s -> Maybe (Node s a) -> Int -> Maybe a -> Node s a
createNewNode st p d a = newNode
    where 
        newNode = NewNode st p d a chldrn
        chldrn = filter (\(NewNode nst _ _ _ _) -> nst /= st) $ map (\(act, newst) -> createNewNode newst (Just newNode) (d + 1) (Just act)) $ successors st

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace st = createNewNode st Nothing 0 Nothing

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfsHelper :: ([Node s a], [Node s a]) -> [([Node s a], [Node s a])]
bfsHelper (_, []) = []
bfsHelper anterior@(_, frontier) = [anterior] ++ (bfsHelper current)
        where
            chldrn = nodeChildren (head frontier) 
            current = (chldrn, (tail frontier) ++ chldrn)

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs node = bfsHelper ([node], [node])--[(nodeChildren node, f)]



{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS = undefined


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath = undefined



{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined
