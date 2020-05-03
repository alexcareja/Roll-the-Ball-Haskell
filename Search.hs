{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import Data.Maybe

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

-- showNode :: (ProblemState s a, Show s) => Node s a -> [Char]
-- showNode (NewNode s _ _ _ _) = show s

-- instance (ProblemState s a, Show s) => Show (Node s a)
--     where show = showNode

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

bfsHelper :: (ProblemState s a, Eq s, Eq a) => ([Node s a], [Node s a]) -> [s] -> [([Node s a], [Node s a])]
bfsHelper (_, []) _ = [([], [])]
bfsHelper anterior@(_, frontier) visited =  [anterior] ++ (bfsHelper current (visited ++ (map nodeState chldrn)))
        where
            chldrn = filter (\node -> not (elem (nodeState node) visited)) (nodeChildren (head frontier))
            current = (chldrn, (tail frontier) ++ chldrn)

bfs :: (ProblemState s a, Ord s, Eq s, Eq a) => Node s a -> [([Node s a], [Node s a])]
bfs node = bfsHelper ([node], [node]) [nodeState node]



{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

intersect :: (ProblemState s a, Eq s) => [Node s a] -> [Node s a] -> Bool
intersect l1 l2 = 0 < length (filter (\x -> elem (nodeState x) (map nodeState l2)) l1)

intersection :: (ProblemState s a, Eq s) => [Node s a] -> [Node s a] -> (Node s a, Node s a)
intersection l1 l2 = (s1, s2)
    where
        s1 = head (filter (\x -> elem (nodeState x) (map nodeState l2)) l1)
        s2 = head (filter (\x -> (nodeState x) == (nodeState s1)) l2)

bidirBFSHelper :: (ProblemState s a, Eq a, Eq s, Ord s) => [([Node s a], [Node s a])] -> [([Node s a], [Node s a])] -> (Node s a, Node s a)
bidirBFSHelper b1 b2
    | intersect (fst (head b1)) (snd (head b2)) = intersection (fst (head b1)) (snd (head b2))
    | intersect (fst (head b2)) (snd (head b1)) = intersection (snd (head b1)) (fst (head b2))
    | otherwise = bidirBFSHelper (tail b1) (tail b2)


bidirBFS :: (ProblemState s a, Eq a, Eq s, Ord s) => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS node1 node2 = bidirBFSHelper bfs1 bfs2
    where
        bfs1 = bfs node1
        bfs2 = bfs node2

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

nodeParentMaybe :: Maybe (Node s a) -> Maybe (Node s a)
nodeParentMaybe Nothing = Nothing
nodeParentMaybe (Just node) = nodeParent node

extractPath :: (Eq a, Eq s) => Node s a -> [(Maybe a, s)]
extractPath n = reverse $ map (\node -> ((nodeAction (fromJust node), nodeState (fromJust node)))) p
    where
        p = (Just n) : (takeWhile (\x -> x /= Nothing) $ iterate nodeParentMaybe (nodeParent n))

{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s, Eq a)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve initial final = extractPath m1 ++ secondHalf
    where
        (m1, m2) = bidirBFS (createStateSpace initial) $ createStateSpace final
        path2 = reverse $ extractPath m2
        actions =  map fromJust $ map fst $ init $ path2
        states =  map snd $ tail $ path2
        secondHalf = map (\x -> (Just (fst x), snd x)) $ map reverseAction $ zip actions states
