{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A
import Data.Maybe()

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = EmptySpace |
            EmptyCell | 
            StartCell {dir :: Directions } |
            FinishCell {dir :: Directions} |
            SimpleCell {dir :: Directions } |
            CurvedCell {dir1 :: Directions, dir2 :: Directions}
    deriving (Eq, Ord)
{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level (A.Array Position Cell)
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}


showLevel :: Level -> [Char]
showLevel (Level arr) = endl : showLevelHelper (A.elems arr) (1 + (snd $ snd $ A.bounds arr))
-- showLevel l@(Level arr) = endl : showLevelHelper l positions height
--     where
--         height = 1 + (fst $ snd $ A.bounds arr)
--         width = 1 + (snd $ snd $ A.bounds arr)
--         m = width * height - 1
--         positions = [((quot i width), (mod i width)) | i <- [0..m]]

-- showLevelHelper :: Level -> [Position] -> Int -> [Char]
-- showLevelHelper l@(Level arr) positions height = foldr (\x -> printCharacter l height x) [] positions

-- printCharacter :: Level -> Int -> [Char] -> Position -> [Char]
-- printCharacter (Level arr) width xs pos@(x, y)
--     | y == width - 1 = xs ++ [getChr (arr A.! pos)] ++ [endl]
--     | otherwise = xs ++ [getChr (arr A.! pos)]
showLevelHelper :: [Cell] -> Int -> [Char]
showLevelHelper elemlist width
    | elemlist == [] = []
    | otherwise = (showLine (take width elemlist)) ++ (showLevelHelper (drop width elemlist) width)

showLine :: [Cell] -> [Char]
showLine elemlist
    | elemlist == [] = [endl]
    | otherwise = (getChr (head elemlist)) : (showLine $ tail elemlist)

getChr :: Cell -> Char
getChr EmptySpace = emptySpace
getChr EmptyCell = emptyCell
getChr (StartCell West) = startLeft
getChr (StartCell North) = startUp
getChr (StartCell East) = startRight
getChr (StartCell South) = startDown
getChr (FinishCell West) = winLeft
getChr (FinishCell North) = winUp
getChr (FinishCell East) = winRight
getChr (FinishCell South) = winDown
getChr (SimpleCell West) = horPipe
getChr (SimpleCell North) = verPipe
getChr (CurvedCell West North) = botRight
getChr (CurvedCell North East) = botLeft
getChr (CurvedCell East South) = topLeft
getChr (CurvedCell South West) = topRight
getChr _ = endl

instance Show Level 
    where show = showLevel

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel pos = Level (A.array ((0, 0), pos) [(((quot i width), (mod i width)), EmptySpace) | i <- [0..m]])
    where 
        width = 1 + snd pos
        height = 1 + fst pos
        m = width * height - 1

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

createCell :: Char -> Cell
createCell cellCharacter
    | cellCharacter == startLeft = StartCell West
    | cellCharacter == startUp = StartCell North
    | cellCharacter == startRight = StartCell East
    | cellCharacter == startDown = StartCell South
    | cellCharacter == winLeft = FinishCell West
    | cellCharacter == winUp = FinishCell North
    | cellCharacter == winRight = FinishCell East
    | cellCharacter == winDown = FinishCell South
    | cellCharacter == horPipe = SimpleCell West
    | cellCharacter == verPipe = SimpleCell North
    | cellCharacter == botRight = CurvedCell West North
    | cellCharacter == botLeft = CurvedCell North East
    | cellCharacter == topLeft = CurvedCell East South
    | cellCharacter == topRight = CurvedCell South West
    | otherwise = EmptyCell

addCell :: (Char, Position) -> Level -> Level
addCell (cellChar, (x, y)) l@(Level arr)
    | (x < 0) || (y < 0) || (x > height) || (y > width) = l
    | arr A.! (x, y) /= EmptySpace = l
    | otherwise = (Level (arr A.// [((x, y), (createCell cellChar))]))
    where
        height = fst $ snd $ A.bounds arr
        width = snd $ snd $ A.bounds arr 

{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos lst = foldr addCell (emptyLevel pos) lst


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}
nextPos :: Position -> Directions -> Position
nextPos (x, y) direction
    | direction == West = (x, y - 1)
    | direction == North = (x - 1, y)
    | direction == East = (x, y + 1)
    | otherwise = (x + 1, y)

invalidCell :: Cell -> Bool
invalidCell (EmptySpace) = True
invalidCell (StartCell _) = True
invalidCell (FinishCell _) = True
invalidCell _ = False

invalidPos :: Position -> Int -> Int -> Bool
invalidPos (x, y) h w
    | x == -1 = True
    | x == h = True
    | y == -1 = True
    | y == w = True
    | otherwise = False

moveCell :: Position -> Directions -> Level -> Level
moveCell pos direction l@(Level arr)
    | invalidCell currentCell = l
    | invalidPos newPos height width = l
    | nextCell /= EmptySpace = l
    | otherwise = (Level (arr A.// [(newPos, currentCell), (pos, EmptySpace)]))
    where
        newPos = (nextPos pos direction)
        nextCell = arr A.! newPos
        height = 1 + (fst $ snd $ A.bounds arr)
        width = 1 + (snd $ snd $ A.bounds arr)
        currentCell = arr A.! pos

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}

getComplementary :: Directions -> Directions
getComplementary West = East
getComplementary East = West
getComplementary North = South
getComplementary South = North

getDir :: Cell -> Directions -> Maybe Directions
getDir (StartCell West) West = Just West
getDir (StartCell West) _ = Nothing
getDir (StartCell North) North = Just North
getDir (StartCell North) _ = Nothing
getDir (StartCell East) East = Just East
getDir (StartCell East) _ = Nothing
getDir (StartCell South) South = Just South
getDir (StartCell South) _ = Nothing
getDir (FinishCell West) West = Just West
getDir (FinishCell West) _ = Nothing
getDir (FinishCell North) North = Just North
getDir (FinishCell North) _ = Nothing
getDir (FinishCell East) East = Just East
getDir (FinishCell East) _ = Nothing
getDir (FinishCell South) South = Just South
getDir (FinishCell South) _ = Nothing
getDir (SimpleCell West) West = Just West
getDir (SimpleCell West) East = Just East
getDir (SimpleCell West) _ = Nothing
getDir (SimpleCell North) North = Just North
getDir (SimpleCell North) South = Just South
getDir (SimpleCell North) _ = Nothing
getDir (CurvedCell West North) West = Just West
getDir (CurvedCell West North) North = Just North
getDir (CurvedCell West North) _ = Nothing
getDir (CurvedCell North East) North = Just North
getDir (CurvedCell North East) East = Just East
getDir (CurvedCell North East) _ = Nothing
getDir (CurvedCell East South) East = Just East
getDir (CurvedCell East South) South = Just South
getDir (CurvedCell East South) _ = Nothing
getDir (CurvedCell South West) South = Just South
getDir (CurvedCell South West) West = Just West
getDir (CurvedCell South West) _ = Nothing
getDir _ _ = Nothing

connection :: Cell -> Cell -> Directions -> Bool
connection c1 c2 direction
    | (d1 /= Nothing) && (d2 /= Nothing) = True
    | otherwise = False
    where
        complementaryDirection = getComplementary direction
        d1 = getDir c1 direction
        d2 = getDir c2 complementaryDirection

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

isStartCell :: Cell -> Bool
isStartCell (StartCell _) = True
isStartCell _ = False

isFinishCell :: Cell -> Bool
isFinishCell (FinishCell _) = True
isFinishCell _ = False

getStartCellDir :: Cell -> Directions
getStartCellDir (StartCell West) = West
getStartCellDir (StartCell North) = North
getStartCellDir (StartCell East) = East
getStartCellDir (StartCell South) = South
getStartCellDir _ = West    -- redundant, dar evit warning de non-exhaustive pattern

findStart :: Level -> [Position] -> Position
findStart l@(Level arr) positions
    | positions == [] = (0, 0)
    | isStartCell (arr A.! pos) = pos
    | otherwise = findStart l $ tail positions
    where 
        pos = head positions

getCellSecondDirection :: Cell -> Directions -> Directions
getCellSecondDirection (SimpleCell _) direction = getComplementary direction
getCellSecondDirection (CurvedCell West North) West = North
getCellSecondDirection (CurvedCell West North) North = West
getCellSecondDirection (CurvedCell North East) North = East
getCellSecondDirection (CurvedCell North East) East = North
getCellSecondDirection (CurvedCell East South) East = South
getCellSecondDirection (CurvedCell East South) South = East
getCellSecondDirection (CurvedCell South West) South = West
getCellSecondDirection (CurvedCell South West) West = South
getCellSecondDirection _ _ = West    -- redundant, dar evit warning de non-exhaustive pattern


checkLevel :: Level -> Position -> Directions -> Bool
checkLevel l@(Level arr) pos direction
    | isFinishCell (arr A.! pos) = True
    | invalidPos nextP height width = False
    | otherwise =  (connection (arr A.! pos) (arr A.! nextP) direction)
        && (checkLevel l nextP (getCellSecondDirection (arr A.! nextP) (getComplementary direction)))
        where
            nextP = nextPos pos direction
            height = 1 + (fst $ snd $ A.bounds arr)
            width = 1 + (snd $ snd $ A.bounds arr)

wonLevel :: Level -> Bool
wonLevel l@(Level arr) = checkLevel l startPos $ getStartCellDir startCell
    where
        height = 1 + (fst $ snd $ A.bounds arr)
        width = 1 + (snd $ snd $ A.bounds arr)
        m = width * height - 1
        positions = [((quot i width), (mod i width)) | i <- [0..m]]
        startPos = findStart l positions
        startCell = arr A.! startPos

stateBuilder :: Level -> Position -> Directions -> [((Position, Directions), Level)]
stateBuilder l@(Level arr) pos direction
    | (invalidPos nextP height width) || ((arr A.! nextP) /= EmptySpace) = []
    | otherwise = [((pos, direction), moveCell pos direction l)]
        where
            nextP = nextPos pos direction
            height = 1 + (fst $ snd $ A.bounds arr)
            width = 1 + (snd $ snd $ A.bounds arr)

func :: Level -> Position -> [((Position, Directions), Level)]
func l@(Level arr) pos
    | invalidCell (arr A.! pos) = []
    | otherwise =  w ++ a ++ s ++ d
    where
        w = stateBuilder l pos North
        a = stateBuilder l pos West
        s = stateBuilder l pos South
        d = stateBuilder l pos East

-- level1Solved :: Level
-- level1Solved = createLevel (3,3) [
--     (startDown, (0,0)), (emptyCell, (0,1)), (emptyCell, (0,2)), (emptyCell, (0,3)),
--     (verPipe, (1,0)), (emptyCell, (1,1)), (emptyCell, (1,2)), (emptyCell, (1,3)),
--     (verPipe, (2,0)), (emptyCell, (2,2)), (emptyCell, (2,3)),
--     (botLeft, (3,0)), (horPipe, (3,1)), (horPipe, (3,2)), (winLeft, (3,3))
--     ]

-- level3 = createLevel (3,3) [
--     (startDown, (0,0)), (emptyCell, (0,1)), (emptyCell, (0,2)), (emptyCell, (0,3)),
--     (verPipe, (1,0)), (emptyCell, (1,1)), (emptyCell, (1,2)), (winDown, (1,3)),
--     (botLeft, (2,0)), (horPipe, (2,1)), (botRight, (2,2)), (emptyCell, (2,3)),
--     (horPipe, (3,3))
--     ]

instance ProblemState Level (Position, Directions) where
    successors l@(Level arr) = foldr (++) [] (map (func l) positions) --[(((0,0)South), l)]
        where
            height = 1 + (fst $ snd $ A.bounds arr)
            width = 1 + (snd $ snd $ A.bounds arr)
            m = width * height - 1
            positions = [((quot i width), (mod i width)) | i <- [0..m]]

    isGoal = wonLevel

    -- reverseAction :: (a, s) -> (a, s)
    reverseAction ((pos, dir), l) = ((nextP, compDir), moveCell pos compDir l)
        where
            compDir = getComplementary dir
            nextP = nextPos pos compDir
