{-# LANGUAGE DeriveGeneric #-}

module Challenges (TileEdge(..),Tile(..),Puzzle,isPuzzleComplete, Rotation(..),solveCircuit, LExpr(..),Bind(..),prettyPrint,parseLetx, LamExpr(..),letEnc,compareRedn) where

-- Standard Library Imports
import Data.Maybe
import Data.Ix (range)
import Data.Set (Set)

-- Qualified Imports
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Project-specific Imports
import Parsing

-- Challenge 1: Testing Circuits
data TileEdge = North | East | South | West deriving (Eq, Ord, Show, Read)
data Tile = Source [TileEdge] | Sink [TileEdge] | Wire [TileEdge] deriving (Eq, Show, Read)
type Puzzle = [[Tile]]

-- Check if a position is within the bounds of the puzzle grid
validGrid :: (Int, Int) -> Puzzle -> Bool
validGrid (xFocus, yFocus) puzzle =
  let xs = length puzzle
      ys = length (head puzzle)
  in xFocus >= 0 && xFocus < xs && yFocus >= 0 && yFocus < ys

-- Check if a tile is a source
sourceState :: Tile -> Bool
sourceState (Source _) = True
sourceState _          = False

-- Check if a tile is a sink
sinkState :: Tile -> Bool
sinkState (Sink _)     = True
sinkState _            = False

-- Retrieve the edges of a given tile
checkStateEdges :: Tile -> [TileEdge]
checkStateEdges (Source stateEdges) = stateEdges
checkStateEdges (Sink stateEdges)   = stateEdges
checkStateEdges (Wire stateEdges)   = stateEdges

-- Convert tile edges to a set for easier validation
checkStateEdgesSet :: Tile -> Set TileEdge
checkStateEdgesSet = Set.fromList . checkStateEdges

-- Validate the presence of a specific edge in a tile
validStateEdgesSet :: TileEdge -> Tile -> Bool
validStateEdgesSet changingStateEdge = Set.member changingStateEdge . checkStateEdgesSet

-- Get the tile at a specified position, with default handling for invalid indices
lookupState :: Puzzle -> (Int, Int) -> Tile
lookupState puzzle (xFocus, yFocus)
  | xFocus >= 0 && xFocus < length puzzle && yFocus >= 0 && yFocus < length (head puzzle) = puzzle !! xFocus !! yFocus
  | otherwise = Wire []  -- Default Tile for invalid indices

-- Function to check if a puzzle circuit is complete
-- Checks for presence of terminal types, validates individual tiles, and ensures connectivity from sources to sinks
isPuzzleComplete :: Puzzle -> Bool
isPuzzleComplete puzzle = 
    checkStateTerminalType puzzle && 
    all (uncurry (checkStateCons puzzle)) puzzleGrid && 
    any (checkReachable puzzle) sourcePositions
  where
    xs = length puzzle
    ys = length (head puzzle)
    puzzleGrid = range ((0, 0), (xs - 1, ys - 1))  -- Grid coordinates
    sourcePositions = getSourcePositions puzzle  -- Starting points for search

-- Verify the puzzle contains both sources and sinks
checkStateTerminalType :: Puzzle -> Bool
checkStateTerminalType puzzle = any sourceState flatPuzzle && any sinkState flatPuzzle
  where
    flatPuzzle = concat puzzle  -- Flatten the puzzle for easier processing

-- Check if any source is connected to a sink
checkReachable :: Puzzle -> (Int, Int) -> Bool
checkReachable puzzle sourcePos = breadthFirst puzzle [sourcePos] Set.empty

-- BFS Algorithm to check if any source is connected to a sink
breadthFirst :: Puzzle -> [(Int, Int)] -> Set.Set (Int, Int) -> Bool
breadthFirst _ [] _ = False
breadthFirst puzzle (focusState:queue) visited
  | Set.member focusState visited = breadthFirst puzzle queue visited  -- Skip already visited states
  | sinkState (lookupState puzzle focusState) = True  -- Success if sink is reached
  | otherwise = breadthFirst puzzle (queue ++ nextPositions) (Set.insert focusState visited)  -- Continue with unvisited states
  where
    nextPositions = subseqTraversals puzzle focusState visited

-- Generate subsequent state positions to visit from a given state
subseqTraversals :: Puzzle -> (Int, Int) -> Set.Set (Int, Int) -> [(Int, Int)]
subseqTraversals puzzle (xFocus, yFocus) visited =
    concatMap (filter (`Set.notMember` visited) . subseqPos puzzle (xFocus, yFocus)) $ 
    checkStateEdges (lookupState puzzle (xFocus, yFocus))

-- Calculate the next position based on the current tile's edge
subseqPos :: Puzzle -> (Int, Int) -> TileEdge -> [(Int, Int)]
subseqPos puzzle (xFocus, yFocus) changingStateEdge
  | Just ((xDelta, yDelta), _) <- Map.lookup changingStateEdge stateEdgeDirMap = 
    [(xFocus + xDelta, yFocus + yDelta)]  -- Adjust position based on edge
  | otherwise = []

-- Retrieve positions of all source tiles in the puzzle
getSourcePositions :: Puzzle -> [(Int, Int)]
getSourcePositions puzzle = 
    [(xFocus, yFocus) | (xFocus, tiles) <- zip [0..] puzzle, 
                               (yFocus, state) <- zip [0..] tiles, 
                               sourceState state]

-- Validate if an individual tile is correctly connected at its position
checkStateCons :: Puzzle -> Int -> Int -> Bool
checkStateCons puzzle xFocus yFocus = 
    all (checkEdgeCons puzzle (xFocus, yFocus)) 
    (checkStateEdges $ lookupState puzzle (xFocus, yFocus))

-- Check if each edge of a tile is properly connected to its neighboring tile
checkEdgeCons :: Puzzle -> (Int, Int) -> TileEdge -> Bool
checkEdgeCons puzzle (xFocus, yFocus) changingStateEdge =
  let Just ((xDelta, yDelta), changingStateEdge') = Map.lookup changingStateEdge stateEdgeDirMap
      position' = (xFocus + xDelta, yFocus + yDelta)
  in validGrid position' puzzle && validStateEdgesSet changingStateEdge' (lookupState puzzle position')

-- Mapping of tile edges to their corresponding deltas and opposite edges
stateEdgeDirMap :: Map.Map TileEdge ((Int, Int), TileEdge)
stateEdgeDirMap = Map.fromList [
    (North, ((-1, 0), South)),
    (South, ((1, 0), North)),
    (East, ((0, 1), West)),
    (West, ((0, -1), East))
  ]

-- Challenge 2: Solving Circuit Puzzles
data Rotation = R0 | R90 | R180 | R270
  deriving (Eq, Show, Read)

-- Solves the circuit puzzle and returns the rotations of each tile
solveCircuit :: Puzzle -> Maybe [[Rotation]]
solveCircuit puzzle
  -- Check if initial puzzle state is solvable
  | not $ firstGlanceAttempt (length puzzle, length $ head puzzle) (0, 0) puzzle = Nothing
  -- Return Nothing if no solution found
  | null listSolutions = Nothing
  -- Return the first valid solution
  | otherwise = Just $ head listSolutions
  where
    gridSize = getPuzzleDimensions puzzle
    initialGridState = initialDegrees gridSize
    potentialChanges = stateChanges $ checkStateEdges $ lookupState puzzle (0, 0)
    listStates = (puzzle, initialGridState) : [applyStateChange puzzle initialGridState (0, 0) rot | rot <- potentialChanges]
    listSolutions = concatMap (uncurry (recursiveAttempt gridSize (0, 0))) listStates

-- Retrieve dimensions of the puzzle grid
getPuzzleDimensions :: Puzzle -> (Int,Int)
getPuzzleDimensions ((_:xFocus):puzzle) = (length puzzle, length xFocus)
getPuzzleDimensions _ = error "Invalid"

-- Initialize a rotation matrix with all tiles set to R0
initialDegrees :: (Int,Int) -> [[Rotation]]
initialDegrees (xFocus,yFocus) = [[R0 |_ <- [0..yFocus] ] | _<- [0..xFocus]]

-- Determine if the initial puzzle state can lead to a solution
firstGlanceAttempt :: (Int, Int) -> (Int, Int) -> Puzzle -> Bool
firstGlanceAttempt gridSize focusState puzzle
  | isJust $ subseqState gridSize focusState = firstGlanceAttempt gridSize (fromJust $ subseqState gridSize focusState) puzzle
  | otherwise = case state of
      Sink [] -> False
      Source [] -> False
      _ -> not stateWires || length stateEdges /= 1
  where
    state = lookupState puzzle focusState
    stateEdges = checkStateEdges state
    stateWires = not (sinkState state || sourceState state)

-- Recursive function to attempt solving the puzzle
recursiveAttempt :: (Int,Int) -> (Int,Int) -> Puzzle -> [[Rotation]] -> [[[Rotation]]]
recursiveAttempt gridSize focusState puzzle currentState
  -- Return solution if the last state is reached
  | checkFinalState gridSize focusState = checkFinalGrid puzzle currentState
  -- Skip this path if current state doesn't match
  | not $ checkStateNeighbours puzzle focusState gridSize = []
  -- Continue with the next state in the solution path
  | otherwise = get1stVal gridSize subsequentState puzzle currentState
  where
    Just subsequentState = subseqState gridSize focusState

-- Check if the current puzzle state is solved
checkFinalGrid :: Puzzle -> [[Rotation]] -> [[[Rotation]]]
checkFinalGrid puzzle currentState
  | isPuzzleComplete puzzle = [currentState]
  | otherwise = []

-- Get the first valid solution from the list of potential solutions
get1stVal :: (Int, Int) -> (Int, Int) -> Puzzle -> [[Rotation]] -> [[[Rotation]]]
get1stVal gridSize subsequentState puzzle currentState = take 1 $ concatMap (uncurry (recursiveAttempt gridSize subsequentState)) listStates
  where
    potentialChanges = stateChanges (checkStateEdges $ lookupState puzzle subsequentState)
    listStates = (puzzle, currentState) : map (applyStateChange puzzle currentState subsequentState) potentialChanges

-- Check if the current tile is the last tile in the grid
checkFinalState :: (Int, Int) -> (Int, Int) -> Bool
checkFinalState gridSize focusState = null $ subseqState gridSize focusState

-- Check if the current tile's rotation matches with its neighbors
checkStateNeighbours :: Puzzle -> (Int, Int) -> (Int, Int) -> Bool
checkStateNeighbours = checkStateAlignment

-- Calculate the next tile to consider in the puzzle grid
subseqState :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
subseqState (xLimit, yLimit) (xFocus, yFocus)
  | xFocus < xLimit || (xFocus == xLimit && yFocus < yLimit) = Just (xSubseq, ySubseq)
  | otherwise = Nothing
  where
    xSubseq = if yFocus == yLimit then xFocus + 1 else xFocus
    ySubseq = if yFocus == yLimit then 0 else yFocus + 1

-- Apply a rotation to a tile in the puzzle
applyStateChange :: Puzzle -> [[Rotation]] -> (Int, Int) -> Rotation -> (Puzzle, [[Rotation]])
applyStateChange puzzle currentState (xFocus, yFocus) changeApplied =
    (nextGridState puzzle xFocus yFocus $ stateEdgeChange changeApplied focusState,
     nextGridState currentState xFocus yFocus changeApplied)
  where
    focusState = lookupState puzzle (xFocus, yFocus)
    nextGridState focusGridState xGrid yGrid updated =
        take xGrid focusGridState ++ [xUpdate (focusGridState !! xGrid) yGrid updated] ++ drop (xGrid + 1) focusGridState
    xUpdate xFocus' yGrid updated = take yGrid xFocus' ++ [updated] ++ drop (yGrid + 1) xFocus'

-- Determine the effective rotations of a tile based on its edges
stateChanges :: [TileEdge] -> [Rotation]
stateChanges stateEdges = case stateEdges of
    [] -> []
    [_] -> potentialStateChanges
    [firstStateEdge, secondStateEdge] | secondStateEdge == stateEdgeChangeApplied R180 firstStateEdge -> [R90] -- Line Wire
             | otherwise -> potentialStateChanges -- Curved Wire
    _ -> [] -- Full Edges or more than two stateEdges
  where
    potentialStateChanges = [R90, R180, R270]

-- Apply rotation change to a tile's edges
stateEdgeChange :: Rotation -> Tile -> Tile
stateEdgeChange changeApplied state = case state of
    Source stateEdges -> Source $ rotateEdges stateEdges
    Sink stateEdges   -> Sink $ rotateEdges stateEdges
    Wire stateEdges   -> Wire $ rotateEdges stateEdges
  where
    rotateEdges = map (stateEdgeChangeApplied changeApplied)

-- Map rotation degrees to edges
stateEdgeChangeApplied :: Rotation -> TileEdge -> TileEdge
stateEdgeChangeApplied changeApplied changingStateEdge = fromMaybe changingStateEdge $ lookup (changeApplied, changingStateEdge) rotationMap

rotationMap :: [((Rotation, TileEdge), TileEdge)]
rotationMap =
    [ ((R0, stateEdge), stateEdge) | stateEdge <- edgePairing ] ++
    [ ((R90, North), East), ((R90, East), South), ((R90, South), West), ((R90, West), North) ] ++
    [ ((R180, North), South), ((R180, East), West), ((R180, South), North), ((R180, West), East) ] ++
    [ ((R270, North), West), ((R270, East), North), ((R270, South), East), ((R270, West), South) ]
  where
    edgePairing = [North, East, South, West]

-- Verify the alignment of a tile with its neighboring tiles
checkStateAlignment :: Puzzle -> (Int, Int) -> (Int, Int) -> Bool
checkStateAlignment puzzle (xFocus, yFocus) (xLimit, yLimit) = verifyLeft && verifyUp && verifyGridConstraints
  where
    focusState = puzzle !! xFocus !! yFocus
    adjacentLeft = if yFocus > 0 then Just (puzzle !! xFocus !! (yFocus - 1)) else Nothing
    adjacentUp = if xFocus > 0 then Just (puzzle !! (xFocus - 1) !! yFocus) else Nothing

    verifyLeft = case adjacentLeft of
                  Just state -> validStateEdgesSet West focusState == validStateEdgesSet East state
                  Nothing -> not $ validStateEdgesSet West focusState
    verifyUp = case adjacentUp of
                  Just state -> validStateEdgesSet North focusState == validStateEdgesSet South state
                  Nothing -> not $ validStateEdgesSet North focusState
    verifyGridConstraints = (xFocus < xLimit || not (validStateEdgesSet South focusState)) &&
                      (yFocus < yLimit || not (validStateEdgesSet East focusState))

-- Challenge 3: Pretty Printing Let Expressions
data LExpr = Var Int | App LExpr LExpr | Let Bind LExpr LExpr | Pair LExpr LExpr | Fst LExpr | Snd LExpr | Abs Bind LExpr
  deriving (Eq, Show, Read)
data Bind = Discard | V Int
  deriving (Eq, Show, Read)

-- Primary function for pretty-printing lambda expressions.
prettyPrint :: LExpr -> String
prettyPrint (Var x) = "x" ++ show x
prettyPrint (App e1 e2) = concat [lApp e1, " ", rApp e2]
prettyPrint (Let b e1 e2) = concat ["let ", strBind b, " = ", prettyPrint e1, " in ", prettyPrint e2]
prettyPrint (Pair e1 e2) = concat ["(", prettyPrint e1, ", ", prettyPrint e2, ")"]
prettyPrint (Fst e) = "fst " ++ expectedPair e
prettyPrint (Snd e) = "snd " ++ expectedPair e
prettyPrint (Abs b e) = "\\" ++ strBind b ++ nestedLambda e

-- Helper function to handle left side of application expressions.
lApp :: LExpr -> String
lApp e@(Abs _ _) = "(" ++ prettyPrint e ++ ")"
lApp e = prettyPrint e

-- Helper function to handle right side of application expressions.
rApp :: LExpr -> String
rApp e@(Var _) = prettyPrint e
rApp e@(Abs _ _) = prettyPrint e
rApp e@(Pair _ _) = prettyPrint e -- Handle pairs without extra parentheses
rApp e = "(" ++ prettyPrint e ++ ")"

-- Helper function for formatting nested lambda abstractions.
nestedLambda :: LExpr -> String
nestedLambda (Abs b e) = " " ++ strBind b ++ nestedLambda e
nestedLambda e = " -> " ++ prettyPrint e

-- Helper function for pretty-printing expected pairs.
expectedPair :: LExpr -> String
expectedPair e@(Pair _ _) = prettyPrint e
expectedPair e = "(" ++ prettyPrint e ++ ")"

-- Helper function for converting bindings to strings.
-- Represents discarded variables as "_" and variable bindings as "xN".
strBind :: Bind -> String
strBind Discard = "_"
strBind (V x)   = 'x' : show x

-- Challenge 4: Parsing Let_x Expressions
-- Main parsing function that parses a given string into a LExpr data structure.
-- Returns Nothing if parsing fails.
parseLetx :: String -> Maybe LExpr
parseLetx inputString = case parse exprE inputString of
  [(parsedExpr, "")] -> Just parsedExpr
  _ -> Nothing

-- Primary parser for LExpr, combining different expression parsers.
exprE :: Parser LExpr
exprE = letE <|> lambdaE <|> pairE <|> fstE <|> sndE <|> appE

-- Parser for application expressions, combines multiple sub-expressions.
appE :: Parser LExpr
appE = foldl1 App <$> some subE

-- Parser for variable expressions.
varE :: Parser LExpr
varE = Var <$> (space *> char 'x' *> nat <* space)

-- Parser for lambda expressions.
lambdaE :: Parser LExpr
lambdaE = do
  char '\\'
  lambdaBindings <- some bindingP <* space <* string "->" <* space
  lambdaBody <- exprE
  return $ foldr Abs lambdaBody lambdaBindings

-- Parser for let expressions.
letE :: Parser LExpr
letE = do
  string "let" *> space
  bindings <- some bindingP <* space
  char '=' *> space
  letBody <- exprE <* space
  string "in" *> space
  inExpression <- exprE <* space
  return $ case bindings of
    [singleBinding] -> Let singleBinding letBody inExpression
    (firstBinding:restBindings) -> Let firstBinding (foldr Abs letBody restBindings) inExpression

-- Parser for sub-expressions.
subE :: Parser LExpr
subE = parensE <|> varE <|> letE <|> lambdaE <|> pairE <|> fstE <|> sndE

-- Parser for expressions within parentheses.
parensE :: Parser LExpr
parensE = char '(' *> space *> exprE <* space <* char ')' <* space

-- Parser for pair expressions.
pairE :: Parser LExpr
pairE = Pair <$> (char '(' *> space *> exprE <* space <* char ',' <* space) <*> (exprE <* space <* char ')')

-- Parser for first element expressions.
fstE :: Parser LExpr
fstE = Fst <$> (string "fst" *> space *> exprE <* space)

-- Parser for second element expressions.
sndE :: Parser LExpr
sndE = Snd <$> (string "snd" *> space *> exprE <* space)

-- Parser for bindings.
bindingP :: Parser Bind
bindingP = bindingVar <|> bindingDisc

-- Parser for variable bindings.
bindingVar :: Parser Bind
bindingVar = V <$> (space *> char 'x' *> int <* space)

-- Parser for discard bindings.
bindingDisc :: Parser Bind
bindingDisc = Discard <$ (space *> char '_' <* space)

-- Challenge 5: Let Encoding in Lambda
data LamExpr = LamVar Int | LamApp LamExpr LamExpr | LamAbs Int LamExpr
  deriving (Eq, Show, Read)

letEnc :: LExpr -> LamExpr
letEnc (Var n) = LamVar n
letEnc (App e1 e2) = LamApp (letEnc e1) (letEnc e2)
letEnc (Let bind e1 e2) = LamApp (LamAbs (bindToInt bind) (letEnc e2)) (letEnc e1)
letEnc (Pair e1 e2) = LamAbs 0 $ LamApp (LamApp (LamVar 0) (letEnc e1)) (letEnc e2)
letEnc (Fst e) = LamApp (letEnc e) (LamAbs 0 $ LamAbs 1 $ LamVar 0)
letEnc (Snd e) = LamApp (letEnc e) (LamAbs 0 $ LamAbs 1 $ LamVar 1)
letEnc (Abs bind e) = LamAbs (bindToInt bind) (letEnc e)

bindToInt :: Bind -> Int
bindToInt Discard = 0
bindToInt (V n) = n

-- Challenge 6: Compare Innermost Reduction for Let_x and its Lambda Encoding

------------
-- LAMBDA --
------------
free :: Int -> LamExpr -> Bool
free x (LamVar y) = x == y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)

rename :: Int -> LamExpr -> Int
rename x e
  | free (x + 1) e = rename (x + 1) e
  | otherwise = x + 1

subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e | x /= y && not (free x e) = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e | x /= y && (free x e) = let x' = (rename x e1) in subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e | x == y = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)

isLamValue :: LamExpr -> Bool
isLamValue (LamVar _) = True
isLamValue (LamAbs _ _) = True
isLamValue _ = False

-- CALL BY VALUE --
cbvlam1 :: LamExpr -> Maybe LamExpr
-- Contexts
cbvlam1 (LamApp e1 e2) | not (isLamValue e1) =
  do
    e' <- cbvlam1 e1
    return (LamApp e' e2)
cbvlam1 (LamApp e1 e2) | not (isLamValue e2) =
  do
    e' <- cbvlam1 e2
    return (LamApp e1 e')
-- Reductions
cbvlam1 (LamApp (LamAbs x e1) e) | isLamValue e = Just (subst e1 x e)
-- Otherwise terminated or blocked
cbvlam1 _ = Nothing

-- CALL BY NAME --
cbnlam1 :: LamExpr -> Maybe LamExpr
-- Reductions
cbnlam1 (LamApp (LamAbs x e1) e) = Just (subst e1 x e)
-- Contexts
cbnlam1 (LamApp e1 e2) =
  do
    e' <- cbnlam1 e1
    return (LamApp e' e2)
-- Otherwise terminated or blocked
cbnlam1 _ = Nothing

---------
-- LET --
---------
-- Call-by-value reduction for Let_x expressions.
-- Evaluates expressions based on value of subexpressions.
cbvlet1 :: LExpr -> Maybe LExpr
cbvlet1 (App e1 e2)
    | not (isLet e1) = fmap (`App` e2) (cbvlet1 e1)
    | not (isLet e2) = fmap (App e1) (cbvlet1 e2)
    | otherwise = Just (App e1 e2)
cbvlet1 (Let b e1 e2)
    | not (isLet e1) = fmap (\e1' -> Let b e1' e2) (cbvlet1 e1)
    | otherwise = Just (substLet e2 b e1)
cbvlet1 (Fst (Pair e1 e2)) = Just e1
cbvlet1 (Snd (Pair e1 e2)) = Just e2
cbvlet1 (Abs b e) = fmap (Abs b) (cbvlet1 e)
cbvlet1 _ = Nothing

-- Call-by-name reduction for Let_x expressions.
-- Delays evaluation of expressions until necessary.
cbnlet1 :: LExpr -> Maybe LExpr
cbnlet1 (App (Abs b e1) e2) = Just (substLet e1 b e2)
cbnlet1 (App e1 e2) = fmap (`App` e2) (cbnlet1 e1)
cbnlet1 (Let b e1 e2) = Just (substLet e2 b e1)
cbnlet1 (Fst (Pair e1 _)) = Just e1
cbnlet1 (Snd (Pair _ e2)) = Just e2
cbnlet1 (Abs b e) = fmap (Abs b) (cbnlet1 e)
cbnlet1 _ = Nothing

-- Helper function to check if an expression is a value in Let_x.
isLet :: LExpr -> Bool
isLet (Var _) = True
isLet (Abs _ _) = True
isLet _ = False

-- Substitution function for Let_x expressions.
-- Replaces bindings in an expression with the given value.
substLet :: LExpr -> Bind -> LExpr -> LExpr
substLet (Var n) (V x) val | n == x = val
substLet (Var n) _ _ = Var n
substLet (App e1 e2) b val = App (substLet e1 b val) (substLet e2 b val)
substLet (Let b e1 e2) b' val | b /= b' = Let b (substLet e1 b' val) (substLet e2 b' val)
substLet (Pair e1 e2) b val = Pair (substLet e1 b val) (substLet e2 b val)
substLet (Fst e) b val = Fst (substLet e b val)
substLet (Snd e) b val = Snd (substLet e b val)
substLet (Abs b e) b' val | b /= b' = Abs b (substLet e b' val)
substLet e _ _ = e  -- Other cases

-- Compares the number of reduction steps for different strategies.
-- Returns a tuple with the number of steps for each strategy.
compareRedn :: LExpr -> Int -> (Int, Int, Int, Int)
compareRedn e maxSteps = 
    (countRedn cbvlet1 e maxSteps, countRedn cbvlam1 (letEnc e) maxSteps, 
     countRedn cbnlet1 e maxSteps, countRedn cbnlam1 (letEnc e) maxSteps)

-- Helper function to count the number of reduction steps for a strategy.
countRedn :: (a -> Maybe a) -> a -> Int -> Int
countRedn redn e maxSteps = go e 0
  where
    go expr steps | steps >= maxSteps = steps
                  | Just expr' <- redn expr = go expr' (steps + 1)
                  | otherwise = steps