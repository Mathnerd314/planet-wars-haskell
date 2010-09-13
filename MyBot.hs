{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction #-}
import Data.Ord (comparing)
import Control.Applicative ((<$>))
import Data.List (intercalate, isPrefixOf, partition, foldl', maximumBy, minimumBy, partition, sortBy, delete)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mempty, mappend)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import System.IO

data SPlanet = SPlanet
    { sPlanetId         :: Int
    , sPlanetGrowthRate :: Int
    , sPlanetLocation   :: (Double, Double)
    , sGameInfo         :: GameInfo
    } deriving (Eq,Ord)

instance Show SPlanet where
   show s = "Planet" ++ show (sPlanetId s) ++ "," ++ show (sPlanetGrowthRate s) ++ "," ++ show (sPlanetLocation s) ++ ";"

data GameInfo = GameInfo
    { staticInfo :: IntMap SPlanet
    , distances  :: IntMap (IntMap Int)
    , sortByDist :: IntMap [SPlanet]
    } deriving (Show)

-- All GameInfos should be equal in our bot (play exactly one game)
instance Eq GameInfo where
    x == y = True

instance Ord GameInfo where
    compare x y = EQ

-- | Precalculate useful (static) game info
makeGameInfo :: [SPlanet] -> GameInfo
makeGameInfo list = let gI = GameInfo {
      staticInfo = IM.fromList . zip [0..] . map (\p -> p{sGameInfo = gI}) $ list
    , distances  = IM.fromList . zip [0..] . map (IM.fromList . zip [0..]) $ map (\p1 -> (map (distanceBetween p1) list)) list
    , sortByDist = IM.fromList . zip [0..] . map (\p -> sortBy (comparing $ distanceBetween p) (delete p list)) $ list
    } in gI where
        distanceBetween p1 p2 = let 
                                    (x1,y1) = sPlanetLocation p1
                                    (x2,y2) = sPlanetLocation p2
                                    dx = x1 - x2
                                    dy = y1 - y2
                                in ceiling . sqrt $ dx * dx + dy * dy

-- | Parse first game state into useful info
parseInfo :: [String] -> GameInfo
parseInfo ss = makeGameInfo . reverse $ foldl' parseInitGameInfo [] ss where
    parseInitGameInfo x string = case words string of
        ("P" : xLoc : yLoc : owner : ships : growth : _) ->
            let sPlanet=SPlanet planetId'
                                (read growth)
                                (read xLoc,read yLoc)
                                undefined
            in sPlanet : x
        _ -> x
      where
        planetId' = length x

data Planet = Planet
    { planetOwner      :: Int
    , planetShips      :: Int
    , planetInfo       :: SPlanet
    } deriving (Show,Eq,Ord)

data Fleet = Fleet
    { fleetOwner          :: Int
    , fleetShips          :: Int
    , fleetDestination    :: Int
    , fleetTurnsRemaining :: Int
    } deriving (Show,Eq,Ord)

data GameState = GameState
    { gameStatePlanets :: IntMap Planet
    , gameStateFleets  :: [Fleet]
    , gameInfo         :: GameInfo
    } deriving (Show)

buildInitState :: [String] -> GameState
buildInitState ss = GameState planets fleets info where
    info = parseInfo ss
    (planets,fleets) = foldl' parseState (mempty,[]) ss where
        parseState (planets,fleets) string = case words string of
            ("P" : xLoc : yLoc : owner : ships : growth : _) ->
                let planet = Planet (read owner)
                                    (read ships)
                                    (staticInfo info IM.! planetId')
                in (IM.insert planetId' planet planets,fleets)
            ("F" : owner : ships : source : destination : total_turns : remaining_turns : _) ->
                let fleet = Fleet (read owner)
                                  (read ships)
                                  (read destination)
                                  (read remaining_turns)
                in (planets, fleet : fleets)
            _ -> (planets,fleets)
          where
            planetId' = IM.size $ planets

buildNewState :: GameState -> [String] -> GameState
buildNewState (GameState _ _ info) ss = GameState planets fleets info where
    (planets,fleets) = foldl' parseState (mempty,[]) ss where
        parseState (planets,fleets) string = case words string of
            ("P" : xLoc : yLoc : owner : ships : growth : _) ->
                let planet = Planet (read owner)
                                    (read ships)
                                    (staticInfo info IM.! planetId')
                in (IM.insert planetId' planet planets,fleets)
            ("F" : owner : ships : source : destination : total_turns : remaining_turns : _) ->
                let fleet = Fleet (read owner)
                                  (read ships)
                                  (read destination)
                                  (read remaining_turns)
                in (planets, fleet : fleets)
            _ -> (planets,fleets)
          where
            planetId' = IM.size $ planets

planetId = sPlanetId . planetInfo
planetGrowthRate = sPlanetGrowthRate . planetInfo
planetLocation = sPlanetLocation . planetInfo

distanceBetween p1 p2 = (distances . sGameInfo $ planetInfo p1) IM.! (planetId p1) IM.! (planetId p2)

-- | Representation of an order
--
data Order = Order
    { orderSource      :: Int
    , orderDestination :: Int
    , orderShips       :: Int
    } deriving (Show)

-- | Class for values that are owned by a player
class Resource a where
    owner :: a -> Int

instance Resource Planet where
    owner = planetOwner

instance Resource Fleet where
    owner = fleetOwner

isAllied = (== 1) . owner
isHostile = (>= 2) . owner
isNeutral = (<= 0) . owner

addShips planet n = planet {planetShips = planetShips planet + n}

-- | Attack the given planet with the given fleet (or reinforce it, when the
-- planet is allied to the fleet)
engage planet fleet
    -- Reinforce
    | owner planet == owner fleet = addShips planet $ fleetShips fleet
    -- Conquer planet
    | shipsAfterAttack < 0 =
        planet {planetShips = -shipsAfterAttack, planetOwner = owner fleet}
    -- Attack failed
    | otherwise = planet {planetShips = shipsAfterAttack}
  where
    shipsAfterAttack = planetShips planet - fleetShips fleet

-- | Apply all fleets in the list to all planets
engageAll :: IntMap Planet -> [Fleet] -> IntMap Planet
engageAll planets fleets = foldl engage' planets fleets
  where
    engage' planets' fleet = IM.update (return . flip engage fleet)
                                       (fleetDestination fleet)
                                       planets'

-- | Check if a fleet has arrived
isArrived = (== 0) . fleetTurnsRemaining

-- | List of Planets from a game state.
planets state = map snd $ IM.toList $ gameStatePlanets state

-- | Calculate the production (number of new ships in the next turn) of both
-- players.
production :: GameState  -- ^ Game state to analyze
           -> (Int, Int) -- ^ Pair having the player and enemy's production
production g = foldl' prod (0,0) (planets g)
  where 
    prod (x,y) p = case planetOwner p of
      0 -> (x,y)
      1 -> (x + planetGrowthRate p, y)
      2 -> (x, y + planetGrowthRate p)

-- | Get a planet by ID.
--
planetById :: GameState -> Int -> Maybe Planet
planetById state id' = IM.lookup id' $ gameStatePlanets state

-- | Step the game state for one turn
step state = state
    { gameStatePlanets = IM.map grow $ engageAll (gameStatePlanets state) ready
    , gameStateFleets = fleets'
    }
  where
    (ready, fleets') =
        partition isArrived $ map stepFleet $ gameStateFleets state
    stepFleet fleet = fleet
        { fleetTurnsRemaining = fleetTurnsRemaining fleet - 1
        }
    grow planet | isNeutral planet = planet
                | otherwise = addShips planet (planetGrowthRate planet)

-- | Execute an order
issueOrder (Order source destination ships) =
    putStrLn $ intercalate " " $ map show [source, destination, ships]

-- | Finish your turn
finishTurn :: IO ()   -- ^ Result
finishTurn = do
    putStrLn "go"
    hFlush stdout

-- | Run a deterministic bot
bot f = ioBot $ mapM_ issueOrder . f

readGame :: [String] -> Handle -> IO [String]
readGame state handle = do
        line <- takeWhile (/= '#') <$> hGetLine handle
        if "go" `isPrefixOf` line
            -- Go Go Go!
            then return (reverse state)
            -- Keep building map
            else readGame (line : state) handle

-- | Run an IO bot. This is a more liberal version of 'bot', which allows you to
-- work in the IO monad. However, you need to call 'issueOrder' yourself if you
-- use this function -- 'finishTurn' will still be called automatically.
ioBot :: (GameState -> IO ()) -> IO ()
ioBot f = do
    hSetBuffering stdin NoBuffering
    initGame <- readGame [] stdin
    let initState = buildInitState initGame
    loop initState
  where
    loop state = do
        f state
        finishTurn
        newState <- readGame [] stdin
        loop (buildNewState state newState)

-- | Read a game state from file. The format is the same as the server's output
-- for a turn. Useful when debugging.
stateFromFile path = withFile path ReadMode (fmap buildInitState . readGame [])

-- | Checks if a planet will survive the incoming fleets. A planet survives if
-- its owner is still the same after all known fleets arrive.
willSurviveAttack state pid = survives state
  where
    originalOwner = currentOwner state pid
    survives s = if null $ incomingFleets s pid
      then currentOwner s pid == originalOwner
      else survives $ step s

-- | The owner of a planet in a given game state.
currentOwner state pid = owner $ gameStatePlanets state IM.! pid

-- | List of planets under attack, i.e., that have incoming fleets.
planetsUnderAttack state = map (planetById state) . unique . map fleetDestination $ gameStateFleets state

-- | List of incoming fleets for a given planet in a certain game state.
incomingFleets state pid = filter pidMatches fleets
  where
    pidMatches = (== pid) . fleetDestination
    fleets = gameStateFleets state

-- | Removes duplicates from a list of Ints
unique = IS.toList . IS.fromList

doTurn state = if null myFleets
                  && (not . null) myPlanets
                  && (not . null) notMyPlanets
    -- Simple ai
    then [Order (planetId strongest) (planetId weakest) ships]
    -- If we have a fleet in flight, just do nothing
    else []
  where
    myFleets = filter isAllied $ gameStateFleets state
    -- Partition all planets
    (myPlanets, notMyPlanets) = partition isAllied $ planets state

    -- Find our strongest planet and the weakest neutral/hostile planet
    strongest = maximumBy (comparing planetShips) myPlanets
    weakest = minimumBy (comparing planetShips) notMyPlanets

    -- Select half of the ships
    ships = planetShips strongest `div` 2

main = bot doTurn