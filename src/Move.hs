{-# LANGUAGE OverloadedStrings #-}
module Move where

import Web.Scotty
import Data.Aeson
import Data.Text.Lazy hiding (map, head, filter, zipWith, repeat, init)
import Data.List
import Data.Maybe

import Start

import Control.Monad.Trans

data Pos = Pos { posobject :: !String
               , x         :: Int
               , y         :: Int}
               deriving (Eq, Show)

instance Ord Pos where
    (Pos _ x y) `compare` (Pos _ x2 y2) = (x,y) `compare` (x2, y2)

instance FromJSON Pos where
  parseJSON (Object v) =
    Pos <$> v .: "object"
        <*> v .: "x"
        <*> v .: "y"

data Food = Food { foodinfo   :: [Pos],
                   foodobject :: !String}
                   deriving Show

instance FromJSON Food where
  parseJSON (Object v) =
      Food <$> (v .: "data" >>= mapM parseJSON)
           <*>  v .: "object"

data Snake = Snake { body        :: [Pos]
                   , health      :: Int
                   , id          :: !String
                   , name        :: !String
                   , snakeobject :: !String}
           --      , snaketaunt  :: !String } -- does not work properly
                   deriving (Eq, Show)

instance FromJSON Snake where
  parseJSON (Object v) =
    Snake <$> (v .: "body" >>= (.: "data") >>= mapM parseJSON)
          <*>  v .: "health"
          <*>  v .: "id"
          <*>  v .: "name"
          <*>  v .: "object"
       -- <*>  v .: "taunt" -- does not work properly

data GameState = GameState { food     :: Food
                           , height   :: Int
                           , width    :: Int
                           , turn     :: Int
                           , gid      :: Int
                           , gsobject :: !String
                           , snakes   :: [Snake]
                           , you      :: Snake}
                           deriving Show

instance FromJSON GameState where
  parseJSON (Object v) =
    GameState <$>  v .: "food"
              <*>  v .: "height"
              <*>  v .: "width"
              <*>  v .: "turn"
              <*>  v .: "id"
              <*>  v .: "object"
              <*> (v .: "snakes" >>= (.: "data") >>= mapM parseJSON)
              <*>  v .: "you"

data MoveResponse = MoveResponse { move :: Action }

instance ToJSON MoveResponse where
  toJSON (MoveResponse move) = object [ "move" .= move ]

data Action = U | D | L | R deriving Eq

instance ToJSON Action where
  toJSON U = String "up"
  toJSON D = String "down"
  toJSON L = String "left"
  toJSON R = String "right"

-- TODO: Remove and implement better handling of exceptions in jsonData
emptyGs = GameState (Food [] "") 0 0 0 0 "gsobj" [] (Snake [] 0 "" "" "")

postMove :: ActionM ()
postMove = do
  gs <- jsonData `rescue` (\ass -> do liftIO (print ass); return emptyGs;)
  -- liftIO $ putStrLn (show gs) -- Uncomment to print received GameState
  let m = compute_move gs
  Web.Scotty.json (MoveResponse m)

------------------------------------------------------------------------------
{- SNAKE LOGIC BELOW -}

type Position = (Int, Int) -- (x, y)
posToPosition (Pos _ x y) = (x,y)

-- | Input:  GameState
--   Output: List of coordinate occupied by apples
getfood :: GameState -> [Position]
getfood gs = (map (\p -> (x p, y p)) . foodinfo . food) gs

type Snek = (Position, [Position], Int, String) -- (head, body, health, id)

-- | Input:  GameState
--   Output: A Snek representing your snake
getyou :: GameState -> Snek
getyou gs = (posToPosition (head body), map posToPosition body, health, sid)
  where (Snake body health sid _ _) = you gs

-- | Input:  GameState
--   Output: A list of Snek, representing all other snakes (not including yours)
getsnakes :: GameState -> [Snek]
getsnakes gs = filter (/= getyou gs) $ map snaketosnek (snakes gs)
  where snaketosnek (Snake body health sid _ _ ) =
          (posToPosition (head body),map posToPosition body, health, sid)

compute_move :: GameState -> Action
compute_move gs@(GameState food height width turn gid gsobject snakes you) =
  case (bestMove head gs) of
    [] -> U
    xs -> safeStep xs
  where (head, body, health, sid) = getyou gs
        safeStep xs = correct (Prelude.head xs) gs

correct :: Action -> GameState -> Action
correct a gs =
  case move of
    True -> a -- safe
    False -> correct' a gs
  where move = checkMove a gs

correct' :: Action -> GameState -> Action
correct' a gs
  | a == U || a == D = if checkMove L gs then L else R
  | a == R || a == L = if checkMove U gs then U else R

bestMove :: Position -> GameState -> [Action]
bestMove (x,y) gs@(GameState (Food xs _) h w t _ _ snakes you) =
  map (\(Pos s x' y') -> moveToFoodR snek (x',y') (w,h)) food ++ [U]
  where food = sort xs
        snek = getyou gs
        dir  = direction snek
        (col, coldir) = undefined

moveToFoodR :: Snek -> Position -> Position-> Action
moveToFoodR s@((x,y), bod, _ , _)(fx,fy) (w, h)
  | x < fx = R -- && safe (x+1,y) = R
  | x > fx = L-- && safe (x-1,y) = L
  | y > fy = U -- && safe (x,y-1) = U
  | y < fy = D-- && safe (x,y+1) = D

-- if collision returns true
colWalls :: Snek -> Position -> (Bool, Action)
colWalls s@((x,y),((bx,by):rest),_,_) (w,h)
  | x < 0 = (True, L)
  | x > w = (True, R)
  | y > h = (True, D)
  | y < 0 = (True, U)
  | otherwise = (False, direction s)

checkMove :: Action -> GameState -> Bool
checkMove a gs@(GameState _ h w _ _ _ snakes you) = not col -- return True if is safe
  where (col, _) = colWalls (getyou gs) (w, h)

direction :: Snek -> Action
direction ((hx,hy), ((bx,by):rest),_,_)
  | hy < by   = U
  | hy > by   = D
  | hx < bx   = L
  | hx > bx   = R
