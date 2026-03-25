module Game.Types (
  Tile(..),
  Item(..),
  Enemy(..),
  PlayerStats(..),
  Game(..),
  Arrow(..),
  KeyEvent(..),
  TurnOutcome(..),
  PauseLayer(..),
  AppState(..),
  Pos,
  Grid
) where

import Data.Map.Strict (Map)
import System.Random (StdGen)

type Pos = (Int, Int)
type Grid = [[Tile]]

data Tile
  = Wall
  | Floor
  | Lake
  | Chest
  deriving (Eq, Show)

data Item
  = Weapon String Int
  | Armor String Int
  | Potion String Int
  deriving (Eq, Show)

data Enemy = Enemy
  { eHp :: !Int,
    eMaxHp :: !Int,
    eDmg :: !Int
  }
  deriving (Eq, Show)

data PlayerStats = PlayerStats
  { psHp :: !Int,
    psMaxHp :: !Int,
    psInv :: [Item],
    psWeapon :: Maybe Item,
    psArmor :: Maybe Item
  }
  deriving (Eq, Show)

data Game = Game
  { gWorld :: Grid,
    gPlayerPos :: Pos,
    gPlayer :: PlayerStats,
    gEnemies :: Map Pos Enemy,
    gChestLoot :: Map Pos [Item],
    gRng :: StdGen,
    gLog :: [String],
    gFloor :: !Int,
    gKonami :: !Int,
    gIddqd :: String
  }
  deriving (Eq, Show)

data Arrow = AU | AD | AL | AR
  deriving (Eq, Show)

data KeyEvent = KChar Char | KArrow Arrow
  deriving (Eq, Show)

data TurnOutcome
  = Outcome Game
  | OutcomeAdvance Game
  deriving (Eq, Show)

data PauseLayer = PRoot | PStats | PInv | PInvDrop
  deriving (Eq, Show)

data AppState
  = Playing Game
  | Paused Game PauseLayer
  | ConfirmExit Game AppState
  | GameOver Game
  | Victory Game
  deriving (Eq, Show)
  