{- COMBAT_SPEC:
   1) Игрок заходит на врага (bump): удар (3 + бонус оружия). Контрудара нет — враг бьёт
      в свой ход, если вы рядом.
   2) Если враг мёртв — игрок занимает клетку; если это был последний враг на локации —
      переход на следующий уровень (инвентарь сохраняется). После 5 зачищенных локаций — победа.
   3) После хода игрока (ход / бой / сундук) ходят враги: рядом — удар, иначе шаг к игроку.
-}
module Main (main) where

import Control.Exception (finally)
import Data.Char (isAlpha, toLower)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import System.Console.ANSI (
  Color (..),
  ColorIntensity (..),
  ConsoleLayer (..),
  SGR (..),
  clearScreen,
  hideCursor,
  setSGR,
  showCursor,
 )
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import System.Random (RandomGen, StdGen, newStdGen, randomR, split)

-- | Тайлы мира (сущности на клетке учитываются отдельно в 'Game').
data Tile
  = Wall
  | Floor
  | Lake
  | Chest -- ^ закрытый сундук (лут в 'gChestLoot')
  deriving (Eq, Show)

tileChar :: Tile -> Char
tileChar Wall = '#'
tileChar Floor = '.'
tileChar Lake = '~'
tileChar Chest = '$'

tileSGR :: Tile -> [SGR]
tileSGR Wall = [SetColor Foreground Dull Blue]
tileSGR Floor = [SetColor Foreground Dull White]
tileSGR Lake = [SetColor Foreground Vivid Blue]
tileSGR Chest = [SetColor Foreground Vivid Yellow]

playerSGR :: [SGR]
playerSGR = [SetColor Foreground Vivid Green]

enemySGR :: [SGR]
enemySGR = [SetColor Foreground Vivid Red]

type Pos = (Int, Int)

type Grid = [[Tile]]

width, height :: Int
width = 48
height = 18

-- | Всего локаций до победы (после зачистки 5-й — конец игры).
maxStoryFloors :: Int
maxStoryFloors = 5

levelEnemyCount :: Int -> Int
levelEnemyCount lvl = 3 + (lvl - 1) * 2

levelChestCount :: Int -> Int
levelChestCount lvl = 2 + (lvl - 1) `div` 2 + if lvl >= 4 then 1 else 0

inBounds :: Pos -> Bool
inBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height

onOuterWall :: Pos -> Bool
onOuterWall (x, y) = x == 0 || y == 0 || x == width - 1 || y == height - 1

at :: Grid -> Pos -> Maybe Tile
at g (x, y)
  | inBounds (x, y) = Just ((g !! y) !! x)
  | otherwise = Nothing

walkableForPlayer :: Grid -> Pos -> Bool
walkableForPlayer g p = at g p == Just Floor

setAt :: Pos -> Tile -> Grid -> Grid
setAt (x, y) t g =
  take y g ++ [row'] ++ drop (y + 1) g
  where
    row = g !! y
    row' = take x row ++ [t] ++ drop (x + 1) row

baseGrid :: Grid
baseGrid =
  let border = replicate width Wall
      innerRow = Wall : replicate (width - 2) Floor ++ [Wall]
   in border : replicate (height - 2) innerRow ++ [border]

interiorPositions :: [Pos]
interiorPositions = [(x, y) | y <- [1 .. height - 2], x <- [1 .. width - 2]]

randomInterior :: RandomGen g => g -> (Pos, g)
randomInterior gen =
  let (x, g1) = randomR (1, width - 2) gen
      (y, g2) = randomR (1, height - 2) g1
   in ((x, y), g2)

neighbors4 :: Pos -> [Pos]
neighbors4 (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

adjacent4 :: Pos -> Pos -> Bool
adjacent4 a b = manhattan a b == 1

paintDisk :: Tile -> Pos -> Int -> Grid -> Grid
paintDisk newTile (cx, cy) r g =
  foldl step g coords
  where
    r2 = r * r
    coords =
      [ (x, y)
        | y <- [max 1 (cy - r) .. min (height - 2) (cy + r)],
          x <- [max 1 (cx - r) .. min (width - 2) (cx + r)],
          let dx = x - cx
              dy = y - cy,
          dx * dx + dy * dy <= r2
      ]
    step g' pos = case at g' pos of
      Just Floor -> setAt pos newTile g'
      _ -> g'

paintWallRect :: (Int, Int) -> (Int, Int) -> Grid -> Grid
paintWallRect (rx, ry) (rw, rh) g =
  foldl (\g' pos -> case at g' pos of Just Floor -> setAt pos Wall g'; _ -> g') g cells
  where
    cells =
      [ (x, y)
        | y <- [ry .. min (height - 2) (ry + rh - 1)],
          x <- [rx .. min (width - 2) (rx + rw - 1)]
      ]

manhattanPath :: Pos -> Pos -> [Pos]
manhattanPath p1@(x1, y1) p2@(x2, y2)
  | p1 == p2 = [p1]
  | x1 < x2 = p1 : manhattanPath (x1 + 1, y1) p2
  | x1 > x2 = p1 : manhattanPath (x1 - 1, y1) p2
  | y1 < y2 = p1 : manhattanPath (x1, y1 + 1) p2
  | otherwise = p1 : manhattanPath (x1, y1 - 1) p2

carvePath :: Grid -> Pos -> Pos -> Grid
carvePath g p1 p2 =
  foldl
    (\gr pos -> if onOuterWall pos then gr else setAt pos Floor gr)
    g
    (manhattanPath p1 p2)

floorCells :: Grid -> [Pos]
floorCells g =
  [p | p <- interiorPositions, at g p == Just Floor]

bfsFloorComponent :: Grid -> Pos -> [Pos]
bfsFloorComponent g p0 =
  case at g p0 of
    Just Floor -> go (Set.singleton p0) [p0] []
    _ -> []
  where
    go _ [] acc = reverse acc
    go seen (p : q) acc =
      let ns =
            [ n
              | n <- neighbors4 p,
                inBounds n,
                at g n == Just Floor,
                n `Set.notMember` seen
              ]
          seen' = foldr Set.insert seen ns
       in go seen' (q ++ ns) (p : acc)

components :: Grid -> [[Pos]]
components g =
  go (Set.fromList (floorCells g)) []
  where
    go s acc
      | Set.null s = acc
      | otherwise =
          let p = Set.findMin s
              c = bfsFloorComponent g p
              s' = Set.difference s (Set.fromList c)
           in go s' (c : acc)

ensureConnected :: Grid -> Grid
ensureConnected g =
  case components g of
    [] -> g
    [_] -> g
    (c1 : c2 : _) ->
      let p1 = head c1
          p2 = head c2
       in ensureConnected (carvePath g p1 p2)

addLakes :: Int -> Grid -> StdGen -> (Grid, StdGen)
addLakes n g gen
  | n <= 0 = (g, gen)
  | otherwise =
      let (center, g1) = randomInterior gen
          (rad, g2) = randomR (3, 8) g1
          g' = paintDisk Lake center rad g
       in addLakes (n - 1) g' g2

addWallRuins :: Int -> Grid -> StdGen -> (Grid, StdGen)
addWallRuins n g gen
  | n <= 0 = (g, gen)
  | otherwise =
      let (rw, g1) = randomR (3, 9) gen
          (rh, g2) = randomR (2, 5) g1
          maxX = width - 2 - rw
          maxY = height - 2 - rh
          (g', gOut)
            | maxX < 1 || maxY < 1 = (g, g2)
            | otherwise =
                let (rx, g3) = randomR (1, maxX) g2
                    (ry, g4) = randomR (1, maxY) g3
                 in (paintWallRect (rx, ry) (rw, rh) g, g4)
       in addWallRuins (n - 1) g' gOut

buildTerrain :: StdGen -> Grid
buildTerrain gen =
  let (nLakes, g1) = randomR (2, 4) gen
      (nRuins, g2) = randomR (4, 9) g1
      (gL, g3) = addLakes nLakes baseGrid g2
      (gR, _) = addWallRuins nRuins gL g3
   in ensureConnected gR

-- | Предметы: оружие (+урон к базе), броня (−к входящему), зелье.
data Item
  = Weapon String Int
  | Armor String Int
  | Potion String Int
  deriving (Eq, Show)

itemLine :: Item -> String
itemLine (Weapon n d) = n ++ " (оружие +" ++ show d ++ ")"
itemLine (Armor n a) = n ++ " (броня " ++ show a ++ ")"
itemLine (Potion n h) = n ++ " (+" ++ show h ++ " HP)"

data Enemy = Enemy
  { eHp :: !Int,
    eMaxHp :: !Int,
    eDmg :: !Int
  }
  deriving (Show)

data PlayerStats = PlayerStats
  { psHp :: !Int,
    psMaxHp :: !Int,
    psInv :: [Item],
    psWeapon :: Maybe Item,
    psArmor :: Maybe Item
  }
  deriving (Eq, Show)

playerAttackBonus :: PlayerStats -> Int
playerAttackBonus ps =
  3 + case psWeapon ps of
    Just (Weapon _ d) -> d
    _ -> 0

armorReduction :: PlayerStats -> Int
armorReduction ps = case psArmor ps of
  Just (Armor _ a) -> a
  _ -> 0

data Game = Game
  { gWorld :: Grid,
    gPlayerPos :: Pos,
    gPlayer :: PlayerStats,
    gEnemies :: Map Pos Enemy,
    gChestLoot :: Map Pos [Item],
    gRng :: StdGen,
    gLog :: [String],
    gFloor :: !Int, -- ^ текущая локация 1..maxStoryFloors
    gKonami :: !Int, -- ^ прогресс Konami в меню статов (0..9)
    gIddqd :: String -- ^ префикс IDDQD в меню статов
  }

-- | Ввод с клавиатуры (стрелки как в ANSI-терминале).
data Arrow = AU | AD | AL | AR
  deriving (Eq, Show)

data KeyEvent = KChar Char | KArrow Arrow
  deriving (Eq, Show)

readKeyEvent :: IO KeyEvent
readKeyEvent = do
  c <- getChar
  case c of
    '\ESC' -> do
      c2 <- getChar
      if c2 == '['
        then do
          c3 <- getChar
          case c3 of
            'A' -> pure (KArrow AU)
            'B' -> pure (KArrow AD)
            'C' -> pure (KArrow AR)
            'D' -> pure (KArrow AL)
            _ -> pure (KChar '\ESC')
        else pure (KChar c2)
    _ -> pure (KChar c)

eventMatches :: KeyEvent -> KeyEvent -> Bool
eventMatches (KChar a) (KChar b) = toLower a == toLower b
eventMatches x y = x == y

konamiSequence :: [KeyEvent]
konamiSequence =
  [ KArrow AU,
    KArrow AU,
    KArrow AD,
    KArrow AD,
    KArrow AL,
    KArrow AR,
    KArrow AL,
    KArrow AR,
    KChar 'b',
    KChar 'a'
  ]

advanceKonami :: KeyEvent -> Int -> (Int, Bool)
advanceKonami ev step
  | step >= 0 && step < 9 && eventMatches ev (konamiSequence !! step) =
      (step + 1, False)
  | step == 9 && eventMatches ev (konamiSequence !! 9) =
      (0, True)
  | otherwise = (0, False)

iddqdTarget :: String
iddqdTarget = "iddqd"

stepIddqd :: KeyEvent -> String -> String
stepIddqd (KArrow _) _ = ""
stepIddqd (KChar c) buf
  | not (isAlpha c) = ""
  | otherwise =
      let cl = toLower c
          i = length buf
       in if i < length iddqdTarget && cl == iddqdTarget !! i
            then buf ++ [cl]
            else if cl == 'i' then "i" else ""

cheatKonamiApply :: Game -> Game
cheatKonamiApply g =
  let p = gPlayer g
      w = Weapon "↑↑↓↓←→←→BA" 100
      ar = Armor "↑↑↓↓←→←→BA" 100
      inv1 = maybe id (\x -> (x :)) (psWeapon p) (psInv p)
      inv2 = maybe id (\x -> (x :)) (psArmor p) inv1
      p' = p {psWeapon = Just w, psArmor = Just ar, psInv = inv2}
   in logMsg "Чит: ↑↑↓↓←→←→BA" $ g {gPlayer = p', gIddqd = ""}

cheatIddqdApply :: Game -> Game
cheatIddqdApply g =
  let p = gPlayer g
      p' = p {psHp = 999999, psMaxHp = 999999}
   in logMsg "Чит: IDDQD" $ g {gPlayer = p', gKonami = 0}

processCheatInput :: KeyEvent -> Game -> Game
processCheatInput ev g =
  let buf = gIddqd g
      buf' = stepIddqd ev buf
      (km', fireK) = advanceKonami ev (gKonami g)
      g1 = g {gIddqd = buf', gKonami = km'}
   in if buf' == iddqdTarget
        then cheatIddqdApply $ g1 {gIddqd = ""}
        else
          if fireK
            then cheatKonamiApply $ g1 {gKonami = 0}
            else g1

-- | Остаёмся в меню статов, если ввод относится к читам (прогресс или сработал эффект).
cheatStatsStayOpen :: Game -> Game -> Bool
cheatStatsStayOpen g g' =
  gKonami g /= gKonami g'
    || gIddqd g /= gIddqd g'
    || gPlayer g /= gPlayer g'

data PauseLayer = PRoot | PStats | PInv | PInvDrop
  deriving (Eq, Show)

data AppState
  = Playing Game
  | Paused Game PauseLayer
  | GameOver Game
  | Victory Game

takeNth :: Int -> [a] -> Maybe (a, [a])
takeNth _ [] = Nothing
takeNth 0 (x : xs) = Just (x, xs)
takeNth n (x : xs) = case takeNth (n - 1) xs of
  Nothing -> Nothing
  Just (y, rest) -> Just (y, x : rest)

logMsg :: String -> Game -> Game
logMsg m g = g {gLog = take 5 (m : gLog g)}

playerAlive :: Game -> Bool
playerAlive g = psHp (gPlayer g) > 0

damagePlayer :: Int -> Game -> Game
damagePlayer rawDmg g =
  let p = gPlayer g
      mit = armorReduction p
      dmg = max 1 (rawDmg - mit)
      h' = max 0 (psHp p - dmg)
      p' = p {psHp = h'}
   in logMsg ("Вам нанесли " ++ show dmg ++ " урона") g {gPlayer = p'}

healPlayer :: Int -> Game -> Game
healPlayer amt g =
  let p = gPlayer g
      h' = min (psMaxHp p) (psHp p + amt)
   in g {gPlayer = p {psHp = h'}}

-- | Баланс врагов — по таблице GameGuide.txt.
enemyHpDmg :: Int -> ((Int, Int), (Int, Int))
enemyHpDmg lvl = case lvl of
  1 -> ((7, 9), (1, 2))
  2 -> ((8, 11), (2, 4))
  3 -> ((9, 13), (2, 5))
  4 -> ((10, 15), (3, 6))
  _ -> ((11, 17), (4, 7))

mkEnemy :: Int -> StdGen -> (Enemy, StdGen)
mkEnemy lvl g =
  let ((hLo, hHi), (dLo, dHi)) = enemyHpDmg lvl
      (hp, g1) = randomR (hLo, hHi) g
      (dmg, g2) = randomR (dLo, dHi) g1
   in (Enemy hp hp dmg, g2)

-- | Диапазоны статов предметов (skew = этаж − 1) — GameGuide.txt.
weaponStatRange :: Int -> (Int, Int)
weaponStatRange skew = case skew of
  0 -> (2, 3)
  1 -> (2, 4)
  2 -> (3, 5)
  3 -> (4, 6)
  _ -> (5, 10)

armorStatRange :: Int -> (Int, Int)
armorStatRange skew = case skew of
  0 -> (1, 2)
  1 -> (2, 3)
  2 -> (2, 5)
  3 -> (3, 5)
  _ -> (5, 7)

potionStatRange :: Int -> (Int, Int)
potionStatRange skew = case skew of
  0 -> (5, 10)
  1 -> (5, 15)
  2 -> (10, 17)
  3 -> (13, 20)
  _ -> (15, 30)

chestItemCountRange :: Int -> (Int, Int)
chestItemCountRange lvl = case lvl of
  1 -> (1, 3)
  2 -> (1, 4)
  3 -> (2, 4)
  4 -> (2, 5)
  _ -> (3, 5)

rollItem :: Int -> StdGen -> (Item, StdGen)
rollItem lvl g =
  let skew = lvl - 1
      (wLo, wHi) = weaponStatRange skew
      (aLo, aHi) = armorStatRange skew
      (pLo, pHi) = potionStatRange skew
      (r, g1) = randomR (1 :: Int, 100) g
   in if r <= 35
        then
          let (names, g2) = pickWeaponNames skew g1
              (d, g3) = randomR (wLo, wHi) g2
           in (Weapon names d, g3)
        else
          if r <= 70
            then
              let (names, g2) = pickArmorNames skew g1
                  (a, g3) = randomR (aLo, aHi) g2
               in (Armor names a, g3)
            else
              let (names, g2) = pickPotionNames skew g1
                  (h, g3) = randomR (pLo, pHi) g2
               in (Potion names h, g3)
  where
    pick xs g0 =
      let (i, g') = randomR (0, length xs - 1) g0
       in (xs !! i, g')
    pickWeaponNames s g0
      | s <= 0 = pick ["Ржавый клинок", "Кинжал", "Топорик"] g0
      | s <= 2 = pick ["Короткий меч", "Секира", "Копьё"] g0
      | otherwise = pick ["Меч закалённый", "Боевой топор", "Клинок тени"] g0
    pickArmorNames s g0
      | s <= 0 = pick ["Кожанка", "Кольчуга", "Латы"] g0
      | s <= 2 = pick ["Крепкая кожа", "Стальной нагрудник", "Латы рыцаря"] g0
      | otherwise = pick ["Доспех стража", "Пластины", "Латы закалённые"] g0
    pickPotionNames s g0
      | s <= 0 = pick ["Флакон", "Настойка", "Бинты"] g0
      | s <= 2 = pick ["Эликсир", "Сильное зелье", "Целебный отвар"] g0
      | otherwise = pick ["Большой флакон", "Нектар", "Философский эликсир"] g0

rollChestLoot :: Int -> StdGen -> ([Item], StdGen)
rollChestLoot lvl g =
  let (lo, hi) = chestItemCountRange lvl
      (n, g1) = randomR (lo, hi) g
   in go n g1 []
  where
    go :: Int -> StdGen -> [Item] -> ([Item], StdGen)
    go 0 gn acc = (acc, gn)
    go k gn acc =
      let (it, gn') = rollItem lvl gn
       in go (k - 1) gn' (it : acc)

placeWorld ::
  Int ->
  Int ->
  Int ->
  [Pos] ->
  Grid ->
  StdGen ->
  (Grid, Pos, Map Pos Enemy, Map Pos [Item], StdGen)
placeWorld nEnemies nChests lvl floors grid gen =
  case shuffle floors gen of
    (shuffled, g0) ->
      case shuffled of
        [] -> error "placeWorld: пусто"
        p : r1 ->
          let (enemyPoss, r2) = splitAt nEnemies r1
              (chestPoss, _) = splitAt nChests r2
              (enemyMap, g1) = foldl placeEnemy (M.empty, g0) enemyPoss
              (chestMap, g2) = foldl placeChest (M.empty, g1) chestPoss
              gW =
                foldl (\gr pos -> setAt pos Chest gr) grid chestPoss
           in (gW, p, enemyMap, chestMap, g2)
  where
    placeEnemy (m, g') pos =
      let (e, g'') = mkEnemy lvl g'
       in (M.insert pos e m, g'')
    placeChest (m, g') pos =
      let (loot, g'') = rollChestLoot lvl g'
       in (M.insert pos loot m, g'')

initialPlayer :: PlayerStats
initialPlayer =
  PlayerStats
    { psHp = 30,
      psMaxHp = 30,
      psInv = [],
      psWeapon = Nothing,
      psArmor = Nothing
    }

tryBuildFloor :: Int -> PlayerStats -> StdGen -> Maybe (Game, StdGen)
tryBuildFloor floorNum player g0 = go (0 :: Int) g0
  where
    maxAttempts = 400 :: Int
    nE = levelEnemyCount floorNum
    nC = levelChestCount floorNum
    needed = 1 + nE + nC
    go n g
      | n >= maxAttempts = Nothing
      | otherwise =
          let (gA, gRest) = split g
              terrain = buildTerrain gA
              floors = floorCells terrain
           in if length floors < needed
                then go (n + 1) gRest
                else
                  let (w, p0, em, cm, gEnd) =
                        placeWorld nE nC floorNum floors terrain gRest
                      game =
                        Game
                          { gWorld = w,
                            gPlayerPos = p0,
                            gPlayer = player,
                            gEnemies = em,
                            gChestLoot = cm,
                            gRng = gEnd,
                            gLog =
                              [ "Локация "
                                  ++ show floorNum
                                  ++ "/"
                                  ++ show maxStoryFloors
                                  ++ ". Убейте всех врагов."
                              ],
                            gFloor = floorNum,
                            gKonami = 0,
                            gIddqd = ""
                          }
                   in Just (game, gEnd)

buildFloorOrDie :: Int -> PlayerStats -> StdGen -> (Game, StdGen)
buildFloorOrDie lvl p g = go (0 :: Int) g
  where
    go n gen
      | n > (600 :: Int) = error "buildFloorOrDie: не удалось сгенерировать уровень"
      | otherwise =
          case tryBuildFloor lvl p gen of
            Nothing -> go (n + 1) (snd (split gen))
            Just x -> x

shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle xs g0 = go xs [] g0
  where
    go [] acc g = (acc, g)
    go (x : xs') acc g =
      let (k, g') = randomR (0, length acc) g
          acc' = take k acc ++ [x] ++ drop k acc
       in go xs' acc' g'

-- | Результат хода: обычная игра или локация зачищена (нужен следующий этаж).
data TurnOutcome
  = Outcome Game
  | OutcomeAdvance Game

advanceFromCleared :: Game -> AppState
advanceFromCleared g
  | not (playerAlive g) = GameOver g
  | gFloor g >= maxStoryFloors = Victory g
  | otherwise =
      let next = gFloor g + 1
          (gNew, r) = buildFloorOrDie next (gPlayer g) (gRng g)
          gFinal =
            gNew
              { gRng = r,
                gKonami = gKonami g,
                gIddqd = gIddqd g,
                gLog =
                  take
                    5
                    ( ("Локация зачищена! Переход на " ++ show next ++ ".")
                        : gLog gNew
                    )
              }
       in Playing gFinal

resolveTurn :: TurnOutcome -> AppState
resolveTurn (Outcome g)
  | not (playerAlive g) = GameOver g
  | otherwise = Playing g
resolveTurn (OutcomeAdvance g)
  | not (playerAlive g) = GameOver g
  | otherwise = advanceFromCleared g

-- | Клетка, куда враг может шагнуть (пол, без игрока, без других врагов).
enemyCanStep :: Game -> Pos -> Bool
enemyCanStep g p
  | not (inBounds p) = False
  | p == gPlayerPos g = False
  | M.member p (gEnemies g) = False
  | otherwise = at (gWorld g) p == Just Floor

tryMovePlayer :: Pos -> Game -> TurnOutcome
tryMovePlayer dxy g =
  let p0 = gPlayerPos g
      p' = addPos p0 dxy
   in if not (playerAlive g)
        then Outcome g
        else case () of
          ()
            | M.member p' (gEnemies g) -> playerAttackEnemy p' g
            | at (gWorld g) p' == Just Chest -> Outcome (openChest p' g)
            | walkableForPlayer (gWorld g) p' ->
                Outcome $
                  afterPlayerAction True $
                    g {gPlayerPos = p'}
            | otherwise -> Outcome g

openChest :: Pos -> Game -> Game
openChest pos g =
  case M.lookup pos (gChestLoot g) of
    Nothing -> g
    Just loot ->
      let p = gPlayer g
          p' = p {psInv = psInv p ++ loot}
          w' = setAt pos Floor (gWorld g)
       in afterPlayerAction True $
            logMsg ("Сундук: +" ++ show (length loot) ++ " предм.") $
              g
                { gWorld = w',
                  gPlayer = p',
                  gChestLoot = M.delete pos (gChestLoot g)
                }

playerAttackEnemy :: Pos -> Game -> TurnOutcome
playerAttackEnemy pos g =
  case M.lookup pos (gEnemies g) of
    Nothing -> Outcome g
    Just e ->
      let atk = playerAttackBonus (gPlayer g)
          e' = e {eHp = eHp e - atk}
          gHit =
            logMsg ("Вы бьёте на " ++ show atk) g
       in if eHp e' <= 0
            then
              let es' = M.delete pos (gEnemies gHit)
                  w' = setAt pos Floor (gWorld gHit)
                  gKill =
                    logMsg "Враг повержен." $
                      gHit
                        { gEnemies = es',
                          gWorld = w',
                          gPlayerPos = pos
                        }
               in if M.null es'
                    then OutcomeAdvance gKill
                    else Outcome (afterPlayerAction True gKill)
            else
              Outcome $
                afterPlayerAction True $
                  gHit
                    { gEnemies = M.insert pos e' (gEnemies gHit)
                    }

-- | Если игрок совершил действие, ходят враги (если игрок жив).
afterPlayerAction :: Bool -> Game -> Game
afterPlayerAction acted g
  | not acted = g
  | not (playerAlive g) = g
  | otherwise = enemyTurn g

enemyTurn :: Game -> Game
enemyTurn g
  | not (playerAlive g) = g
  | otherwise =
      foldl (flip processOneEnemy) g (sort (M.keys (gEnemies g)))
  where
    processOneEnemy pos g0 =
      if not (playerAlive g0) || not (M.member pos (gEnemies g0))
        then g0
        else case M.lookup pos (gEnemies g0) of
          Nothing -> g0
          Just e ->
            let pp = gPlayerPos g0
             in if adjacent4 pos pp
                  then damagePlayer (eDmg e) g0
                  else case pickEnemyStep g0 pos pp of
                    Nothing -> g0
                    Just to ->
                      let eMap' = M.insert to e (M.delete pos (gEnemies g0))
                       in g0 {gEnemies = eMap'}

pickEnemyStep :: Game -> Pos -> Pos -> Maybe Pos
pickEnemyStep g from to =
  let cands = filter (enemyCanStep g) (neighbors4 from)
      scored = [(manhattan c to, c) | c <- cands]
   in case scored of
        [] -> Nothing
        _ ->
          let best = minimum scored
           in Just (snd best)

addPos :: Pos -> Pos -> Pos
addPos (x, y) (dx, dy) = (x + dx, y + dy)

applyPlayingKeyEvent :: KeyEvent -> Game -> AppState
applyPlayingKeyEvent ev g
  | not (playerAlive g) = GameOver g
  | otherwise =
      case ev of
        KChar c
          | c == 'q' || c == 'Q' -> GameOver g
          | c == 'w' || c == 'W' -> resolveTurn (tryMovePlayer (0, -1) g)
          | c == 's' || c == 'S' -> resolveTurn (tryMovePlayer (0, 1) g)
          | c == 'a' || c == 'A' -> resolveTurn (tryMovePlayer (-1, 0) g)
          | c == 'd' || c == 'D' -> resolveTurn (tryMovePlayer (1, 0) g)
          | c == 'p' || c == 'P' -> Paused g PRoot
          | otherwise -> Playing g
        KArrow _ -> Playing g

-- | Меню статистики: читы (Konami, IDDQD) + Enter — назад в паузу.
handleStatsMenuKey :: KeyEvent -> Game -> AppState
handleStatsMenuKey ev g =
  case ev of
    KChar '\n' -> Paused g PRoot
    KChar '\r' -> Paused g PRoot
    _ ->
      let g' = processCheatInput ev g
       in if cheatStatsStayOpen g g' then Paused g' PStats else Paused g' PRoot

-- | В паузе: предмет слота n (0 = первый) — умное действие.
applyPauseKeyEvent :: KeyEvent -> Game -> PauseLayer -> AppState
applyPauseKeyEvent ev g layer =
  case layer of
    PStats -> handleStatsMenuKey ev g
    PRoot ->
      case ev of
        KArrow _ -> Paused g PRoot
        KChar ch ->
          case ch of
            'r' -> Playing g
            'R' -> Playing g
            's' -> Paused g PStats
            'S' -> Paused g PStats
            'i' -> Paused g PInv
            'I' -> Paused g PInv
            'q' -> GameOver g
            'Q' -> GameOver g
            _ -> Paused g PRoot
    PInv ->
      case ev of
        KArrow _ -> Paused g PInv
        KChar ch ->
          case ch of
            'b' -> Paused g PRoot
            'B' -> Paused g PRoot
            'd' -> Paused g PInvDrop
            'D' -> Paused g PInvDrop
            _
              | ch >= '1' && ch <= '9' ->
                  let idx = fromEnum ch - fromEnum '1'
                   in Paused (useOrEquip idx g) PInv
              | otherwise -> Paused g PInv
    PInvDrop ->
      case ev of
        KArrow _ -> Paused g PInvDrop
        KChar ch ->
          case ch of
            'b' -> Paused g PInv
            'B' -> Paused g PInv
            _
              | ch >= '1' && ch <= '9' ->
                  let idx = fromEnum ch - fromEnum '1'
                   in Paused (dropInvSlot idx g) PInv
              | otherwise -> Paused g PInvDrop

useOrEquip :: Int -> Game -> Game
useOrEquip idx g =
  let p = gPlayer g
   in case takeNth idx (psInv p) of
        Nothing -> g
        Just (it, inv') -> case it of
          Potion _ h ->
            logMsg ("Выпито: +" ++ show h ++ " HP") $
              healPlayer h $
                g {gPlayer = p {psInv = inv'}}
          w@(Weapon _ _) ->
            let oldW = psWeapon p
                inv'' = case oldW of Nothing -> inv'; Just ow -> ow : inv'
             in logMsg "Оружие экипировано." $
                  g {gPlayer = p {psWeapon = Just w, psInv = inv''}}
          a@(Armor _ _) ->
            let oldA = psArmor p
                inv'' = case oldA of Nothing -> inv'; Just oa -> oa : inv'
             in logMsg "Броня надета." $
                  g {gPlayer = p {psArmor = Just a, psInv = inv''}}

dropInvSlot :: Int -> Game -> Game
dropInvSlot idx g =
  let p = gPlayer g
   in case takeNth idx (psInv p) of
        Nothing -> g
        Just (_, inv') ->
          logMsg "Предмет выброшен." $
            g {gPlayer = p {psInv = inv'}}

drawCell :: Game -> Int -> Int -> IO ()
drawCell game x y
  | (x, y) == gPlayerPos game = do
      setSGR playerSGR
      putChar '@'
      setSGR [Reset]
  | M.member (x, y) (gEnemies game) = do
      setSGR enemySGR
      putChar 'e'
      setSGR [Reset]
  | otherwise = case at (gWorld game) (x, y) of
      Just t -> do
        setSGR (tileSGR t)
        putChar (tileChar t)
        setSGR [Reset]
      Nothing -> putChar '?'

drawGame :: Game -> IO ()
drawGame game = mapM_ drawRow [0 .. height - 1]
  where
    drawRow y = do
      mapM_ (\x -> drawCell game x y) [0 .. width - 1]
      putChar '\n'

statsLines :: Game -> [String]
statsLines g =
  let p = gPlayer g
      wline = maybe "Оружие: нет" itemLine (psWeapon p)
      aline = maybe "Броня: нет" itemLine (psArmor p)
      elines =
        [ "Враг " ++ show pos ++ ": HP " ++ show (eHp e) ++ "/" ++ show (eMaxHp e) ++ ", урон " ++ show (eDmg e)
          | (pos, e) <- M.toList (gEnemies g)
        ]
   in [ "Локация: " ++ show (gFloor g) ++ "/" ++ show maxStoryFloors ++ " (победа после " ++ show maxStoryFloors ++ " зачисток)",
        "HP: " ++ show (psHp p) ++ "/" ++ show (psMaxHp p),
        "Ваш удар: " ++ show (playerAttackBonus p) ++ " | снижение урона бронёй: " ++ show (armorReduction p),
        wline,
        aline,
        "--- Враги ---"
      ]
        ++ (if null elines then ["(нет)"] else elines)

pauseRootText :: String
pauseRootText =
  unlines
    [ "=== ПАУЗА (P) ===",
      "R — продолжить",
      "S — статы (вы и враги)",
      "I — инвентарь (1..9 — использовать/экипировать, D — выбросить)",
      "Q — выход из игры"
    ]

inventoryLines :: Game -> [String]
inventoryLines g =
  case psInv (gPlayer g) of
    [] -> ["(пусто)"]
    xs ->
      let shown = take 9 xs
          lines' =
            [ show i ++ ". " ++ itemLine it
              | (i, it) <- zip [(1 :: Int) ..] shown
            ]
       in lines' ++ if length xs > 9 then ["...ещё " ++ show (length xs - 9)] else []

instructions :: String
instructions =
  unlines
    [ "WASD — ходить (во врага — удар), P — пауза, Q — выход",
      "Цель: зачистить " ++ show maxStoryFloors ++ " локаций (все враги на каждой). Лут и экипировка сохраняются.",
      "@ игрок  . пол  # стена  ~ вода  e враг  $ сундук",
      ""
    ]

drawPlayingUI :: Game -> IO ()
drawPlayingUI g = do
  putStrLn instructions
  mapM_ putStrLn (gLog g)
  putChar '\n'
  drawGame g

drawPausedUI :: Game -> PauseLayer -> IO ()
drawPausedUI g layer = do
  drawGame g
  putChar '\n'
  case layer of
    PRoot -> putStrLn pauseRootText
    PStats -> do
      mapM_ putStrLn (statsLines g)
      putStrLn ""
      putStrLn "Enter — назад в меню паузы."
    PInv -> do
      putStrLn "=== Инвентарь ==="
      mapM_ putStrLn (inventoryLines g)
      putStrLn "1..9 — использовать/экипировать  |  D — режим выброса  |  B — назад"
    PInvDrop -> do
      putStrLn "=== Выбросить предмет ==="
      mapM_ putStrLn (inventoryLines g)
      putStrLn "1..9 — выбросить слот  |  B — отмена"

drawGameOver :: Game -> IO ()
drawGameOver g = do
  putStrLn ""
  if psHp (gPlayer g) <= 0
    then putStrLn "Вы погибли."
    else putStrLn "Выход из игры."
  putStrLn ""
  putStrLn "Любая клавиша — закрыть."

drawVictory :: Game -> IO ()
drawVictory g = do
  putStrLn ""
  if psHp (gPlayer g) > 30
    then putStrLn "Победа с читами - не победа вовсе."
    else putStrLn "Победа! Все пять локаций зачищены."
  putStrLn ""
  putStrLn "Любая клавиша — закрыть."

setupTerminal :: IO ()
setupTerminal = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hideCursor

restoreTerminal :: IO ()
restoreTerminal = do
  showCursor
  hSetEcho stdin True

stepApp :: KeyEvent -> AppState -> AppState
stepApp kev st =
  case st of
    Playing g -> applyPlayingKeyEvent kev g
    Paused g layer -> applyPauseKeyEvent kev g layer
    GameOver g -> GameOver g
    Victory g -> Victory g

mainLoop :: AppState -> IO ()
mainLoop st = do
  clearScreen
  case st of
    Playing g -> drawPlayingUI g
    Paused g layer -> drawPausedUI g layer
    GameOver g -> drawGameOver g
    Victory g -> drawVictory g
  kev <- readKeyEvent
  case st of
    GameOver {} -> pure ()
    Victory {} -> pure ()
    Playing {} -> mainLoop (stepApp kev st)
    Paused {} -> mainLoop (stepApp kev st)

main :: IO ()
main = do
  g <- newStdGen
  case tryBuildFloor 1 initialPlayer g of
    Nothing -> putStrLn "Не удалось сгенерировать карту."
    Just (game0, _) -> do
      setupTerminal
      mainLoop (Playing game0) `finally` restoreTerminal
