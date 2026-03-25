module Game.Generation (
  baseGrid,
  inBounds,
  onOuterWall,
  at,
  setAt,
  neighbors4,
  manhattan,
  adjacent4,
  floorCells,
  components,
  buildTerrain,
  enemyCountFormula,
  chestCountFormula,
  enemyHpDmg,
  mkEnemy,
  weaponStatRange,
  armorStatRange,
  potionStatRange,
  chestItemCountRange,
  rollItem,
  rollChestLoot,
  placeWorld,
  tryBuildFloor,
  buildFloorOrDie,
  shuffle
) where

import Game.Types
import Game.Config
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.List (foldl')
import System.Random (RandomGen, StdGen, randomR, splitGen)

inBounds :: (Int, Int) -> Bool
inBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height

onOuterWall :: (Int, Int) -> Bool
onOuterWall (x, y) = x == 0 || y == 0 || x == width - 1 || y == height - 1

at :: [[Tile]] -> (Int, Int) -> Maybe Tile
at g (x, y)
  | inBounds (x, y) = Just ((g !! y) !! x)
  | otherwise = Nothing

setAt :: (Int, Int) -> Tile -> [[Tile]] -> [[Tile]]
setAt (x, y) t g =
  take y g ++ [row'] ++ drop (y + 1) g
  where
    row = g !! y
    row' = take x row ++ [t] ++ drop (x + 1) row

baseGrid :: [[Tile]]
baseGrid =
  let border = replicate width Wall
      innerRow = Wall : replicate (width - 2) Floor ++ [Wall]
   in border : replicate (height - 2) innerRow ++ [border]

interiorPositions :: [(Int, Int)]
interiorPositions = [(x, y) | y <- [1 .. height - 2], x <- [1 .. width - 2]]

neighbors4 :: (Int, Int) -> [(Int, Int)]
neighbors4 (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

adjacent4 :: (Int, Int) -> (Int, Int) -> Bool
adjacent4 = ((== 1) .) . manhattan

paintDisk :: Tile -> (Int, Int) -> Int -> [[Tile]] -> [[Tile]]
paintDisk newTile (cx, cy) r g =
  foldl' step g coords
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

paintWallRect :: (Int, Int) -> (Int, Int) -> [[Tile]] -> [[Tile]]
paintWallRect (rx, ry) (rw, rh) g =
  foldl' (\g' pos -> case at g' pos of Just Floor -> setAt pos Wall g'; _ -> g') g cells
  where
    cells =
      [ (x, y)
        | y <- [ry .. min (height - 2) (ry + rh - 1)],
          x <- [rx .. min (width - 2) (rx + rw - 1)]
      ]

manhattanPath :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
manhattanPath p1@(x1, y1) p2@(x2, y2)
  | p1 == p2 = [p1]
  | x1 < x2 = p1 : manhattanPath (x1 + 1, y1) p2
  | x1 > x2 = p1 : manhattanPath (x1 - 1, y1) p2
  | y1 < y2 = p1 : manhattanPath (x1, y1 + 1) p2
  | otherwise = p1 : manhattanPath (x1, y1 - 1) p2

carvePath :: [[Tile]] -> (Int, Int) -> (Int, Int) -> [[Tile]]
carvePath g p1 p2 =
  foldl'
    (\gr pos -> if onOuterWall pos then gr else setAt pos Floor gr)
    g
    (manhattanPath p1 p2)

floorCells :: [[Tile]] -> [(Int, Int)]
floorCells g =
  [p | p <- interiorPositions, at g p == Just Floor]

bfsFloorComponent :: [[Tile]] -> (Int, Int) -> [(Int, Int)]
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

components :: [[Tile]] -> [[(Int, Int)]]
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

ensureConnected :: [[Tile]] -> [[Tile]]
ensureConnected g =
  case components g of
    [] -> g
    [_] -> g
    (c1 : c2 : _) ->
      let p1 = head c1
          p2 = head c2
       in ensureConnected (carvePath g p1 p2)

addLakes :: Int -> [[Tile]] -> StdGen -> ([[Tile]], StdGen)
addLakes n g gen
  | n <= 0 = (g, gen)
  | otherwise =
      let (center, g1) = randomInterior gen
          (rad, g2) = randomR lakeRadiusRange g1
          g' = paintDisk Lake center rad g
       in addLakes (n - 1) g' g2
  where
    randomInterior gen =
      let (x, g1) = randomR (1, width - 2) gen
          (y, g2) = randomR (1, height - 2) g1
       in ((x, y), g2)

addWallRuins :: Int -> [[Tile]] -> StdGen -> ([[Tile]], StdGen)
addWallRuins n g gen
  | n <= 0 = (g, gen)
  | otherwise =
      let (rw, g1) = randomR ruinsWidthRange gen
          (rh, g2) = randomR ruinsHeightRange g1
          maxX = width - 2 - rw
          maxY = height - 2 - rh
          (g', gOut)
            | maxX < 1 || maxY < 1 = (g, g2)
            | otherwise =
                let (rx, g3) = randomR (1, maxX) g2
                    (ry, g4) = randomR (1, maxY) g3
                 in (paintWallRect (rx, ry) (rw, rh) g, g4)
       in addWallRuins (n - 1) g' gOut

buildTerrain :: StdGen -> [[Tile]]
buildTerrain gen =
  let (nLakes, g1) = randomR lakeCountRange gen
      (nRuins, g2) = randomR ruinsCountRange g1  
      (gL, g3) = addLakes nLakes baseGrid g2
      (gR, _) = addWallRuins nRuins gL g3
   in ensureConnected gR

mkEnemy :: Int -> StdGen -> (Enemy, StdGen)
mkEnemy lvl g =
  let ((hLo, hHi), (dLo, dHi)) = enemyHpDmg lvl
      (hp, g1) = randomR (hLo, hHi) g
      (dmg, g2) = randomR (dLo, dHi) g1
   in (Enemy hp hp dmg, g2)

rollItem :: Int -> StdGen -> (Item, StdGen)
rollItem lvl g =
  let skew = lvl - 1
      (wLo, wHi) = weaponStatRange skew
      (aLo, aHi) = armorStatRange skew
      (pLo, pHi) = potionStatRange skew
      (r, g1) = randomR (1 :: Int, 100) g
  in case () of
       _
         | r <= weaponChance ->                             
             let (names, g2) = pickWeaponNames skew g1
                 (d, g3) = randomR (wLo, wHi) g2
             in (Weapon names d, g3)

         | r <= weaponChance + armorChance ->              
             let (names, g2) = pickArmorNames skew g1
                 (a, g3) = randomR (aLo, aHi) g2
             in (Armor names a, g3)

         | otherwise ->
             let (names, g2) = pickPotionNames skew g1
                 (h, g3) = randomR (pLo, pHi) g2
             in (Potion names h, g3)
  where
    pick xs g0 =
      let (i, g') = randomR (0, length xs - 1) g0
      in (xs !! i, g')

    pickWeaponNames s g0
      | s <= 0 = pick (head weaponNames) g0  
      | s <= 2 = pick (weaponNames !! 1) g0  
      | otherwise = pick (weaponNames !! 2) g0 

    pickArmorNames s g0
      | s <= 0 = pick (head armorNames) g0  
      | s <= 2 = pick (armorNames !! 1) g0   
      | otherwise = pick (armorNames !! 2) g0 

    pickPotionNames s g0
      | s <= 0 = pick (head potionNames) g0  
      | s <= 2 = pick (potionNames !! 1) g0  
      | otherwise = pick (potionNames !! 2) g0 

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

shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle xs g = go xs [] g
  where
    go [] acc g = (acc, g)
    go (x : xs') acc g =
      let (k, g') = randomR (0, length acc) g
          acc' = take k acc ++ [x] ++ drop k acc
       in go xs' acc' g'

placeWorld ::
  Int ->
  Int ->
  Int ->
  [Pos] ->
  Grid ->
  StdGen ->
  (Grid, Pos, M.Map Pos Enemy, M.Map Pos [Item], StdGen)
placeWorld nEnemies nChests lvl floors grid gen =
  case shuffle floors gen of
    (shuffled, g0) ->
      case shuffled of
        [] -> (grid, (0,0), M.empty, M.empty, gen)
        p : r1 ->
          let (enemyPoss, r2) = splitAt nEnemies r1
              (chestPoss, _) = splitAt nChests r2
              (enemyMap, g1) = foldl' placeEnemy (M.empty, g0) enemyPoss
              (chestMap, g2) = foldl' placeChest (M.empty, g1) chestPoss
              gW =
                foldl' (\gr pos -> setAt pos Chest gr) grid chestPoss
           in (gW, p, enemyMap, chestMap, g2)
  where
    placeEnemy (m, g') pos =
      let (e, g'') = mkEnemy lvl g'
       in (M.insert pos e m, g'')
    placeChest (m, g') pos =
      let (loot, g'') = rollChestLoot lvl g'
       in (M.insert pos loot m, g'')

tryBuildFloor :: Int -> PlayerStats -> StdGen -> Maybe (Game, StdGen)
tryBuildFloor floorNum player = go (0 :: Int)
  where
    maxAttempts = maxBuildAttempts  
    nE = enemyCountFormula floorNum  
    nC = chestCountFormula floorNum  
    needed = 1 + nE + nC
    go n g
      | n >= maxAttempts = Nothing
      | otherwise =
          let (gA, gRest) = splitGen g
              terrain = buildTerrain gA
              floors = floorCells terrain
           in if null (drop (needed - 1) floors)
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
                            gLog = [levelStartMessage floorNum],
                            gFloor = floorNum,
                            gKonami = 0,
                            gIddqd = ""
                          }
                   in Just (game, gEnd)

buildFloorOrDie :: Int -> PlayerStats -> StdGen -> (Game, StdGen)
buildFloorOrDie lvl p = go (0 :: Int)
  where
    go n gen
      | n > maxFloorBuildAttempts = error "buildFloorOrDie: не удалось сгенерировать уровень"
      | otherwise =
          case tryBuildFloor lvl p gen of
            Nothing -> go (n + 1) (snd (splitGen gen))
            Just x -> x
            