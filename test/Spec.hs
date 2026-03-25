module Main (main) where

import Control.Monad (unless)
import qualified Data.Map.Strict as M
import System.Exit (exitFailure)
import System.Random (mkStdGen)
import Game.Core

check :: String -> Bool -> IO Bool
check name ok = do
  putStrLn $ (if ok then "[OK]   " else "[FAIL] ") ++ name
  pure ok

makeGrid :: [Pos] -> Grid
makeGrid = foldl (\gr p -> setAt p Floor gr) baseGrid

makeGame
  :: PlayerStats
  -> Grid
  -> Pos
  -> [(Pos, Enemy)]
  -> [(Pos, [Item])]
  -> Int
  -> Game
makeGame player world pos enemies chests floorN =
  Game
    { gWorld = world
    , gPlayerPos = pos
    , gPlayer = player
    , gEnemies = M.fromList enemies
    , gChestLoot = M.fromList chests
    , gRng = mkStdGen 1
    , gLog = []
    , gFloor = floorN
    , gKonami = 0
    , gIddqd = ""
    }

outerPositions :: [Pos]
outerPositions =
  [(x, 0) | x <- [0 .. width - 1]]
    ++ [(x, height - 1) | x <- [0 .. width - 1]]
    ++ [(0, y) | y <- [1 .. height - 2]]
    ++ [(width - 1, y) | y <- [1 .. height - 2]]

-- Базовые утилиты
testAddPos :: IO Bool
testAddPos =
  and <$> sequence
        [ check "addPos (0,0)" (addPos (1, 2) (0, 0) == (1, 2))
        , check "addPos positive" (addPos (1, 2) (3, 4) == (4, 6))
        , check "addPos negative" (addPos (5, 5) (-2, -3) == (3, 2))
        ]

testNeighbors4 :: IO Bool
testNeighbors4 =
  let neighbors = neighbors4 (5, 5)
  in and <$> sequence
        [ check "neighbors4 count" (length neighbors == 4)
        , check "neighbors4 contains right" ((6, 5) `elem` neighbors)
        , check "neighbors4 contains left" ((4, 5) `elem` neighbors)
        , check "neighbors4 contains down" ((5, 6) `elem` neighbors)
        , check "neighbors4 contains up" ((5, 4) `elem` neighbors)
        ]

testSetAt :: IO Bool
testSetAt =
  let world = baseGrid
      world' = setAt (5, 5) Wall world
  in and <$> sequence
        [ check "setAt changes tile" (at world' (5, 5) == Just Wall)
        , check "setAt preserves other tiles" (at world' (1, 1) == at world (1, 1))
        ]

-- Таблицы и баланс
testLevelTables :: IO Bool
testLevelTables =
  and <$> sequence
    [ check "enemyCountFormula 1" (enemyCountFormula 1 == 3)
    , check "enemyCountFormula 2" (enemyCountFormula 2 == 5)
    , check "enemyCountFormula 5" (enemyCountFormula 5 == 11)
    , check "chestCountFormula 1" (chestCountFormula 1 == 2)
    , check "chestCountFormula 3" (chestCountFormula 3 == 3)
    , check "chestCountFormula 5" (chestCountFormula 5 == 5)
    ]

testStatRanges :: IO Bool
testStatRanges =
  and <$> sequence
    [ check "weaponStatRange 0" (weaponStatRange 0 == (2, 3))
    , check "weaponStatRange 1" (weaponStatRange 1 == (2, 4))
    , check "weaponStatRange 2" (weaponStatRange 2 == (3, 5))
    , check "weaponStatRange 3" (weaponStatRange 3 == (4, 6))
    , check "weaponStatRange 4" (weaponStatRange 4 == (5, 10))
    , check "armorStatRange 0" (armorStatRange 0 == (1, 2))
    , check "armorStatRange 1" (armorStatRange 1 == (2, 3))
    , check "armorStatRange 2" (armorStatRange 2 == (2, 5))
    , check "armorStatRange 3" (armorStatRange 3 == (3, 5))
    , check "armorStatRange 4" (armorStatRange 4 == (5, 7))
    , check "potionStatRange 0" (potionStatRange 0 == (5, 10))
    , check "potionStatRange 1" (potionStatRange 1 == (5, 15))
    , check "potionStatRange 2" (potionStatRange 2 == (10, 17))
    , check "potionStatRange 3" (potionStatRange 3 == (13, 20))
    , check "potionStatRange 4" (potionStatRange 4 == (15, 30))
    , check "chestItemCountRange 1" (chestItemCountRange 1 == (1, 3))
    , check "chestItemCountRange 5" (chestItemCountRange 5 == (3, 5))
    ]

testEnemyHpDmgAllLevels :: IO Bool
testEnemyHpDmgAllLevels = 
  and <$> sequence
    [ check ("enemyHpDmg level " ++ show lvl) 
        (let (hp, dmg) = enemyHpDmg lvl
         in uncurry (<=) hp && uncurry (<=) dmg)
    | lvl <- [1..7]
    ]

-- Предметы
testItemLine :: IO Bool
testItemLine =
  and <$> sequence
    [ check "itemLine weapon" (itemLine (Weapon "Меч" 4) == "Меч (оружие +4)")
    , check "itemLine armor" (itemLine (Armor "Панцирь" 3) == "Панцирь (броня 3)")
    , check "itemLine potion" (itemLine (Potion "Зелье" 12) == "Зелье (+12 HP)")
    ]

testItemLineLongNames :: IO Bool
testItemLineLongNames = 
  and <$> sequence
        [ check "weapon with long name" 
            (itemLine (Weapon "Длинное название оружия" 10) == "Длинное название оружия (оружие +10)")
        , check "armor with zero" 
            (itemLine (Armor "Слабая броня" 0) == "Слабая броня (броня 0)")
        ]

testRollItemSeedConsistency :: IO Bool
testRollItemSeedConsistency = 
  let (item1, _) = rollItem 1 (mkStdGen 42)
      (item2, _) = rollItem 1 (mkStdGen 42)
  in check "rollItem deterministic" (item1 == item2)

testRollItemAllLevels :: IO Bool
testRollItemAllLevels =
  and <$> sequence
        [ check ("rollItem level " ++ show lvl) 
            (let (item, _) = rollItem lvl (mkStdGen 42)
              in case item of
                   Weapon _ d -> d >= fst (weaponStatRange (lvl-1)) && d <= snd (weaponStatRange (lvl-1))
                   Armor _ a -> a >= fst (armorStatRange (lvl-1)) && a <= snd (armorStatRange (lvl-1))
                   Potion _ h -> h >= fst (potionStatRange (lvl-1)) && h <= snd (potionStatRange (lvl-1)))
        | lvl <- [1..5]
        ]
        
-- Игрок (статы, урон, лечение)
testCombatStats :: IO Bool
testCombatStats =
  let p =
        initialPlayer
          { psWeapon = Just (Weapon "Клинок" 5)
          , psArmor = Just (Armor "Доспех" 3)
          }
   in and <$> sequence
        [ check "playerAttackBonus" (playerAttackBonus p == 8)
        , check "armorReduction" (armorReduction p == 3)
        ]

testDamageAndHeal :: IO Bool
testDamageAndHeal =
  let p =
        initialPlayer
          { psHp = 10
          , psMaxHp = 12
          , psArmor = Just (Armor "Броня" 4)
          }
      g = makeGame p baseGrid (1, 1) [] [] 1
      g1 = damagePlayer 2 g
      g2 = damagePlayer 10 g1
      g3 = healPlayer 20 g2
   in and <$> sequence
        [ check "damagePlayer min 1" (psHp (gPlayer g1) == 9)
        , check "damagePlayer with armor" (psHp (gPlayer g2) == 3)
        , check "healPlayer capped" (psHp (gPlayer g3) == 12)
        ]

testUseOrEquip :: IO Bool
testUseOrEquip =
  let pPotion =
        initialPlayer
          { psHp = 10
          , psMaxHp = 20
          , psInv = [Potion "Зелье" 7]
          }
      gPotion = makeGame pPotion baseGrid (1, 1) [] [] 1
      gPotion' = useOrEquip 0 gPotion

      pWeapon =
        initialPlayer
          { psInv = [Weapon "Новое оружие" 5]
          , psWeapon = Just (Weapon "Старое оружие" 2)
          }
      gWeapon = makeGame pWeapon baseGrid (1, 1) [] [] 1
      gWeapon' = useOrEquip 0 gWeapon

      pArmor =
        initialPlayer
          { psInv = [Armor "Новая броня" 4]
          , psArmor = Just (Armor "Старая броня" 2)
          }
      gArmor = makeGame pArmor baseGrid (1, 1) [] [] 1
      gArmor' = useOrEquip 0 gArmor
   in and <$> sequence
        [ check "potion heals" (psHp (gPlayer gPotion') == 17)
        , check "potion removed" (null (psInv (gPlayer gPotion')))
        , check "weapon equipped" (case psWeapon (gPlayer gWeapon') of Just (Weapon _ 5) -> True; _ -> False)
        , check "old weapon returned to inventory" (case psInv (gPlayer gWeapon') of (Weapon _ 2 : _) -> True; _ -> False)
        , check "armor equipped" (case psArmor (gPlayer gArmor') of Just (Armor _ 4) -> True; _ -> False)
        , check "old armor returned to inventory" (case psInv (gPlayer gArmor') of (Armor _ 2 : _) -> True; _ -> False)
        ]

testUseOrEquipPotionFullHp :: IO Bool
testUseOrEquipPotionFullHp = 
  let p = initialPlayer { psHp = 20, psMaxHp = 20, psInv = [Potion "Test" 10] }
      g = makeGame p baseGrid (1, 1) [] [] 1
      g' = useOrEquip 0 g
  in check "potion at full HP not wasted" 
       (psHp (gPlayer g') == 20 && null (psInv (gPlayer g')))

testUseOrEquipNoItem :: IO Bool
testUseOrEquipNoItem = 
  let g = makeGame initialPlayer baseGrid (1, 1) [] [] 1
      g' = useOrEquip 0 g
  in check "useOrEquip with empty inventory" 
       (gPlayer g' == gPlayer g)

testDropInvSlot :: IO Bool
testDropInvSlot =
  let p =
        initialPlayer
          { psInv = [Weapon "A" 1, Potion "B" 2, Armor "C" 3]
          }
      g = makeGame p baseGrid (1, 1) [] [] 1
      g' = dropInvSlot 1 g
   in and <$> sequence
        [ check "drop removes selected slot" (length (psInv (gPlayer g')) == 2)
        , check "drop keeps other items" (case psInv (gPlayer g') of [Weapon _ 1, Armor _ 3] -> True; _ -> False)
        ]

testDropInvSlotInvalid :: IO Bool
testDropInvSlotInvalid = 
  let g = makeGame initialPlayer baseGrid (1, 1) [] [] 1
      g' = dropInvSlot 5 g
  in check "dropInvSlot with invalid index" 
       (gPlayer g' == gPlayer g)

-- Карта и генерация
testTerrain :: IO Bool
testTerrain =
  and <$> sequence
    [ check ("buildTerrain connected seed " ++ show seed) $
        let t = buildTerrain (mkStdGen seed)
         in not (null (floorCells t))
              && length (components t) == 1
              && all (\p -> at t p == Just Wall) outerPositions
    | seed <- [1 .. 20]
    ]

testComponentsEmpty :: IO Bool
testComponentsEmpty = 
  let emptyWorld = replicate height (replicate width Wall)
  in check "components of empty world" (null (components emptyWorld))

testWalkableForPlayer :: IO Bool
testWalkableForPlayer =
  let world = makeGrid [(2, 2), (3, 2)]
  in and <$> sequence
        [ check "walkableForPlayer on floor" (walkableForPlayer world (2, 2))
        , check "walkableForPlayer on wall" (not (walkableForPlayer world (0, 0)))
        ]

-- Движение и взаимодействие
testMovement :: IO Bool
testMovement =
  let world = makeGrid [(2, 2), (3, 2)]
      g = makeGame initialPlayer world (2, 2) [] [] 1
   in case tryMovePlayer (1, 0) g of
        Outcome g' -> check "player moves into floor" (gPlayerPos g' == (3, 2))
        _ -> check "player moves into floor" False

testTryMovePlayerWall :: IO Bool
testTryMovePlayerWall = 
  let g = makeGame initialPlayer baseGrid (1, 1) [] [] 1
  in check "tryMovePlayer into wall" 
       (case tryMovePlayer (0, 0) g of Outcome g' -> gPlayerPos g' == (1, 1); _ -> False)

testTryMovePlayerChest :: IO Bool
testTryMovePlayerChest = 
  let world = setAt (2, 2) Chest baseGrid
      g = makeGame initialPlayer world (1, 1) [] [((2, 2), [Potion "Test" 10])] 1
  in case tryMovePlayer (1, 1) g of
       Outcome g' -> check "tryMovePlayer opens chest" 
                        (at (gWorld g') (2, 2) == Just Floor && 
                         length (psInv (gPlayer g')) == 1)
       _ -> check "tryMovePlayer opens chest" False

testOpenChest :: IO Bool
testOpenChest =
  let world0 = makeGrid [(2, 2), (3, 2)]
      world = setAt (3, 2) Chest world0
      loot = [Potion "Флакон" 7, Weapon "Меч" 2]
      g = makeGame initialPlayer world (2, 2) [] [((3, 2), loot)] 1
      g' = openChest (3, 2) g
   in and <$> sequence
        [ check "chest removed from map" (M.notMember (3, 2) (gChestLoot g'))
        , check "chest tile becomes floor" (at (gWorld g') (3, 2) == Just Floor)
        , check "items added to inventory" (length (psInv (gPlayer g')) == 2)
        ]

-- Боевая система
testCombat :: IO Bool
testCombat =
  let world = makeGrid [(2, 2), (3, 2)]
      p = initialPlayer { psHp = 20 }
      g = makeGame p world (2, 2) [((3, 2), Enemy 1 1 4)] [] 1
   in case tryMovePlayer (1, 0) g of
        OutcomeAdvance g' ->
          check "killing last enemy advances" (gPlayerPos g' == (3, 2) && M.null (gEnemies g'))
        _ -> check "killing last enemy advances" False

testEnemyTurnAttack :: IO Bool
testEnemyTurnAttack =
  let world = makeGrid [(2, 2), (3, 2)]
      p = initialPlayer { psHp = 20 }
      g = makeGame p world (2, 2) [((3, 2), Enemy 5 5 4)] [] 1
      g' = enemyTurn g
   in check "adjacent enemy hits player" (psHp (gPlayer g') == 16)

testEnemyTurnStep :: IO Bool
testEnemyTurnStep =
  let world = makeGrid [(2, 2), (3, 2), (4, 2), (5, 2)]
      g = makeGame initialPlayer world (2, 2) [((5, 2), Enemy 5 5 4)] [] 1
      g' = enemyTurn g
   in check "enemy steps closer" (M.member (4, 2) (gEnemies g') && M.notMember (5, 2) (gEnemies g'))

testEnemyTurnMultiple :: IO Bool
testEnemyTurnMultiple =
  let world = makeGrid [(2, 2), (3, 2), (4, 2)]
      g = makeGame initialPlayer world (2, 2) 
          [((3, 2), Enemy 5 5 4), ((4, 2), Enemy 5 5 4)] [] 1
      g' = enemyTurn g
  in check "multiple enemies take turns" 
       (M.size (gEnemies g') == 2)

testEnemyCanStep :: IO Bool
testEnemyCanStep = 
  let world = makeGrid [(2, 2), (3, 2)]
      g = makeGame initialPlayer world (2, 2) [((3, 2), Enemy 5 5 4)] [] 1
  in and <$> sequence
        [ check "enemyCanStep on floor" (enemyCanStep g (4, 2))
        , check "enemyCanStep on player" (not (enemyCanStep g (2, 2)))
        , check "enemyCanStep on enemy" (not (enemyCanStep g (3, 2)))
        , check "enemyCanStep out of bounds" (not (enemyCanStep g (100, 100)))
        ]

-- Игровой цикл
testAfterPlayerAction :: IO Bool
testAfterPlayerAction = 
  let world = makeGrid [(2, 2), (3, 2)]
      g = makeGame initialPlayer world (2, 2) [((3, 2), Enemy 5 5 4)] [] 1
      g' = afterPlayerAction True g
  in check "afterPlayerAction triggers enemy turn" 
       (psHp (gPlayer g') < psHp (gPlayer g))

testResolveTurnNonDead :: IO Bool
testResolveTurnNonDead = 
  let g = makeGame initialPlayer baseGrid (1, 1) [] [] 1
  in check "resolveTurn with non-dead player" 
       (case resolveTurn (Outcome g) of Playing _ -> True; _ -> False)

testResolveTurnDead :: IO Bool
testResolveTurnDead = 
  let g = makeGame (initialPlayer { psHp = 0 }) baseGrid (1, 1) [] [] 1
  in check "resolveTurn with dead player" 
       (case resolveTurn (Outcome g) of GameOver _ -> True; _ -> False)

testResolveAndVictory :: IO Bool
testResolveAndVictory =
  let deadGame = makeGame (initialPlayer { psHp = 0 }) baseGrid (1, 1) [] [] 1
      winGame = makeGame initialPlayer baseGrid (1, 1) [] [] maxStoryFloors
   in and <$> sequence
        [ check "resolveTurn dead -> GameOver" (case resolveTurn (Outcome deadGame) of GameOver _ -> True; _ -> False)
        , check "advanceFromCleared on final floor -> Victory" (case advanceFromCleared winGame of Victory _ -> True; _ -> False)
        ]

-- Генерация уровня
testTryBuildFloor :: IO Bool
testTryBuildFloor =
  case firstSuccessfulFloor [1 .. 100] of
    Nothing -> check "tryBuildFloor succeeds on some seed" False
    Just g ->
      and <$> sequence
        [ check "floor number" (gFloor g == 1)
        , check "enemy count" (M.size (gEnemies g) == enemyCountFormula 1)
        , check "chest count" (M.size (gChestLoot g) == chestCountFormula 1)
        , check "player stands on floor" (walkableForPlayer (gWorld g) (gPlayerPos g))
        , check "all enemies on floor" (all (\p -> at (gWorld g) p == Just Floor) (M.keys (gEnemies g)))
        , check "all chests marked as chest" (all (\p -> at (gWorld g) p == Just Chest) (M.keys (gChestLoot g)))
        ]

-- Читы и лог
testCheats :: IO Bool
testCheats =
  let base = makeGame initialPlayer baseGrid (1, 1) [] [] 1
      konami =
        [ KArrow AU
        , KArrow AU
        , KArrow AD
        , KArrow AD
        , KArrow AL
        , KArrow AR
        , KArrow AL
        , KArrow AR
        , KChar 'b'
        , KChar 'a'
        ]
      afterKonami = foldl (flip processCheatInput) base konami
      afterIddqd = foldl (flip processCheatInput) base (map KChar "iddqd")
   in and <$> sequence
        [ check "Konami gives weapon" (case psWeapon (gPlayer afterKonami) of Just _ -> True; _ -> False)
        , check "Konami gives armor" (case psArmor (gPlayer afterKonami) of Just _ -> True; _ -> False)
        , check "IDDQD sets huge HP" (psHp (gPlayer afterIddqd) == 999999)
        , check "IDDQD sets huge max HP" (psMaxHp (gPlayer afterIddqd) == 999999)
        ]

testLogMsg :: IO Bool
testLogMsg = 
  let g = makeGame initialPlayer baseGrid (1, 1) [] [] 1
      g' = logMsg "Test message" g
  in check "logMsg adds message" 
       (length (gLog g') == 1 && head (gLog g') == "Test message")

testLogMsgLimit :: IO Bool
testLogMsgLimit = 
  let g = makeGame initialPlayer baseGrid (1, 1) [] [] 1
      g' = foldl (flip logMsg) g (map show [1..10])
  in check "logMsg limits to 5 messages" 
       (length (gLog g') == 5)

firstSuccessfulFloor :: [Int] -> Maybe Game
firstSuccessfulFloor [] = Nothing
firstSuccessfulFloor (s : ss) =
  case tryBuildFloor 1 initialPlayer (mkStdGen s) of
    Nothing -> firstSuccessfulFloor ss
    Just (g, _) -> Just g

-- Граничные случаи
testBoundaryConditions :: IO Bool
testBoundaryConditions =
  let g = makeGame initialPlayer baseGrid (1, 1) [] [] 1
  in and <$> sequence
        [ check "tryMovePlayer out of bounds" 
            (case tryMovePlayer (-100, -100) g of
                 Outcome g' -> gPlayerPos g' == (1, 1)
                 _ -> False)
        , check "damagePlayer with zero" 
            (psHp (gPlayer (damagePlayer 0 g)) < psHp (gPlayer g))
        , check "healPlayer with zero" 
            (psHp (gPlayer (healPlayer 0 g)) == psHp (gPlayer g))
        ]

main :: IO ()
main = do
  putStrLn "Running Haskell Rogue-like tests..."
  putStrLn "================================\n"
  
  results <- sequence
    [ testAddPos
    , testNeighbors4
    , testSetAt
    , testLevelTables
    , testStatRanges
    , testEnemyHpDmgAllLevels
    , testItemLine
    , testItemLineLongNames
    , testRollItemSeedConsistency
    , testRollItemAllLevels
    , testCombatStats
    , testDamageAndHeal
    , testUseOrEquip
    , testUseOrEquipPotionFullHp
    , testUseOrEquipNoItem
    , testDropInvSlot
    , testDropInvSlotInvalid
    , testTerrain
    , testComponentsEmpty
    , testWalkableForPlayer
    , testMovement
    , testTryMovePlayerWall
    , testTryMovePlayerChest
    , testOpenChest
    , testCombat
    , testEnemyTurnAttack
    , testEnemyTurnStep
    , testEnemyTurnMultiple
    , testEnemyCanStep
    , testAfterPlayerAction
    , testResolveTurnNonDead
    , testResolveTurnDead
    , testResolveAndVictory
    , testTryBuildFloor
    , testCheats
    , testLogMsg
    , testLogMsgLimit
    , testBoundaryConditions
    ]

  let passed = length (filter id results)
      total = length results

  putStrLn ""
  putStrLn "================================="
  putStrLn $ "Passed: " ++ show passed ++ "/" ++ show total
  putStrLn $ "Failed: " ++ show (total - passed)
  
  if passed == total
    then putStrLn "All tests passed!"
    else putStrLn "Some tests failed."

  unless (and results) exitFailure
  