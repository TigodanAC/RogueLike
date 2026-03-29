module Game.Logic (
  itemLine,
  playerAttackBonus,
  armorReduction,
  playerAlive,
  logMsg,
  damagePlayer,
  healPlayer,
  useOrEquip,
  dropInvSlot,
  playerAttackEnemy,
  enemyCanStep,
  pickEnemyStep,
  enemyTurn,
  afterPlayerAction,
  tryMovePlayer,
  openChest,
  advanceFromCleared,
  resolveTurn,
  walkableForPlayer,
  konamiSequence,
  eventMatches,
  advanceKonami,
  stepIddqd,
  cheatKonamiApply,
  cheatIddqdApply,
  processCheatInput,
  cheatStatsStayOpen,
  takeNth,
  addPos
) where

import Game.Config
import Game.Types
import Game.Generation
import qualified Data.Map.Strict as M
import Data.List (sort)
import Data.Char (toLower, isAlpha)

itemLine :: Item -> String
itemLine (Weapon n d) = n ++ " (оружие +" ++ show d ++ ")"
itemLine (Armor n a) = n ++ " (броня " ++ show a ++ ")"
itemLine (Potion n h) = n ++ " (+" ++ show h ++ " HP)"

playerAttackBonus :: PlayerStats -> Int
playerAttackBonus ps =
  basePlayerDamage + case psWeapon ps of
    Just (Weapon _ d) -> d
    _ -> 0

armorReduction :: PlayerStats -> Int
armorReduction = maybe 0 (\(Armor _ a) -> a) . psArmor

addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos (x, y) (dx, dy) = (x + dx, y + dy)

playerAlive :: Game -> Bool
playerAlive = (> 0) . psHp . gPlayer

logMsg :: String -> Game -> Game
logMsg m g = g {gLog = take maxLogMessages (m : gLog g)}

damagePlayer rawDmg g =
  let p = gPlayer g
      mit = armorReduction p
      dmg = max 1 (rawDmg - mit)
      h' = max 0 (psHp p - dmg)
      p' = p {psHp = h'}
   in logMsg (damageLogMessage dmg) $ g {gPlayer = p'}

healPlayer :: Int -> Game -> Game
healPlayer amt g =
  let p = gPlayer g
      h' = min (psMaxHp p) (psHp p + amt)
   in g {gPlayer = p {psHp = h'}}

takeNth :: Int -> [a] -> Maybe (a, [a])
takeNth n xs =
  case splitAt n xs of
    (l, x:r) -> Just (x, l ++ r)
    _ -> Nothing

useOrEquip :: Int -> Game -> Game
useOrEquip idx g = maybe g handleItem $ takeNth idx (psInv (gPlayer g))
  where
    handleItem (it, inv') = case it of
      Potion _ h ->
        logMsg (healLogMessage h) $
          healPlayer h $
            g {gPlayer = (gPlayer g) {psInv = inv'}}
      
      w@(Weapon _ _) ->
        let p = gPlayer g
            newInv = maybe inv' (: inv') (psWeapon p)
         in logMsg weaponEquipMessage $
              g {gPlayer = p {psWeapon = Just w, psInv = newInv}}
      
      a@(Armor _ _) ->
        let p = gPlayer g
            newInv = maybe inv' (: inv') (psArmor p)
         in logMsg armorEquipMessage $
              g {gPlayer = p {psArmor = Just a, psInv = newInv}}

dropInvSlot :: Int -> Game -> Game
dropInvSlot idx g =
  let p = gPlayer g
   in case takeNth idx (psInv p) of
        Nothing -> g
        Just (_, inv') ->
          logMsg itemDropMessage $
            g {gPlayer = p {psInv = inv'}}

playerAttackEnemy :: (Int, Int) -> Game -> TurnOutcome
playerAttackEnemy pos g = maybe (Outcome g) attackEnemy $ M.lookup pos (gEnemies g)
  where
    attackEnemy e =
      let atk = playerAttackBonus (gPlayer g)
          e' = e {eHp = eHp e - atk}
          gHit = logMsg (enemyHitMessage atk) g
       in if eHp e' <= 0
            then handleKill pos gHit
            else Outcome $ afterPlayerAction True $ updateEnemy pos e' gHit

    handleKill pos gHit =
      let gKill = logMsg enemyKillMessage
                $ gHit { gEnemies = M.delete pos (gEnemies gHit)
                       , gWorld = setAt pos Floor (gWorld gHit)
                       , gPlayerPos = pos }
       in if M.null (gEnemies gKill)
            then OutcomeAdvance gKill
            else Outcome (afterPlayerAction True gKill)

    updateEnemy pos e' = overEnemies (M.insert pos e')
    overEnemies f g' = g' {gEnemies = f (gEnemies g')}

enemyCanStep g p =
  inBounds p
    && p /= gPlayerPos g
    && not (M.member p (gEnemies g))
    && at (gWorld g) p == Just Floor

pickEnemyStep :: Game -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
pickEnemyStep g from to =
  let scored = [(manhattan c to, c) | c <- filter (enemyCanStep g) (neighbors4 from)]
  in snd <$> safeMinimum scored

safeMinimum :: Ord a => [a] -> Maybe a
safeMinimum [] = Nothing
safeMinimum xs = Just (minimum xs)

enemyTurn :: Game -> Game
enemyTurn g
  | not (playerAlive g) = g
  | otherwise =
      foldl (flip processOneEnemy) g (M.keys (gEnemies g))
  where
    processOneEnemy pos g0 =
      case M.lookup pos (gEnemies g0) of
        Nothing -> g0
        Just e
          | not (playerAlive g0) -> g0
          | adjacent4 pos (gPlayerPos g0) ->
              damagePlayer (eDmg e) g0
          | otherwise ->
              maybe g0 (moveEnemy e) (pickEnemyStep g0 pos (gPlayerPos g0))
      where
        moveEnemy e to =
          g0
            { gEnemies =
                M.insert to e (M.delete pos (gEnemies g0))
            }

afterPlayerAction :: Bool -> Game -> Game
afterPlayerAction acted g
  | not acted = g
  | not (playerAlive g) = g
  | otherwise = enemyTurn g

tryMovePlayer dxy g
  | not (playerAlive g) = Outcome g
  | M.member newPos (gEnemies g) = playerAttackEnemy newPos g
  | at (gWorld g) newPos == Just Chest = Outcome (openChest newPos g)
  | walkableForPlayer (gWorld g) newPos =
      Outcome $ afterPlayerAction True $ g {gPlayerPos = newPos}
  | otherwise = Outcome g
  where
    newPos = addPos (gPlayerPos g) dxy

openChest :: (Int, Int) -> Game -> Game
openChest pos g =
  case M.lookup pos (gChestLoot g) of
    Nothing -> g
    Just loot ->
      let p = gPlayer g
          p' = p {psInv = psInv p ++ loot}
          w' = setAt pos Floor (gWorld g)
       in afterPlayerAction True $
            logMsg (chestLogMessage (length loot)) $
              g
                { gWorld = w',
                  gPlayer = p',
                  gChestLoot = M.delete pos (gChestLoot g)
                }

walkableForPlayer :: [[Tile]] -> (Int, Int) -> Bool
walkableForPlayer g p = at g p == Just Floor

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
                gUsedCheats = gUsedCheats g,
                gLog =
                  take
                    5
                    ( levelClearedMessage next
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

eventMatches :: KeyEvent -> KeyEvent -> Bool
eventMatches (KChar a) (KChar b) = toLower a == toLower b
eventMatches x y = x == y

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
      w = Weapon cheatWeaponName cheatWeaponBonus
      ar = Armor cheatArmorName cheatArmorBonus
      inv1 = maybe id (:) (psWeapon p) (psInv p)
      inv2 = maybe id (:) (psArmor p) inv1
      p' = p {psWeapon = Just w, psArmor = Just ar, psInv = inv2}
   in logMsg konamiCheatMessage $ g {gPlayer = p', gIddqd = "", gUsedCheats = True}

cheatIddqdApply :: Game -> Game
cheatIddqdApply g =
  let p = gPlayer g
      p' = p {psHp = 999999, psMaxHp = 999999}
   in logMsg iddqdCheatMessage $ g {gPlayer = p', gKonami = 0, gUsedCheats = True}

processCheatInput ev g
  | buf' == iddqdTarget = cheatIddqdApply $ g1 {gIddqd = ""}
  | fireK = cheatKonamiApply $ g1 {gKonami = 0}
  | otherwise = g1
  where
    buf = gIddqd g
    buf' = stepIddqd ev buf
    (km', fireK) = advanceKonami ev (gKonami g)
    g1 = g {gIddqd = buf', gKonami = km'}

cheatStatsStayOpen :: Game -> Game -> Bool
cheatStatsStayOpen g g' =
  gKonami g /= gKonami g'
    || gIddqd g /= gIddqd g'
    || gPlayer g /= gPlayer g'
    