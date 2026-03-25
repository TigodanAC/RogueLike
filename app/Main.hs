module Main where

import Game.Core
import qualified Data.Map.Strict as M
import Control.Exception (finally)
import System.Console.ANSI (
    SGR(..),
    clearScreen,
    hideCursor,
    setSGR,
    showCursor
  )
import System.IO (BufferMode(NoBuffering), hSetBuffering, hSetEcho, stdin)
import System.Random (newStdGen)
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)

drawCell :: Game -> Int -> Int -> IO ()
drawCell game x y
  | (x, y) == gPlayerPos game = drawWithSGR playerSGR playerChar
  | M.member (x, y) (gEnemies game) = drawWithSGR enemySGR enemyChar
  | otherwise = maybe (putChar '?') drawTile $ at (gWorld game) (x, y)
  where
    drawWithSGR sgr ch = setSGR sgr >> putChar ch >> setSGR [Reset]
    drawTile t = setSGR (tileSGR t) >> putChar (tileChar t) >> setSGR [Reset]

drawGame :: Game -> IO ()
drawGame game = mapM_ drawRow [0 .. height - 1]
  where
    drawRow y = do
      mapM_ (\x -> drawCell game x y) [0 .. width - 1]
      putChar '\n'

statsLines :: Game -> [String]
statsLines g =
  [ locationLine
  , hpLine
  , combatLine
  , weaponLine
  , armorLine
  , statsEnemiesHeader
  ]
  ++ maybeEnemies (map mkEnemyLine (M.toList (gEnemies g)))
  where
    mkEnemyLine (pos, e) =
      "Враг " ++ show pos
        ++ ": HP " ++ show (eHp e) ++ "/" ++ show (eMaxHp e)
        ++ ", урон " ++ show (eDmg e)
    locationLine =
      statsLocationPrefix ++ show (gFloor g) ++ "/" ++ show maxStoryFloors
        ++ statsLocationSuffix ++ show maxStoryFloors ++ " зачисток)"
    hpLine =
      statsHpPrefix ++ show (psHp (gPlayer g))
        ++ statsHpSeparator ++ show (psMaxHp (gPlayer g))
    combatLine =
      statsCombatPrefix ++ show (playerAttackBonus (gPlayer g))
        ++ statsCombatSeparator ++ show (armorReduction (gPlayer g))
    weaponLine =
      maybe statsWeaponNone itemLine (psWeapon (gPlayer g))
    armorLine =
      maybe statsArmorNone itemLine (psArmor (gPlayer g))

    maybeEnemies [] = statsNoEnemies
    maybeEnemies xs = xs

inventoryLines :: Game -> [String]
inventoryLines = maybe inventoryEmpty formatInventory . nonEmpty . psInv . gPlayer
  where
    nonEmpty [] = Nothing
    nonEmpty xs = Just xs
    
    formatInventory xs = 
      let shown = take 9 xs
          indices = [1 :: Int .. length shown]
       in zipWith mkLine indices shown ++ extraLine (length xs)
      where
        mkLine i it = show i ++ ". " ++ itemLine it
        extraLine n = [inventoryMorePrefix ++ show (n - 9) | n > 9]

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
      putStrLn pauseEnterHint
    PInv -> do
      putStrLn inventoryHeader
      mapM_ putStrLn (inventoryLines g)
      putStrLn pauseInventoryHint
    PInvDrop -> do
      putStrLn "=== Выбросить предмет ==="
      mapM_ putStrLn (inventoryLines g)
      putStrLn pauseDropHint

drawGameOver :: Game -> IO ()
drawGameOver g = do
  putStrLn ""
  if psHp (gPlayer g) <= 0
    then putStrLn gameOverDead
    else putStrLn gameOverExit
  putStrLn ""
  putStrLn gameOverAnyKey

drawVictory :: Game -> IO ()
drawVictory _ = do
  putStrLn ""
  putStrLn victoryText
  putStrLn ""
  putStrLn victoryAnyKey

drawConfirmExit :: Game -> IO ()
drawConfirmExit g = do
  drawGame g
  putStrLn ""
  putStrLn confirmExitText
  putStrLn confirmExitYes
  putStrLn confirmExitNo

readKeyEvent :: IO KeyEvent
readKeyEvent = do
  c <- getChar
  if c /= '\ESC'
    then pure (KChar c)
    else do
      c2 <- getChar
      if c2 /= '['
        then pure (KChar c2)
        else do
          KArrow <$> readArrow
  where
    readArrow = do
      c3 <- getChar
      pure $ case c3 of
        'A' -> AU
        'B' -> AD
        'C' -> AR
        'D' -> AL
        _   -> AU

applyPlayingKeyEvent :: KeyEvent -> Game -> AppState
applyPlayingKeyEvent ev g
  | not (playerAlive g) = GameOver g
  | otherwise = handleEvent ev
  where
    handleEvent (KChar c) = fromMaybe (Playing g) (lookup c moveMap)
    handleEvent (KArrow _) = Playing g
    move dx dy = resolveTurn (tryMovePlayer (dx, dy) g)
    moveMap =
      [ (c, ConfirmExit g (Playing g)) | c <- quitKeys ]
      ++ [ (c, move 0 (-1)) | c <- moveUpKeys ]
      ++ [ (c, move 0 1) | c <- moveDownKeys ]
      ++ [ (c, move (-1) 0) | c <- moveLeftKeys ]
      ++ [ (c, move 1 0) | c <- moveRightKeys ]
      ++ [ (c, Paused g PRoot) | c <- pauseKeys ]

handleStatsMenuKey :: KeyEvent -> Game -> AppState
handleStatsMenuKey ev g =
  case ev of
    KChar c | c `elem` returnKeys -> Paused g PRoot
    _ ->
      let g' = processCheatInput ev g
       in if cheatStatsStayOpen g g' then Paused g' PStats else Paused g' PRoot

handleConfirmExit :: KeyEvent -> Game -> AppState -> AppState
handleConfirmExit ev g returnState =
  case ev of
    KChar c | c `elem` yesKeys -> GameOver g
    KChar c | c `elem` noKeys  -> returnState
    _ -> ConfirmExit g returnState

applyPauseKeyEvent :: KeyEvent -> Game -> PauseLayer -> AppState
applyPauseKeyEvent ev g layer = case layer of
  PStats   -> handleStatsMenuKey ev g
  PRoot    -> handlePRoot ev g
  PInv     -> handlePInv ev
  PInvDrop -> handlePInvDrop ev
  where
    handlePRoot ev' g' = case ev' of
      KArrow _ -> Paused g' PRoot
      KChar ch -> fromMaybe (Paused g' PRoot) (lookup ch prMap)
      where
        prMap =
          [ (c, Playing g') | c <- "rR" ]
          ++ [ (c, Paused g' PStats) | c <- statsMenuKeys ]
          ++ [ (c, Paused g' PInv) | c <- inventoryMenuKeys ]
          ++ [ (c, ConfirmExit g' (Paused g' PRoot)) | c <- quitKeys ]

    handlePInv (KChar ch)
      | ch `elem` backKeys   = Paused g PRoot
      | ch `elem` dropKeys   = Paused g PInvDrop
      | ch >= '1' && ch <= '9' =
          let idx = digitToInt ch - 1
          in Paused (useOrEquip idx g) PInv
    handlePInv _ = Paused g PInv

    handlePInvDrop (KChar ch)
      | ch `elem` backKeys   = Paused g PInv
      | ch >= '1' && ch <= '9' =
          let idx = digitToInt ch - 1
          in Paused (dropInvSlot idx g) PInv
    handlePInvDrop _ = Paused g PInvDrop

stepApp :: KeyEvent -> AppState -> AppState
stepApp kev st =
  case st of
    Playing g -> applyPlayingKeyEvent kev g
    Paused g layer -> applyPauseKeyEvent kev g layer
    ConfirmExit g returnState -> handleConfirmExit kev g returnState
    GameOver g -> GameOver g
    Victory g -> Victory g

setupTerminal :: IO ()
setupTerminal = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hideCursor

restoreTerminal :: IO ()
restoreTerminal = do
  showCursor
  hSetEcho stdin True

mainLoop :: AppState -> IO ()
mainLoop st = do
  clearScreen
  drawState st
  kev <- readKeyEvent
  unless (isTerminal st) $ mainLoop (stepApp kev st)
  where
    drawState (Playing g) = drawPlayingUI g
    drawState (Paused g l) = drawPausedUI g l
    drawState (ConfirmExit g _) = drawConfirmExit g
    drawState (GameOver g) = drawGameOver g
    drawState (Victory g) = drawVictory g
    
    isTerminal GameOver{} = True
    isTerminal Victory{} = True
    isTerminal _ = False

main :: IO ()
main = do
  g <- newStdGen
  case tryBuildFloor 1 initialPlayer g of
    Nothing -> putStrLn "Не удалось сгенерировать карту."
    Just (game0, _) -> do
      setupTerminal
      mainLoop (Playing game0) `finally` restoreTerminal
      