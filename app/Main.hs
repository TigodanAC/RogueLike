module Main where

import Game.Core
import qualified Data.Map.Strict as M
import Control.Exception (finally)
import System.Console.ANSI (
    Color(..),
    ColorIntensity(..),
    ConsoleLayer(..),
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

drawCell :: Game -> Int -> Int -> IO ()
drawCell game x y
  | (x, y) == gPlayerPos game = drawWithSGR playerSGR '@'
  | M.member (x, y) (gEnemies game) = drawWithSGR enemySGR 'e'
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
  , enemiesHeader
  ]
  ++ maybeEnemies (map mkEnemyLine (M.toList (gEnemies g)))
  where
    mkEnemyLine (pos, e) =
      "Враг " ++ show pos
        ++ ": HP " ++ show (eHp e) ++ "/" ++ show (eMaxHp e)
        ++ ", урон " ++ show (eDmg e)
    locationLine =
      "Локация: " ++ show (gFloor g) ++ "/" ++ show maxStoryFloors
        ++ " (победа после " ++ show maxStoryFloors ++ " зачисток)"
    hpLine =
      "HP: " ++ show (psHp (gPlayer g))
        ++ "/" ++ show (psMaxHp (gPlayer g))
    combatLine =
      "Ваш удар: " ++ show (playerAttackBonus (gPlayer g))
        ++ " | снижение урона бронёй: " ++ show (armorReduction (gPlayer g))
    weaponLine =
      maybe "Оружие: нет" itemLine (psWeapon (gPlayer g))
    armorLine =
      maybe "Броня: нет" itemLine (psArmor (gPlayer g))

    enemiesHeader = "--- Враги ---"
    maybeEnemies [] = ["(нет)"]
    maybeEnemies xs = xs

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
inventoryLines = maybe ["(пусто)"] formatInventory . nonEmpty . psInv . gPlayer
  where
    nonEmpty [] = Nothing
    nonEmpty xs = Just xs
    
    formatInventory xs = 
      let shown = take 9 xs
          indices = [1 :: Int .. length shown]
       in zipWith mkLine indices shown ++ extraLine (length xs)
      where
        mkLine i it = show i ++ ". " ++ itemLine it
        extraLine n = ["...ещё " ++ show (n - 9) | n > 9]

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
drawVictory _ = do
  putStrLn ""
  putStrLn "Победа! Все пять локаций зачищены."
  putStrLn ""
  putStrLn "Любая клавиша — закрыть."

drawConfirmExit :: Game -> IO ()
drawConfirmExit g = do
  drawGame g
  putStrLn ""
  putStrLn "Вы точно хотите выйти?"
  putStrLn "Y — да, выйти"
  putStrLn "N — нет, продолжить"

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
      [ ('q', ConfirmExit g), ('Q', ConfirmExit g)
      , ('w', move 0 (-1)), ('W', move 0 (-1))
      , ('s', move 0 1),    ('S', move 0 1)
      , ('a', move (-1) 0), ('A', move (-1) 0)
      , ('d', move 1 0),    ('D', move 1 0)
      , ('p', Paused g PRoot), ('P', Paused g PRoot)
      ]

handleStatsMenuKey :: KeyEvent -> Game -> AppState
handleStatsMenuKey ev g =
  case ev of
    KChar '\n' -> Paused g PRoot
    KChar '\r' -> Paused g PRoot
    _ ->
      let g' = processCheatInput ev g
       in if cheatStatsStayOpen g g' then Paused g' PStats else Paused g' PRoot

handleConfirmExit :: KeyEvent -> Game -> AppState
handleConfirmExit ev g =
  case ev of
    KChar 'y' -> GameOver g
    KChar 'Y' -> GameOver g
    KChar 'n' -> Playing g
    KChar 'N' -> Playing g
    _ -> ConfirmExit g

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
          [ ('r', Playing g'), ('R', Playing g')
          , ('s', Paused g' PStats), ('S', Paused g' PStats)
          , ('i', Paused g' PInv),   ('I', Paused g' PInv)
          , ('q', GameOver g'),      ('Q', GameOver g')
          ]

    handlePInv (KChar ch)
      | ch `elem` "bB"      = Paused g PRoot
      | ch `elem` "dD"      = Paused g PInvDrop
      | ch `elem` ['1'..'9'] =
          let idx = digitToInt ch - 1
          in Paused (useOrEquip idx g) PInv
    handlePInv _ = Paused g PInv

    handlePInvDrop (KChar ch)
      | ch `elem` "bB"      = Paused g PInv
      | ch `elem` ['1'..'9'] =
          let idx = digitToInt ch - 1
          in Paused (dropInvSlot idx g) PInv
    handlePInvDrop _ = Paused g PInvDrop

stepApp :: KeyEvent -> AppState -> AppState
stepApp kev st =
  case st of
    Playing g -> applyPlayingKeyEvent kev g
    Paused g layer -> applyPauseKeyEvent kev g layer
    ConfirmExit g -> handleConfirmExit kev g
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
    drawState (ConfirmExit g) = drawConfirmExit g
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