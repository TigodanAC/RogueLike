module Game.Config where

import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..))
import Game.Types (Tile(..), PlayerStats(..))

-- Размеры карты
width, height :: Int
width = 48
height = 18

-- Игровые параметры
maxStoryFloors :: Int
maxStoryFloors = 5

-- Начальные статы игрока
initialHp, initialMaxHp :: Int
initialHp = 30
initialMaxHp = 30

-- Базовый урон игрока
basePlayerDamage :: Int
basePlayerDamage = 3

-- Начальный игрок
initialPlayer :: PlayerStats
initialPlayer =
  PlayerStats
    { psHp = initialHp,
      psMaxHp = initialMaxHp,
      psInv = [],
      psWeapon = Nothing,
      psArmor = Nothing
    }

-- Параметры генерации карты
lakeCountRange :: (Int, Int)
lakeCountRange = (2, 4)

ruinsCountRange :: (Int, Int)
ruinsCountRange = (4, 9)

lakeRadiusRange :: (Int, Int)
lakeRadiusRange = (3, 8)

ruinsWidthRange :: (Int, Int)
ruinsWidthRange = (3, 9)

ruinsHeightRange :: (Int, Int)
ruinsHeightRange = (2, 5)

maxBuildAttempts :: Int
maxBuildAttempts = 400

maxFloorBuildAttempts :: Int
maxFloorBuildAttempts = 600

-- Формула количества врагов на уровне
enemyCountFormula :: Int -> Int
enemyCountFormula lvl = 3 + (lvl - 1) * 2

-- Формула количества сундуков на уровне
chestCountFormula :: Int -> Int
chestCountFormula lvl = 2 + (lvl - 1) `div` 2 + if lvl >= 4 then 1 else 0

-- HP и урон врагов по уровням
enemyHpDmg :: Int -> ((Int, Int), (Int, Int))
enemyHpDmg lvl = case lvl of
  1 -> ((7, 9), (1, 2))
  2 -> ((8, 11), (2, 4))
  3 -> ((9, 13), (2, 5))
  4 -> ((10, 15), (3, 6))
  _ -> ((11, 17), (4, 7))

-- Диапазоны статов предметов по скиллу (этаж - 1)
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

-- Вероятности выпадения предметов (в процентах)
weaponChance, armorChance, potionChance :: Int
weaponChance = 35
armorChance = 35
potionChance = 30  -- 100 - 35 - 35 = 30

-- Диапазоны количества предметов в сундуке
chestItemCountRange :: Int -> (Int, Int)
chestItemCountRange lvl = case lvl of
  1 -> (1, 3)
  2 -> (1, 4)
  3 -> (2, 4)
  4 -> (2, 5)
  _ -> (3, 5)

-- Названия предметов (по скиллу)
weaponNames :: [[String]]
weaponNames =
  [ ["Ржавый клинок", "Кинжал", "Топорик"]
  , ["Короткий меч", "Секира", "Копьё"]
  , ["Меч закалённый", "Боевой топор", "Клинок тени"]
  ]

armorNames :: [[String]]
armorNames =
  [ ["Кожанка", "Кольчуга", "Латы"]
  , ["Крепкая кожа", "Стальной нагрудник", "Латы рыцаря"]
  , ["Доспех стража", "Пластины", "Латы закалённые"]
  ]

potionNames :: [[String]]
potionNames =
  [ ["Флакон", "Настойка", "Бинты"]
  , ["Эликсир", "Сильное зелье", "Целебный отвар"]
  , ["Большой флакон", "Нектар", "Философский эликсир"]
  ]

-- Имена для читерских предметов
cheatWeaponName, cheatArmorName :: String
cheatWeaponName = "↑↑↓↓←→←→BA"
cheatArmorName = "↑↑↓↓←→←→BA"

cheatWeaponBonus, cheatArmorBonus :: Int
cheatWeaponBonus = 100
cheatArmorBonus = 100

cheatHp :: Int
cheatHp = 999999

-- Логирование
maxLogMessages :: Int
maxLogMessages = 5

-- Символы для отрисовки
tileCharWall, tileCharFloor, tileCharLake, tileCharChest :: Char
tileCharWall = '#'
tileCharFloor = '.'
tileCharLake = '~'
tileCharChest = '$'

playerChar, enemyChar :: Char
playerChar = '@'
enemyChar = 'e'

-- Функция для получения символа тайла
tileChar :: Tile -> Char
tileChar Wall = tileCharWall
tileChar Floor = tileCharFloor
tileChar Lake = tileCharLake
tileChar Chest = tileCharChest

-- Цвета (SGR)
tileSGRWall, tileSGRFloor, tileSGRLake, tileSGRChest :: [SGR]
tileSGRWall = [SetColor Foreground Dull Blue]
tileSGRFloor = [SetColor Foreground Dull White]
tileSGRLake = [SetColor Foreground Vivid Blue]
tileSGRChest = [SetColor Foreground Vivid Yellow]

playerSGR :: [SGR]
playerSGR = [SetColor Foreground Vivid Green]

enemySGR :: [SGR]
enemySGR = [SetColor Foreground Vivid Red]

-- Функция для получения SGR тайла
tileSGR :: Tile -> [SGR]
tileSGR Wall = tileSGRWall
tileSGR Floor = tileSGRFloor
tileSGR Lake = tileSGRLake
tileSGR Chest = tileSGRChest

-- Тексты UI
instructions :: String
instructions =
  unlines
    [ "WASD — ходить (во врага — удар), P — пауза, Q — выход",
      "Цель: зачистить " ++ show maxStoryFloors ++ " локаций (все враги на каждой). Лут и экипировка сохраняются.",
      "@ игрок  . пол  # стена  ~ вода  e враг  $ сундук",
      ""
    ]

pauseRootText :: String
pauseRootText =
  unlines
    [ "=== ПАУЗА (P) ===",
      "R — продолжить",
      "S — статы (вы и враги)",
      "I — инвентарь (1..9 — использовать/экипировать, D — выбросить)",
      "Q — выход из игры"
    ]

pauseEnterHint :: String
pauseEnterHint = "Enter — назад в меню паузы."

pauseInventoryHint :: String
pauseInventoryHint = "1..9 — использовать/экипировать  |  D — режим выброса  |  B — назад"

pauseDropHint :: String
pauseDropHint = "1..9 — выбросить слот  |  B — отмена"

inventoryHeader :: String
inventoryHeader = "=== Инвентарь ==="

inventoryEmpty :: [String]
inventoryEmpty = ["(пусто)"]

inventoryDropHeader :: String
inventoryDropHeader = "=== Выбросить предмет ==="

inventoryMorePrefix :: String
inventoryMorePrefix = "...ещё "

-- Заголовки статистики
statsLocationPrefix :: String
statsLocationPrefix = "Локация: "

statsLocationSuffixStart :: String
statsLocationSuffixStart = " (победа после "

statsLocationSuffixEnd :: String
statsLocationSuffixEnd = " зачисток)"

statsHpPrefix :: String
statsHpPrefix = "HP: "

statsHpSeparator :: String
statsHpSeparator = "/"

statsCombatPrefix :: String
statsCombatPrefix = "Ваш удар: "

statsCombatSeparator :: String
statsCombatSeparator = " | снижение урона бронёй: "

statsWeaponNone :: String
statsWeaponNone = "Оружие: нет"

statsArmorNone :: String
statsArmorNone = "Броня: нет"

statsEnemiesHeader :: String
statsEnemiesHeader = "--- Враги ---"

statsNoEnemies :: [String]
statsNoEnemies = ["(нет)"]

generationFailedMessage :: String
generationFailedMessage = "Не удалось сгенерировать карту."

-- Тексты подтверждения выхода
confirmExitText :: String
confirmExitText = "Вы точно хотите выйти?"

confirmExitYes :: String
confirmExitYes = "Y — да, выйти"

confirmExitNo :: String
confirmExitNo = "N — нет, продолжить"

-- Тексты окончания игры
gameOverDead :: String
gameOverDead = "Вы погибли."

gameOverExit :: String
gameOverExit = "Выход из игры."

gameOverAnyKey :: String
gameOverAnyKey = "Любая клавиша — закрыть."

victoryText :: String
victoryText = "Победа! Все пять локаций зачищены."

victoryAnyKey :: String
victoryAnyKey = "Любая клавиша — закрыть."

-- Текст при генерации уровня
levelStartMessage :: Int -> String
levelStartMessage floorNum =
  "Локация " ++ show floorNum ++ "/" ++ show maxStoryFloors ++ ". Убейте всех врагов."

levelClearedMessage :: Int -> String
levelClearedMessage next = "Локация зачищена! Переход на " ++ show next ++ "."

-- Тексты логов
damageLogMessage :: Int -> String
damageLogMessage dmg = "Вам нанесли " ++ show dmg ++ " урона"

healLogMessage :: Int -> String
healLogMessage hp = "Выпито: +" ++ show hp ++ " HP"

chestLogMessage :: Int -> String
chestLogMessage count = "Сундук: +" ++ show count ++ " предм."

weaponEquipMessage :: String
weaponEquipMessage = "Оружие экипировано."

armorEquipMessage :: String
armorEquipMessage = "Броня надета."

itemDropMessage :: String
itemDropMessage = "Предмет выброшен."

enemyKillMessage :: String
enemyKillMessage = "Враг повержен."

enemyHitMessage :: Int -> String
enemyHitMessage dmg = "Вы бьёте на " ++ show dmg

konamiCheatMessage :: String
konamiCheatMessage = "Чит: ↑↑↓↓←→←→BA"

iddqdCheatMessage :: String
iddqdCheatMessage = "Чит: IDDQD"

-- Коды клавиш
moveUpKeys, moveDownKeys, moveLeftKeys, moveRightKeys :: [Char]
moveUpKeys = "wW"
moveDownKeys = "sS"
moveLeftKeys = "aA"
moveRightKeys = "dD"

pauseKeys :: [Char]
pauseKeys = "pP"

quitKeys :: [Char]
quitKeys = "qQ"

yesKeys, noKeys :: [Char]
yesKeys = "yY"
noKeys = "nN"

returnKeys :: [Char]
returnKeys = "\n\r"

statsMenuKeys :: [Char]
statsMenuKeys = "sS"

inventoryMenuKeys :: [Char]
inventoryMenuKeys = "iI"

backKeys :: [Char]
backKeys = "bB"

dropKeys :: [Char]
dropKeys = "dD"
