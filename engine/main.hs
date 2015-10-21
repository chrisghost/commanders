data Game = Game {
    commanders :: [Commander]
  , highestLevel :: Int
} deriving (Show)

type Army = [Soldier]
data Commander = Commander {
    name :: String
  , army :: Army
  , attack :: Attack
} deriving (Show)

data AttackKind = Melee | Air | Ranged deriving (Show)
data Attack = Attack {
    kind :: AttackKind
  , power :: Double
} deriving (Show)

data Soldier = Soldier {
    sName :: String
  , life :: Double
} deriving (Show)

data Fight = Fight {
    attacker :: Commander
  , defender :: Commander
  , rounds :: [FightRound]
} deriving (Show)

data FightRound = FightRound {
    roundAttacker :: Commander
  , roundDefender :: Commander
  , roundAttack :: RoundAttack
} deriving (Show)

data RoundAttack = RoundAttack {
    attackName :: AttackKind
  , damagesInflicted :: Double
  , roundAttackFrom :: String
  , roundAttackTo :: String
} deriving (Show)

combat :: Fight -> Fight
combat f@(Fight att@(Commander _ attArmy _) def@(Commander _ defArmy _) rounds)
  | isDead att || isDead def = f
  | otherwise = let nrounds@(fr : sr@(FightRound nAtt nDef rndAttack):[]) = roundFight att def
                in combat $ Fight nAtt nDef (rounds ++ nrounds)

roundFight :: Commander -> Commander -> [FightRound]
roundFight att@(Commander _ attArmy _) def@(Commander _ defArmy _) = let round1@(FightRound nAtt nDef _) = damage att def
                      in let round2@(FightRound nnAtt nnDef _) = damage nDef nAtt
                          in [round1, round2]

damage :: Commander -> Commander -> FightRound
damage attacker@(Commander attName attArmy (Attack attAttackName attAttackPow)) defender@(Commander defName defArmy defAttack) = FightRound attacker defender {army = applyDamage defArmy attAttackPow} (RoundAttack attAttackName attAttackPow attName defName)

--fightRound :: Int -> Fight -> Fight
--fightRound index fgt@(Fight _ _ lastRound@( FightRound lastAttackers lastDefenders : _ ))
--  | index `mod` 2 == 1 = let (attk, def) = attack (head lastAttackers) (head lastDefenders)
--                          in fgt { rounds = [FightRound attk def] ++ (rounds fgt) }
--  | otherwise = fgt

isDead :: Commander -> Bool
isDead (Commander name soldiers attack) = foldl (\acc x -> life x + acc) 0 soldiers <= 0.0

applyDamage :: Army -> Double -> Army
applyDamage [] d = []
applyDamage a@(s@(Soldier _ slife):t) d
  | d <= 0.0    = a
  | slife > d   = s { life = slife - d } : t
  | slife <= d  = applyDamage t (d - slife)

printRound :: FightRound -> String
printRound fr@(FightRound commandAttacker commandDefender ra@(RoundAttack attName dmgs from to)) = from ++ " deals " ++ show dmgs ++ " damages to " ++ to ++ " with a " ++ (show attName) ++ " attack!" ++ "\n\t" ++ (armyLife commandAttacker) ++ "\n\t" ++ (armyLife commandDefender) ++ "\n\n"

armyLife :: Commander -> String
armyLife (Commander name soldiers _) = name ++"'s army has " ++ show (foldl (\acc x -> life x + acc) 0 soldiers) ++ " health"

main = do
  let comm = Commander "Henri I" [(Soldier "SwordMaster" 100.0)] (Attack Melee 10)

  let ennemy = Commander "Xerluk" [(Soldier "Devil" 40.0), (Soldier "Devil" 40.0), (Soldier "Devil" 40.0), (Soldier "Devil" 40.0), (Soldier "Devil" 40.0), (Soldier "Devil" 40.0)] (Attack Ranged 5)

  let (Fight attacker defender rounds) = combat $ Fight comm ennemy []

  --print $ (army (snd (damage comm ennemy)))

  --print $ applyDamage [(Soldier "tester" 40.0)] 10

  putStrLn "............."
  print $ (head rounds)
  print $ armyLife (roundDefender (head rounds))
  putStrLn "............."

  mapM (putStr . printRound ) rounds


