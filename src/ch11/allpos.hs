import Data.List

data OperatingSystem =
      GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill 
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
      Haskell
    | Agda
    | Idris
    | PureScript 
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
               , lang :: ProgLang }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem] 
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill 
    , Mac
    , Windows
    ]
        
allLanguages :: [ProgLang] 
allLanguages = 
    [Haskell, Agda, Idris, PureScript] 


allProgrammers :: [Programmer]
allProgrammers = nub $ allProgrammers' allOperatingSystems allLanguages
    where allProgrammers' [] _ = []
          allProgrammers' _ [] = []
          allProgrammers' allOs@(os:oss) allLang@(l:langs) =
            Programmer { lang = l
                       , os = os } : (allProgrammers' allOs langs ++ allProgrammers' oss allLang)

allProgrammersFolds :: [Programmer]
allProgrammersFolds = map tupleToProgramer allTuples
    where allTuples = foldr f [] allLanguages 
          f lang' acc = (langos lang') ++ acc
          langos lang' = foldr (f' lang') [] allOperatingSystems
          f' lang' os' acc' = (os', lang') : acc'
          tupleToProgramer (x, y) = Programmer {os = x, lang = y}