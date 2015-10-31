import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Eq, Show)

type Code = String

type Locker = (LockerState, Code)

type LockerMap = Map.Map Int Locker

exampleLockers :: LockerMap
exampleLockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  

grantLockerCode :: Int -> Locker -> Either String Code
grantLockerCode lockerNumber (Taken, _) =
    Left ("Locker with number " ++ show lockerNumber ++ " is already taken.")

grantLockerCode _ (Free, code) =
    Right code

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
      Nothing -> Left ("Locker with number " ++ show lockerNumber ++ " doesn't exist.")
      Just locker ->  grantLockerCode lockerNumber locker
