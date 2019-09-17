module RegisteredUser where

    newtype Username      = Username String
    newtype AccountNumber = AccountNumber Integer
    data User = UnregisteredUser | RegisteredUser Username AccountNumber

    printUser :: User -> IO ()
    printUser UnregisteredUser = putStrLn "UnregisteredUser"
    printUser (RegisteredUser (Username name) (AccountNumber number)) = putStrLn $ name ++ " " ++ show number

    -- Note how that type of RegisteredUser is a function that takes two arguments and returns a User
    -- RegisteredUser> :t RegisteredUser
    -- RegisteredUser :: Username -> AccountNumber -> User


    data WherePenguinsLive =
          Galapagos
        | Antarctica
        | Australia
        | SouthAfrica
        | SouthAmerica
        deriving (Eq, Show)

    data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

    -- Bad
    isSouthAfrica :: WherePenguinsLive -> Bool
    isSouthAfrica SouthAfrica  = True
    isSouthAfrica Galapagos    = False
    isSouthAfrica Antarctica   = False
    isSouthAfrica Australia    = False
    isSouthAfrica SouthAmerica = False

    -- Good
    isSouthAfrica' :: WherePenguinsLive -> Bool
    isSouthAfrica' SouthAfrica = True
    isSouthAfrica' _           = False

    gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
    gimmeWhereTheyLive (Peng whereItLives) = whereItLives