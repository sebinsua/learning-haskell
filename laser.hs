data Target=Fool String
    | Crowd Integer
    | TheMoon
    deriving (Eq, Ord, Show)

fireOn :: Target -> String
fireOn (Fool s)=s ++ " blasted!"
fireOn (Crowd n)=show n ++ "'s murdered!"
fireOn TheMoon="How did you miss a shot like that?"
