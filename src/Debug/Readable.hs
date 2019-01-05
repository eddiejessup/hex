module Debug.Readable where

class Show a => Readable a where
    describe :: a -> String
    describe = show
