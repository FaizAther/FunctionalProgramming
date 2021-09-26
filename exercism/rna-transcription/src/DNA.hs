module DNA (toRNA) where

transcribe :: Char -> Either Char Char
transcribe 'G' = Right 'C'
transcribe 'C' = Right 'G'
transcribe 'T' = Right 'A'
transcribe 'A' = Right 'U'
transcribe ch = Left ch

toRNA :: String -> Either Char String
toRNA = mapM transcribe
