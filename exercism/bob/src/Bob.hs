module Bob (responseFor) where
import Data.Char
import Data.List

responseFor :: String -> String
responseFor xs
  | all isSpace xs     = "Fine. Be that way!"
  | anyUpper && allCap = if isQ then "Calm down, I know what I'm doing!"
                         else "Whoa, chill out!"
  | isQ                = "Sure."
  | otherwise          = "Whatever."
  where xs'      = dropWhileEnd isSpace xs
        isQ      = last        xs' == '?'
        anyUpper = any isUpper xs'
        allCap   = all (\ch -> isUpper ch || not (isLower ch)) xs'
