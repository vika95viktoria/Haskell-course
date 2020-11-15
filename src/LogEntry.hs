import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import Data.Time
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString log = timeToString  (timestamp log) ++ ": " ++ show  (logLevel log)  ++ ": " ++ message log


data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p1 {lastName = lastName p2}

abbrFirstName :: Person -> Person
abbrFirstName p@(Person{firstName = f})  = p {firstName = newName} where newName = if length f > 1 then [head f, '.'] else f