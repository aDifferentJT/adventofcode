
fuelForModule : Int -> Int
fuelForModule = flip (-) 2 . flip div 3

fuelForSpacescraft : List Int -> Int
fuelForSpacescraft = foldr ((+) . fuelForModule) 0

processFile : File -> IO Int
processFile f = fGetLine f >>= \r => case r of
  Left e => pure 0
  Right "" => pure 0
  Right line =>
    let mass = the Int (cast line) in
    let fuel = fuelForModule mass in
    (+) fuel <$> processFile f

main : IO ()
main = do
  Right f <- openFile "input.txt" Read
  fuel <- processFile f
  printLn fuel

