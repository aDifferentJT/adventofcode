
fuelForMass : Int -> Int
fuelForMass mass =
  let fuel = flip (-) 2 . flip div 3 $ mass in
  if fuel <= 0
  then 0
  else fuel + fuelForMass fuel

processFile : File -> IO Int
processFile f = fGetLine f >>= \r => case r of
  Left e => pure 0
  Right "" => pure 0
  Right line =>
    let mass = the Int (cast line) in
    let fuel = fuelForMass mass in
    (+) fuel <$> processFile f

main : IO ()
main = do
  Right f <- openFile "input.txt" Read
  fuel <- processFile f
  printLn fuel

