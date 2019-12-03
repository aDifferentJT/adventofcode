put : Nat -> a -> List a -> List a
put  Z    x (y::ys) = x :: ys
put (S n) x (y::ys) = y :: put n x ys

getValue : Int -> (Int, List Int, List Int) -> Int
getValue i (pc, xs, ys) =
  case 
    if i >= pc
    then index' (toNat (i - pc)) ys
    else index' (toNat (pc - i - 1)) xs
  of
    Just v  => v
    Nothing => 0

putValue : Int -> Int -> (Int, List Int, List Int) -> (Int, List Int, List Int)
putValue i v (pc, xs, ys) =
  if i >= pc
  then (pc, xs, put (toNat (i - pc)) v ys)
  else (pc, put (toNat (pc - i - 1)) v xs, ys)

runOpcode : (Int, List Int, List Int) -> (Bool, (Int, List Int, List Int))
runOpcode p = case p of
  (pc, xs, 1::s1::s2::d::ys) =>
    let res = getValue s1 p + getValue s2 p in
    (True, putValue d res p)
  (pc, xs, 2::s1::s2::d::ys) =>
    let res = getValue s1 p * getValue s2 p in
    (True, putValue d res p)
  (pc, xs, 99::ys) => (False, p)

incPC : (Int, List Int, List Int) -> (Int, List Int, List Int)
incPC (pc, xs, y1::y2::y3::y4::ys) = (pc + 4, y4::y3::y2::y1::xs, ys)

runProgram' : (Int, List Int, List Int) -> List Int
runProgram' p with (runOpcode p)
  | (True, p') = runProgram' . incPC $ p'
  | (False, (_, xs, ys)) = reverse xs ++ ys

runProgram : List Int -> List Int
runProgram ys = runProgram' (0, [], ys)

parseProgram : String -> List Int
parseProgram = map cast . split (== ',')

run : List Int -> Int -> Int -> Int
run p1 noun verb =
  let p2 = put 1 noun . put 2 verb $ p1 in
  let p3 = runProgram p2 in
  let (res::_) = p3 in
  res

findNounVerb : List Int -> Int -> (Int, Int)
findNounVerb p1 res =
    let (n, v) = f 0 in
    (toIntNat n, toIntNat v)
  where g : Nat -> Nat -> Maybe (Nat, Nat)
        g n  Z    = Nothing
        g n (S v) =
          if run p1 (toIntNat n) (toIntNat v) == res
          then Just (n, v)
          else g n v
        f : Nat -> (Nat, Nat)
        f n with (g n n)
          | Just nv = nv
          | Nothing = f (S n)

main : IO ()
main = do
  Right f <- readFile "input.txt"
  let p1 = parseProgram f
  printLn (run p1 12 02)
  let (noun, verb) = findNounVerb p1 19690720
  printLn (100 * noun + verb)

