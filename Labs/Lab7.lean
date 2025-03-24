-- Task 1
def main₀: IO Unit := do
  IO.print "Enter a sentence : "
  let stdint ← IO.getStdin
  let input ← stdint.getLine
  let words := (input.splitOn).filter λ x => x ≠ ""
  IO.println s!"Sentence contains {words.length} words."


-- Task 2
def kleisli [Monad t] (f: α → t β) (g: β → t γ) : (α → t γ) :=
  λ x => f x >>= g

#check Function.comp


-- Task 3
instance : Monad List where
  pure x := [ x ]
  bind xs f := List.flatten (Functor.map f xs )

def join [Monad t] (x: t (t α)) : t α :=
  x >>= id


#eval join [[1, 2], [3, 4]]


-- Task 4
def binaryLists (n : Nat) : List (List Nat) :=
  match n with
  | 0 => pure []
  | .succ m => do
    let prev ← binaryLists m
    let curr ← id [0, 1]
    pure (curr :: prev)


#eval binaryLists 0
#eval binaryLists 2


-- Task 5
def addIO : IO (Option Int) := do
  IO.print "Please enter 1. numer: "
  let input ← IO.getStdin >>= IO.FS.Stream.getLine
  let number₁ := (String.trim input).toInt?
  IO.print "Please enter 2. numer: "
  let input ← IO.getStdin >>= IO.FS.Stream.getLine
  let number₂ := (String.trim input).toInt?
  match number₁, number₂ with
  | some x, some y => pure (some (x + y))
  | _, _ => pure (none)


def main : IO Unit := do
  let stdout <- IO.getStdout
  let add <- addIO
  stdout.putStrLn s!"{ add }"

