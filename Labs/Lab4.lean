-- Test
def foldl' (f: β → α → β) (init: β) (xs: List α) : β :=
  match xs with
  | [] => init
  | x :: xs => foldl' f (f init x) xs


#eval foldl' (. + .) 0 [1, 2, 3]

-- Task 1
def add' : Nat × Nat → Nat :=
  fun (x , y ) => x + y

#check add'
#eval add' (2, 3)

def curry (f : α × β → γ) : α → β → γ :=
  λ x y => f (x, y)

#check curry add'
#eval curry add' 2 3

def uncurry (f : α → β → γ) : α × β → γ :=
  λ (x, y) => f x y

#check uncurry (curry add')
#eval uncurry (curry add') (2, 3)

-- Task 2
def andl : List Bool → Bool :=
  λ xs => List.foldr Bool.and true xs

#eval andl [true, true, false]
#eval andl [true, true, true]
#eval andl []

-- Task 3
def mull : List Nat → Nat :=
  λ xs => List.foldr (. * .) 1 xs

#eval mull [1, 2, 3, 4]

-- Task 4
def mul10 (xs: List Nat) : List Nat :=
  match xs with
  | [] => []
  | x :: xs => if Nat.mod x 10 == 0 then x :: mul10 xs else mul10 xs

#eval mul10 [0, 1, 2, 3, 10, 20]

-- Task 5
def ignore_lowerCaseVowels (s : String) : String :=
  let rec removeVowels (xs : List Char) : List Char :=
    match xs with
    | [] => []
    | x :: xs =>
      if x = 'a' || x = 'e' || x = 'i' || x = 'o' || x = 'u' then
        removeVowels xs
      else
        x :: removeVowels xs
  List.asString (removeVowels s.toList)


#eval ignore_lowerCaseVowels "the cat who saw the moon ."
#eval ignore_lowerCaseVowels "the cat who sAw the moon ."

-- Task 6
  





