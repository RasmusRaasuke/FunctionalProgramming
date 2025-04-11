-- Task 1
def ary_op : Nat → Type → Type
  | 0, α       => α
  | .succ n, α => α → ary_op n α

def majority : ary_op 3 Bool :=
  λ a b c =>
    if a = b then 
      a
    else if a = c then 
      a
    else 
      b

#eval majority true false false
#eval majority true false true


-- Task 2
#print List.filter

def list_majority_helper (xs: List Bool) (count: Int) : Bool :=
  match xs, count with
  | [], count => if count > 0 then true else false
  | x :: xs, count =>
    let new_count := if x then count + 1 else count - 1
    list_majority_helper xs new_count

def list_majority (xs: List Bool) : Bool :=
  list_majority_helper xs 1


#eval list_majority [ true , false ]
#eval list_majority [ true , false , false ]


-- Task 3
def as_fin (n m : Nat) : Option (Fin m) :=
  if h : n < m then
    some ⟨n, h⟩
  else
    none

#check as_fin 2 4
#check as_fin 2 3
#eval as_fin 2 3
#check as_fin 2 2
#eval as_fin 2 2


-- Task 4
def is_even: Nat → Bool
  | 0 => true
  | 1 => false
  | n + 2 => is_even n

#eval is_even 5
#eval is_even 6

inductive Even : Type where
  | zero : Even
  | succ : Even → Even


-- Task 5
inductive Sorted : List Nat → Prop where
  | nil : Sorted []
  | single : Sorted [n]
  | cons (n m : Nat) (xs : List Nat) (h1 : n ≤ m) (h2 : Sorted (m :: xs)) : Sorted (n :: m :: xs) 

