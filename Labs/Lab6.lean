-- Task 1
def HPair (α : Type ) : Type := α × α

def HPair.map (f : α → β) (hp : HPair α) : HPair β :=
  match hp with
  | (x, y) => ((f x), (f y))

#eval HPair.map id (2, 3)
#eval HPair.map ((2 * .) ∘ (. + 1)) (2, 3)
#eval HPair.map (2 * .) (HPair.map (. + 1) (2, 3))

instance : Functor HPair where
  map := HPair.map

-- Task 2
instance {α: Type} : Functor (α → .) where
  map {β γ: Type} (f: β → γ) (g: α → β) := λ x => f (g x)


-- Task 3
class Monoid (α : Type ) where
  add : α → α → α
  zero : α

instance : Monoid Nat where
  add := Nat.add
  zero := 0

instance : Monoid String where
  add := String.append
  zero := ""

#eval Monoid.add (α := Nat) 0 5
#eval Monoid.add (α := Nat) 5 Monoid.zero

#eval Monoid.add (α := String) Monoid.zero "hello"
#eval Monoid.add (α := String) "hello" ""

-- Task 4
def accumulate {α: Type} [Monoid α] (xs: List α) : α :=
  match xs with
  | [] => Monoid.zero
  | x :: ys => Monoid.add x (accumulate ys)

#eval accumulate [1, 2, 3, 4]

-- Task 5
def List.seq (lf : List (α → β)) (la : Unit → List α) : List β :=
  match lf, la () with
  | f :: fs, as =>
      (f <$> as) ++ (seq fs (λ _ => as))
  | _, _ => []

instance : Applicative List where
  pure x := [x]
  seq := List.seq

def liftA2 [Applicative t] (f : α → β → γ) (ta : t α) (tb : t β) : t γ :=
  f <$> ta <*> tb

#eval liftA2 Nat.add [1,2] [3,4]

-- Task 6
instance : Monad Option where
  bind oa f :=
    match oa with
    | none => none
    | some a => f a

-- Task 7
def safediv ( n m : Nat ) : Option Nat :=
  if m == 0 then
    none
  else
    some ( n / m )

def divdiv (p q r : Nat): Option Nat :=
  safediv p q >>= λ result => safediv result r

#eval divdiv 12 2 3
#eval divdiv 12 2 0
#eval divdiv 12 0 2
















