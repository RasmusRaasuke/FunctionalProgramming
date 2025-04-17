-- Third Example : Less Or Equal Than
-- p ≤ n
inductive Leq : (p : Nat) → (n : Nat) → Type where
  | LeqZ : Leq 0 n
  | LeqS : Leq p n → Leq (p + 1) (n + 1)
open Leq


-- create notation for Leq as ≤'
notation p " ≤' " n => Leq p n


-- Task 1
def leTrans : {m n p : Nat} → (m ≤' n) → (n ≤' p) → (m ≤' p) :=
  fun h₁ h₂ =>
    match h₁, h₂ with
    | LeqZ, _ => LeqZ
    | LeqS h₁', LeqS h₂' => LeqS (leTrans h₁' h₂')


-- Task 2
def succLarger : {n : Nat} → n ≤' (n + 1)
  | 0 => LeqZ
  | .succ _ => LeqS succLarger


#check succLarger (n := 3)


-- Task 3
def leWeakRight : {m n : Nat} → (h : m ≤' n) → m ≤' (n + 1) :=
  fun h =>
    match h with
    | LeqZ => LeqZ
    | LeqS i => LeqS (leWeakRight i)

#check leWeakRight (LeqS (LeqS LeqZ))

def leWeakLeft : {m n : Nat} → (h : (m + 1) ≤' n) → m ≤' n :=
  fun h =>
    match h with
    | LeqS i => leWeakRight i

#check leWeakLeft (LeqS (LeqS (LeqS LeqZ)))


-- Task 4
def zeroPlusLeft : {m n : Nat} → (0 + n) ≤' m + n :=
  fun {m n} =>
    match n with
    | 0 => LeqZ
    | .succ k => LeqS (zeroPlusLeft (m:=m) (n:=k))

#check zeroPlusLeft (m:=3) (n:=2)
#reduce zeroPlusLeft (m:=3) (n:=2)


-- Task 5
def listLengthLeqSucc : {xs ys : List α} → (List.length xs ≤' List.length ys) → List.length (x :: xs) ≤' List.length (y :: ys) :=
    fun h => LeqS h
  





















