-- Task 1 --
def xor' : Bool → Bool → Bool
  | .true, .false => .true
  | .false, .true => .true
  | _, _ => .false

#eval xor true false
#eval xor true true


-- Task 2 --
inductive Prob : Type where
  | Definately
  | Likely
  | Doubtful
  | Impossible


-- Task 3 --
namespace Lab2
open Prob
def and : Prob → Prob → Prob
 | Definately, p => p
 | p, Definately => p
 | Impossible, _ => Impossible
 | _, Impossible => Impossible
 | Likely, Likely => Likely
 | _, _ => Doubtful

#eval and Definately Doubtful


-- Task 4 --
def factorial' : Nat → Nat
  | 0 => 1
  | .succ n => factorial' n * .succ n

#eval factorial' 5


-- Task 5 --
def mul (n m : Nat) : Nat :=
  match n with
  | 0 => 0
  | .succ n => mul n m + m

#eval mul 3 10


-- Task 6 --
inductive AExp : Type where
  | Num : Nat → AExp
  | Sum : AExp → AExp → AExp
  | Mul : AExp → AExp → AExp

def eval_math : AExp → Nat
  | .Num n => n
  | .Sum n m => eval_math n + eval_math m
  | .Mul n m => eval_math n * eval_math m

#eval eval_math (.Mul (.Sum (.Num 1) (.Num 2)) (.Num 3))


-- Task 7 --
structure Point where
  ( x y : Float )

structure Segment where
  ( start_p end_p : Point )

def length (s : Segment) : Float :=
  Float.sqrt ((s.end_p.x - s.start_p.x)^2 + (s.end_p.y - s.start_p.y)^2)

#eval length ⟨⟨0.0, 0.0⟩, ⟨3.0, 4.0⟩⟩


