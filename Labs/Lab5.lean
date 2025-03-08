-- Task 1
inductive Shape : Type where
  | circ : ( radius : Float ) → Shape
  | rect : ( width height : Float ) → Shape
  | isos : ( base height : Float ) → Shape

instance : ToString Shape where
  toString s :=
    match s with
    | .circ r => "Circle with radius " ++  r.toString
    | .rect w h => "Rectangle with width " ++ w.toString ++ " and height " ++ h.toString
    | .isos b h => "Triangle with base " ++ b.toString ++ " and height " ++ h.toString

#eval toString (Shape.isos 3 4)
#eval toString (Shape.rect 2 3)
#eval toString (Shape.circ 5)


-- Task 2
instance [BEq α] : BEq (List α) where
  beq x y := List.all x (λ z => List.elem z y)

#eval [1, 2, 3] == [3, 2, 1]
#eval [1, 2, 3] == [1, 2, 3, 3]
#eval [1, 2, 3] == [1, 2, 3]
#eval [1, 2, 3] == [1, 2, 4]


-- Task 3
class PreOrd (α : Type) where
  leq : α → α → Bool

instance divides : PreOrd Int where
  leq x y :=
    x % y == 0

#eval PreOrd.leq (Int.ofNat 2) (Int.ofNat 2)
#eval PreOrd.leq (Int.ofNat 12) (Int.ofNat 2)

-- Task 4
inductive AExpr (α : Type) : Type
| V : α → AExpr α
| Plus : AExpr α → AExpr α → AExpr α
| Times : AExpr α → AExpr α → AExpr α

def eval [Add α] [Mul α] (exp : AExpr α) : α :=
  match exp with
  | .V x => x
  | .Plus x y  =>
    let a := eval x
    let b := eval y
    a + b
  | .Times x y =>
    let a := eval x
    let b := eval y
    a * b

instance ord [Add α] [Mul α] [Ord α] : Ord (AExpr α) where
  compare x y := compare (eval x) (eval y)

instance [Ord (AExpr α)] : LE (AExpr α) := leOfOrd
instance [Ord (AExpr α)] : LT (AExpr α) := ltOfOrd
instance [Ord (AExpr α)] : Max (AExpr α) := maxOfLe

open AExpr
#eval (V 2) < (V 3)
#eval (Plus (V 2) (V 1)) < (V 3)
#eval  max (Plus (V 2) (V 4)) (V 3)


-- Task 5
instance : Coe Bool Int where
  coe b :=
    match b with
    | .true => 1
    | .false => 0

#eval (true : Int)
#eval (false : Int)

instance : Coe Int Bool where
  coe i :=
    match i with
    | 0 => false
    | .ofNat _ => true
    | .negSucc _ => false

#eval (Int.ofNat 5 : Bool)
#eval (Int.ofNat 0  : Bool)
#eval (Int.negOfNat 5  : Bool)


