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
  le : α → α → Prop
  refl : le a a
  trans : le a b → le b c → le a c


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

instance [Add α] [Mul α] [LT α] [DecidableLT α] : Ord (AExpr α) where
  compare x y :=
  if eval x < eval y then
    Ordering.lt
  else if eval x > eval y then
    Ordering.gt
  else
    Ordering.eq


-- Task 5
instance : Coe Bool Int where
  coe b :=
    match b with
    | .true => 1
    | .false => 0

instance : Coe Int Bool where
  coe i :=
    match i with
    | .ofNat _ => true
    | .negSucc _ => false

