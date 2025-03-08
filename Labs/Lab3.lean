-- Task 1
def swapPair : Prod α β → Prod β α
  | (x, y) => (y, x)

#eval swapPair ("a", 3)

-- Task 2
def swapSum : Sum α β → Sum β α
  | .inl x => .inr x
  | .inr y => .inl y

#eval swapSum (.inl 1 : Nat ⊕ String)
#eval swapSum (.inr "a" : Nat ⊕ String)

-- Task 3
def reverseList (l :List α): List α :=
  match l with
  | [] => []
  | e :: l => reverseList l ++ [e]

#eval reverseList [1,2,3]

-- Task 4
inductive Tree (α : Type ) where
| leaf : Tree α
| node : Tree α → α → Tree α → Tree α

def t1: Tree Nat := .node (.node .leaf 1 (.node .leaf 3 .leaf)) 5 .leaf
def t2: Tree Nat := .node (.node (.node .leaf 1 .leaf) 2 (.node .leaf 3 .leaf)) 4 (.node (.node .leaf 5 .leaf) 6 (.node .leaf 7 .leaf))

-- Task 5
def size : Tree α → Nat
  | .leaf => 0
  | .node l₁ _ l₂ => (size l₁) + 1 + (size l₂)

#eval size t1
#eval size t2

-- Task 6
def n_to_lu : Nat -> List Unit
  | 0 => []
  | .succ s => [()] ++ n_to_lu s


def lu_to_n : List Unit -> Nat
  | [] => 0
  | _ :: l => 1 + lu_to_n l


#eval lu_to_n (n_to_lu 42)
#eval n_to_lu (lu_to_n [(), (), ()])

#eval n_to_lu 10
