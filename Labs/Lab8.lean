-- Task 1
def fin_2_bool (v: Fin 2) : Bool :=
  match v with
  | 0 => false
  | 1 => true

def bool_2_fin (v: Bool) : Fin 2 :=
  match v with
  | false => 0
  | true => 1

#check ( fin_2_bool ∘ bool_2_fin ) true
#check ( fin_2_bool ∘ bool_2_fin ) false
#check ( bool_2_fin ∘  fin_2_bool) 0
#check ( bool_2_fin ∘  fin_2_bool) 1


-- Task 2
inductive Vect (α : Type ) : ( n : Nat ) → Type where
  | nil : Vect α 0
  | cons : α → Vect α n → Vect α ( n + 1)

def vect_to_string [ToString α] : Vect α n → String
  | Vect.nil => ""
  | Vect.cons x Vect.nil => toString x
  | Vect.cons x xs => toString x ++ ", " ++ vect_to_string xs

instance [ToString α] : ToString (Vect α n) where
  toString v := "[" ++ vect_to_string v ++ "]"

def Vect.map (f: α → β) (v: Vect α n) : Vect β n :=
  match v with
  | nil => nil
  | cons x nil => cons (f x) nil
  | cons x xs => cons (f x) (Vect.map f xs)

def vect1 : Vect Nat 2 := Vect.cons 1 (Vect.cons 2 Vect.nil)

#eval Vect.map (. + 1) vect1

def Vect.unzip (v: Vect (α × β) n) : Vect α n × Vect β n :=
  match v with
  | nil => (nil, nil)
  | cons (x, y) nil => (cons x nil, cons y nil)
  | cons (x, y) vs => (cons x (unzip vs).fst, cons y (unzip vs).snd) 


def vect2 : Vect (Nat × Bool) 2 := Vect.cons (0, false) (Vect.cons (1, true)  Vect.nil)

#eval Vect.unzip vect2

def Vect.snoc (e: α) (v: Vect α n) : Vect α (n + 1) :=
  match v with
  | nil => cons e nil
  | cons x xs => cons x (snoc e xs)

#eval Vect.snoc 4 vect1

def Vect.reverse (v: Vect α n) : Vect α n :=
  match v with
  | nil => nil
  | cons x xs => snoc x (reverse xs)

#eval Vect.reverse vect1


-- Task 3
def as_top (v : Nat) : Fin (v + 1) :=
  Fin.mk v (Nat.lt_succ_self v)

#check as_top 0


-- Task 4
def ind_pair (p: (α × β)) : Σ _ : α, β :=
  ⟨p.fst, p.snd⟩

#eval ind_pair (true, 3)


-- Task 5
def vect_to_list (v: Vect α n) : List α :=
  match v with
  | .nil => []
  | .cons x xs => [x] ++ vect_to_list xs

#eval vect_to_list vect1
#eval vect_to_list vect2

def list_to_vect (xs: List α) : Vect α xs.length :=
  match xs with
  | [] => Vect.nil
  | x :: xs => Vect.cons x (list_to_vect xs)

#check list_to_vect [1, 2, 3]
