{-
data Nat = Z | S Nat

plus : Nat -> Nat -> Nat
plus Z y = y 
plus (S x) y = S (plus x y)
-}

data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : a -> Vect k a -> Vect (S k) a

%name Vect xs, ys, zs

append : Vect n a -> Vect m a -> Vect (n + m) a

map : (a -> b) -> Vect n a -> Vect n b

zipWith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c

transpose_vec : Vect n (Vect m a) -> Vect m (Vect n a)

