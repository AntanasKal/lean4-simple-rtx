import «Vec3»

def min_rand_nat := 0
def max_rand_nat := 100000000000000

def random_float_unit : IO Float := do
  let nat ←  IO.rand min_rand_nat max_rand_nat;
  pure ((nat-min_rand_nat).toFloat / (max_rand_nat-min_rand_nat).toFloat)

def random_float (min max : Float) : IO Float :=
  do 
    let unit ← random_float_unit;
    pure (min + (max-min)*unit)

def clamp (x min max : Float) :=
  if x < min then min
  else if x > max then max
  else x



def generate_rand_vec3_in_cube (min max : Float) : IO vec3 :=
  do
    let x ← random_float min max; 
    let y ← random_float min max;
    let z ← random_float min max;
    pure (vec3.mk x y z)

def generate_rand_vec3_in_unit_disc (n : Nat) : IO vec3 :=
  match n with
  | 0 => pure (vec3.mk 1 0 0)
  | nn+1 =>
    do
      let x ← random_float (-1.0) (1.0)
      let y ← random_float (-1.0) (1.0)
      let some_vec := (vec3.mk x y 0)
      if some_vec.length_squared ≤ 1 then
        pure some_vec
      else
        generate_rand_vec3_in_unit_disc nn

def max_tries := 1000

def generate_rand_vec3_inside_sphere (n : Nat) : IO vec3 :=
  match n with
  | 0 => pure (vec3.mk 0 0 0)
  | nn+1 => 
    do
      let some_vec ← generate_rand_vec3_in_cube (-1) 1;
      if some_vec.length_squared <= 1 then pure some_vec
      else generate_rand_vec3_inside_sphere nn

def generate_rand_vec3_unit (n : Nat) : IO vec3 :=
  do
    let v ← generate_rand_vec3_inside_sphere n;
    pure v.unit_vector


-- def generate_rand_vecs (n : Nat) : IO (List (vec3 × Float)) :=
--   match n with
--   | 0 => pure []
--   | nn+1 => do
--     -- let v ← generate_rand_vec3_in_sphere max_tries;
--     let v ← generate_rand_vec3_inside_sphere max_tries;
--     let fl ← random_float_unit
--     pure ((v, fl)::(← generate_rand_vecs nn))
--     -- pure (v.unit_vector::(← generate_rand_vecs nn))
--     -- pure (v::(← generate_rand_vecs nn))


-- def generate_rand_array (n depth : Nat) : IO (List (vec3 × Float × Float × (List (vec3 × Float)))) :=
--   match n with
--   | 0 => pure ([])
--   | nn+1 => do 
--     let defocus_vec ← generate_rand_vec3_in_unit_disc max_tries;
--     let a ← random_float_unit;
--     let b ← random_float_unit;
--     let c ← generate_rand_vecs depth;
--     pure ((defocus_vec,a,b, c) :: (← generate_rand_array nn depth))

