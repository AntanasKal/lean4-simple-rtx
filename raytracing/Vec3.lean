-- Types and functions for basic 3d vector algebra

structure vec3 where
  x : Float
  y : Float
  z : Float
deriving Repr

def fmin (a b : Float) : Float :=
  if a > b then b else a


def vec3.add (a b : vec3) : vec3 :=
  {x := a.x+b.x, y:= a.y+b.y, z := a.z+b.z}

def vec3.cross (u v : vec3) : vec3 :=
  let x := u.y*v.z - u.z*v.y
  let y := u.z*v.x - u.x*v.z
  let z := u.x*v.y - u.y*v.x
  {x, y, z}

instance : HAdd vec3 vec3 vec3 where
  hAdd := vec3.add

def vec3.neg (a : vec3) : vec3 :=
  {x := -a.x, y:= -a.y, z := -a.z}

instance : Neg vec3  where
  neg := vec3.neg

instance : HSub vec3 vec3 vec3  where
  hSub a b:= vec3.add a b.neg

def vec3.scalar_mult (a : vec3) (t : Float) : vec3 :=
  {x := t*a.x, y:= t*a.y, z := t*a.z}

instance : HMul vec3 Float vec3 where
  hMul := vec3.scalar_mult

instance : HMul Float vec3 vec3 where
  hMul t a := vec3.scalar_mult a t

def vec3.scalar_div (a : vec3) (t : Float) : vec3 :=
  vec3.scalar_mult a (1/t)

instance : HDiv vec3 Float vec3 where
  hDiv := vec3.scalar_div

def vec3.length_squared (a : vec3) : Float :=
  (a.x*a.x) + (a.y*a.y) + (a.z*a.z)

def vec3.length (a : vec3) : Float :=
  Float.sqrt (a.length_squared)

def vec3.dot (a b : vec3) : Float :=
  a.x*b.x + a.y*b.y +a.z*b.z

instance : HMul vec3 vec3 Float where
  hMul := vec3.dot

def vec3.unit_vector (a : vec3) : vec3 :=
  vec3.scalar_div a (vec3.length a)

def near_zero_threshold := 1e-8
def vec3.near_zero (v : vec3) : Bool :=
  (v.length < near_zero_threshold)


structure point3 where
  coord : vec3
deriving Repr
