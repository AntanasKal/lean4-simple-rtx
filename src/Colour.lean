structure colour where
  r : Float
  g : Float
  b : Float
deriving Repr

def colour.add (x y : colour) : colour :=
  {r := x.r + y.r,
   g := x.g + y.g,
   b := x.b + y.b}

instance : HMul Float colour colour where
  hMul t col := colour.mk (t*col.r) (t*col.g) (t*col.b)

instance : HMul colour colour colour where
  hMul col1 col2 := colour.mk (col1.r*col2.r) (col1.g*col2.g) (col1.b*col2.b)

