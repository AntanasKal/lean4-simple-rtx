import «Colour»

inductive material where
  | lambertian (albedo : colour) : material
  | metal (albedo : colour) (fuzz : Float) : material
  | dialectric (index_of_refraction : Float) : material
deriving Repr
