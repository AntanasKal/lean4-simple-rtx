import «Vec3»
import «Raytracing»

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


def generate_world_final_scene : IO (List hittable) :=
  do
    let mut world := []
    for a in [0: 22] do
      for b in [0: 22] do
        let choose_mat ← random_float_unit
        let center : point3 :=  point3.mk ( vec3.mk (a.toFloat-11+0.9*(← random_float_unit)) 0.2 (b.toFloat-11+0.9*(← random_float_unit)))
        if (center.coord - (vec3.mk 4 0.2 0)).length > 0.9 then
          if choose_mat < 0.8 then
            let rand_colour := colour.mk (← random_float_unit) (← random_float_unit) (← random_float_unit)
            let albedo := rand_colour * rand_colour
            let mat := material.lambertian albedo
            world := world.cons $ hittable.sphere center 0.2 mat
            
          else if choose_mat < 0.95 then
            let albedo := colour.mk (← random_float 0.5 1) (← random_float 0.5 1) (← random_float 0.5 1)
            let fuzz ← random_float 0 0.5
            let mat := material.metal albedo fuzz
            world := world.cons $ hittable.sphere center 0.2 mat
            
          else
            let mat := material.dialectric 1.5
            world := world.cons $ hittable.sphere center 0.2 mat
            
        else
          pure ()
    let material_ground : material := material.lambertian $ colour.mk 0.5 0.5 0.5
    world := world.cons $ hittable.sphere (point3.mk (vec3.mk     0  (-1000) (0))) 1000 material_ground

    let material1 : material := material.dialectric $ 1.5
    world := world.cons $ hittable.sphere (point3.mk (vec3.mk 0 (1) (0))) 1 material1
    
    let material2 : material := material.lambertian $ colour.mk 0.4 0.2 0.1
    world := world.cons $ hittable.sphere (point3.mk (vec3.mk (-4) (1) (0))) 1 material2

    let material3 : material := material.metal (colour.mk 0.7  0.6  0.5) 0.0
    world := world.cons $ hittable.sphere (point3.mk (vec3.mk (4) (1) (0))) 1 material3

    pure world



def generate_world_another_scene : IO (List hittable) :=
  do
    let mut world := []
    for a in [0: 22] do
      for b in [0: 22] do
        let choose_mat ← random_float_unit
        let center : point3 :=  point3.mk ( vec3.mk (a.toFloat-11+0.9*(← random_float_unit)) 0.2 (b.toFloat-11+0.9*(← random_float_unit)))
        if (center.coord - (vec3.mk 4 0.2 0)).length > 0.9 then
          if choose_mat < 0.8 then
            let rand_colour := colour.mk (← random_float_unit) (← random_float_unit) (← random_float_unit)
            let albedo := rand_colour * rand_colour
            let mat := material.lambertian albedo
            world := world.cons $ hittable.sphere center 0.2 mat
            
          else if choose_mat < 0.95 then
            let albedo := colour.mk (← random_float 0.5 1) (← random_float 0.5 1) (← random_float 0.5 1)
            let fuzz ← random_float 0 0.5
            let mat := material.metal albedo fuzz
            world := world.cons $ hittable.sphere center 0.2 mat
            
          else
            let mat := material.dialectric 1.5
            world := world.cons $ hittable.sphere center 0.2 mat
            
        else
          pure ()
    let material_ground : material := material.lambertian $ colour.mk 0.5 0.5 0.5
    world := world.cons $ hittable.sphere (point3.mk (vec3.mk     0  (-1000) (0))) 1000 material_ground

    let material1 : material := material.dialectric $ 1.5
    world := world.cons $ hittable.sphere (point3.mk (vec3.mk 0 (1) (0))) 1 material1
    world := world.cons $ hittable.sphere (point3.mk (vec3.mk 0 (3) (0))) 1 material1
    
    let material2 : material := material.lambertian $ colour.mk 0.4 0.2 0.1
    world := world.cons $ hittable.sphere (point3.mk (vec3.mk (-4) (1) (0))) 1 material2
    world := world.cons $ hittable.sphere (point3.mk (vec3.mk (-4) (3) (0))) 1 material2

    let material3 : material := material.metal (colour.mk 0.7  0.6  0.5) 0.0
    world := world.cons $ hittable.sphere (point3.mk (vec3.mk (4) (1) (0))) 1 material3
    world := world.cons $ hittable.sphere (point3.mk (vec3.mk (4) (3) (0))) 1 material3

    pure world

