-- import «OneWeekend»

import «Generators»
import «Vec3»
import «Ray»
import «Scatter»
import «Hittables»


def infinity : Float := 100000000000000000000000000000.0
def shadow_acne_bound : Float := 0.001
def pi : Float := 3.1415926535897932385


def aspect_ratio : Float := 3.0/2.0
def image_width : Nat := 500
def image_height : Nat := (image_width.toFloat / aspect_ratio).toUInt32.toNat
def max_range : Float := 255.999

def samples_per_pixel := 80
def max_depth := 30


structure camera where
  origin : point3
  horizontal : vec3
  vertical : vec3
  lower_left_corner : vec3
  u : vec3
  v : vec3
  w : vec3
  lens_radius : Float
deriving Repr

def camera.create (lookfrom lookat: point3) (vup : vec3) (vfov aspect_ratio aperture focus_dist: Float) : camera :=
  let theta : Float := vfov
  let h : Float := Float.tan (theta/2)
  let viewport_height : Float := 2.0 * h
  let viewport_width : Float := aspect_ratio * viewport_height
  let w := (lookfrom.coord-lookat.coord).unit_vector
  let u := (vup.cross w).unit_vector
  let v := w.cross u

  let origin : point3 := lookfrom
  let horizontal : vec3 := focus_dist * viewport_width * u
  let vertical : vec3 := focus_dist * viewport_height * v
  let lower_left_corner : vec3 := origin.coord - (horizontal/2.0) - (vertical/2.0) - (w*focus_dist)
  let lens_radius := aperture/2.0
  {origin, horizontal, vertical, lower_left_corner, u, v, w, lens_radius}


def camera.get_ray (cam : camera) (defocus_vec : vec3) (s t : Float) : ray :=
  let rd := cam.lens_radius * defocus_vec;
  let offset := cam.u*rd.x + cam.v * rd.y
  let orig := point3.mk (cam.origin.coord + offset)
  ray.mk orig (cam.lower_left_corner+ (s*cam.horizontal) +t*cam.vertical - cam.origin.coord - offset) 



def colour.write (col : colour) : IO Unit :=
  let r := (col.r / samples_per_pixel.toFloat).sqrt
  let g := (col.g / samples_per_pixel.toFloat).sqrt
  let b := (col.b / samples_per_pixel.toFloat).sqrt
  let ir : UInt32 := ((clamp r 0 0.999) * max_range).toUInt32
  let ig : UInt32 := ((clamp g 0 0.999) * max_range).toUInt32
  let ib : UInt32 := ((clamp b 0 0.999) * max_range).toUInt32
  do
    (← IO.getStdout).putStrLn s!"{ir} {ig} {ib}"


def ray_colour_io (r : ray) (world : List hittable) (depth : Nat): IO colour :=
  do
  match depth with
  | 0 => pure $ colour.mk 0 0 0
  | nn+1 =>
    let record? := hitlist none r world shadow_acne_bound infinity
    match record? with
    | some record =>
        let vc ← generate_rand_vec3_inside_sphere max_tries
        let fl ← random_float_unit
        let scatter_result := record.mat.scatter (vc, fl) r record;
        match scatter_result with
        | none => pure $ colour.mk 0 0 0
        | some (attenuation, scattered) =>
          pure $ attenuation * (← ray_colour_io scattered world nn)
    | none => 
      let t := 0.5*(r.dir.unit_vector.y + 1);
      pure { r := 1-t + 0.5*t,
             g := 1-t + 0.7*t,
             b := 1-t + t
            }


def ray_colour (r : ray) (world : List hittable) (random_seed : List (vec3 × Float)): colour :=
  match random_seed with
  | [] => colour.mk 0 0 0
  | v::vs =>
    let record? := hitlist none r world shadow_acne_bound infinity
    match record? with
    | some record =>
        let scatter_result := record.mat.scatter v r record;
        match scatter_result with
        | none => colour.mk 0 0 0
        | some (attenuation, scattered) =>
          attenuation * (ray_colour scattered world vs)
    | none => 
      let t := 0.5*(r.dir.unit_vector.y + 1);
      { r := 1-t + 0.5*t,
        g := 1-t + 0.7*t,
        b := 1-t + t
      }

def lookfrom : point3 := (point3.mk (vec3.mk (13) 2 3))
def lookat : point3 := (point3.mk (vec3.mk 0 0 (0)))
def vup : vec3 := vec3.mk 0 1 0
def dist_to_focus := 10.0
def aperture := 0.1
def cam : camera := camera.create
  lookfrom
  lookat
  vup
  (pi/9.0)
  aspect_ratio
  aperture
  dist_to_focus

def get_colour_sum_io (world : List hittable) (col_sum : colour) (i j : Nat) (samples depth : Nat) : IO colour :=
  match samples with
  | 0 => pure col_sum
  | nn+1 =>
    do
      let defocus_vec ←  generate_rand_vec3_in_unit_disc max_tries;
      let x1 ← random_float_unit;
      let x2 ← random_float_unit;
      let u := (i.toFloat + x1) / (image_width.toFloat - 1);
      let v := (j.toFloat + x2) / (image_height.toFloat - 1);
      let r : ray := cam.get_ray defocus_vec u v
      let r_col ←  ray_colour_io r world depth
      pure (← get_colour_sum_io world (col_sum.add r_col) i j nn depth)



def get_colour_sum (world : List hittable) (col_sum : colour) (i j : Nat) (rand_array : List (vec3 × Float × Float × (List (vec3 × Float)))) : colour :=
  match rand_array with
  | [] => col_sum
  | (defocus_vec,x1,x2,array)::xs => 
    let u := (i.toFloat + x1) / (image_width.toFloat - 1);
    let v := (j.toFloat + x2) / (image_height.toFloat - 1);
    let r : ray := cam.get_ray defocus_vec u v
    let r_col : colour := ray_colour r world array
    (get_colour_sum world (col_sum.add r_col) i j xs)

def print_gradient (world : List hittable) (i : Nat) (j : Nat) : IO Unit :=
  do 
    -- let array_tuples ← generate_rand_array (samples_per_pixel) max_depth;
    -- let col_sum := get_colour_sum world (colour.mk 0 0 0) i j array_tuples
    let col_sum ←  get_colour_sum_io world (colour.mk 0 0 0) i j samples_per_pixel max_depth
    col_sum.write


def print_picture_line (world : List hittable) (i j : Nat) : IO Unit :=
  do
    print_gradient world (image_width - i - 1) j
    match i with
    | 0 => pure ()
    | ii+1 => print_picture_line world ii j

def print_picture (world : List hittable) (i j : Nat) : IO Unit :=
  do
    (← IO.getStderr).putStrLn s!"\r Scanlines remaining: {j}"
    print_picture_line world i j
    match j with
    | 0 => pure ()
    | jj+1 => print_picture world (image_width-1) jj
    
  
def generate_world : IO (List hittable) :=
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


def main : IO Unit := do
  let stdout ← IO.getStdout;
  stdout.putStrLn "P3";
  stdout.putStrLn s!"{image_width} {image_height}";
  stdout.putStrLn s!"255"
  let world ← generate_world
  (←  IO.getStderr).putStrLn s!"{world.length}"
  print_picture world (image_width-1) (image_height-1)
