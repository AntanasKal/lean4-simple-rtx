
import «RandUtils»
import «Vec3»
import «Raytracing»
import «Camera»

def infinity : Float := 10e50
def shadow_acne_bound : Float := 10e-3
def pi : Float := 3.1415926535897932385


def aspect_ratio : Float := 3.0/2.0
def image_width : Nat := 1200
def image_height : Nat := (image_width.toFloat / aspect_ratio).toUInt32.toNat
def max_range : Float := 255.999

def samples_per_pixel := 240
def max_depth := 30


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
      pure $ (1-t)*(colour.mk 1 1 1) + t*(colour.mk 0.5 0.7 1)
      

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


def print_gradient (world : List hittable) (samples : Nat) (i j: Nat) : IO Unit :=
  do 
    let col_sum ←  get_colour_sum_io world (colour.mk 0 0 0) i j samples max_depth
    col_sum.write


def print_picture_line (world : List hittable) (i j : Nat) : IO Unit :=
  do
    print_gradient world samples_per_pixel (image_width - i - 1) j
    match i with
    | 0 => pure ()
    | ii+1 => print_picture_line world ii j

def get_picture_line (world : List hittable) (samples : Nat) (i j : Nat) : IO (List colour) := do
  let col_sum ← get_colour_sum_io world (colour.mk 0 0 0) (image_width - i - 1) j samples max_depth;
  match i with
  | 0 => pure [col_sum]
  | ii+1 => pure $ col_sum :: (← get_picture_line world samples ii j)

def print_line (col_list : List colour) : IO Unit :=
  match col_list with
  | [] => pure ()
  | (x::xs) => do x.write; print_line xs


def print_picture (world : List hittable) (i j : Nat) : IO Unit :=
  do
    (← IO.getStderr).putStrLn s!"\r Scanlines remaining: {j}"
    print_picture_line world i j
    match j with
    | 0 => pure ()
    | jj+1 => print_picture world (image_width-1) jj


def sum_colour_lists (list_of_lists : List (List colour)) : (List colour) :=
  match list_of_lists with
  | [] => []
  | [ls1] => ls1
  | (ls1::ls2::[]) => (List.zipWith colour.add ls1 ls2)
  | (ls1::ls2::lss) => List.zipWith colour.add (List.zipWith colour.add ls1 ls2) (sum_colour_lists lss)


def print_picture_multithreaded (world : List hittable) (number_of_threads : Nat) (width row : Nat) : IO Unit :=
  do
    (← IO.getStderr).putStrLn s!"\r Scanlines remaining: {row}"
    let mut taskList : List ( Task (Except IO.Error (List colour)) ):= []
    for _ in [0:number_of_threads]
      do 
        let a_task ←  (get_picture_line world (samples_per_pixel/number_of_threads) width row).asTask
        taskList := (a_task:: taskList)
    
    let mut line_list : List (List colour) := []
    for i in [0:number_of_threads]
      do
        let tsk? := List.get? taskList i
        match tsk? with
        | some tsk => 
            let task_result := tsk.get
            match task_result with
            | Except.error a => pure ()
            | Except.ok result =>
                line_list := (result :: line_list)
        | none     => pure ()

    let line := sum_colour_lists line_list
    print_line line
    match row with
    | 0 => pure ()
    | jj+1 => print_picture_multithreaded world number_of_threads width jj

def num_threads := 8

def main : IO Unit := do
  let stdout ← IO.getStdout;
  stdout.putStrLn "P3";
  stdout.putStrLn s!"{image_width} {image_height}";
  stdout.putStrLn s!"255"
  let world ← generate_world_final_scene
  (←  IO.getStderr).putStrLn s!"{world.length}"
  print_picture_multithreaded world num_threads (image_width-1) (image_height-1)





-- def main : IO Unit := do
--   let stdout ← IO.getStdout;
--   stdout.putStrLn "P3";
--   stdout.putStrLn s!"{image_width} {image_height}";
--   stdout.putStrLn s!"255"
--   let world ← generate_world
--   (←  IO.getStderr).putStrLn s!"{world.length}"
--   print_picture world (image_width-1) (image_height-1)
