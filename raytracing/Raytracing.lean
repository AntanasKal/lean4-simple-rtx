import «Vec3»
import «Colour»



-- Struct for rays

structure ray where
  orig : point3
  dir : vec3
deriving Repr

def ray.at (r : ray) (t : Float) :=
  r.orig.coord.add (r.dir.scalar_mult t)

-- Type for material

inductive material where
  | lambertian (albedo : colour) : material
  | metal (albedo : colour) (fuzz : Float) : material
  | dialectric (index_of_refraction : Float) : material
deriving Repr

-- Type for hittable objects

inductive hittable where
  | sphere (center : point3) (radius : Float) (mat : material) : hittable


structure hit_record where
  t : Float
  p : point3
  r : ray
  outward_normal : vec3
  mat : material
deriving Repr

def hit_record.front_face (hr : hit_record) : Bool :=
  (hr.r.dir.dot hr.outward_normal) < 0

def hit_record.normal (hr : hit_record) : vec3 :=
  if hr.front_face then hr.outward_normal else (-hr.outward_normal)

-- Solving for rays hitting spheres

def hit (r : ray) (htbl : hittable) (t_min t_max : Float) : Option hit_record :=
  match htbl with
  | hittable.sphere center radius mat =>
    let oc : vec3 := r.orig.coord.add center.coord.neg;
    let a := r.dir.dot r.dir;
    let half_b : Float :=  (oc.dot r.dir);
    let c := (oc.dot oc) - radius*radius;
    let discriminant := half_b*half_b - a*c;
    if discriminant < 0 then none else
      let sqrtd := discriminant.sqrt
      let root1 := (-half_b - sqrtd)/a
      let root2 := (-half_b + sqrtd)/a
      let inrange := (fun x => x ≥ t_min ∧ x ≤ t_max)
      let root :=
        if inrange root1 then
          some root1
        else
          if inrange root2 then
            some root2
          else none
    match root with
    | none => none
    | some rt =>
      some {
        t := rt,
        p := point3.mk (r.at rt),
        r := r, outward_normal := ((r.at rt)-center.coord)/radius
        mat := mat
      }

def hitlist (best_so_far : Option hit_record) (r : ray) (htbls : List hittable) (t_min t_max : Float) : Option hit_record :=
  match htbls with
  | [] => best_so_far
  | h::hs =>
    let record? := hit r h t_min t_max;
    match record? with
    | none => hitlist best_so_far r hs t_min t_max
    | some record  => hitlist record r hs t_min record.t



-- Functions for scattering

def hemisphere_scattering (nrm : vec3) (v : vec3) : vec3 :=
  if (nrm.dot v) > 0 then
    v
  else
    (-v)

def lambertian_scattering (nrm : vec3) (v : vec3) : vec3 :=
  let dir := nrm+v.unit_vector
  if dir.near_zero then nrm else dir

def approx_lambertian_scattering (nrm : vec3) (v : vec3) : vec3 :=
  nrm+v

def reflectance (cosine ref_idx : Float) : Float :=
  let r0 := ((1-ref_idx)/(1+ref_idx))^2
  r0 + (1 - r0)*((1-cosine)^5)

def reflect (nrm : vec3) (v : vec3) : vec3 :=
  v - (2*(v.dot nrm)*nrm)

def refract (nrm : vec3) (uv : vec3) (etai_over_etat : Float) :=
  let dot_product := -uv.dot nrm
  let cos_theta := if dot_product < 1.0 then dot_product else 1.0
  let r_out_perp : vec3 := etai_over_etat * (uv + cos_theta*nrm)
  let r_out_parallel : vec3 :=  -(1 - r_out_perp.length_squared).abs.sqrt * nrm
  r_out_perp + r_out_parallel

def material.scatter (mat : material) (rand_seed : vec3 × Float) (ray_in : ray) (rec : hit_record) : Option (colour × ray) :=
  let rand_vec := rand_seed.fst;
  match mat with
  | lambertian albedo =>
    let scatter_direction := lambertian_scattering rec.normal rand_vec
    let scattered := ray.mk rec.p scatter_direction
    let attenuation := albedo
    some (attenuation, scattered)
  | metal albedo fuzz=>
    let reflected := reflect rec.normal ray_in.dir.unit_vector;
    let scattered := ray.mk rec.p (reflected + fuzz*rand_vec)
    let attenuation := albedo
    if scattered.dir.dot rec.normal > 0 then
      some (attenuation, scattered)
    else
      none
  | dialectric ir =>
    let attenuation := colour.mk 1 1 1
    let reafraction_ratio := if rec.front_face then (1.0/ir) else ir
    let unit_direction : vec3 := ray_in.dir.unit_vector
    let cos_theta := fmin (-unit_direction.dot rec.normal) (1.0)
    let sin_theta := (1.0 - cos_theta*cos_theta).sqrt
    let cannot_refract : Bool := reafraction_ratio*sin_theta > 1.0
    let direction : vec3 :=
      if (cannot_refract ∨ (reflectance cos_theta reafraction_ratio > rand_seed.snd)) then
        reflect rec.normal unit_direction
      else
        refract rec.normal unit_direction reafraction_ratio
    let scattered := ray.mk rec.p direction
    some (attenuation, scattered)


