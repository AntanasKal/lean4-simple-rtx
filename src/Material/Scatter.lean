import «Ray»
import «Material»
import «Hittables»

def hemisphere_scattering (nrm : vec3) (v : vec3) : vec3 :=
  if (nrm.dot v) > 0
    then v
  else
    (-v)


def lambertian_scattering (nrm : vec3) (v : vec3) : vec3 :=
  let dir := nrm+v.unit_vector
  if dir.near_zero then nrm else dir

def approx_lambertian_scatterin (nrm : vec3) (v : vec3) : vec3 :=
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
      -- if (cannot_refract) then
        reflect rec.normal unit_direction
      else
        refract rec.normal unit_direction reafraction_ratio
    -- let refracted := refract rec.normal unit_direction reafraction_ratio
    let scattered := ray.mk rec.p direction
    some (attenuation, scattered)

