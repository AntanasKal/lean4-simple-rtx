
import «Vec3»
import «Material»
import «Ray»

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
