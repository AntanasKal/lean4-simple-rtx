import «Vec3»
import «Raytracing»

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

