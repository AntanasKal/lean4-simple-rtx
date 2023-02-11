import Lake
open Lake DSL

package «one-weekend» {
  -- add package configuration options here
}

lean_lib «Vec3» {
  srcDir := "raytracing"
  -- add library configuration options here
}

lean_lib «Colour» {
    srcDir := "raytracing"
  -- add library configuration options here
}

lean_lib «Raytracing» {
  srcDir := "raytracing"
  -- add library configuration options here
}

lean_lib «Scatter» {
    srcDir := "raytracing"
  -- add library configuration options here
}

lean_lib «Hittables» {
    srcDir := "raytracing"
  -- add library configuration options here
}

lean_lib «Camera» {
  srcDir := "raytracing"
  -- add library configuration options here
}

lean_lib «RandUtils» {
    srcDir := "raytracing"
  -- add library configuration options here
}


@[default_target]
lean_exe «one-weekend» {
  root := `Main
}
