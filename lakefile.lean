import Lake
open Lake DSL

package «one-weekend» {
  -- add package configuration options here
}

lean_lib «Vec3» {
  srcDir := "src"
  -- add library configuration options here
}

lean_lib «Ray» {
  srcDir := "src"
  -- add library configuration options here
}


lean_lib «Colour» {
    srcDir := "src"
  -- add library configuration options here
}

lean_lib «Material» {
    srcDir := "src/Material"
  -- add library configuration options here
}

lean_lib «Scatter» {
    srcDir := "src/Material"
  -- add library configuration options here
}

lean_lib «Hittables» {
    srcDir := "src"
  -- add library configuration options here
}

lean_lib «Generators» {
    srcDir := "src"
  -- add library configuration options here
}


@[default_target]
lean_exe «one-weekend» {
  root := `Main
}
