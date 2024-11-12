let bin = "simple-ddl"
let package = "simple-ddl"

let generate_rules base = begin
  (* Elaboration tests *)
  Printf.printf
    {|
      (rule
       (with-stdin-from ../%s.txt
        (with-stdout-to %s.core.stdout.tmp
         (run %%{bin:%s} elab))))
    |}
    base base bin;
  Printf.printf
    {|
      (rule
       (alias runtest)
       (package %s)
       (action
        (diff ../%s.core.stdout %s.core.stdout.tmp)))
    |}
    package base base;

  (* Compilation tests *)
  Printf.printf
    {|
      (rule
       (with-stdin-from ../%s.txt
        (with-stdout-to %s.stdout.rs.tmp
         (run %%{bin:%s} compile))))
    |}
    base base bin;
  Printf.printf
    {|
      (rule
       (alias runtest)
       (package %s)
       (action
        (diff ../%s.rs.stdout %s.stdout.rs.tmp)))
    |}
    package base base;
end

let () =
  Sys.readdir ".."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter_map (Filename.chop_suffix_opt ~suffix:".txt")
  |> List.iter generate_rules
