module To_current = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_402)(Migrate_parsetree.OCaml_current)

open Source_pos

open Generator_utils

let drop_prefix prefix str =
  let len = String.length prefix in
  let rest = (String.length str) - len in
  String.sub str len rest

let read_lines path =
  let file = open_in path in
  let rec consume acc =
    match input_line file with
    | line -> consume (line :: acc)
    | exception End_of_file ->
        close_in file;
        acc
  in
  consume [] |> List.rev

let () =
  let argv = Sys.argv |> Array.to_list in

  let () = Log.is_verbose := match List.find ((=) "-verbose") argv with
    | _ -> true
    | exception Not_found -> false
  in

  let here_dir = Sys.getcwd() in

  let here_scheme_path = match List.find (is_prefixed "-schema=") argv with
    | arg -> drop_prefix "-schema=" arg
    | exception Not_found -> "graphql_schema.json" (* the default path so it won't break backward compatibility *)
  in

  let out_prefix = "-out=" in
  let here_outfile_path = match List.find (is_prefixed out_prefix) argv with
    | arg -> drop_prefix out_prefix arg
    | exception Not_found -> "schema.mli"
  in

  let schema = Lazy.force( Read_schema.get_schema here_dir here_scheme_path ) in

  let mli_tree = Typegen_to_mli_tree.from_ast schema in

  Typegen_to_mli_file.print here_outfile_path mli_tree
