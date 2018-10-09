open Ast_402
open Parsetree
open Asttypes

open Generator_utils

let gql_to_ml_type name meta =
  match meta with
  | Schema.Enum({em_values; em_name}) -> Some({
      ptype_name = { txt=(
          let n = (uncapitalize_ascii em_name) in
          Log.log n;
          n); loc=Location.none };
      ptype_params = [];
      ptype_cstrs = [];
      ptype_manifest = None;
      ptype_kind = Ptype_variant(
          em_values
          |> List.map (fun (e: Schema.enum_value_meta) -> {
                pcd_name = { txt=e.evm_name; loc=Location.none };
                pcd_args = [];
                pcd_res = None;
                pcd_loc = Location.none;
                pcd_attributes = [];
              })
        );
      ptype_loc = Location.none;
      ptype_attributes = [];
      ptype_private = Public;
    })
  | _ -> None

let is_internal_type = is_prefixed "__"

let from_ast: Schema.schema -> Parsetree.signature =
  fun ast ->
    let types: Parsetree.type_declaration list =
      Hashtbl.fold
        (fun k v acc ->
          if is_internal_type k
          then acc
          else
            match gql_to_ml_type k v with
            | Some(t) -> t :: acc
            | None -> acc)
        ast.type_map
        []
    in
    [{ psig_desc = Psig_type(types); psig_loc = Location.rhs_loc 0; }]
