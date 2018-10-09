open Parsetree
open Pprintast

let print filepath parsetree =
  let oc = open_out filepath in
  let f = Format.make_formatter
      (output oc)
      (fun () -> flush oc)
  in
  Pprintast.signature f parsetree;
  close_out oc;

