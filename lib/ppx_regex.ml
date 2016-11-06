open Parsetree
open Asttypes
open Ast_mapper
open Ppx_core.Std
open Ast_builder.Default

let accept_cases e cases =
  let f case =
    begin match case.pc_guard with
    | None -> ()
    | Some { pexp_loc; _ } -> raise Location.(Error (error ~loc:pexp_loc "Guards are not supported."))
    end;
    match case with
    | { pc_lhs = { ppat_desc = Ppat_any; ppat_loc }; pc_guard; pc_rhs } ->
      { pc_lhs = { ppat_desc = Ppat_any; ppat_loc; ppat_attributes = [] };
        pc_guard = None;
        pc_rhs }
    | { pc_lhs = { ppat_desc = Ppat_constant str; ppat_loc }; pc_guard; pc_rhs } ->
      let guard =
        let nl v = (Nolabel, v) in
        let regex =
          pexp_apply ~loc:ppat_loc [%expr Re_str.regexp] [nl (pexp_constant ~loc:ppat_loc str)] in
        pexp_apply ~loc:ppat_loc [%expr Re_str.string_match] [nl regex; nl e; nl [%expr 0]] in
      { pc_lhs = { ppat_desc = Ppat_any; ppat_loc; ppat_attributes = [] };
        pc_guard = Some guard;
        pc_rhs}
    | { pc_lhs = { ppat_loc; _ }; _ } -> raise Location.(Error (error ~loc:ppat_loc "unkown pattern type")) in
  List.map f cases

let regex_mapper argv =
  let expr mapper expr =
    match expr with
    | { pexp_desc = Pexp_extension ({ txt = "regex"; _ }, PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_match (e, cases) }, attrs) }]); pexp_loc; _ } ->
      let new_cases = accept_cases e cases in
      pexp_match ~loc:pexp_loc e new_cases
    | _ -> default_mapper.expr mapper expr in
  { default_mapper with expr }

let () =
  run_main regex_mapper
