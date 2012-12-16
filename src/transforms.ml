open List
open FormatExt

type field_name = string

type presence =
  | Present
  | Maybe
  | Absent

(* Import the real pat lib? *)
module Pat = struct
  type t =
    | PSingle of string
    | PAll

  let pat p = match p with
    | PSingle s -> text s
    | PAll -> text "Str"

  let all = PAll
  let singleton (s : string) = PSingle s

  let is_singleton p = match p with PSingle _ -> true | _ -> false
  let string_of_singleton pat = match pat with
    | PSingle s -> s
    | _ -> failwith "Not a singleton!"
end

module Spec = struct
  type t =
    | SBoolean
    | SNumber
    | SString of Pat.t
    | SObject of spec_obj_expr
  (* List of fields will not contain duplicate field names and
   * will be sorted. *)
  and spec_obj_expr = (field_name * spec_expr) list

  let rec spec s = match s with
    | SBoolean -> text "Boolean"
    | SNumber -> text "Number"
    | SString p -> Pat.pat p
    | SObject fields ->
        braces (intersperse (text ",")
          (map (fun (f, s) -> horz [text f; spec s]) fields))

  let lookup_field (field : string) (spec : spec_expr) : spec_expr option =
    match spec with
    | SObject fields -> begin
      try Some (snd (find (fun (f, s) -> field = f) fields))
      with Not_found -> None
    end
    | _ -> None

end
open Spec

type type_expr =
  | TTop
  | TString of Pat.t
  | TBoolean
  | TNumber
  | TArrow of type_expr list * type_expr
  | TObject of type_obj_expr
(* List of fields will not contain duplicate field names and
 * will be sorted. *)
and type_obj_expr = (field_name * presence * type_expr) list

(* Some shorthand *)
let sStr = SString Pat.all
let tStr = TString Pat.all
let sSingle s = SString (Pat.singleton s)
let tSingle s = TString (Pat.singleton s)

let rec ty_of_spec spec = match spec with
  | SString p -> TString p
  | SBoolean -> TBoolean
  | SNumber -> TNumber
  | SObject fields ->
      TObject (map (fun (f, s) -> (f, Present, ty_of_spec s)) fields)

let transform_bb (options : spec_expr option) (spec : spec_expr) : type_expr =
  match spec with
  | SObject fields ->
    let id_field_opt = match options with
      | None -> None
      | Some options -> begin
          match lookup_field "idAttribute" options with
          | Some (SString p) ->
              if not (Pat.is_singleton p) then None
              else begin
                match lookup_field (Pat.string_of_singleton p) spec with
                | Some id_field_spec ->
                    Some ("id", Present, ty_of_spec id_field_spec)
                | None -> None
              end
          | _ -> None
      end
    in
    let out_fields = match id_field_opt with
      | Some field -> [field]
      | None -> [("id", Absent, TTop)]
    in
    let attrs =
      TObject (map (fun (f, s) -> (f, Present, ty_of_spec s)) fields) in
    let out_fields =
      ("attributes", Present, attrs) ::
      ("cid", Present, tStr) ::
      out_fields
    in
    TArrow ([ty_of_spec spec], TObject out_fields)
  | _ -> failwith "transform_bb expected spec object"
