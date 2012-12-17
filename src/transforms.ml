open Prelude
open List
open Format
open FormatExt 

type field_name = string

type presence =
  | Present
  | Maybe
  | Absent
let string_of_pres p = match p with
  | Present -> "x"
  | Maybe -> "o"
  | Absent -> "a"

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
  and spec_obj_expr = (field_name * t) list

  let rec spec s = match s with
    | SBoolean -> text "Boolean"
    | SNumber -> text "Number"
    | SString p -> Pat.pat p
    | SObject fields ->
        braces (vert
          (map (fun (f, s) -> horz [text f; spec s]) fields))
  let to_string = FormatExt.to_string spec

  let lookup_field (field : string) (spec : t) : t option =
    match spec with
    | SObject fields -> begin
      try Some (snd (find (fun (f, s) -> field = f) fields))
      with Not_found -> None
    end
    | _ -> None

end
open Spec

module Ty = struct
  type t =
    | TTop
    | TString of Pat.t
    | TBoolean
    | TNumber
    | TArrow of t list * t
    | TObject of ty_obj_expr
  (* List of fields will not contain duplicate field names and
   * will be sorted. *)
  and ty_obj_expr = (field_name * presence * t) list

  let empty_obj = TObject []

  let guard_obj f ty = match ty with
    | TObject fields -> f fields
    | _ -> failwith "Expected TObject!"

  let compare_fields = (fun (f1, _, _) (f2, _, _) -> compare f1 f2)
  let add_field field obj = guard_obj (fun fields ->
    TObject (merge compare_fields [field] fields)) obj
  let add_fields fields obj = fold_left (flip add_field) obj fields

  let rec ty t = match t with
    | TTop -> text "Top"
    | TString p -> Pat.pat p
    | TBoolean -> text "Boolean"
    | TNumber -> text "Number"
    | TArrow (args, ret) ->
        horz ((map ty args) @ [text "->"; ty ret])
    | TObject fields ->
        braces (vert
          (map (fun (f, p, t) ->
                  horz [squish [text f; text ":"; text (string_of_pres p)];
                        ty t])
            fields))
  let to_string = FormatExt.to_string ty

  let rec from_spec spec = match spec with
    | SString p -> TString p
    | SBoolean -> TBoolean
    | SNumber -> TNumber
    | SObject fields ->
        TObject (map (fun (f, s) -> (f, Present, from_spec s)) fields)
end
open Ty

(* Some shorthand *)
let sStr = SString Pat.all
let tStr = TString Pat.all
let sSingle s = SString (Pat.singleton s)
let tSingle s = TString (Pat.singleton s)

let transform_bb (options : Spec.t option) (spec : Spec.t) : Ty.t =
  match spec with
  | SObject fields ->
      let id_attr_opt = match options with
        | None -> None
        | Some options -> begin
            match lookup_field "idAttribute" options with
            | Some (SString p) ->
                if Pat.is_singleton p
                then Some (Pat.string_of_singleton p)
                else failwith "idAttribute given Str"
            | _ -> None
        end
      in
      let out_ty = Ty.empty_obj in
      let out_ty = match id_attr_opt with
        | None -> Ty.add_field ("id", Absent, TTop) out_ty
        | Some id_attr ->
            match lookup_field id_attr spec with
            | None -> Ty.add_field ("id", Absent, TTop) out_ty
            | Some id_field_spec ->
                Ty.add_field ("id", Present, Ty.from_spec id_field_spec) out_ty
      in
      let attrs_ty = match id_attr_opt with
        | None -> Ty.empty_obj
        | Some id_attr ->
            match lookup_field id_attr spec with
            | Some id_field_spec -> Ty.empty_obj
            | None -> Ty.add_field (id_attr, Absent, TTop) Ty.empty_obj
      in
      let attrs_ty = 
        Ty.add_fields
          (map (fun (f, s) -> (f, Present, Ty.from_spec s)) fields)
          attrs_ty
      in
      let out_ty =
        Ty.add_fields [
          ("attributes", Present, attrs_ty);
          ("cid", Present, tStr)
        ] out_ty
      in
      TArrow ([attrs_ty], out_ty)
  | _ -> failwith "transform_bb expected spec object"
