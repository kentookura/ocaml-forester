open Prelude
open Core

module Printer =
struct
  module P0 =
  struct
    type out = Format.formatter
    let text txt fmt =
      Format.fprintf fmt "%s" txt
  end

  include Printer_kit.Kit (P0)

  let contents (printer : t) : string =
    Format.asprintf "%a" (fun fmt _ -> printer fmt) ()
end

type cfg = {tex : bool}

let rec render_node ~cfg : Sem.node -> Printer.t =
  function
  | Sem.Text txt ->
    Printer.text txt
  | Sem.Math (_, xs) ->
    render ~cfg xs
  | Sem.Tag (name, _, body) ->
    render_tag ~cfg name body
  | Sem.If_tex (x , y) ->
    if cfg.tex then render ~cfg x else render ~cfg y
  | node ->
    Format.eprintf "missing case: %a@." Sem.pp_node node;
    failwith "Render_verbatim.render_node"

and render_tag ~cfg name body =
  Printer.seq
    [Printer.text "\\";
     Printer.text name;
     render_arg ~cfg Braces body]

and render ~cfg xs =
  Printer.iter ~sep:Printer.space (render_node ~cfg) xs

and render_arg ~cfg delim (arg : Sem.t) : Printer.t =
  match arg with
  | [] -> Printer.nil
  | _ ->
    let l, r =
      match delim with
      | Braces -> "{", "}"
      | Squares -> "[", "]"
      | Parens -> "(", ")"
    in
    Printer.seq
      [Printer.text l;
       render ~cfg arg;
       Printer.text r]