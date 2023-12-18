open Forester_lsp
open Cmdliner
module Lsp = Forester_lsp.Make (Core.Reporter.Message)
module Tty = Asai.Tty.Make (Core.Reporter.Message)

let () =
  let load_file ~display x = () in
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  Printexc.record_backtrace true;
  Eio_main.run @@ fun env ->
  Core.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  Lsp.start ~source:(Some "trees/index.tree") ~init:(fun ~root -> ()) ~load_file
