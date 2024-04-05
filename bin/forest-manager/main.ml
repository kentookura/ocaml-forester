open Lwt.Syntax
open Repr

(* type foo = { v : string } *)
(**)
(* module Abstract : Irmin.Contents.S with type t = foo = struct *)
(*   open Repr *)
(*   type t = foo *)
(*   let t : foo ty = *)
(*     let a1 _ = assert false in *)
(*     let a2 _ _ = assert false in *)
(*     abstract ~pp:a2 ~of_string:a1 ~json:(a2, a1) *)
(*       ~bin:(a2, a2, Size.custom_dynamic ()) *)
(*       ~equal:a2 ~compare:a2 *)
(*       ~short_hash:(fun ?seed:_ -> a1) *)
(*       ~pre_hash:a2 () *)
(**)
(*   let merge = Irmin.Merge.(option (idempotent t)) *)
(* end *)

module Store = Irmin_git_unix.FS.KV (Core.Rep.Tree) 
module Info = Irmin_unix.Info(Store.Info)

let info message = Info.v ~author:"Example" "%s" message

let main_branch config =
  let* repo = Store.Repo.v config in
  Store.main repo

let main =
  let test_val = Core.Sem.empty_tree in
  let config = Irmin_git.config ~bare:true "/tmp/irmin" in
  let* t = main_branch config in
  let* () = Store.set_exn t ["a"] test_val ~info:(info "first commit") in
  let+ s = Store.get t ["a"] in
  assert (s = test_val ) 

let () = Lwt_main.run main
