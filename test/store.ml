open Lwt.Syntax
open Core

module Store = Irmin_git_unix.FS.KV(Rep.Tree)
module Info = Irmin_unix.Info(Store.Info)
module Sync = Irmin.Sync.Make(Store)
module Tree = Store.Tree

let config = Irmin_git.config ~bare:true "/tmp/irmin"

let main_branch config =
  let* repo = Store.Repo.v config in
  Store.main repo


let fold ~init ~f tree=
  Tree.fold ~order:`Sorted ~contents:(fun k _v acc -> if k = [] then Lwt.return acc else f k acc) tree init

let main () =
  let* t = main_branch config in
  let* tree = Store.tree t in
  let+ _ = fold ~init:[] ~f:(fun k acc -> Lwt.return (k::acc)) tree in 
  ()

let () = Lwt_main.run (main ())

  (* let addr = (User_addr "foo-0001") in *)
  (* let* () = Store.set_exn t ["foo-0001"] (Sem.empty_tree ~addr) ~info:(info "first commit") in *)
(* let info message = Info.v ~author:"kentookura" "%s" message *)

  (* let* s = Store.get t ["foo-0001"] in *)
  (* let* public_trees = Store.remote "https://git.sr.ht/~jonsterling/public-trees" in *)
  (* let order = `Sorted in *)
  (* let+ () = migrate_from_git public_trees in *)

(* let migrate_from_git remote = *)
(*   let* repo = Store.Repo.v config in *)
(*   let* t = Store.main repo in *)
(*   let* status = Sync.pull_exn t remote `Set in *)
(*   let+ list = Store.list t [] in *)
(*   List.iter (fun (step, tree) -> match Store.Tree.destruct tree with *)
(*     | `Node _ -> Printf.printf "NODE: %s\n" step *)
(*     | `Contents _ -> Printf.printf "CONTENTS: %s\n" step)  *)
(*     list *)
 
(* let store : forest -> Store.t Lwt.t = fun forest -> *)
(*   let prefixes =  *)
(*     Analysis.Map.bindings forest.trees  *)
(*     |> List.to_seq *)
(*     |> Seq.map fst *)
(*     |> Seq.filter_map Addr.to_user_addr *)
(*     |> fun addrs -> Forest.prefixes ~addrs *)
(*   in main_branch config *)
