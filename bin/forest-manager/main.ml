open Lwt.Syntax
open Repr

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
