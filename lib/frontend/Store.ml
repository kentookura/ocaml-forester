open Core
open Lwt.Syntax

module Forest_store = Irmin_git_unix.FS.KV (Rep.Tree)
module Forest_info = Irmin_unix.Info(Forest_store.Info)

let config = Irmin_git.config ~bare:true "/tmp/irmin/test"

let main_branch config =
  let* repo = Forest_store.Repo.v config in
  Forest_store.main repo

let branch config name =
  let* repo = Forest_store.Repo.v config in
  Forest_store.of_branch repo name

let info message = Forest_info.v ~author:"kentookura" "%s" message

let main =
  let* t = main_branch config in
  let* () = Forest_store.set_exn t ["a"; "b"; "c";] Sem.empty_tree ~info:(info "my first commit") in
  let+ s = Forest_store.get t ["a"; "b"; "c";] in 
  assert (s = Sem.empty_tree)

