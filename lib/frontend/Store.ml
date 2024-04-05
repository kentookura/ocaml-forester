open Core
open Lwt.Syntax

module P =
struct
  type data = Sem.tree
  type tag = unit

  type hook = unit
  type context = unit
end

module Scope = Yuujinchou.Scope.Make (P)

(* Causes stack overflow, probably due to unimplemented abstract type reprs :( *)
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

let contribute t path data =
  let* t = main_branch config in
  let info = info "example transaction" in
  Forest_store.with_tree_exn t [] ~info ~strategy:`Set (fun tree ->
    (* WARNING copy-pasted example from irmin.org. Confusing nomenclature: these are not our Sem.trees *)
    let tree = match tree with Some t -> t | None -> Forest_store.Tree.empty () in
    let* tree = Forest_store.Tree.remove tree ["foo"; "bar"] in
    let* tree = Forest_store.Tree.add tree ["a"; "b"; "c"] Sem.empty_tree in
    let* tree = Forest_store.Tree.add tree ["d"; "e"; "f"] Sem.empty_tree in
    Lwt.return_some tree)

