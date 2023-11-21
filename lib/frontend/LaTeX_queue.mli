module type S =
sig
  val enqueue : name:string -> packages:string list -> source:string -> unit
  val process : env:_ Build_latex.env -> Eio.Fs.dir_ty Eio.Path.t list
end

module Make (_ : sig val max_fibers : int val ignore_tex_cache : bool end) : S
