open Core

type config =
  {env : Eio_unix.Stdenv.base;
   assets_dirs : Eio.Fs.dir_ty Eio.Path.t list;
   theme_dir : Eio.Fs.dir_ty Eio.Path.t;
   root : string option;
   base_url : string option;
   stylesheet : string;
   ignore_tex_cache : bool;
   no_assets: bool;
   no_theme: bool;
   max_fibers : int}

type raw_forest = Code.tree list

type forest =
  {trees : Sem.tree Analysis.Map.t;
   analysis : Analysis.analysis Lazy.t}

val plant_forest : raw_forest -> forest
val render_trees : cfg:config -> forest:forest -> unit
val create_tree : cfg:config -> addrs:string Seq.t -> dest:Eio.Fs.dir_ty Eio.Path.t -> prefix:string -> template:string option -> mode:[`Sequential | `Random] -> string

val complete : forest:forest -> string -> (string * string) Seq.t
val prefixes : addrs:string Seq.t -> string list
val taxa : forest:forest-> (string * string) Seq.t
val tags : forest:forest -> (string * string list) Seq.t
val run_renderer : cfg:config -> forest -> ( unit -> 'a) -> 'a
val render_json : cwd:[> Eio__.Fs.dir_ty ] Eio.Path.t -> Core.Sem.tree Analysis.Map.t -> unit
