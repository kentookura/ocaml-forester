open Repr

let string_source : Range.string_source t =
  let open Range in
  record "string_source" (fun title content -> { title; content })
  |+ field "title" (option string) (fun s -> s.title)
  |+ field "content" string (fun s -> s.content)
  |> sealr

let source_repr : Range.source ty =
  let open Range in
  variant "source" (fun file string -> function
    | `File s -> file s | `String s -> string s)
  |~ case1 "`File" string (fun s -> `File s)
  |~ case1 "`String" string_source (fun s -> `String s)
  |> sealv

let position : Range.position ty =
  let open Range in
  record "position" (fun source offset start_of_line line_num ->
      { source; offset; start_of_line; line_num })
  |+ field "source" source_repr (fun t -> t.source)
  |+ field "offset" int (fun t -> t.offset)
  |+ field "start_of_line" int (fun t -> t.offset)
  |+ field "line_num" int (fun t -> t.offset)
  |> sealr

let range : Range.t ty =
  let open Range in
  (* required stuff for serializing abstract types *)
  let pp formatter a = () in
  let b =
    { source = `File "todo"; offset = 0; start_of_line = 0; line_num = 0 }
  in
  let e =
    { source = `File "todo"; offset = 0; start_of_line = 0; line_num = 0 }
  in
  (* We need to choose a string representation of ranges. Should we coordinate with the Asai team or just go for it? *)
  let of_string _ = Ok (Range.make (b, e)) in
  let encode _ _ = () in
  let decode _ = Ok (Range.make (b, e)) in
  let encode_bin : _ encode_bin = fun _ _ -> () in
  let decode_bin _ _ = Range.make (b, e) in
  let size_of : _ size_of = Size.custom_dynamic () in
  let equal _ _ = false in
  let compare _ _ = 0 in
  let short_hash ?seed a = 0 in
  let pre_hash _ _ = () in
  abstract ~pp ~of_string ~json:(encode, decode)
    ~bin:(encode_bin, decode_bin, size_of)
    ~equal ~compare ~short_hash ~pre_hash ()

let prim : Prim.t ty=
  let open Prim in
  enum "prim"
    [
      ("P", `P);
      ("Ol", `Ol);
      ("Ul", `Ul);
      ("Li", `Li);
      ("Em", `Em);
      ("Strong", `Strong);
      ("Code", `Code);
      ("Blockquote", `Blockquote);
      ("Pre", `Pre);
    ]

let date : Prelude.Date.t ty =
  let pp formatter a = () in
  let of_string s = Ok (Prelude.Date.parse s) in
  let encode _ _ = () in
  let decode _ = Ok (Prelude.Date.parse "todo") in
  let encode_bin : _ encode_bin = fun _ _ -> () in
  let decode_bin _ _ = Prelude.Date.parse "todo" in
  let size_of : _ size_of = Size.custom_dynamic () in
  let equal _ _ = false in
  let compare _ _ = 0 in
  let short_hash ?seed a = 0 in
  let pre_hash _ _ = () in
  abstract ~pp ~of_string ~json:(encode, decode)
    ~bin:(encode_bin, decode_bin, size_of)
    ~equal ~compare ~short_hash ~pre_hash ()

module Tree : Irmin.Contents.S with type t = Sem.tree = struct
  type t = Sem.tree

  let math_mode = enum "math_mode" [ ("inline", Base.Inline); ("display", Display) ]

  let ref_cfg =
    let open Sem in
    record "ref_cfg"
      (fun address -> { address })
      |+ field "address" string (fun t -> t.address)
      |> sealr

  let rec sem_node () : Sem.node ty =
    let open Sem in
    variant "node"
      (fun
        text
        transclude
        subtree
        query
        xml_tag
        unresolved
        math
        link
        embed_tex
        img
        if_tex
        prim
        object_
        ref
      -> function
      | Text s -> text s
      | Transclude (x, y) -> transclude (x, y)
      | Subtree (x, y) -> subtree (x, y)
      | Query (x, y) -> query (x, y)
      | Xml_tag (x, y, z) -> xml_tag (x, y, z)
      | Unresolved x -> unresolved x
      | Math (x, y) -> math (x, y)
      | Link x -> link x
      | Embed_tex x -> embed_tex x
      | Img x -> img x
      | If_tex (x, y) -> if_tex (x, y)
      | Prim (x, y) -> prim (x, y)
      | Object x -> object_ x
      | Ref x -> ref x)
    |~ case1 "Text" string (fun s -> Text s)
    |~ case1 "Transclude"
         (pair (tranclusion_opts ()) string)
         (fun (x, y) -> Transclude (x, y))
    |~ case1 "Subtree"
         (pair (tranclusion_opts ()) (tree ()))
         (fun (x, y) -> Subtree (x, y))
    |~ case1 "Query"
         (pair (tranclusion_opts ()) (query (list (located_sem_node ()))))
         (fun (x, y) -> Query (x, y))
    |~ case1 "Xml_tag"
         (triple string
            (list @@ pair string (list (located_sem_node ())))
            (list (located_sem_node ())))
         (fun (x, y, z) -> Xml_tag (x, y, z))
    |~ case1 "Unresolved" string (fun s -> Unresolved s)
    |~ case1 "Math"
         (pair math_mode (list (located_sem_node ())))
         (fun (x, y) -> Math (x, y))
    |~ case1 "Link" (link ()) (fun s -> Link s)
    |~ case1 "Embed_tex" (embed_tex ()) (fun s -> Embed_tex s)
    |~ case1 "Img" img (fun s -> Img s)
    |~ case1 "If_tex"
         (pair (list (located_sem_node ())) (list (located_sem_node ())))
         (fun (x, y) -> If_tex (x, y))
    |~ case1 "Prim" (pair prim (list (located_sem_node ()))) (fun (x, y) -> Prim (x, y))
    |~ case1 "Object_" symbol (fun s -> Object s)
    |~ case1 "Ref" ref_cfg (fun s -> Ref s)
    |> sealv

  and embed_tex () : Sem.embed_tex ty =
    let open Sem in
    record "embed_tex" (fun preamble source -> { preamble; source })
    |+ field "preamble" (list (located_sem_node ())) (fun t -> t.preamble)
    |+ field "source" (list (located_sem_node ())) (fun t -> t.source)
    |> sealr

  and modifier =
    let open Sem in
    enum "modifier" [ ("sentence_case", `Sentence_case)]

  and img : Sem.img ty =
    let open Sem in
    record "img" (fun path -> { path })
    |+ field "path" string (fun t -> t.path)
    |> sealr

  and ref_cfg : Sem.ref_cfg ty =
    let open Sem in
    record "ref_cfg" (fun address -> { address })
    |+ field "address" string (fun t -> t.address)
    |> sealr

  and symbol : Symbol.t ty = let open Symbol in
    pair (list string) int


  and link () : Sem.link ty =
    let open Sem in
    record "link" (fun dest label modifier -> { dest; label; modifier })
    |+ field "dest" string (fun t -> t.dest)
    |+ field "label" (option (list (located_sem_node ()))) (fun t -> t.label)
    |+ field "modifier" (option modifier) (fun t -> t.modifier)
    |> sealr

  and query a : 'a Query.t ty =
    let open Query in
    variant "query" (fun author tag taxon meta or_ and_ not_ true_ -> function
      | Author x -> author x
      | Tag x -> tag x
      | Taxon x -> taxon x
      | Meta (x, y) -> meta (x, y)
      | Or x -> or_ x
      | And x -> and_ x
      | Not x -> not_ x
      | True -> true_)
    |~ case1 "Author" a (fun x -> Author x)
    |~ case1 "Tag" a (fun x -> Tag x)
    |~ case1 "Taxon" a (fun x -> Taxon x)
    |~ case1 "Meta" (pair string a) (fun (x, y) -> Meta (x, y))
    |~ case1 "Or" (list (query (list (located_sem_node ())))) (fun x -> Or x)
    |~ case1 "And" (list (query (list (located_sem_node ())))) (fun x -> And x)
    |~ case1 "Not" (query a) (fun x -> Not x)
    |~ case0 "True" True |> sealv

  and located_sem_node () : Sem.node Range.located ty =
    let open Asai in
    let open Range in
    record "located_sem_node" (fun loc value -> { loc; value })
    |+ field "loc" (option range) (fun t -> None)
    |+ field "value" (sem_node ()) (fun t -> t.value)
    |> sealr

  and tranclusion_opts () =
    let open Sem in
    record "tranclusion_opts"
      (fun
        toc
        show_heading
        show_metadata
        title_override
        taxon_override
        expanded
        numbered
      ->
        {
          toc;
          show_heading;
          show_metadata;
          title_override;
          taxon_override;
          expanded;
          numbered;
        })
    |+ field "toc" bool (fun t -> t.toc)
    |+ field "show_heading" bool (fun t -> t.show_heading)
    |+ field "show_metadata" bool (fun t -> t.show_metadata)
    |+ field "title_override"
         (option (list (located_sem_node ())))
         (fun t -> t.title_override)
    |+ field "taxon_override" (option string) (fun t -> t.taxon_override)
    |+ field "expanded" bool (fun t -> t.expanded)
    |+ field "numbered" bool (fun t -> t.numbered)
    |> sealr

  and frontmatter () =
    let open Sem in
    record "frontmatter"
      (fun
        title
        taxon
        authors
        contributors
        dates
        addr
        metas
        tags
        physical_parent
        designated_parent
        source_path
        number
      ->
        {
          title;
          taxon;
          authors;
          contributors;
          dates;
          addr;
          metas;
          tags;
          physical_parent;
          designated_parent;
          source_path;
          number;
        })
    |+ field "title" (option (list (located_sem_node ()))) (fun t -> t.title)
    |+ field "taxon" (option string) (fun t -> t.taxon)
    |+ field "authors" (list string) (fun t -> t.authors)
    |+ field "contributors" (list string) (fun t -> t.contributors)
    |+ field "dates" (list date) (fun t -> t.dates)
    |+ field "addr" (option string) (fun t -> t.addr)
    |+ field "metas"
         (list (pair string (list (located_sem_node ()))))
         (fun t -> t.metas)
    |+ field "tags" (list string) (fun t -> t.tags)
    |+ field "physical_parent" (option string) (fun t -> t.physical_parent)
    |+ field "designated_parent" (option string) (fun t -> t.designated_parent)
    |+ field "source_path" (option string) (fun t -> t.source_path)
    |+ field "number" (option string) (fun t -> t.number)
    |> sealr

  and tree () : Sem.tree ty =
    let open Sem in
    record "tree" (fun fm body : Sem.tree -> { fm; body })
    |+ field "fm" (frontmatter ()) (fun t -> t.fm)
    (* without annotation compiler thinks that t is obj_method due to `body` field *)
    |+ field "body" (list (located_sem_node ())) (fun (t : Sem.tree) -> t.body)
    |> sealr

  let t = tree ()
  let merge = Irmin.Merge.(option (idempotent t))
end
