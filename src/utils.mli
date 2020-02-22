open Js_of_ocaml

val seq_ : 'a -> ('a -> unit) list -> unit
val add_class : string -> Dom_html.element Js.t -> Dom_html.element Js.t

val nodelike_iter :
  ('a -> 'b -> unit) ->
  < item : int -> 'a Js_of_ocaml.Js.meth;
    length : < get : int; .. > Js_of_ocaml.Js.gen_prop; .. >
  Js_of_ocaml.Js.t -> 'b -> unit

val node_iter :
  'a Dom.nodeList Js.t -> ('a Js.t -> unit) -> unit

val classes_iter :
  Dom_html.tokenList Js.t -> (Js.js_string Js.t -> unit) -> unit

val node_fold :
  ('a -> 'b Js.t -> 'a) -> 'a ->
  'b Dom.nodeList Js.t ->
  'a

val nodelike_consume :
  ('a -> 'b -> unit) ->
  < item : int -> 'a Js_of_ocaml.Js.meth;
    length : < get : int; .. > Js_of_ocaml.Js.gen_prop; .. >
  Js_of_ocaml.Js.t -> 'b -> unit

val node_consume :
  'a Dom.nodeList Js.t -> ('a Js.t -> unit) -> unit

val classes_consume :
  Dom_html.tokenList Js.t -> (Js.js_string Js.t -> unit) -> unit

val node_iteri :
  'a Dom.nodeList Js.t ->
  (int -> 'a Js.t -> unit) ->
  unit

val is_prefix : string -> string -> bool
val split_string : char -> string -> string list

val fold_attribute :
  Js.js_string Js.t ->
  (Dom_html.element Js.t -> Js.js_string Js.t -> 'a -> 'a) ->
  'a -> Dom.node Js.t -> 'a

val iter_attribute :
  Js.js_string Js.t ->
  (Dom_html.element Js.t -> Js.js_string Js.t -> unit) ->
  Dom.node Js.t -> unit

val may : ('a -> unit) -> 'a option -> unit
val may_ctx : (ctx:'a -> unit) -> 'a option -> unit

val get_or_create : string -> Dom_html.element Js.t

val insertFirst :
  #Dom.node Js.t -> #Dom.node Js.t -> unit

val transfer_attrs :
  Dom_html.element Js.t -> Dom_html.element Js.t -> Dom_html.element Js.t

val transfer_classes :
  Dom_html.element Js.t -> Dom_html.element Js.t -> Dom_html.element Js.t

val transfer_childs :
  Dom_html.element Js.t -> Dom_html.element Js.t -> Dom_html.element Js.t
