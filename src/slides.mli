open Js_of_ocaml

val getRaws : Dom_html.document Js.t -> Dom_html.element Dom.nodeList Js.t
val get : Dom_html.document Js.t -> Dom_html.element Dom.nodeList Js.t

val create_slide : unit -> Dom_html.element Js.t
val create_slide_interior : unit -> Dom_html.divElement Js.t
val encapsulate : Dom_html.element Js.t -> Dom_html.element Js.t -> Dom.node Js.t
val translate : Dom_html.element Js.t -> Dom_html.element Js.t
val if_attr :
  Dom_html.element Js.t -> Js.js_string Js.t -> (Js.js_string Js.t -> unit) -> unit
val add_title : Dom_html.element Js.t -> unit
