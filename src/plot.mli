open Js_of_ocaml

type geometry = { w : float; h : float; }

val inv : geometry -> geometry
val ( %% ) : geometry -> geometry -> geometry
val null_geometry : geometry
val ratio : geometry -> float
val horizontal : geometry -> bool
val min_g : geometry -> geometry

type context = Dom_html.canvasRenderingContext2D Js.t

val scale_geometry :
  ctx:context -> geometry -> unit

val draw_image :
  ctx:context ->
  Dom_html.imageElement Js.t ->
  float * float -> float * float ->
  unit

val sq3o4 : float
val col : CSS.Color.t -> Js.js_string Js.t
val redish : Js.js_string Js.t
val red : Js.js_string Js.t
val blue : Js.js_string Js.t
val blueish : Js.js_string Js.t
val green : Js.js_string Js.t
val greenish : Js.js_string Js.t
val black : Js.js_string Js.t

val with_style :
  ctx:context -> f:(context -> unit) ->
  Js.js_string Js.t -> unit

val draw_circle :
  ctx:context -> radius:float -> pos:(float * float * float) -> unit

val draw_quad :
  ctx:context ->
  close:(context -> unit) ->
  float * float -> float * float -> float * float ->
  unit

val draw_para :
  ctx:context ->
  close:(context -> unit) ->
  float * float -> float * float -> float * float -> float * float ->
  unit

val draw_cube : ctx:context -> unit

val grab_ctx :
  string -> context option ref -> geometry ref -> unit
