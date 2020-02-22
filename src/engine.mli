open Js_of_ocaml

val unfocused : Dom_html.element Js.t
val unfocus : #Dom.node Js.t -> Dom.node Js.t

val slide_prepare :
  Dom_html.element Dom.nodeList Js.t ->
  Dom_html.element Dom.nodeList Js.t

val mod_font_size : float -> int ref -> unit

type events

val no_events : events
val advance_events :
  (string -> Timeline.animation) -> events ref -> unit
val reverse_events :
  (string -> Timeline.animation) -> events ref -> unit
val reset_events : events ref -> unit
val prepare_events :
  (string -> Timeline.animation) ->
  events ref -> Dom.node Js.t -> unit

type frame_info

val initial_frame_info :
  int -> frame_info
val drop :
  (string -> Timeline.animation) -> events ref -> frame_info -> unit
val pick_frame :
  (string -> Timeline.animation) -> events ref -> int -> unit
val pick :
  (string -> Timeline.animation) -> events ref -> frame_info -> unit
val incr_slide : frame_info -> unit
val decr_slide : frame_info -> unit
val incr_time :
  (string -> Timeline.animation) -> events ref -> frame_info -> unit
val decr_time :
  (string -> Timeline.animation) -> events ref -> frame_info -> unit

val keyboard_action :
  int ref ->
  (string -> Timeline.animation) ->
  events ref ->
  frame_info ->
  Dom_html.keyboardEvent Js.t ->
  unit Lwt.t ->
  unit Lwt.t
