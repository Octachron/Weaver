open Js_of_ocaml

type tag = string

val digit : char -> int
val parse_error : string -> 'a
val unexpected_end : string -> 'a
val expected_char : char -> string -> 'a
val unexpected_char : char -> string -> 'a
val unexpected : char option -> string -> 'a

type interval =
  | Interval of int * int
  | Point of int
  | Right of int

type action =
    Tag of string
  | Run of string
  | Suspend of string
  | Step of string

type signal = { domain : interval list; actions : action list; }

val default_tag : string
val print : signal list -> unit
val parse : string -> signal list
val ( <? ) : int -> interval list -> bool
val upper_time : interval list -> int

type status = Activate | Desactivate

type event = {
  time : int;
  status : status;
  ev_actions : action list;
  node : Dom_html.element Js.t;
}

type animation_signal = status -> Dom_html.element Js.t -> unit

type animation = {
  name : string;
  run : animation_signal;
  step : animation_signal;
  suspend : animation_signal;
}

type animator = string -> animation

val jtime : Js.js_string Js.t

val construct_chronology :
  Dom.node Js.t -> event list

val save_ast : Dom_html.element Js.t -> Dom_html.element Js.t

val tags : signal list -> string list
val runs : signal list -> string list
val prepare_events_tags : Dom.node Js.t -> unit
val clear_status : Dom.node Js.t -> unit

val reset_animations :
  (string -> animation) -> Dom.node Js.t -> unit

val flow_event_to :
  event list ->
  'a ->
  event list ->
  (int -> 'a -> bool) -> (event -> unit) -> event list * event list

val reverse_status : status -> status
val apply_tag_event : event -> string -> unit
val apply_anim_event : (string -> animation) -> event -> action -> unit
val apply_event : (string -> animation) -> event -> unit
val reverse_event : (string -> animation) -> event -> unit
