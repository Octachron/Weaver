module Js = Js_of_ocaml.Js
module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom

open Consts

let unfocused  =
  let unfocused = document##createElement(Js.string "section")
                  |> Utils.add_class "unfocused" in
  let () = Dom.appendChild body unfocused in
  unfocused

let unfocus (element : #Dom.node Js.t)  = unfocused##appendChild((element :> Dom.node Js.t) )

let slide_prepare (slides:Dom_html.element Dom.nodeList Js.t) =
  Utils.node_consume slides ( fun n ->
      n
      |> Slides.translate
      |> unfocus
      |> ignore
    ) ;
  document##querySelectorAll(Js.string ".slide")


let mod_font_size factor font_size =
  let s = !font_size in
  let s = int_of_float @@ factor *. float_of_int s in
  let sty = Js.string @@ Printf.sprintf "%d%%" s in
  body##.style##.fontSize := sty;
  font_size := s

type events = {
    past:Timeline.event list;
    present : int;
    futur : Timeline.event list
  }

let no_events = { past = []; present=(-1); futur = []  }

let advance_events animator events  =
  let {past;present;futur} = !events in
  let apply = Timeline.apply_event animator in
  let past,futur = Timeline.flow_event_to past (present+1) futur (<=) apply in
  events := {past; present = present+1; futur}

let reverse_events animator events =
  let {past;present;futur} = !events in
  let apply = Timeline.reverse_event animator in
  let futur,past = Timeline.flow_event_to futur (present-1) past (>) apply in
  events := { past; present= present-1; futur}

let reset_events events = events := no_events

let prepare_events animator events slide =
  let () =
    events := { past= []; present=  -1 ; futur = Timeline.construct_chronology slide};
    Timeline.prepare_events_tags slide
  in
  advance_events animator events

type frame_info = { n_slides: int; mutable current : int }

let initial_frame_info n_slides =
  { n_slides; current = 0 }

let drop animator events frame_info =
  let focus = body##.firstChild
   and c = frame_info.current in
  let really_drop focus =
   Timeline.clear_status focus;
   Timeline.reset_animations animator focus;
   if c== frame_info.n_slides - 1 then
    ignore @@ unfocus focus
  else
    let cf = unfocused##.childNodes##item(c) in
    Dom.insertBefore unfocused focus cf in
 Js.Opt.iter focus really_drop;
 reset_events events

let pick_frame animator events k =
  let cf=unfocused##.childNodes##item(k) in
  Js.Opt.map cf (fun x -> prepare_events animator events x; Utils.insertFirst body x )
  |> ignore

let pick animator events frame_info =
  pick_frame animator events frame_info.current

let incr_slide frame_info =
  let c = frame_info.current in
  if c = frame_info.n_slides-1
  then
    ()
  else
    frame_info.current <- c+1

let decr_slide frame_info =
  let c = frame_info.current in
  if c = 0
  then
    ()
  else
    frame_info.current <- c-1

let incr_time animator events frame_info = match !events.futur with
  | [] -> Utils.seq_ frame_info [
		       drop animator events;
		       incr_slide;
		       pick animator events
		     ]
  | _ -> advance_events animator events

let decr_time animator events frame_info =
  match !events.present with
  | 0 -> Utils.seq_ frame_info [
		      drop animator events;
		      decr_slide;
		      pick animator events
		    ]
  | _ -> reverse_events animator events

let keyboard_action font_size animator events frame_info key _handler =
  let key = key##.keyCode in
  match key  with
  | 37 | 38 -> Lwt.return @@ decr_time animator events frame_info
  | 39 | 40 -> Lwt.return @@ incr_time animator events frame_info
  | 109 -> mod_font_size 0.9 font_size; Lwt.return()
  | 107 -> mod_font_size 1.1 font_size; Lwt.return ()
  | _ -> Lwt.return ()

