open Js_of_ocaml
open Consts

(**
Timeline grammar :

 timeline := element timeline_queue
 element := tags:timegroup
 tags := tag tag_queue
 tag_queue :=
   | µ
   | + tags
 timeline_queue :=
   | µ
   | + timeline
 timegroup :=
   | time
   | [ time_list ]
 time_list :=
   | time time_list_queue
 time_list_queue :=
   | µ
   | + time_list
 time := int time_queue
 time_queue:=
   | - int
   | ..
   | µ
*)

type tag = string

let digit c = (int_of_char c) -48

let parse_error s =
  window##alert(Js.string s);
  failwith "timeline: parse error"

let unexpected_end s =
  parse_error @@ Printf.sprintf "Unexpected end when parsing %s" s

let expected_char c s =
  parse_error @@ Printf.sprintf "Expected [%c] character when parsing %s" c s

let unexpected_char c s =
  parse_error @@ Printf.sprintf "Unexpected [%c] character when parsing %s" c s

let unexpected c s =
  match c with
  | None -> unexpected_end s
  | Some c -> unexpected_char c s

let arity_error k =
  parse_error @@ Printf.sprintf "No known action of arity %d " k

let unknown_action s =
  parse_error @@ Printf.sprintf "Unknown action %s " s

type interval =
 | Interval of int*int
 | Point of int
 | Right of int

type action = Tag of string | Run of string | Suspend of string | Step of string

type signal = { domain : interval list ; actions : action list }
let default_tag = "activate"

let print signals=
  let sp = Printf.sprintf in
  let sprint_interval = function
    | Point p -> sp "Point %d" p
    | Interval (s,e) -> sp "Interval %d-%d" s e
    | Right r -> sp "Right %d.." r in
  let sprint_intervals is = is |> List.map sprint_interval   |> String.concat "+" in
  let sprint_action = function
    | Tag s -> sp "Tag[%s]" s
    | Run s -> sp "Run[%s]" s
    | Step s-> sp "Step[%s]" s
    | Suspend s -> sp "Suspend[%s]" s in
  let sprint_tags actions = String.concat "," (List.map sprint_action actions) in
  let print_signal s =
    let s = Printf.sprintf "Times:[%s], tags:[%s]" (sprint_intervals s.domain) (sprint_tags s.actions) in
    window##alert(Js.string s ) in
  List.iter print_signal signals


let parse s =
  let n = String.length s-1 in
  let pos = ref 0 in
  let token =
    fun () -> let p = !pos in
	      if p> n then None else (incr pos; Some s.[p]) in
  let undo () = decr pos in
  let ( |>| ) f g = let a = f () in a ::g () in
  let ( >>> ) f g=  let a= f ()  in g a in
  let rec parse_timeline ()=  parse_signal |>|  parse_union
  and parse_signal () =
    parse_actions >>> ( fun actions ->
     { actions; domain = parse_timegroup() }
    )
  and parse_timegroup () = match token () with
    | Some('[') -> parse_time_list ()
    | _c -> undo () ; [ parse_interval () ]
  and parse_time_list () = parse_interval |>| parse_time_list_queue
  and parse_time_list_queue () = match token () with
    | Some('+') -> parse_time_list ()
    | Some(']') -> []
    | c  -> unexpected c "time interval list"
  and parse_actions () = match token () with
    | Some('[') |Some ( '0'..'9' ) -> undo(); [Tag default_tag]
    | _ -> undo();  parse_action (Buffer.create 20) |>| parse_actions_queue
  and parse_action b ()= match token() with
    | Some('+') -> undo(); parse_action_f @@ Buffer.contents b
    | Some(':') -> undo(); parse_action_f @@ Buffer.contents b
    | Some( c ) -> Buffer.add_char b c; parse_action b ()
    | c -> unexpected c "tag"
  and parse_action_f s = match Utils.split_string ' ' s with
    | [a] -> Tag a
    | [a;b] -> ( match a with
		 | "run" -> Run b
		 | "suspend" -> Suspend b
		 | "step" -> Step b
		 | "tag" -> Tag b
		 |  _ -> unknown_action a )
    | q -> arity_error @@ List.length q
  and parse_actions_queue () = match token () with
    | Some('+') -> parse_actions()
    | Some(':') -> []
    | c -> unexpected c "tag queue"
  and parse_interval () = (parse_int 0) >>> parse_r_interval
  and parse_r_interval ns = match token () with
    | Some('-') -> parse_int 0 >>> fun n -> Interval (ns, n )
    | Some('.') ->
       let tok = token () in
       if tok = Some('.') then  Right ns else parse_error @@ Printf.sprintf "Expected character [.] in timeline %s at position %d " s !pos
    | None -> Point ns
    | _  ->  undo(); Point ns
  and parse_int n ()= match token () with
    | Some('0'..'9' as c) ->  parse_int ( n*10 + digit c ) ()
    | None ->  n
    | _ -> undo(); n
  and parse_union () = match token () with
    | Some('+') -> parse_timeline ()
    | None -> []
    | Some(c) -> parse_error @@ Printf.sprintf "Unexpected [%c] character while parsing timeline %s" c s in
  parse_timeline ()


let ( <? ) x is =
  let x_is_in = function
    | Point p -> x=p
    | Right p -> x>=p
    | Interval (s,e) -> x>=s && x<=e in
  List.fold_left (fun t i -> t || x_is_in i ) false is

let upper_time is  =
  let upper = function
    | Point p-> p
    | Right _p -> 0
    | Interval(_s,e) -> e in
List.fold_left (fun u i -> max u @@ upper i ) 0 is


type status = Activate | Desactivate

type  event = {time:int; status:status; ev_actions : action list;  node: Dom_html.element Js.t}

type animation_signal =  status -> Dom_html.element Js.t -> unit

type animation = {
  name : string;
  run : animation_signal;
  step : animation_signal;
  suspend: animation_signal;
}

type animator = string -> animation


let jtime = Js.string "timeline"

let construct_chronology slide=
  let blink n ev_actions start ending events =
    {time=ending+1; status=Desactivate; ev_actions; node  = n }
    ::{time = start; status=Activate; ev_actions;  node = n} :: events in
  let add_interval n ev_actions events = function
    | Point p -> blink n ev_actions p p events
    | Interval (s,e) -> blink n ev_actions s e events
    | Right s -> {time=s;status=Activate; ev_actions; node = n} ::events in
  let add_signal n events (signal:signal) =
    List.fold_left (add_interval n signal.actions) events signal.domain in
  let add_timeline n attr events  =
    let timeline = parse @@ Js.to_string attr in
    List.fold_left (add_signal n) events timeline in
  let events = Utils.fold_attribute jtime add_timeline [] slide in
  let compare_event ev1 ev2 = compare ev1.time ev2.time in
  List.sort compare_event events

let save_ast slide =
  let construct element attr =
    let tml = attr |> Js.to_string |> parse in
    element##(setAttribute
                (Js.string "tml_ast")
                (Js.string @@  Marshal.to_string tml []) ) in
  Utils.iter_attribute jtime construct (slide :> Dom.node Js.t ); slide


let tml_marker= "tmln_"

let marker_on tag =
  Js.string ( tml_marker  ^ tag^"_on"  )
let marker_off tag =
  Js.string( tml_marker ^ tag ^ "_off")

let filter_kind base_filter signals =
  let fold_sign tags signal =  List.fold_left base_filter tags signal.actions  in
  signals
  |> List.fold_left fold_sign []
  |> List.sort_uniq (compare:string->string->int)

let tags signals=
  let add_if_tag l = function
    | Tag t -> t::l
    | _ -> l in
  filter_kind add_if_tag signals

let runs signals =
  let add_if_run l = function
    | Run t -> t::l
    | _ -> l in
  filter_kind add_if_run signals

let prepare_events_tags slide  =
  let prepare_et node  attr ()=
    let cl = node##.classList in
    Js.to_string attr |> parse |> tags |>
      List.iter (fun tag -> cl##add(marker_off tag); cl##remove(marker_on tag) ) in
  Utils.fold_attribute jtime prepare_et () slide

let clear_status slide =
  let clear_cl cl c =
    let s= Js.to_string c in
    if Utils.is_prefix tml_marker s then cl##remove(c) in
  let clear element _attr =
    let cl = element##.classList in
    Utils.classes_consume cl ( clear_cl cl) in
  Utils.iter_attribute jtime clear slide

let reset_animations animator slide  =
  let stop element attr =
    attr |> Js.to_string |>  parse |> runs |> List.iter (fun name -> (animator name).run Desactivate element) in
  Utils.iter_attribute jtime stop slide

let flow_event_to past target future test action =
  let rec move past = function
    | [] -> past, []
    | a::q when test a.time target -> action a; move (a::past) q
    | l -> past, l in
  move past future

let reverse_status = function
  | Activate -> Desactivate
  | Desactivate -> Activate

let apply_tag_event e tag  =
  let cl = e.node##.classList in
  match e.status with
  | Activate  -> (
     cl##add(marker_on tag);
     cl##remove(marker_off tag) )
  | Desactivate ->(
     cl##remove(marker_on tag);
     cl##add(marker_off tag)
  )

let apply_anim_event animator e=
  let eval f = f e.status e.node in
  function
  | Run a -> eval (animator a).run
  | Step a ->  eval (animator a).step
  | Suspend a -> eval (animator a).suspend
  | Tag a -> apply_tag_event e a

let apply_event animator e =
  List.iter (apply_anim_event animator e) e.ev_actions

let reverse_event animator e =
  apply_event animator { e with status = reverse_status e.status }
