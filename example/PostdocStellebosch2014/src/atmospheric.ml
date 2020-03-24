open Js_of_ocaml
open Js_of_ocaml_lwt
open Weaver
open Atmospheric_core
let window= Js.Unsafe.variable "window"

let dt = ref 0.05
let step_r = ref 0


let env = [| random_config 0.02 0; random_config 0.02 1; random_config 0.005 1000 |]
let n_step = Array.length env


let env () =  env.(!step_r)



let ctx : Dom_html.canvasRenderingContext2D Js.t option ref = ref None
let geom = ref Plot.null_geometry
let phydt = 0.025

let draw_full env= let open Plot in
  let clear ctx = ctx##clearRect 0. 0. 1. 1. in
  may clear !ctx;
  may draw_cube !ctx;
  may (draw env) !ctx


let rec update ()=
  let env = env () in
  update_g env !dt;
  draw_full env;
  Lwt.( Lwt_js.sleep phydt >>= update )

let timer = ref ( Lwt.return () )


let start _el =
  match !ctx with
  | Some _ctx -> timer := update ()
  | None -> Plot.grab_ctx "atmospheric" ctx geom ; timer := update ()

let stop _el =
  Lwt.cancel !timer; timer:= Lwt.return ();
  step_r := 0;
  draw_full (env())

let run status el =
  let open Timeline in
  match status with
  | Activate -> start el
  | Desactivate -> stop el

let step status _el =
  match status with
  | Timeline.Activate -> if !step_r<n_step-1 then incr step_r
  | Timeline.Desactivate -> if !step_r>0 then decr step_r

let suspend status el = run (Timeline.reverse_status status) el

let name= "atmospheric"

let animation = Timeline.{ name; run ; step ;suspend }
