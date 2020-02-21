open Js_of_ocaml
open Js_of_ocaml_lwt
open Weaver
open Atmospheric_core
let window= Js.Unsafe.variable "window"

let dt = ref 0.05
let step_r = ref 0
let steps_size = [|50;1;1000|]
type env2 = {mutable  n : int ; mutable config:env }

let n_full {config;_} = Atmospheric_core.n config
let n_at_step step= steps_size.(step)

let env =
  let n = steps_size.(!step_r) in
  { n ; config = random_config 0.01 n }
let reset step env =
  let n = n_at_step step in
  env.n<- n ;
  env.config <- random_config 0.01 n

let n env = env.n


let ctx=ref None
let geom = ref Plot.null_geometry

let phydt = 0.025
let sq3o4 = (sqrt 3.) /. 4.

let ratio () = Plot.ratio !geom

let count_stat {config;n}  =
  let front = ref 0 in
  iter_n n (fun pos -> if config.position.{pos,0} < 0.5 then incr front );
  !front

let paint_half ctx = let open Plot in
  let close ctx = ctx##fill in
  let xd, yd = sq3o4/.2., -0.125 in
  let l = ( -.sq3o4, -.0.25 ) and r = (xd, yd) and d = (0.,0.5) in
  ctx##.fillStyle := greenish;
  draw_para close ctx (0.5,0.5)  l r d;
  ctx##.fillStyle := blueish;
  draw_para close ctx (0.5+.xd,0.5+.yd) l r d;
  ctx##.fillStyle := black ;
  draw_quad (fun ctx -> ctx##stroke) ctx  (0.5+.xd,0.5+.yd) l d

let draw_rect_fc ctx style (x0,y0) w h=
  ctx##.fillStyle := style;
  ctx##fillRect x0 y0 w h;
  ctx##.fillStyle := Plot.black;
  ctx##strokeRect x0 y0 w h

let draw {n;config} ctx =
  iter_n n (draw_p config ctx)

let draw_counter env ctx =
  let open Plot in
  let y0 = 1.1 and h=0.2 in
  let count = float_of_int @@ count_stat env in
  let ratio = count /. float_of_int (n env) in
  draw_rect_fc ctx greenish (0.,y0) ratio h ;
  draw_rect_fc ctx blueish (ratio,y0) (1.-.ratio) h

let clear ctx = ctx##clearRect 0. 0. 1. (1./. ratio ())
let draw_full env ctx = let open Plot in
  clear ctx;
  draw_cube ctx;
  draw env ctx;
  paint_half ctx;
  draw_counter env ctx


let rec update_0 ()=
  update_g env.config !dt;
  Utils.may (draw_full env) !ctx;
  Lwt.( Lwt_js.sleep phydt >>= update_0 )

let update_1 () =
  Lwt.return()

let rec update_2 () =
  let () =
    let n= env.n in
    if n + 1 < n_full env then
      env.n <- n + 1 in
  Utils.may (draw_full env) !ctx;
  Lwt.( Lwt_js.sleep phydt >>= update_2 )


let timer = ref ( Lwt.return () )
let steps = [| update_0;update_1;update_2|]

let n_step = Array.length steps

let start _el =
  let () = match !ctx with
    | Some _ -> ()
    | None -> Plot.grab_ctx "atmospheric_half" ctx geom  in
  step_r:=0;
  timer:= update_0 ()

let stop _ =
  Lwt.cancel !timer;  timer:= Lwt.return ();
  step_r:=0; reset 0 env;
  Utils.may (draw_full env)  !ctx

let run status el =
  let open Timeline in
  match status with
  | Activate -> start el
  | Desactivate -> stop el


let do_step () =
  let step = !step_r in
  let () =
    reset step env;
    if step > 0 then env.n<-1;
    Utils.may (draw_full env) !ctx in
  timer:= steps.(!step_r) ()

let step status _el =
  let () = Lwt.cancel !timer in
  let () =
  match status with
  | Timeline.Activate -> if !step_r + 1 <n_step  then incr step_r
  | Timeline.Desactivate -> if !step_r>0 then decr step_r
  in
  do_step ()

let suspend status el = run (Timeline.reverse_status status) el

let name= "atmospheric_half"

let animation = Timeline.{ name; run ; step ;suspend }
