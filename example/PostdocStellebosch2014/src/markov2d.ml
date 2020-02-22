open Js_of_ocaml
open Js_of_ocaml_lwt
open Weaver
type vec = {x:int; y : int }

module Ops = struct
  let ( + )  {x;y} v = {x=x+v.x; y = y + v.y }
end

let rand () =
  let h_or_v () = (Random.int 2) in
  let d  = (2 * Random.int 2) -1 in
  if h_or_v()  == 1 then
    { x=0; y = d}
  else
    { y = 0; x= d}

type odyssey = vec list
let time odyssey = List.length odyssey

let foi = float_of_int


let start = {x= -50; y = 0 }
let home = {x=0;y=30}


let last path = match path with
  | [] -> start
  | a::_ -> a

let is_home vec= vec=home

let home_image =
  let img = Dom_html.( createImg document ) in
  let () = img##.src := (Js.string "Images/home.png") in
  img

let ant_image =
  let img = Dom_html.( createImg document ) in
  let () = img##.src := (Js.string "Images/ant2.png") in
  img



let ( +> ) exp step =
  match exp with
  | [] -> [step]
  | a::_ -> if is_home a then exp else  Ops.(a+step)::exp

let env = ref [start]
let reset () = env := [start]

let phydt = 0.02

let t_scale vec =
  let norm = vec.x * vec.x + vec.y * vec.y in
  let f = norm/100 in
  let max = 100000 in
  let cap x = min x max in
  100 + f*f |> cap

let geom = ref Plot.null_geometry
let ratio geom = Plot.ratio !geom
let ctx = ref None
let step_r = ref 0
let n_grid = 100
let scale  = 0.5 /. foi n_grid
let view vec =
  let cx = ratio geom /. 2. in
  let s_grid center x = center +. foi  x /. foi n_grid in
  ( s_grid cx vec.x , s_grid 0.5 vec.y )

let timer = ref ( Lwt.return () )


let draw_point ~ctx vec =
  let (x,y) = view vec in
  ctx##fillRect x y scale scale

let draw_path ~ctx path  =
  List.iter (fun x -> draw_point ~ctx x) path

let clear ~ctx = ctx##clearRect 0. 0. (ratio geom) 1.

let draw ~ctx path =
  let (hx,hy) = view home in
  clear ~ctx;
  draw_path ~ctx path;
  Plot.draw_image ~ctx home_image (0.1,0.1) (hx-.0.05, hy -. 0.05)


let happy_ending ~ctx _env =
  clear ~ctx;
  Plot.draw_image ~ctx ant_image (0.2, 0.2) (0.9, 0.6);
  Plot.draw_image ~ctx home_image (0.5,0.5) (0.75,0.25)

let fold_rv f start gen_rv k  =
  let rec transform k state = match k with
    | 0 -> state
    | k -> gen_rv ()  |> f state |> transform (k-1)
  in
  transform k start

let update_0 () =
  let draw_ant ~ctx =
    Plot.(
      clear ~ctx;
      draw_image ~ctx ant_image (0.2,0.2) (0.2, 0.4 );
      draw_image ~ctx home_image (0.1,0.1) (1.7, 0.4 )
    ) in
  Lwt.return Utils.(may_ctx draw_ant !ctx)

let rec update ()=
  let path = !env  in
  let () =
    let n_iter = t_scale @@ last path in
    let path = fold_rv (+>) path  rand n_iter in
    env := path ;
    Utils.may_ctx (draw path) !ctx in
  if is_home (last path) then
   (  Utils.may_ctx (happy_ending path)  !ctx; Lwt.return() )
  else
    Lwt.( Lwt_js.sleep phydt >>= update )

let start _el =
  let () =
  match !ctx with
  | Some _ctx -> ()
  | None -> Plot.grab_ctx "markov2d" ctx geom in
  Random.init 10;
  step_r:=0;
  timer:=  update_0 ()

let stop _el  =
  Lwt.cancel !timer; timer := Lwt.return() ;
  reset() ;
  step_r := 0;
  Utils.may_ctx (draw !env) !ctx

let run status el =
  let open Timeline in
  match status with
  | Activate -> start el
  | Desactivate -> stop el

let steps = [|update_0; update |]
let n_step = Array.length steps

let step status _el =
  let open Timeline in
  let step = !step_r in
  let () =
    Lwt.cancel !timer;
    timer := Lwt.return() in
  let () =
    match status with
    | Activate -> if step+1< n_step then incr step_r
    | Desactivate -> if step >0 then decr step_r in
  timer:= steps.(!step_r) ()


let suspend status el  = run (Timeline.reverse_status status) el

let name= "markov2d"

let animation = Timeline.{name;run;step;suspend}
