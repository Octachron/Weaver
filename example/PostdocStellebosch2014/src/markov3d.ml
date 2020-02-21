open Js_of_ocaml
open Js_of_ocaml_lwt
open Weaver
type vec = {x:int; y : int; z : int }

module Ops = struct
  let ( + )  {x;y;z} v = {x=x+v.x; y = y + v.y; z=z+v.z }
end

let zero = { x=0;y=0;z=0 }

let rand () =
  let dim () = (Random.int 3) in
  let d  = (2 * Random.int 2) -1 in
  match dim () with
 | 0 ->{zero with x=d }
 | 1 ->{ zero with y=d }
 | _ ->{ zero  with z=d }

type odyssey = vec list
let time odyssey = List.length odyssey

let foi = float_of_int


let start = {x= -50; y = 0; z = 0 }
let home = {x=0;y=0; z=0 }

let home_image =
  let img = Dom_html.( createImg document ) in
  let () = img##.src := (Js.string "Images/home.png") in
  img

let last path = match path with
  | [] -> start
  | a::_ -> a

let is_home vec= vec=home


let ( +> ) exp step =
  match exp with
  | [] -> [step]
  | a::_ -> if is_home a then exp else  Ops.(a+step)::exp

let env = ref [start]
let reset () = env := [start]

let phydt = 0.02

let t_scale _vec =
  100

let geom = ref Plot.null_geometry
let ratio geom = Plot.ratio !geom
let ctx = ref None
let step_r = ref 0
let n_grid = 100
let scale  = 0.5 /. foi n_grid

let theta_of_t t = foi t /. 10000.

let view theta vec =
  let cos_t = cos theta and sin_t = sin theta in
  let x = cos_t *. foi  vec.x  +.  sin_t *. foi vec.z in
  let cx = ratio geom /. 2. in
  let s_grid center x = ( center +.  x /. foi n_grid ) -. scale/.2.  in
  ( s_grid cx x , s_grid 0.5 @@ foi  vec.y )

let timer = ref ( Lwt.return () )


let draw_point vec theta ctx =
  let (x,y) = view theta vec in
  ctx##fillRect x y scale scale

 let draw_path path theta ctx  =
    List.iter (fun x -> draw_point x theta ctx) path

let clear ctx = ctx##clearRect 0. 0. (ratio geom) 1.

let with_style style f ctx =
  ctx##.fillStyle := style; f ctx; ctx##.fillStyle := Plot.black

let draw path theta ctx =
  let hx,hy = view theta home in
  clear ctx;
  Plot.draw_image  home_image (0.05,0.05) (hx-.0.025,hy-.0.025) ctx;
  draw_path path theta ctx

let happy_ending _env ctx =
  with_style Plot.blueish (fun ctx -> ctx##fillRect 0. 0. (ratio geom) 1.) ctx

let fold_rv f start gen_rv k  =
  let rec transform k state = match k with
    | 0 -> state
    | k -> gen_rv ()  |> f state |> transform (k-1)
  in
  transform k start

let rec update ()=
  let path = !env  in
  let () =
    let n_iter = t_scale @@ last path in
    let path = fold_rv (+>) path  rand n_iter in
    env := path ;
    let theta = time path |> theta_of_t in
    Utils.may (draw path theta) !ctx in
  if is_home (last path) then
   (  Utils.may (happy_ending path) !ctx; Lwt.return() )
  else
    Lwt.( Lwt_js.sleep phydt >>= update )

let start _el =
  let () =
  match !ctx with
  | Some _ -> ()
  | None -> Plot.grab_ctx "markov3d" ctx geom in
  timer:= update ()

let stop _el =
  Lwt.cancel !timer; timer := Lwt.return() ;
  reset() ;
  step_r := 0;
  Utils.may (draw !env 0.) !ctx

let run status el =
  let open Timeline in
  match status with
      |Activate -> start el
      |Desactivate -> stop el

let step _status _el = ()

let suspend status el  = run (Timeline.reverse_status status) el

let name= "markov3d"

let animation = Timeline.{name;run;step;suspend}
