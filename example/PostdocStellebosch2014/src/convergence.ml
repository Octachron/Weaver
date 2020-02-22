open Js_of_ocaml_lwt
open Weaver
type experience = float list
let time exp = List.length exp
let foi = float_of_int

let ( +> ) exp x =
  match exp with
  | [] -> [x]
  | a::_ -> let t = foi @@ time exp in
      let y = ( t  *. a +. x )  /. (t +. 1.) in
      y::exp

let start = []


let n_grid = 10
let n_exp = n_grid * n_grid
let time_f = 200
let phydt = 0.1
let env = Array.make n_exp start
let reset () = Array.iteri (fun k _ -> env.(k) <- start ) env

let geom = ref Plot.null_geometry
let ctx = ref None
let step_r = ref 0
let timer = ref ( Lwt.return () )

let () = Random.init 1
let rand () = float_of_int @@ Random.int 2



let draw_partial ~ctx env k =
  let open Plot in
  let exp = env.(k) in
  let padding = 0.1 in
  let w = 1. -. 2. *. padding in
  let adapt y = w*. y +. padding in
  let xf = foi @@ k mod n_grid and yf = foi @@ k/n_grid in
  let xfp = xf +. padding in
  let yfp = yf +. padding in
  let t = foi @@ time exp in
  let delta = w /. t in
  let axis () =
    ctx##beginPath;
    ctx##moveTo xfp (yfp+.w);
    ctx##lineTo xfp yfp;
    ctx##moveTo xfp (yfp+.w);
    ctx##lineTo (xfp+.w) (yfp+.w);
    ctx##stroke in
  let path () =
    ctx##beginPath;
    ctx##moveTo xfp yfp;
    List.rev exp
    |> List.fold_left (fun x y -> ctx##lineTo x (adapt y); x+.delta) (xfp+.delta)
    |> ignore in
  let () =
    ctx##clearRect xf yf w w;
    ctx##.fillStyle := blue;
    ctx##fillRect xfp yfp w w;
    axis () ;
    ctx##.fillStyle := green;
    path ();
    ctx##lineTo (xfp+.w) padding;
    ctx##fill;
    ctx##.fillStyle := black;
    path() ;
    ctx##stroke
  in
  ()


let rec update_0 ()=
  let exp = env.(0) in
  let () =
    env.(0) <- exp +> rand ()  ;
    Utils.may_ctx (draw_partial env 0) !ctx in
   Lwt.( Lwt_js.sleep phydt >>= update_0 )

let start _el =
  let () =
  match !ctx with
  | Some _ -> ()
  | None -> Plot.grab_ctx "convergence" ctx geom in
  timer:= update_0 ()

let stop _el  =
  Lwt.cancel !timer; timer := Lwt.return() ;
  reset() ;
  step_r := 0;
  Utils.may_ctx (draw_partial env 0) !ctx

let run status el =
  let open Timeline in
  match status with
  | Activate -> start el
  | Desactivate -> stop el

let step _status _el = ()

let suspend status el  = run (Timeline.reverse_status status) el

let name= "convergence"

let animation = Timeline.{name;run;step;suspend}
