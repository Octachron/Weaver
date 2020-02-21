open Js_of_ocaml_lwt
open Weaver
type asep = bool array
let n_site = 100

let empty = false
let full = true

let env = Array.make n_site empty

let alpha = 0.1
let beta =  0.1
let forward = 1.
let backward = 0.

let maybe p f =
  if Random.float 1. < p then
    f ()

let create asep () =
  let do_create () = if env.(0) = empty then asep.(0) <- full in
  maybe alpha do_create

let annihilate asep () =
  let do_ () = if asep.(n_site-1) = full then asep.(n_site-1) <- empty in
  maybe beta do_

let jump asep k ()=
  let do_ j () = if env.(j) = empty then asep.(k) <- empty ; asep.(j) <- full in
  if asep.(k) = full then (
    if k+1 < n_site then  maybe forward (do_ @@ k+1);
    if k>0 then maybe backward (do_ @@ k-1)
  )

let evolve asep () =
  create asep ();
  Array.iteri (fun k _ -> jump asep k () ) asep ;
  annihilate asep ()

let reset () = Array.iteri (fun k _ -> env.(k) <- empty ) env

let phydt = 0.02

let geom_r = ref Plot.null_geometry
let ctx = ref None
let step_r = ref 0

let foi = float_of_int
let ratio () = Plot.ratio !geom_r
let scale ()  = ratio()   /. foi n_site
let view k =
  let sc = scale() in
  ( foi k  *. sc,  0.5 -. sc/. 2.   )

let timer = ref ( Lwt.return () )


let draw_site env k ctx =
  let scale = scale() in
  let (x,y) = view k in
  if env.(k) = true then
    ctx##fillRect x y scale scale


 let draw_sites env ctx  =
    Array.iteri (fun k _ -> draw_site env k ctx) env


let clear ctx = ctx##clearRect 0. 0. (ratio ()) 1.

let draw asep ctx =
  clear ctx;
  Plot.(with_style blue (draw_sites asep) ctx)

let repeat n f=
  for _ = 1 to n do f () done

let n_iter = 100

let rec update ()=
  let asep = env  in
  let () =
    repeat n_iter (evolve asep) ;
    Utils.may (draw asep) !ctx in
  Lwt.( Lwt_js.sleep phydt >>= update )

let start _el =
  let () =
  match !ctx with
  | Some _ -> ()
  | None -> Plot.grab_ctx "asep" ctx geom_r in
  Random.init 10;
  timer:= update ()

let stop _el  =
  Lwt.cancel !timer; timer := Lwt.return() ;
  reset() ;
  step_r := 0(*;
  Utils.may (draw env) !ctx *)

let run status el =
  let open Timeline in
  match status with
      |Activate -> start el
      |Desactivate -> stop el

let step _status _el = ()

let suspend status el  = run (Timeline.reverse_status status) el

let name= "asep"

let animation = Timeline.{name;run;step;suspend}
