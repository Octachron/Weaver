module Js = Js_of_ocaml.Js
module Dom_html = Js_of_ocaml.Dom_html
module CSS = Js_of_ocaml.CSS

let may f = Utils.may f

(** Plotting functions *)
type geometry = { w: float ; h : float }

let inv geometry = {w=1./. geometry.w ; h = 1. /. geometry.h  }
let ( %% ) g1 g2 = { w=g1.w *. g2. w ; h = g1.h *. g2.h }

let null_geometry = { w= 0.; h=0. }
let ratio {w;h}  = w /. h
let horizontal {w;h} = w > h

let min_g g = let w = min g.w g.h in
	      { w; h=w }


let scale_geometry geometry ctx =
  ctx##(scale geometry.w geometry.h)

let draw_image img (w,h) (x,y) ctx=
  let foi = float_of_int in
  let g2 = { w= foi img##.width; h = foi img##.height } and g3 = {w;h} in
  let p = { w=x; h = y } in
  let g = inv g2 %% g3  in
  let gi = inv g in
  let p = gi %% p  in
  ctx |> scale_geometry g;
  ctx##drawImage(img,p.w,p.h);
  ctx |> scale_geometry (gi)


let  sq3o4 = (sqrt 3.)/. 4.



let col c = (CSS.Color.js c :> Js.js_string Js.t)

let redish = col CSS.Color.(hsl ~a:0.3 0 100 50)
let red=  col CSS.Color.(hsl 0 100 50)

let blue = col CSS.Color.(hsl 250 100 40 )
let blueish = col CSS.Color.(hsl ~a:0.3 250 100 40)


let green = col CSS.Color.(hsl 100 100 30 )
let greenish = col CSS.Color.(hsl ~a:0.3 100 100 30)
let black = col CSS.Color.(Name Black)

let with_style style f ctx =
  ctx##.fillStyle := style; f ctx; ctx##.fillStyle := black

let draw_circle ctx radius (x,y,z) =
  let yp = 0.5 +. 0.5 *.y -. 0.25 *.( x +. z ) in
  let xp = 0.5 +. sq3o4 *. ( x -. z ) in
  ctx##beginPath();
  ctx##arc(xp,yp,radius,0.,6.3, Js._false);
  ctx##fill()

let draw_quad close ctx (x0,y0) (tx1,ty1) (tx2,ty2) =
  ctx##beginPath();
  ctx##moveTo (x0,y0);
  let x1,y1 = x0 +. tx1, y0 +. ty1 in
  ctx##lineTo(x1,y1);
  ctx##lineTo(x1+.tx2,y1+.ty2);
  ctx##lineTo(x0+.tx2,y0+.ty2);
  ctx##closePath();
  close(ctx)

let draw_para close ctx start v1 v2 v3 =
    let quad = draw_quad close ctx start in
    quad v1 v3; quad v2 v3; quad v1 v2

let draw_cube ctx =
  let close ctx = ctx##stroke() in
  let start =  (0.5,0.5) in
  let l = sq3o4,-0.25 and r = -.sq3o4, -0.25 and d=(0.,0.5) in
  draw_para close ctx start l r d

 (** Canvas manipulation function *)
 let grab_ctx name ctx_r geometry_r =
  let doc = Dom_html.document in
  let mcanvas = doc##getElementById(Js.string name ) in
  let canvas =  Js.Opt.bind mcanvas Dom_html.CoerceTo.canvas in
  let mctx=Js.Opt.map canvas ( fun canvas->
			       (canvas##getContext(Dom_html._2d_ )), (canvas##.width), (canvas##.height)  ) in
  Js.Opt.iter mctx ( fun (ctx_v,w,h) ->
		     let w, h = float_of_int w, float_of_int h in
		     let g = {w;h} in
		     may (fun ctx-> ctx##.fillStyle := Js.string "black") !ctx_r;
		     ctx_r:= Some ctx_v;
		     geometry_r := g ;
		     scale_geometry (min_g g) ctx_v;
		     ctx_v##.lineWidth := 0.01
		   )
