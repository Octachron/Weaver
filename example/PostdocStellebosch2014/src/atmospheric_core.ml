open Weaver
let dim = 3

module Bigarray =
  struct
    module Array1 = struct
      let get a x = let s = x * dim in
        a.(s), a.(s+1), a.(s+2)
      let set a p (x,y,z) = let s = p * dim in
            a.(s)<-x; a.(s+1)<-y; a.(s+2)<-z

    end
    module Array2= struct
      let get a x y = a.(x*dim+y)
      let set a x y v = a.(x*dim+y) <- v
      end
  end


type env = {position:float array;velocity:float array;  radius: float }

let n env = ( Array.length @@ env.position) /dim
let iter_n n f= for k=0 to n-1 do f k done
let iter_k f= iter_n dim f

let iter_p env f =  iter_n (n env) f



let random_vect a pos = iter_k ( fun k -> a.{pos,k}<- Random.float 1. )
let random_config radius n =
  let p = Array.make (dim*n) 0. and v = Array.make (dim*n) 0. in
  iter_n n @@ random_vect p; iter_n n @@ random_vect v;
    {velocity=v;position=p;radius}


let update_p {position;velocity;_} dt pos = iter_k (fun i ->
  position.{pos,i} <- position.{pos,i} +. dt *. velocity.{pos,i}
               )

let guard {position;velocity;radius} pos =
  let reflect d i =
    position.{pos,i}<-d ; velocity.{pos,i} <- -.velocity.{pos,i} in
  iter_k (fun i ->
    let x= position.{pos,i} in
    let rad = radius in
    match x<rad,x+.rad>=1. with
    | true, _  -> reflect rad i
    | _, true -> reflect (1.-.rad) i
    | _ -> ()
   )




let (|||) f g = fun a -> f a; g a

let update_g env dt =
  iter_p env ( update_p env dt ||| guard env  )

let draw_p ~ctx {position;radius;_} pos =
  Plot.draw_circle ~ctx ~radius ~pos:position.{pos}

let draw ~ctx env = iter_p env @@ draw_p ~ctx env
