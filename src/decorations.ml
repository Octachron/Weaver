module Js = Js_of_ocaml.Js
module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom

let center document =
  let targets = document##querySelectorAll(Js.string "[center]") in
  let center node =
    let centering = Dom_html.createDiv document in
    node##removeAttribute(Js.string "center");
    centering##.classList##add(Js.string "centered");
    Slides.encapsulate centering node |> ignore in
  Utils.node_iter targets center

let css_width document =
   let css_width_element element attr =
     element##.style##.width := attr in
  Utils.iter_attribute (Js.string "css-width") css_width_element ( document :> Dom.node Js.t)
