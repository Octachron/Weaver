

let document = Dom_html.document

let getRaws document =
  document##getElementsByTagName(Js.string "slide")

let get document =
  document##querySelectorAll(".slide")

let root =  Utils.get_or_create "html"
let body = Utils.get_or_create "body"


let parent_or_root element =
  let node = (element :> Dom.node Js.t) in
  Js.Opt.get (node##.parentNode) (fun () -> (root :> Dom.node Js.t)  )

let create_slide () =
  let slide = document##createElement(Js.string "article")  in
  slide##.classList##add(Js.string "slide");
  slide

let create_slide_interior () =
  let div = Dom_html.createDiv document in
  div##.classList##add(Js.string "slide_interior");
  div


let encapsulate new_father element =
  let new_father, element= ( new_father :> Dom.node Js.t), (element :> Dom.node Js.t) in
  let grand_father = parent_or_root element in
  grand_father##(replaceChild new_father element) |> ignore;
  new_father##appendChild(element)

let translate raw_slide =
  let slide = create_slide () in
  let slide_interior = create_slide_interior () in
  let slide = Utils.transfer_attrs raw_slide slide in
  let slide_interior = slide_interior |> Utils.transfer_childs raw_slide
  and parent = parent_or_root raw_slide in
  slide##.classList##add(Js.string "slide");
  slide##appendChild((slide_interior:> Dom.node Js.t) ) |> ignore;
  Dom.replaceChild parent slide raw_slide;
  slide

let if_attr element attr f =
  Js.Opt.iter (element##getAttribute attr) f

let add_title frame=
  let add t =
    let h = Dom_html.createH1 document
    and text = document##createTextNode(t) in
    Dom.appendChild h text;
    Utils.insertFirst frame h in
  if_attr frame (Js.string "title" ) add



