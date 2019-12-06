

let document = Js_of_ocaml.Dom_html.document
let window = Js_of_ocaml.Js.Unsafe.variable "window"
let root =  Utils.get_or_create "html"
let body = Utils.get_or_create "body"
