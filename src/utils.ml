
let window = Js.Unsafe.variable "window"
let document = Dom_html.document

(** *)
let seq_ x fs = List.iter (fun f -> f x) fs

(** Html_element manipulation *)
let add_class c element  = element##.classList##add(Js.string c) ; element

(** Iteration on nodelist or classlist *)
let nodelike_iter extract nl f =
  let n = nl##.length in
  for k=0 to n-1 do
    extract (nl##item(k)) f
  done


let node_iter nl f = nodelike_iter Js.Opt.iter nl f

let classes_iter nl f = nodelike_iter Js.Optdef.iter nl f


let node_fold f start nl=
  let result = ref start in
  node_iter nl ( fun x -> result := f !result x );
  !result

let nodelike_consume iter nl f =
  let rec consume () = match nl##.length with
    | 0 -> ()
    | _ -> iter (nl##item(0)) f ; consume () in
consume ()

let node_consume nl f= nodelike_consume Js.Opt.iter nl f
let classes_consume nl f= nodelike_consume Js.Optdef.iter nl f

let node_iteri nl f =
  let n = nl##.length in
  for k=0 to n-1 do
    match Js.Opt.to_option (nl##item(k)) with
    | Some x -> f k x
    | None -> ()
  done


(** String supplementary function *)
let is_prefix pre s =
  let len = String.length pre in
  let pos = ref 0 in
  while !pos<len && s.[!pos] = pre.[!pos]  do incr pos done;
  !pos = len

let split_string sep s =
  let n = String.length s in
  let split pos=
    try Some  (String.index_from s pos sep) with
    | Not_found -> None  in
  let rec split_all l pos=
    let sub ende = String.sub s pos (ende-pos) in
    match split pos with
  | None -> sub n :: l
  | Some p -> split_all ( sub p::l) (p+1) in
  List.rev @@ split_all [] 0

(** Iteration on attributes *)
let fold_attribute attr f start origin =
  let element_f n acc = match Js.Opt.to_option @@ n##getAttribute(attr) with
    | Some attr -> f n attr acc
    | None -> acc in
  let node_f acc n = let e =  Dom_html.CoerceTo.element n in
  match Js.Opt.to_option e with
  | Some e ->  element_f e acc
  | None -> acc  in
let rec children acc parent = node_fold children (node_f acc parent) parent##.childNodes in
children start origin

let iter_attribute attr f origin =
  let element_f n = Js.Opt.iter ( n##getAttribute(attr) ) (f n)  in
  let node_f n= Js.Opt.iter (Dom_html.CoerceTo.element n) element_f in
  let rec children node = node_f node; node_iter (node##.childNodes) children in
  children origin


(**Conditionnal function *)
 let may f = function
  | Some x-> f x
  | None -> ()

(** Dom manipulation *)
let get_or_create name =
  let jname= Js.string name in
  let m_element = document##(getElementsByTagName jname)##(item 0) in
  Js.Opt.get m_element (fun () -> document##createElement(jname) )

let insertFirst p e = Dom.insertBefore p e @@ p##.firstChild

let transfer_attrs origin target =
  let transfer attr =  target##(setAttribute attr##name attr##value) in
  node_iter origin##attributes transfer; target

let transfer_classes origin target =
  let clt = target##classList in
  classes_iter (origin##classList) (fun c ->  clt##add(c) |> ignore );
  target

let transfer_childs origin target  =
  let transfer child = target##appendChild(child) |> ignore in
  node_consume (origin##.childNodes) transfer; target

