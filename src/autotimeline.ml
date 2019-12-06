
type generator =
  | Rise
  | Blink

type selector =
  | All
  | Element
  | Text
  | Ignore
  | Leaf

type autogen = {kind:generator ; tags:Timeline.tag list; selectors : selector list}

let parse_selector = function
  | "" -> Leaf
  | "a" -> All
  | "e" -> Element
  | "t" -> Text
  | "i" -> Ignore
  | "l" -> Leaf
  | _ -> Timeline.parse_error "Invalid selector in timeline generator "

let parse_generator = function
  | ""  -> Rise
  | "r" -> Rise
  | "b" -> Blink
  | _ -> Timeline.parse_error "Invalid generator in timeline generator"

let parse _s =
  let default= {kind = Rise; tags= [Timeline.default_tag]; selectors = [Leaf] } in
  let _parse_group s =
  match Utils.split_string ':' s with
  | [] -> default
  | [tag] -> {default with tags=[tag] }
  | [tag;kind] -> { default with tags=[tag]; kind=parse_generator kind }
  | [tag;kind;selectors] -> {tags=[tag]; kind=parse_generator kind; selectors = [parse_selector selectors] }
  | _ -> Timeline.parse_error "Invalide timeline generator" in
      ()
