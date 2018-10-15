
type entity = string;;
type obj = Text of string | Ent of string;;

type objectList = obj list ;;

type predicate = entity * objectList;;

type predicateList = predicate list;;

type stmt = entity * predicateList;;

type stmtList = stmt list;;

type document = stmtList;;

let eValue (ent : string) = String.concat "" ["<"; ent ; ">"];;
let tValue (t : string) = String.concat "" ["\""; t ; "\""];;

let oValue (o: obj) =
    match o with
    |Text(a) -> tValue a
    |Ent(a) -> eValue a
;;

let object_to_string ent pred obj = String.concat " " [eValue ent; eValue pred; oValue obj; "."];;

let pred_to_string ent (pred:predicate) = match pred with
        | predname, olist -> String.concat "\n" (List.map (object_to_string ent predname) olist)
;;

let stmt_to_string (s:stmt) = match s with
        | ent, plist -> String.concat "\n" (List.map (pred_to_string ent) plist)
;;

(* Function to generate the document out of the AST *)
let ntriples_of_ast (ast:document) = 
        String.concat "\n" (List.map stmt_to_string ast)
;;
