(* ASD type *)
(* type document = *)
  (* Fill here! *)

type obj = Text of text | Ent of entity;;

type text = string;;

type entity = string;;

type objectList = Cons of obj list ;;

type predicate = Couple of obj * objectList;;

type predicateList = ConsPred of predicate list;;

type stmt = Triple of obj * predicateList;;

type stmtList = ConsStmt of stmt list;;

type turtle = Statements of stmtList;;

let tValue (ent: entity) = string.concat "" ["<"; (ent: string) ; ">"];;
let eValue (t : text) = string.concat "" ["<"; (t:string) ; ">"];;

let oValue (o: obj) =
    match o with
    |Text(a) -> tValue a
    |Ent(a) -> eValue a
;;

let pValue (p:predicate) = match p with
    |Couple(a,b) -> oValue a
;;

let sValue (s:stmt) = match s with
    |Triple(a,b) -> oValue a
;;

let objectPrint (sujet: string) (predicate:string) (o : obj) = string.concat \n [sujet ; predicate ; oValue o]
;;

let objectListPrint (sujet : string) (predicate:string) (l:objectList) =
    string.concat \n (list.map (objectPrint sujet predicate) l)
;;

let predicatePrint (sujet : string) (pred : predicate) = match pred with
    |Couple(a,b) -> objectListPrint sujet (oValue a) b
;;

let predicateListPrint (sujet: string) (p : predicateList) = string.concat \n (list.map (predicatePrint sujet) p) ;;

let stmtPrint (sujet:stmt) = match m with
    |Triple(a,b) -> predicateListPrint (oValue a) b
;;

let stmtListPrint (sujets : stmtList) = string.concat \n (list.map stmtPrint sujets);;

(* Function to generate the document out of the AST *)
let rec ntriples_of_ast ast = match ast with
    |Statements(a) -> stmtListPrint a
;;
  (* Fill here! *)
