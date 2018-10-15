open ASD
open Token

let rec parse = parser
        | [< 'Entity ename; pl = parse_predicat_list; 'Point; tail = parse >] ->
                       ((ename, pl) :: tail: document)
        | [< >] -> []
and parse_predicat_list = parser
        | [< 'Entity predname; ol = parse_object_list; ss >] ->
                        begin match ss with parser
                        | [< 'Semicolon; tail = parse_predicat_list >] -> (predname,ol) :: tail
                        | [< >] -> [(predname, ol)]
                        end
and parse_object_list = parser
        | [< obj = parse_object; ss >] ->
                        begin match ss with parser
                        | [< 'Comma; tail = parse_object_list >] -> obj :: tail
                        | [< >] -> [obj]
                        end
and parse_object = parser
        | [< 'Entity e >] -> Ent e
        | [< 'String s >] -> Text s
;;
