open! Core
open Compiler

let print_ast lst =
    let string_of_op = function
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
    in

    let rec string_of_exps exprs = 
        exprs
        |> List.map ~f:string_of_exp
        |> String.concat ~sep:" "

    and string_of_exp = function
        | Op (op, exps) -> 
            Printf.sprintf "(%s %s)" (string_of_op op) (string_of_exps exps)
        | Num i -> Int.to_string i
        | Var var -> var
        | Def (name, exp) ->
            Printf.sprintf "(define %s %s)" name (string_of_exp exp)
    in
    lst
    |> string_of_exps
    |> print_endline
;;
