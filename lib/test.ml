open! Core
open Compiler

let print_tokens lst =
    lst
    |> List.map ~f:(function
        | LParen -> "("
        | RParen -> ")"
        | Int i -> "i" ^ Int.to_string i
        | String s -> "'" ^ s ^ "'"
    )
    |> String.concat ~sep:" "
    |> print_endline
;;

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
    in
    lst
    |> string_of_exps
    |> print_endline
;;

let%expect_test "token_test" =
  print_tokens (tokenize "(+ 1 (* 2 3 4) 5)");
  [%expect {| ( '+' i1 ( '*' i2 i3 i4 ) i5 ) |}]

let%expect_test "parse_test" =
  print_ast (build_ast (tokenize "(+ 1 (* 2 3 4) 5)"));
  [%expect {| (+ 1 (* 2 3 4) 5) |}]

let%expect_test "eval_test" =
  Printf.printf "%d\n" (eval (build_ast (tokenize "(+ 1 (* 2 3 4))")));
  [%expect {| 25 |}]

let%expect_test "eval_test2" =
  Printf.printf "%d\n" (eval (build_ast (tokenize "(/ 100 10 5)")));
  [%expect {| 2 |}]
