open Core

type op = Add
        | Sub
        | Mul
        | Div

type exp = Op of op * exp list
         | Num of int

type ast = exp list

type token = LParen
           | RParen
           | Int of int
           | String of string

(* Parse a single integer *)
let parse_int lst = 
    let rec loop acc = function
        | ('0'..'9' as digit) :: rest -> 
            loop (digit :: acc) rest

        | [] as rest
        | rest -> 
            let num = String.of_char_list (List.rev acc) in
            Int (Int.of_string num), rest
    in
    loop [] lst
;;

let string_of_chars chars =
    let str = String.of_char_list (List.rev chars) in
    String str
;;

(* Parse multiple characters into a string *)
let parse_string lst =
    let rec loop acc = function
        | [] as rest ->
            string_of_chars acc, rest

        | ('0'..'9' | '(' | ')' | ' ' | '\t' | '\n' | '\r' as hd) :: rest ->
            string_of_chars acc, hd :: rest

        | hd :: rest -> 
            loop (hd :: acc) rest
    in
    loop [] lst
;;


(** Turn a string into a token list *)
let tokenize (input : string) : token list =
    let chars = String.to_list input in

    (* Top-level loop to parse entire string *)
    let rec loop acc = function
        | [] -> acc

        (* skip whitespace *)
        | ' ' :: rest
        | '\t' :: rest
        | '\n' :: rest
        | '\r' :: rest -> loop acc rest

        (* parentheses *)
        | '(' :: rest -> loop (LParen :: acc) rest
        | ')' :: rest -> loop (RParen :: acc) rest

        (* integers *)
        | ('0'..'9' as hd) :: rest -> 
            let num, rest = parse_int (hd :: rest) in
            loop (num :: acc) rest

        (* string *)
        | hd :: rest -> 
            let str, rest = parse_string (hd :: rest) in
            loop (str :: acc) rest
    in

    (* Run the loop *)
    chars
    |> loop []
    |> List.rev
;;

let parse_op = function
    | "+" -> Add
    | "-" -> Sub
    | "*" -> Mul
    | "/" -> Div
    | _ -> failwith "Invalid op"
;;

let build_ast (lst : token list) : ast =
    let rec parse_operator (lst : token list) : exp * token list =
        match lst with
        | String s :: rest ->
            let op = parse_op s in

            let rec loop acc = function
                | [] -> failwith "Expected right paren"
                | RParen :: rest -> acc, rest
                | rest ->
                    let exp, rest = parse_exp [] rest in
                    loop (acc @ exp) rest
            in
            let exprs, rest = loop [] rest in
            Op (op, List.rev exprs), rest

        | _ -> failwith "Cannot parse input"

    and parse_exp acc : token list -> exp list * token list = function
        (* End of input, success *)
        | [] -> List.rev acc, []

        (* Start of an operator *)
        | LParen :: (String _ as op) :: rest -> 
            let op, rest = parse_operator (op :: rest) in
            parse_exp (op :: acc) rest

        (* End of operator *)
        | RParen :: rest ->
            acc, RParen :: rest

        (* Integer *)
        | Int i :: rest -> 
            parse_exp (Num i :: acc) rest

        (* Error *)
        | _ -> failwith "Error parsing input"
    in

    parse_exp [] lst |> fst
;;

let eval (ast : ast) =
    (* Only return the last expression's result *)
    let last = List.last_exn ast in

    let rec compute : exp -> int = 
        let reduce op exps =
            let nums = List.map exps ~f:compute in
            List.reduce_exn ~f:op nums
        in
        function
        | Op (Add, exps) -> reduce ( + ) exps
        | Op (Sub, exps) -> reduce ( - ) exps
        | Op (Mul, exps) -> reduce ( * ) exps
        | Op (Div, exps) -> reduce ( / ) exps
        | Num num -> num
    in

    compute last
;;

let gen_code (ast : ast) =
    let last = List.last_exn ast in

    let rec normalize exp =
        match exp with
        | Num _ as n -> n
        | Op (_, []) -> failwith "empty op"
        (* Keep this subtraction, generate 
           negation code in the later stage *)
        | Op (Sub, [_]) as op -> op
        (* Unwrap single operations *)
        | Op (_, [exp]) -> normalize exp
        (* Build operations from the left-to-right bottom-up *)
        | Op (op, exps) -> 
            List.reduce_exn ~f:(fun l r ->
                let l = normalize l in
                let r = normalize r in
                Op (op, [l; r])
            ) exps
    in
    
    let open Instructions in
    let rec compute (instrs : int list list) = 
        let do_op op = function
            | [left; right] -> 
                let left = compute [] left in
                let right = compute [] right in
                    push RCX
                    :: op ~src:RBX ~dst:RCX 
                    :: pop RCX
                    :: pop RBX 
                    :: right @ left
                | _ -> failwith "Un-normalized exp" 
        in

        function
        (* Two's complement negation *)
        | Op (Sub, [exp]) ->
            let v = compute [] exp in
            [push RAX;
            add ~dst:RAX ~src:RBX;
            mov_const ~dst:RBX ~const:1;
            not RAX;
            pop RAX] @ v
        | Op (Add, exps) -> do_op add exps
        | Op (Sub, exps) -> do_op sub exps
        | Op (Mul, exps) -> do_op mul exps
        (* Handle division specially to make sure the
           right registers are used for operands *)
        | Op (Div, [left; right]) ->
            let left = compute [] left in
            let right = compute [] right in
            push RAX
            :: div RCX
            :: pop RAX
            :: pop RCX
            :: right @ left
        | Op (Div, _) -> failwith "Bad div op"
        | Num num ->
            push RCX 
            :: mov_const ~dst:RCX ~const:num 
            :: instrs
    in

    let instrs = compute [] (normalize last) in
    let instrs = exit :: pop RBX :: mov_const ~dst:RAX ~const:1 :: instrs in
    instrs
    |> List.rev
    |> List.join
;;

let compile (input : string) =
    input
    |> tokenize
    |> build_ast
    |> gen_code
    |> Elf.write_file
;;

let rec repl() =
    print_string "> ";
    Out_channel.flush stdout;
    match In_channel.input_line In_channel.stdin with
    | None -> ()
    | Some line ->
        try
            compile line;
            Caml.Sys.command "chmod +x my_exe" |> ignore;
            let result = Caml.Sys.command "./my_exe" in
            print_endline (Int.to_string result);
            repl();
        with _ -> (
            print_endline "Error";
            repl();
        )
;;
