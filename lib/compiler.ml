open Core

type op = Add
        | Sub
        | Mul
        | Div

type exp = Op of op * exp list
         | Num of int
         | Def of string * exp
         | Var of string

type ast = exp list

let parse_op = function
    | "+" -> Add
    | "-" -> Sub
    | "*" -> Mul
    | "/" -> Div
    | _ -> failwith "Invalid op"
;;

let is_int n =
    match Int.of_string n with
    | _ -> true
    | exception _ -> false
;;

let build_ast (lst : Sexp.t list) : ast =
    let open Sexp in

    (* Two-level parsing, so we only allow defining variables at the top-level *)
    let rec parse_top_level acc : Sexp.t list -> exp list * Sexp.t list = function
        (* End of input, success *)
        | [] -> List.rev acc, []

        (* Variable definition *)
        | List [Atom "define"; Atom var; exp] :: rest ->
            let value, _ = parse_exp [] [exp] in
            let exp = Def (var, List.hd_exn value) in
            parse_top_level (exp :: acc) rest

        | List (Atom "define" :: _) :: _ ->
            failwith "Error with define"

        (* Forward to sub expression *)
        | exp :: rest -> 
            let exp, _ = parse_exp [] [exp] in
            parse_top_level (List.hd_exn exp :: acc) rest

    and parse_exp acc : Sexp.t list -> exp list * Sexp.t list = function
        (* End of input, success *)
        | [] -> List.rev acc, []

        (* Integer *)
        | Atom i :: rest when is_int i -> 
            parse_exp (Num (Int.of_string i) :: acc) rest

        (* Can't have a nested variable definition *)
        | List (Atom "define" :: _) :: _ ->
            failwith "Variables must be defined at the top level"

        (* Not an integer? probably a var *)
        | Atom var :: rest ->
            parse_exp (Var var :: acc) rest

        (* Operator *)
        | List (Atom ("+" as op) :: args) :: rest
        | List (Atom ("-" as op) :: args) :: rest
        | List (Atom ("*" as op) :: args) :: rest
        | List (Atom ("/" as op) :: args) :: rest ->
            let args = 
                args
                |> List.map ~f:(fun a -> parse_exp [] [a])
                |> List.map ~f:fst
                |> List.map ~f:List.hd_exn
            in
            let exp = Op (parse_op op, args) in
            parse_exp (exp :: acc) rest

        (* Error *)
        | e -> 
            List.iter ~f:(fun e -> print_endline (Sexp.to_string e)) e;
            failwith "Error parsing input"
    in

    parse_top_level [] lst |> fst
;;

let eval (ast : ast) =
    let env = Hashtbl.Poly.create () in

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
        | Var var -> Hashtbl.find_exn env var
        | Def (var, exp) -> 
            let value = compute exp in
            Hashtbl.set env ~key:var ~data:value;
            value
    in

    List.fold ~init:0 ~f:(fun _ e -> compute e) ast
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
        | _ -> failwith "Unhandled normalize"
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
        | _ -> failwith "Unhandled normalize"
    in

    let instrs = compute [] (normalize last) in
    let instrs = exit :: pop RBX :: mov_const ~dst:RAX ~const:1 :: instrs in
    instrs
    |> List.rev
    |> List.join
;;

let compile (input : string) =
    input
    |> Sexp.of_string_many
    |> build_ast
    |> gen_code
    |> Elf.write_file
;;

let run (input : string) =
    input
    |> Sexp.of_string_many
    |> build_ast
    |> eval
;;

let rec repl() =
    print_string "> ";
    Out_channel.flush stdout;
    match In_channel.input_line In_channel.stdin with
    | None -> ()
    | Some line ->
        try
            (*
            compile line;
            Caml.Sys.command "chmod +x my_exe" |> ignore;
            let result = Caml.Sys.command "./my_exe" in
            print_endline (Int.to_string result);
            *)
            print_endline (Int.to_string (run line));
            repl();
        with e -> (
            print_endline "Error";
            print_endline (Exn.to_string e);
            repl();
        )
;;
