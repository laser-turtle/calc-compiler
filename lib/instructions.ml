open! Core

type arg = RAX
         | RDX
         | RBX
         | RCX

let mov_r ~src ~dst =
    let reg = 
        match src, dst with
        | RAX, RAX -> 0xc0
        | RAX, RBX -> 0xd8
        | RAX, RCX -> 0xc8
        | RAX, RDX -> 0xd0

        | RBX, RAX -> 0xc3
        | RBX, RBX -> 0xdb
        | RBX, RCX -> 0xcb
        | RBX, RDX -> 0xd3

        | RCX, RAX -> 0xc1
        | RCX, RBX -> 0xd9
        | RCX, RCX -> 0xc9
        | RCX, RDX -> 0xd1

        | RDX, RAX -> 0xc2
        | RDX, RBX -> 0xda
        | RDX, RCX -> 0xca
        | RDX, RDX -> 0xd2
    in
    [0x48; 0x89; reg]
;;

let exit = [0xcd; 0x80]

let address a =
    [a land 0xff; 
     (a lsr 8) land 0xff;
     (a lsr 16) land 0xff;
     (a lsr 24) land 0xff;
    ]
;;

let const_64 (i : Int64.t) : int list =
    let open Int64 in
    let (land) = bit_and in
    let (lsr) = shift_right_logical in
    let mask = Int64.of_int 0xff in
    let sm v = (i lsr v) land mask |> to_int_exn in
    [sm 0; 
     sm 8;
     sm 16;
     sm 24;
     sm 32;
     sm 40;
     sm 48;
     sm 56;
    ]
;;

let const_64i n = const_64 (Int64.of_int n)

let%expect_test "address" =
  List.iter ~f:(fun i -> Printf.printf "0x%x " i) (address 0x8010b0);
  [%expect {| 0xb0 0x10 0x80 0x0 |}]
;;

let%expect_test "const64" =
  List.iter ~f:(fun i -> Printf.printf "0x%x " i) (const_64 Int64.(of_int 0x12345678ABCDEF12));
;;

let mov_const ~dst ~const =
    let reg =
        match dst with
        | RAX -> 0xc0 
        | RBX -> 0xc3
        | RCX -> 0xc1
        | RDX -> 0xc2
    in
    [0x48; 0xc7; reg] @ (address const)
;;

let mov_const64 ~dst ~const =
    let reg =
        match dst with
        | RAX -> 0xc0 
        | RBX -> 0xc3
        | RCX -> 0xc1
        | RDX -> 0xc2
    in
    [0x48; 0xc7; reg] @ (const_64 const)
;;

let load ~dst ~addr =
    let addr = address addr in
    let reg =
        match dst with
        | RAX -> 0x04
        | RBX -> 0x1c
        | RCX -> 0x0c
        | RDX -> 0x14
    in
    [0x48; 0x8b; reg; 0x25] @ addr
;;

let save ~src ~addr =
    let addr = address addr in
    let reg =
        match src with
        | RAX -> 0x04
        | RBX -> 0x1c
        | RCX -> 0x0c
        | RDX -> 0x14
    in
    [0x48; 0x89; reg; 0x25] @ addr
;;

let get_reg_operands ~src ~dst =
    match dst, src with
    | RAX, RAX -> 0xc0
    | RAX, RBX -> 0xd8
    | RAX, RCX -> 0xc8
    | RAX, RDX -> 0xd0

    | RBX, RAX -> 0xc3
    | RBX, RBX -> 0xdb
    | RBX, RCX -> 0xcb
    | RBX, RDX -> 0xd3

    | RCX, RAX -> 0xc1
    | RCX, RBX -> 0xd9
    | RCX, RCX -> 0xc9
    | RCX, RDX -> 0xd1

    | RDX, RAX -> 0xc2
    | RDX, RBX -> 0xda
    | RDX, RCX -> 0xca
    | RDX, RDX -> 0xd2
;;

let add ~src ~dst =
    let reg = get_reg_operands ~src ~dst in
    [0x48; 0x01; reg]
;;

let sub ~src ~dst =
    let reg = get_reg_operands ~src ~dst in
    [0x48; 0x29; reg]
;;

let not reg =
    let reg =
        match reg with
        | RAX -> 0xd0
        | RBX -> 0xd3
        | RCX -> 0xd1
        | RDX -> 0xd2
    in
    [0x48; 0xf7; reg]
;;

let mul ~src ~dst =
    let reg =
        match dst, src with
        | RAX, RAX -> 0xc0
        | RAX, RBX -> 0xc3
        | RAX, RCX -> 0xc1
        | RAX, RDX -> 0xc2

        | RBX, RAX -> 0xd8
        | RBX, RBX -> 0xdb
        | RBX, RCX -> 0xd9
        | RBX, RDX -> 0xda

        | RCX, RAX -> 0xc8
        | RCX, RBX -> 0xcb
        | RCX, RCX -> 0xc9
        | RCX, RDX -> 0xca

        | RDX, RAX -> 0xd0
        | RDX, RBX -> 0xd3
        | RDX, RCX -> 0xd1
        | RDX, RDX -> 0xd2
    in
    [0x48; 0x0f; 0xaf; reg]
;;

let push = function
    | RAX -> [0x50]
    | RBX -> [0x53]
    | RCX -> [0x51]
    | RDX -> [0x52]
;;

let pop = function
    | RAX -> [0x58]
    | RBX -> [0x5b]
    | RCX -> [0x59]
    | RDX -> [0x5a]
;;

let div = function
    | RAX -> [0x48; 0xf7; 0xf8]
    | RBX -> [0x48; 0xf7; 0xfb]
    | RCX -> [0x48; 0xf7; 0xf9]
    | RDX -> [0x48; 0xf7; 0xfa]
