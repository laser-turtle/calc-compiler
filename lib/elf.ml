open! Core

type code = {
    instructions : int list;
    data : int list;
}

let base_address = 0x401000 + 0x40 + 0x38 + 0x38
let base_data_address = 0x801000 + 0x40 + 0x38 + 0x38

let create ~data ~instrs =
    let size = List.length data in
    let entry = Instructions.const_64i (
        base_address + size
    ) in
    let total_size = 
        (List.length instrs + size + 0x40 + 0x38 + 0x38)
        |> Instructions.const_64i
    in

    let header = [
        0x7f; 0x45; 0x4c; 0x46; (* magic *)
        0x02; 0x01; 0x01; 0x00;
        0x00; 0x00; 0x00; 0x00; (* padding *)
        0x00; 0x00; 0x00; 0x00; 
        0x02; 0x00;
        0x3e; 0x00;
        0x01; 0x00; 0x00; 0x00;
    ] @ entry @ [
        0x40; 0x00; 0x00; 0x00; 
        0x00; 0x00; 0x00; 0x00;

        0x00; 0x00; 0x00; 0x00;
        0x00; 0x00; 0x00; 0x00;

        0x00; 0x00; 0x00; 0x00;

        0x40; 0x00; 0x38; 0x00;
        0x02; 0x00; 0x00; 0x00;
        0x00; 0x00; 0x00; 0x00;
    ] in
    let exec = [
        0x01; 0x00; 0x00; 0x00;
        0x05; 0x00; 0x00; 0x00;

        0x00; 0x00; 0x00; 0x00;
        0x00; 0x00; 0x00; 0x00;

        0x00; 0x10; 0x40; 0x00;
        0x00; 0x00; 0x00; 0x00;

        0x00; 0x10; 0x40; 0x00;
        0x00; 0x00; 0x00; 0x00;
    ] @ total_size @ total_size @ [
        0x00; 0x10; 0x00; 0x00;
        0x00; 0x00; 0x00; 0x00;
    ] in
    let data_sect = [
        0x01; 0x00; 0x00; 0x00;
        0x02; 0x00; 0x00; 0x00;

        0x00; 0x00; 0x00; 0x00;
        0x00; 0x00; 0x00; 0x00;

        0x00; 0x10; 0x80; 0x00;
        0x00; 0x00; 0x00; 0x00;

        0x00; 0x10; 0x80; 0x00;
        0x00; 0x00; 0x00; 0x00;
    ] @ total_size @ total_size @ [
        0x00; 0x10; 0x00; 0x00;
        0x00; 0x00; 0x00; 0x00;
    ] in
    header @ exec @ data_sect @ data @ instrs
;;

let write_file (code : code) : unit =
    let exe = create ~data:code.data ~instrs:code.instructions in
    Out_channel.with_file ~binary:true "my_exe" ~f:(fun chan ->
        List.iter ~f:(fun b ->
            Out_channel.output_byte chan b
        ) exe
    )
;;
