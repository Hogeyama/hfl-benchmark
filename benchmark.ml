open Core

module Util = struct

  let run_command ?(timeout=20.0) cmd =
    let read_file file = In_channel.(with_file file ~f:input_all) in
    let f_out, fd_out = Unix.mkstemp "/tmp/hfl_benchmark_tmp.stdout" in
    let f_err, fd_err = Unix.mkstemp "/tmp/hfl_benchmark_tmp.stderr" in
    let process_status = Lwt_main.run @@
      Lwt_process.exec
        ~timeout
        ~stdout:(`FD_move fd_out)
        ~stderr:(`FD_move fd_err)
        ("", cmd)
    in
    let stdout = read_file f_out in
    let stderr = read_file f_err in
    Unix.remove f_out;
    Unix.remove f_err;
    (process_status, stdout, stderr)

  let ask_yes_or_no () =
    try
      let yes_or_no = read_line() in
      Char.equal (yes_or_no.[0]) 'y' ||
      Char.equal (yes_or_no.[0]) 'Y'
    with _ -> false
end

let dune_root =
  let rec go dir =
    if List.mem ~equal:String.equal (Sys.ls_dir dir) "dune-project"
    then dir
    else go (dir ^ "/..")
  in
  Filename.realpath (go (Sys.getcwd())) ^ "/"

type file_type = HFL | ML | HOCHC
let file_type_of_string =
  let ok x  = `Ok x in
  let err e = `Error e in
  function
  | "hfl"   -> ok HFL
  | "ml"    -> ok ML
  | "hochc" -> ok HOCHC
  | s       -> err ("Unknown file_type: " ^ s)
let pp_file_type ppf = function
  | HFL   -> Fmt.string ppf "hfl"
  | ML    -> Fmt.string ppf "ml"
  | HOCHC -> Fmt.string ppf "hochc"
let file_type_cmdliner_converter = file_type_of_string, pp_file_type

type [@warning "-39"] params =
  { script     : string           [@default "hflmc2"]
    (** Specify a script in ./scripts.
        The script should write one of 'Valid', 'Invalid', 'Unknown', or 'Error' to stdout. *)
  ; file_type  : file_type option [@conv file_type_cmdliner_converter] [@docv "hfl|ml|hochc"]
  ; case_set   : string           [@pos 0] [@docv "CASE"]
    (** Specify a file in ./lists *)
  ; timeout    : int              [@default 20]
    (** Timeout in seconds *)
  ; save_file  : string option
    (** file to save result in json format.
        Default:"./result/SCRIPT-CASE.json" *)
  ; verbose    : bool
  }
  [@@deriving cmdliner]

let get_script params = dune_root^"scripts/"^params.script

let get_file_type params = match params.file_type, params.script with
  | Some ty, _  -> ty
  | _, "mochi"  -> ML
  | _, "horus"  -> HOCHC
  | _, "hflmc"  -> HFL
  | _, "hflmc2" -> HFL
  | _           -> HFL

let list_cases case_set =
  In_channel.(with_file (dune_root^"lists/"^case_set) ~f:input_all)
  |> String.split_lines

type time = float
  [@@deriving yojson]
type success =
  { tag  : [`Valid | `Invalid]
  ; time : time
  } [@@deriving yojson]
type result =
  | Success of success
  | Unknown of string
  | Failure of string
  | Timeout
  [@@deriving yojson]
type case_result =
  { case : string
  ; result : result
  } [@@deriving yojson]

let kill_zombie_processes () =
  List.iter ["hflmc";"hflmc2";"mochi";"horsat";"z3"] ~f:begin fun p ->
    ignore @@ Util.run_command [|"pkill"; p|]
  end

let run_hflmc2 params case file =
  let script = get_script params in
  if not (Sys.file_exists_exn script) then failwith ("script "^script^" not found");
  let cmd     = [| script; file |] in
  let timeout = float_of_int params.timeout in
  let (p, out, err), time =
    let start = Unix.gettimeofday () in
    let r = Util.run_command ~timeout cmd in
    let stop = Unix.gettimeofday () in
    r, stop-.start
  in
  if params.verbose then (Printf.printf "%s" err);
  begin match p with
  | WEXITED 0 ->
      let result = match out with
        | "Valid"   -> Success { tag = `Valid; time }
        | "Invalid" -> Success { tag = `Invalid; time }
        | "Unknown" -> Unknown err
        | "Error"   -> Failure err
        | _ ->
            failwith @@ "parse error: '" ^ String.escaped out ^ "'\n"
                      ^ "expected: 'Valid', 'Invalid', 'Unknown', or 'Error'"
      in { case; result }
  | WEXITED _ ->
      failwith @@ "script "^ script ^" failed"
                 ^"\nstdout:\n" ^ out
                 ^"\nsterr: \n" ^ err
  | _ ->
      { case; result = Timeout }
  end

type meta =
  { command    : string
  ; timeout    : int
  } [@@deriving yojson]

type whole_result =
  { meta : meta
  ; results: case_result list
  } [@@deriving yojson]

let run_bench params =
  let cases = list_cases params.case_set in
  let max_len =
    cases
    |> List.map ~f:String.length
    |> List.fold ~init:0 ~f:max
  in
  let meta =
    { command    = get_script params
    ; timeout    = params.timeout
    }
  in
  let results =
    List.map cases ~f:begin fun case ->
      let formatted_case =
        let pudding = String.(init (max_len - length case) ~f:(Fn.const ' ')) in
        case ^ pudding
      in
      let file = match get_file_type params with
        | HFL   -> dune_root^"inputs/hfl/"  ^case^".in"
        | ML    -> dune_root^"inputs/ml/"   ^case^".ml"
        | HOCHC -> dune_root^"inputs/hochc/"^case^".inp"
      in
      if not params.verbose then (print_string (formatted_case^": "); flush stdout);
      let res = run_hflmc2 params case file in
      if params.verbose then print_string (formatted_case^": ");
      print_endline @@ begin match res.result with
        | Success s -> Format.sprintf "Success %7.3f" s.time
        | Failure _ -> "Failure"
        | Unknown _ -> "Unknown"
        | Timeout   -> "Timeout"
      end;
      kill_zombie_processes();
      res
    end
  in
  { meta; results }

let () =
  try
    let params =
      match Cmdliner.Term.(eval (params_cmdliner_term (), info "hflmc2-benchmark")) with
      | `Ok p -> p
      | _     -> exit 1
    in
    let result = run_bench params in
    let json : Yojson.Safe.t = whole_result_to_yojson result in
    let save_file =
      let default = dune_root^"result/"^params.script^"-"^params.case_set^".json" in
      Option.value ~default params.save_file in
    if Sys.file_exists_exn save_file then begin
      print_string (save_file^" already exists. overwrite?: ");
      flush stdout;
      if Util.ask_yes_or_no() then
        Yojson.Safe.to_file ~std:true save_file json
      else begin
        let file = Filename.temp_file ~in_dir:"/tmp" "hfl-benchmark" ".json" in
        Yojson.Safe.to_file ~std:true file json;
        print_endline @@ "saved in " ^ file
      end
    end else begin
      Yojson.Safe.to_file ~std:true save_file json
    end
  with Failure e -> Printf.eprintf "%s" e; exit 1

