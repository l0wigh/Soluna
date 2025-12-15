let soluna_version = "0.2.0"
type soluna_position = { filename: string; line: int; }
type soluna_expr =
    | Number of int * soluna_position
    | Symbol of string * soluna_position
    | Boolean of bool * soluna_position
    | List of soluna_expr list * soluna_position
    | String of string * soluna_position
    | Primitive of (soluna_expr list -> soluna_expr)
    | Hashmap of (string, soluna_expr) Hashtbl.t * soluna_position
    | Lambda of string list * soluna_expr * (string, soluna_expr) Hashtbl.t
type env = (string, soluna_expr) Hashtbl.t
type soluna_token = { token: string; pos: soluna_position }
let unknown_pos = { filename = "unknown"; line = 0; }
let font_rst = "\x1b[0m"
let font_blue = "\x1b[34m\x1b[1m"
let error_msg = "\x1b[31m\x1b[1mERROR\x1b[0m"
let internal_msg = "\x1b[32m\x1b[1mINTERNAL\x1b[0m"

open Filename

let soluna_parse_atom ptoken =
    let token = ptoken.token in
    let pos = ptoken.pos in
    match token with
    | "true" -> Boolean (true, pos)
    | "false" -> Boolean (false, pos)
    | _ -> begin
        if token.[0] = '\"' then
            let string_split = String.split_on_char '"' token in
            String ((List.nth string_split 1), pos)
        else
            try
                Number ((int_of_string token), pos)
            with
            | Failure _ -> Symbol (token, pos)
    end


let rec soluna_read_tokens token_list =
    match token_list with
    | [] -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Unexpected EOF" error_msg (font_blue ^ (List.hd token_list).pos.filename) (List.hd token_list).pos.line font_rst)
    | h :: t -> begin
        let pos = h.pos in
        match h.token with
        | "(" -> begin
            let (elements, rem_token) = soluna_read_list [] t in
            (List (elements, pos), rem_token)
        end
        | ")" -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Unexpected ')'" error_msg (font_blue ^ pos.filename) pos.line font_rst)
        | _ -> begin
            let atom = soluna_parse_atom h in
            (atom, t)
        end
    end
and soluna_read_list token_acc token_list =
    match token_list with
    | [] -> failwith (Printf.sprintf "Soluna [%s] -> Expression was not closed !" error_msg)
    | h :: t -> begin
        match h.token with
        | ")" -> (List.rev token_acc, t)
        | _ -> begin
            let (sexp, rem_tokens) = soluna_read_tokens (h :: t) in
            soluna_read_list (sexp :: token_acc) rem_tokens
        end
    end

let rec soluna_read_program tok_sexp tok_acc =
    match tok_sexp with
    | [] -> List.rev tok_acc
    | h -> begin
        let (found_tok, rem_tokens) = soluna_read_tokens h in
        soluna_read_program rem_tokens (found_tok :: tok_acc)
    end

let soluna_read_file filename =
    try
        let ic = open_in filename in
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        content
    with
    | Sys_error msg -> failwith (Printf.sprintf "Soluna [%s] -> Cannot open or read file %s: %s" error_msg filename msg)
    | e -> close_in_noerr (open_in filename); raise e

let rec soluna_get_string sexp str =
    match sexp with
    | '"' :: t -> (str, t)
    | h :: t -> soluna_get_string t (str ^ String.make 1 h)
    | _ -> failwith (Printf.sprintf "Soluna [%s] -> String needs to be have \" around them" error_msg)

let soluna_get_pos sexp =
    match sexp with
    | Number (_, pos) | Symbol (_, pos) | Boolean (_, pos) | String (_, pos) | List (_, pos) -> pos
    | Primitive _ | Lambda _ | Hashmap _ -> unknown_pos

let rec soluna_skip_comment sexp =
    match sexp with
    | '\n' :: t -> t
    | _ :: t -> soluna_skip_comment t
    | [] -> sexp

let rec soluna_tokenizer curr_token token_list sexp line filename =
    match sexp with
    | [] -> List.rev (if curr_token != "" then ({token = curr_token; pos = { filename; line }} :: token_list) else token_list)
    | h :: t -> begin
        match h with
        | ' ' | '\r' | '\t' -> if curr_token != "" then soluna_tokenizer "" ({token = curr_token; pos = { filename; line }} :: token_list) t line filename else soluna_tokenizer "" token_list t line filename
        | '\n' -> if curr_token != "" then soluna_tokenizer "" ({token = curr_token; pos = { filename; line }} :: token_list) t (line + 1) filename else soluna_tokenizer "" token_list t (line + 1) filename
        | '(' | ')' -> if curr_token != "" then soluna_tokenizer "" ({token = String.make 1 h; pos = { filename; line }} :: {token = curr_token; pos = { filename; line }} :: token_list) t line filename else soluna_tokenizer "" ({token = String.make 1 h; pos = { filename; line }} :: token_list) t line filename
        | ';' -> soluna_skip_comment t |> (fun r -> soluna_tokenizer "" token_list r (line + 1) filename)
        | '"' -> begin
            let (str, next_sexp) = soluna_get_string t "\"" in
            soluna_tokenizer "" ({token = str; pos = { filename; line }} :: token_list) next_sexp line filename
        end
        | _ -> soluna_tokenizer (curr_token ^ String.make 1 h) token_list t line filename
    end

let rec soluna_string_of_sexp sexp =
  match sexp with
  | Hashmap (h, _) -> begin
      let buffer = Buffer.create 100 in
      Buffer.add_string buffer "{";
      Hashtbl.iter (fun key value ->
          let val_str = soluna_string_of_sexp value in
          Buffer.add_string buffer (Printf.sprintf "\"%s\": %s, " key val_str)
      ) h;
      let output = Buffer.contents buffer in
      if String.length output > 1 then
          String.sub output 0 (String.length output - 2) ^ "}"
      else "{}"
  end
  | Number (n, _) -> Printf.sprintf "%d" n
  | Symbol (s, _) -> s
  | Boolean (b, _) -> if b then "true" else "false"
  | List (expr_list, _) -> begin
      let inner_content = 
          List.map soluna_string_of_sexp expr_list
          |> String.concat " "
      in
      Printf.sprintf "(%s)" inner_content
  end
  | Primitive _ -> Printf.sprintf "Primitive"
  | String (h, _) -> Printf.sprintf "%s" h
  | Lambda _ -> Printf.sprintf "Lambda"

let soluna_apply_arithmetic art args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    let numbers = List.map (fun sexp ->
        match sexp with
        | Number (n, _) -> n
        | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Arithmetic operation requires numbers" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    ) args in
    match numbers with
    | [] -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Arithmetic operation requires at least one argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | h :: t -> begin
        let result = List.fold_left art h t in
        Number (result, unknown_pos)
    end

let rec soluna_eval sexp (env: env) =
    match sexp with
    | Number _  | Boolean _ -> sexp 
    | List ([Symbol ("include", _); String (filename, _)], pos) -> soluna_include_file filename pos env
    | List ((Symbol ("case", _) :: clauses), pos) -> soluna_eval_case clauses pos env
    | List ([Symbol ("defvar", _); Symbol (name, _); raw_val], pos) -> begin
        let bind = soluna_eval raw_val env in
        begin try
            Hashtbl.add env name bind;
        with
        | Invalid_argument _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Cannot redefine immutable variable: %s" error_msg (font_blue ^ pos.filename) pos.line name font_rst)
        end;
        bind
    end
    | List ([Symbol ("if", _); cond_exp; then_exp; else_exp], pos) -> begin
        let eval_cond = soluna_eval cond_exp env in
        (
            match eval_cond with
            | Boolean (true, _) -> soluna_eval then_exp env
            | Boolean (false, _) -> soluna_eval else_exp env
            | _ -> failwith (Printf.sprintf "Solune [%s] %s:%d%s -> if condition must evaluate to a Boolean" error_msg (font_blue ^ pos.filename) pos.line font_rst)
        )
    end
    | List ([Symbol ("function", _); Symbol (name, _); List (params_list, _); body_sexp], pos) -> begin
        let params_names = List.map (fun p ->
            match p with
            | Symbol (s, _) -> s
            | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Function parameters must be symbols" error_msg (font_blue ^ pos.filename) pos.line font_rst)
        ) params_list in
        let named_lambda = Lambda (params_names, body_sexp, env) in
        Hashtbl.replace env name named_lambda;
        named_lambda
    end
    | List ([Symbol ("lambda", _); List (params_list, _); body_sexp], pos) -> begin
        let params_names = List.map (fun p ->
            match p with
            | Symbol (s, _) -> s
            | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Lambda function parameters must be symbols" error_msg (font_blue ^ pos.filename) pos.line font_rst)
        ) params_list in
        Lambda (params_names, body_sexp, env)
    end
    | List ((h :: t), pos) -> soluna_eval_list_form (List ((h :: t), pos)) env
    | List ([], _) -> sexp
    | String (s, pos) -> String (s, pos)
    | Symbol (s, pos) -> (try Hashtbl.find env s with | Not_found -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Unbound symbol '%s'" error_msg (font_blue ^ pos.filename) pos.line font_rst s))
    | _ -> failwith (Printf.sprintf "Soluna [%s] -> soluna_eval is missing something" internal_msg)
and soluna_include_file filename pos env =
    let current_dir = dirname (font_blue ^ pos.filename) in
    let path = concat current_dir filename in
    try
        let content = soluna_read_file path in
        let parsed_sexp = content
        |> String.to_seq
        |> List.of_seq
        in
        let tokenized = soluna_tokenizer "" [] parsed_sexp 1 path in
        let prog = soluna_read_program tokenized [] in
        List.iter (fun sexp -> let _ = soluna_eval sexp env in ()) prog;
        Symbol ("nil", pos)
    with Sys_error msg -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Cannot include file '%s': %s" error_msg (font_blue ^ pos.filename) pos.line path msg font_rst)
and soluna_eval_case clauses pos env =
    match clauses with
    | [] -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'case' should have atlease one clause" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | [List ([Symbol ("default", _); res_expr], _)] -> soluna_eval res_expr env
    | List ([Symbol ("default", _); _], _) :: _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'default' case should be the last" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | List ([test_expr; res_expr], _) :: rst_clauses -> begin
        let eval = soluna_eval test_expr env in
        match eval with
        | Boolean (true, _) -> soluna_eval res_expr env
        | Boolean (false, _) -> soluna_eval_case rst_clauses pos env
        | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Conditionnal expressions should return a Boolean" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    end
        | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Invalid 'case' use. Need at least a default case" error_msg (font_blue ^ pos.filename) pos.line font_rst)
and soluna_eval_list_form sexp env =
    match sexp with
    | List ((Symbol ("do", _) :: expr_list), _) -> soluna_eval_do expr_list env
    | List ((h :: t), pos) -> begin
        let op = soluna_eval h env in
        let args = List.map (fun arg -> soluna_eval arg env) t in
        (
            match op with
            | Primitive fn -> fn args
            | Lambda (params, body, lambda_env) -> begin
                if List.length params <> List.length args then
                    failwith (Printf.sprintf "Solune [%s] %s:%d%s -> Wrong number of arguments passed to function" error_msg (font_blue ^ pos.filename) pos.line font_rst)
                else
                    let local_env = Hashtbl.copy lambda_env in
                    List.iter2 (fun name value -> Hashtbl.replace local_env name value) params args;
                    soluna_eval body local_env
            end
            | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Expected a symbol, got: %s" error_msg (font_blue ^ pos.filename) pos.line (soluna_string_of_sexp op) font_rst)
        )
    end
    | _ -> failwith (Printf.sprintf "Soluna [%s] -> soluna_eval_list_form called with non-list expression" internal_msg)
and soluna_eval_do expr_list env =
    match expr_list with
    | [] -> Symbol ("nil", unknown_pos)
    | [last_expr] -> soluna_eval last_expr env
    | h :: t -> begin
        let _ = soluna_eval h env in
        soluna_eval_do t env
    end

let soluna_write_string s =
    let len = String.length s in
    let buffer = Buffer.create len in
    let rec loop i =
        if i >= len then
            Buffer.contents buffer
        else if i + 1 < len && s.[i] = '\\' then begin
            match s.[i+1] with
            | 'n' -> Buffer.add_char buffer '\n'; loop (i + 2)
            | 't' -> Buffer.add_char buffer '\t'; loop (i + 2)
            | '\\' -> Buffer.add_char buffer '\\'; loop (i + 2)
            | c -> Buffer.add_char buffer '\\'; Buffer.add_char buffer c; loop (i + 2)
        end else begin
            Buffer.add_char buffer s.[i]; loop (i + 1)
        end
    in
    loop 0

let soluna_write_expression mode args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [sexp] -> begin
        let output = match sexp with
        | String (s, _) -> soluna_write_string s
        | _ -> soluna_string_of_sexp sexp in
        if mode then Printf.printf "%s\n" output else Printf.printf "%s" output;
        flush stdout;
        sexp
    end
    | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'write' requires exactly one argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_apply_comp op args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [Number (a, _); Number (b, _)] -> Boolean ((op a b), pos)
    | [String (a, _); String (b, _)] -> begin 
        match (String.compare a b) with
        | 0 -> Boolean (true, pos)
        | _ -> Boolean (false, pos)
    end
    | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Comparison requires two arguments of the same type (string or int)" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_apply_modulo args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [Number (a, _); Number (b, _)] -> begin 
        if b = 0 then failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Division by zero" error_msg (font_blue ^ pos.filename) pos.line font_rst)
        else Number ((a mod b), unknown_pos)
    end
    | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Modulo operation requires two numbers" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_list_primitive args = List (args, unknown_pos)

let soluna_fst_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [List ((h :: _), _)] -> h
    | [List ([], pos)] -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'fst' called on empty list" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'fst' requires exactly one list as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_rst_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [List ((_ :: t), pos)] -> List (t, pos)
    | [List ([], pos)] -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'rst' called on empty list" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'rst' requires exactly one list as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_null_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [List ([], pos)] -> Boolean (true, pos)
    | [List ((_ :: _), pos)] -> Boolean (false, pos)
    | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'null' requires one list argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_num_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [Number _] -> Boolean (true, pos)
    | _ -> Boolean (false, pos)

let soluna_cons_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [h; List (t, pos)] -> List ((h :: t), pos)
    | [_; _] -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'cons' requires a list as its second argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'cons' requires an element and a list" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_map_lambda_to_sexp lambda sexp =
    match lambda with
    | Lambda (params, body, lambda_env) -> begin
        if List.length params <> 1 then
            failwith (Printf.sprintf "Soluna [%s] -> Function passed to 'map' must accept exactly one argument" error_msg)
        else
            let local_env = Hashtbl.copy lambda_env in
            Hashtbl.replace local_env (List.hd params) sexp;
            soluna_eval body local_env
    end
    | Primitive fn -> fn [sexp]
    | _ -> failwith (Printf.sprintf "Soluna [%s] -> First argument to 'map' must be a function" error_msg)

let soluna_map_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [f; List (lst, pos)] -> begin
        let rec map_aux curr_lst =
            match curr_lst with
            | [] -> List ([], pos)
            | h :: t -> begin
                let res_h = soluna_map_lambda_to_sexp f h in
                let res_t_sexp = map_aux t in
                match res_t_sexp with
                | List (l, pos) -> List ((res_h :: l), pos)
                | _ -> failwith (Printf.sprintf "Soluna [%s] -> map recursion broke (tail result was not a List)" internal_msg)
            end
        in
        map_aux lst
    end
    | [_; _] -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'map' requires a function and a list (second argument must be a List)" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'map' requires exactly two arguments" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_filter_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [f; List (lst, p)] -> begin
        let rec filter_aux curr_lst =
            match curr_lst with
            | [] -> List ([], p)
            | h :: t -> begin
                let res_h = soluna_map_lambda_to_sexp f h in
                match res_h with
                | Boolean (true, _) -> begin
                    let res_t = filter_aux t in
                    match res_t with
                    | List (l, _) -> List ((h :: l), pos)
                    | _ -> failwith (Printf.sprintf "Soluna [%s] -> filter recursion broke (tail result was not a List)" internal_msg)
                end
                | Boolean (false, _) -> filter_aux t
                | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Functions passed to filter must return a Boolean" error_msg (font_blue ^ pos.filename) pos.line font_rst)
            end
        in
        filter_aux lst
    end
    | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'filter' requires exactly a function and a list" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_reverse_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [List (h, p)] -> List (List.rev h, p)
    | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'reverse' takes a list as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_dict_primitive args =
    match args with
    | [Number (s, pos)] -> begin
        let new_map = Hashtbl.create s in
        Hashmap (new_map, pos)
    end
    | _ -> failwith (Printf.sprintf "Soluna [%s] -> 'dict' requires a size argument" error_msg)

let soluna_dict_set_primitive args =
    match args with
    | [Hashmap (h, _); String (key, _); value_sexp] -> begin
        Hashtbl.replace h key value_sexp;
        value_sexp
    end
    | _ -> failwith (Printf.sprintf "Soluna [%s] 'dict-set' requires a key and a value" error_msg)

let soluna_dict_get_primitive args =
    match args with
    | [Hashmap (h, pos); String (key, _)] -> begin
        try
            Hashtbl.find h key
        with Not_found ->
            failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> Key '%s' not found in dictionnary" error_msg (font_blue ^ pos.filename) pos.line key font_rst)
    end
    | _ -> failwith (Printf.sprintf "Soluna [%s] -> 'dict-get' requires a dictionnary and a key" error_msg)

let soluna_dict_ref_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [Hashmap (h, _); String (key, _); default] -> begin
        try
            Hashtbl.find h key
        with Not_found -> default
    end
    | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'dict-ref' requires a dictionnary, a key and a default value" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_explode_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [String (s, p)] -> begin
        let res = String.to_seq s
        |> List.of_seq
        |> List.map (fun c -> String (String.make 1 c, p)) in
        List (res, pos)
    end
    | _ -> failwith (Printf.sprintf "Soluna [%s] %s:%d%s -> 'explode' requires a string as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)
let soluna_init_env () : env =
    let env = Hashtbl.create 20 in 
    Hashtbl.replace env "+" (Primitive (soluna_apply_arithmetic (+))); 
    Hashtbl.replace env "-" (Primitive (soluna_apply_arithmetic (-))); 
    Hashtbl.replace env "*" (Primitive (soluna_apply_arithmetic ( * )));
    Hashtbl.replace env "/" (Primitive (soluna_apply_arithmetic (/)));
    Hashtbl.replace env "<" (Primitive (soluna_apply_comp (<)));
    Hashtbl.replace env ">" (Primitive (soluna_apply_comp (>)));
    Hashtbl.replace env "=" (Primitive (soluna_apply_comp (=)));
    Hashtbl.replace env ">=" (Primitive (soluna_apply_comp (>=)));
    Hashtbl.replace env "<=" (Primitive (soluna_apply_comp (<=)));
    Hashtbl.replace env "mod" (Primitive soluna_apply_modulo); 
    Hashtbl.replace env "write" (Primitive (soluna_write_expression false));
    Hashtbl.replace env "writeln" (Primitive (soluna_write_expression true));
    Hashtbl.replace env "list" (Primitive soluna_list_primitive);
    Hashtbl.replace env "fst" (Primitive soluna_fst_primitive);
    Hashtbl.replace env "rst" (Primitive soluna_rst_primitive);
    Hashtbl.replace env "null" (Primitive soluna_null_primitive);
    Hashtbl.replace env "cons" (Primitive soluna_cons_primitive);
    Hashtbl.replace env "map" (Primitive soluna_map_primitive);
    Hashtbl.replace env "num" (Primitive soluna_num_primitive);
    Hashtbl.replace env "filter" (Primitive soluna_filter_primitive);
    Hashtbl.replace env "reverse" (Primitive soluna_reverse_primitive);
    Hashtbl.replace env "explode" (Primitive soluna_explode_primitive);
    Hashtbl.replace env "dict" (Primitive soluna_dict_primitive);
    Hashtbl.replace env "dict-set" (Primitive soluna_dict_set_primitive);
    Hashtbl.replace env "dict-get" (Primitive soluna_dict_get_primitive);
    Hashtbl.replace env "dict-ref" (Primitive soluna_dict_ref_primitive);
    env

let () =
    try
        let filename = match Sys.argv with
        | [|_; "-v"|] -> Printf.printf "Soluna %s\n" (font_blue ^ soluna_version ^ font_rst); exit 0
        | [|_; filename|] -> filename
        | _ -> Printf.eprintf "Usage: %s <filename.luna>" Sys.argv.(0); exit 1 in

        let parsed_sexp = soluna_read_file filename
        |> String.to_seq
        |> List.of_seq in
        let parsed_sexp = soluna_tokenizer "" [] parsed_sexp 1 filename in

        let program = soluna_read_program parsed_sexp [] in
        let global_env = soluna_init_env () in

        List.iter (fun sexp -> let _ = soluna_eval sexp global_env in ()) program;
    with
    | Failure msg -> Printf.eprintf "%s\n" msg; exit 1
    | e -> Printf.eprintf "Soluna [%s] -> Unexpected exception: %s\n" internal_msg (Printexc.to_string e); exit 1
