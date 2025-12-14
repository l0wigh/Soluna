type soluna_position = { line: int; }
type soluna_expr =
    | Number of int * soluna_position
    | Symbol of string * soluna_position
    | Boolean of bool * soluna_position
    | List of soluna_expr list * soluna_position
    | String of string * soluna_position
    | Primitive of (soluna_expr list -> soluna_expr)
    | Lambda of string list * soluna_expr * (string, soluna_expr) Hashtbl.t
type env = (string, soluna_expr) Hashtbl.t
type soluna_token = { token: string; pos: soluna_position }
let unknown_pos = { line = 0; }

let rec soluna_get_string sexp str =
    match sexp with
    | '"' :: t -> (str, t)
    | h :: t -> soluna_get_string t (str ^ String.make 1 h)
    | _ -> failwith "Soluna [ERROR]: String needs to be have \" around them"

let soluna_get_pos sexp =
    match sexp with
    | Number (_, pos) | Symbol (_, pos) | Boolean (_, pos) | String (_, pos) | List (_, pos) -> pos
    | Primitive _ | Lambda _ -> unknown_pos

let rec soluna_skip_comment sexp =
    match sexp with
    | '\n' :: t -> t
    | _ :: t -> soluna_skip_comment t
    | [] -> sexp

let rec soluna_tokenizer curr_token token_list sexp line =
    match sexp with
    | [] -> List.rev (if curr_token != "" then ({token = curr_token; pos = { line }} :: token_list) else token_list)
    | h :: t -> begin
        match h with
        | ' ' | '\r' | '\t' -> if curr_token != "" then soluna_tokenizer "" ({token = curr_token; pos = { line }} :: token_list) t line else soluna_tokenizer "" token_list t line
        | '\n' -> if curr_token != "" then soluna_tokenizer "" ({token = curr_token; pos = { line }} :: token_list) t (line + 1) else soluna_tokenizer "" token_list t (line + 1)
        | '(' | ')' -> if curr_token != "" then soluna_tokenizer "" ({token = String.make 1 h; pos = { line }} :: {token = curr_token; pos = { line }} :: token_list) t line else soluna_tokenizer "" ({token = String.make 1 h; pos = { line }} :: token_list) t line
        | ';' -> soluna_skip_comment t |> (fun r -> soluna_tokenizer "" token_list r (line + 1))
        | '"' -> begin
            let (str, next_sexp) = soluna_get_string t "\"" in
            soluna_tokenizer "" ({token = str; pos = { line }} :: token_list) next_sexp line
        end
        | _ -> soluna_tokenizer (curr_token ^ String.make 1 h) token_list t line
    end

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
    | [] -> failwith (Printf.sprintf "Soluna [ERROR] L%d: Unexpected EOF" (List.hd token_list).pos.line)
    | h :: t -> begin
        let pos = h.pos in
        match h.token with
        | "(" -> begin
            let (elements, rem_token) = soluna_read_list [] t in
            (List (elements, pos), rem_token)
        end
        | ")" -> failwith (Printf.sprintf "Soluna [ERROR] L%d: Unexpected ')'" pos.line)
        | _ -> begin
            let atom = soluna_parse_atom h in
            (atom, t)
        end
    end
and soluna_read_list token_acc token_list =
    match token_list with
    | [] -> failwith "Soluna: List was not closed !"
    | h :: t -> begin
        match h.token with
        | ")" -> (List.rev token_acc, t)
        | _ -> begin
            let (sexp, rem_tokens) = soluna_read_tokens (h :: t) in
            soluna_read_list (sexp :: token_acc) rem_tokens
        end
    end

let rec soluna_string_of_sexp sexp =
  match sexp with
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
        | _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: Arithmetic operation requires numbers" pos.line)
    ) args in
    match numbers with
    | [] -> failwith (Printf.sprintf "Soluna [ERROR] L%d: Arithmetic operation requires at least one argument" pos.line)
    | h :: t -> begin
        let result = List.fold_left art h t in
        Number (result, unknown_pos)
    end

let rec soluna_eval sexp (env: env) =
    match sexp with
    | Number _  | Boolean _ -> sexp 
    | List ([Symbol ("defvar", _); Symbol (name, _); raw_val], pos) -> begin
        let bind = soluna_eval raw_val env in
        begin try
            Hashtbl.add env name bind;
        with
        | Invalid_argument _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: Cannot redefine immutable variable: %s" pos.line name)
        end;
        bind
    end
    | List ([Symbol ("if", _); cond_exp; then_exp; else_exp], pos) -> begin
        let eval_cond = soluna_eval cond_exp env in
        (
            match eval_cond with
            | Boolean (true, _) -> soluna_eval then_exp env
            | Boolean (false, _) -> soluna_eval else_exp env
            | _ -> failwith (Printf.sprintf "Solune [ERROR] L%d: if condition must evaluate to a Boolean" pos.line)
        )
    end
    | List ([Symbol ("function", _); Symbol (name, _); List (params_list, _); body_sexp], pos) -> begin
        let params_names = List.map (fun p ->
            match p with
            | Symbol (s, _) -> s
            | _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: Function parameters must be symbols" pos.line)
        ) params_list in
        let named_lambda = Lambda (params_names, body_sexp, env) in
        Hashtbl.replace env name named_lambda;
        named_lambda
    end
    | List ([Symbol ("lambda", _); List (params_list, _); body_sexp], pos) -> begin
        let params_names = List.map (fun p ->
            match p with
            | Symbol (s, _) -> s
            | _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: Lambda function parameters must be symbols" pos.line)
        ) params_list in
        Lambda (params_names, body_sexp, env)
    end
    | List ((h :: t), pos) -> soluna_eval_list_form (List ((h :: t), pos)) env
    | List ([], _) -> sexp
    | String (s, pos) -> String (s, pos)
    | Symbol (s, pos) -> (try Hashtbl.find env s with | Not_found -> failwith (Printf.sprintf "Soluna [ERROR] L%d: Unbound symbol '%s'" pos.line s))
    | _ -> failwith "Soluna [ERROR]: soluna_eval is missing something"
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
                    failwith (Printf.sprintf "Solune [ERROR] L%d: Wrong number of arguments passed to function" pos.line)
                else
                    let local_env = Hashtbl.copy lambda_env in
                    List.iter2 (fun name value -> Hashtbl.replace local_env name value) params args;
                    soluna_eval body local_env
            end
            | _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: Expected a symbol, got: %s" pos.line (soluna_string_of_sexp op))
        )
    end
    | _ -> failwith "Soluna [INTERNAL ERROR]: soluna_eval_list_form called with non-list expression"
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
    | _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: 'write' requires exactly one argument" pos.line)

let soluna_apply_comp op args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [Number (a, _); Number (b, _)] -> Boolean ((op a b), unknown_pos)
    | _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: Comparison requires exactly two numbers" pos.line)

let soluna_apply_modulo args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [Number (a, _); Number (b, _)] -> begin 
        if b = 0 then failwith (Printf.sprintf "Soluna [ERROR] L%d: Division by zero" pos.line)
        else Number ((a mod b), unknown_pos)
    end
    | _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: Modulo operation requires two numbers" pos.line)

let soluna_list_primitive args = List (args, unknown_pos)

let soluna_fst_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [List ((h :: _), _)] -> h
    | [List ([], pos)] -> failwith (Printf.sprintf "Soluna [ERROR] L%d: 'fst' called on empty list" pos.line)
    | _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: 'fst' requires exactly one list as argument" pos.line)

let soluna_rst_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [List ((_ :: t), pos)] -> List (t, pos)
    | [List ([], pos)] -> failwith (Printf.sprintf "Soluna [ERROR] L%d: 'rst' called on empty list" pos.line)
    | _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: 'rst' requires exactly one list as argument" pos.line)

let soluna_null_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [List ([], pos)] -> Boolean (true, pos)
    | [List ((_ :: _), pos)] -> Boolean (false, pos)
    | _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: 'null' requires one list argument" pos.line)

let soluna_num_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [Number _] -> Boolean (true, pos)
    | _ -> Boolean (false, pos)

let soluna_cons_primitive args =
    let pos = match args with [] -> unknown_pos | h :: _ -> soluna_get_pos h in
    match args with
    | [h; List (t, pos)] -> List ((h :: t), pos)
    | [_; _] -> failwith (Printf.sprintf "Soluna [ERROR] L%d: 'cons' requires a list as its second argument" pos.line)
    | _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: 'cons' requires an element and a list" pos.line)

let soluna_map_lambda_to_sexp lambda sexp =
    match lambda with
    | Lambda (params, body, lambda_env) -> begin
        if List.length params <> 1 then
            failwith "Soluna [ERROR]: Function passed to 'map' must accept exactly one argument"
        else
            let local_env = Hashtbl.copy lambda_env in
            Hashtbl.replace local_env (List.hd params) sexp;
            soluna_eval body local_env
    end
    | Primitive fn -> fn [sexp]
    | _ -> failwith "Soluna [ERROR]: First argument to 'map' must be a function"

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
                | _ -> failwith "Soluna [INTERNAL]: map recursion broke (tail result was not a List)"
            end
        in
        map_aux lst
    end
    | [_; _] -> failwith (Printf.sprintf "Soluna [ERROR] L%d: 'map' requires a function and a list (second argument must be a List)" pos.line)
    | _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: 'map' requires exactly two arguments" pos.line)

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
                    | _ -> failwith "Soluna [INTERNAL]: filter recursion broke (tail result was not a List)"
                end
                | Boolean (false, _) -> filter_aux t
                | _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: Functions passed to filter must return a Boolean" pos.line)
            end
        in
        filter_aux lst
    end
    | _ -> failwith (Printf.sprintf "Soluna [ERROR] L%d: 'filter' requires exactly a function and a list" pos.line)

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
    env

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
    | Sys_error msg -> failwith (Printf.sprintf "Soluna [ERROR]: Cannot open or read file %s: %s" filename msg)
    | e -> close_in_noerr (open_in filename); raise e

let () =
    try
        let sexp = match Sys.argv with
        | [|_; filename|] -> soluna_read_file filename
        | _ -> Printf.eprintf "Usage: %s <filename.luna>" Sys.argv.(0); exit 1 in

        let parsed_sexp = sexp
        |> String.to_seq
        |> List.of_seq in
        let parsed_sexp = soluna_tokenizer "" [] parsed_sexp 1 in

        let program = soluna_read_program parsed_sexp [] in
        let global_env = soluna_init_env () in

        List.iter (fun sexp -> let _ = soluna_eval sexp global_env in ()) program;
    with
    | Failure msg -> Printf.eprintf "%s\n" msg; exit 1
    | e -> Printf.eprintf "Soluna [INTERNAL]: Unexpected exception: %s\n" (Printexc.to_string e); exit 1
