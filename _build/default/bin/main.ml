type soluna_expr =
    | Number of int
    | Symbol of string
    | Boolean of bool
    | List of soluna_expr list
    | Primitive of (soluna_expr list -> soluna_expr)
    | String of string
    | Lambda of string list * soluna_expr * (string, soluna_expr) Hashtbl.t

type env = (string, soluna_expr) Hashtbl.t

let rec soluna_get_string sexp str =
    match sexp with
    | '"' :: t -> (str, t)
    | h :: t -> soluna_get_string t (str ^ String.make 1 h)
    | _ -> failwith "Soluna [ERROR]: String needs to be have \" around them"

let rec soluna_skip_comment sexp =
    match sexp with
    | '\n' :: t -> t
    | _ :: t -> soluna_skip_comment t
    | [] -> sexp

let rec soluna_tokenizer curr_token token_list sexp =
    match sexp with
    | [] -> List.rev (if curr_token != "" then (curr_token :: token_list) else token_list)
    | h :: t -> begin
        match h with
        | ' ' | '\n' | '\r' | '\t' -> if curr_token != "" then soluna_tokenizer "" (curr_token :: token_list) t else soluna_tokenizer "" token_list t
        | '(' | ')' -> if curr_token != "" then soluna_tokenizer "" (String.make 1 h :: curr_token :: token_list) t else soluna_tokenizer "" (String.make 1 h :: token_list) t
        | ';' -> soluna_skip_comment t |> soluna_tokenizer "" token_list
        | '"' -> begin
            let (str, next_sexp) = soluna_get_string t "\"" in
            soluna_tokenizer "" (str :: token_list) next_sexp
        end
        | _ -> soluna_tokenizer (curr_token ^ String.make 1 h) token_list t
    end

let soluna_parse_atom token =
    match token with
    | "true" -> Boolean true
    | "false" -> Boolean false
    | _ -> begin
        if token.[0] = '\"' then
            let string_split = String.split_on_char '"' token in
            String (List.nth string_split 1)
        else
            try
                Number (int_of_string token)
            with
            | Failure _ -> Symbol token
    end

let rec soluna_read_tokens token_list =
    match token_list with
    | [] -> failwith "Soluna [ERROR]: Unexpected EOF"
    | "(" :: t -> begin
        let (elements, rem_token) = soluna_read_list [] t in
        (List elements, rem_token)
    end
    | ")" :: _ -> failwith "Soluna [ERROR]: Unexpected ')'"
    | h :: _ when h == " " -> failwith "Should not happend !"
    | h :: t -> begin
        let atom = soluna_parse_atom h in
        (atom, t)
    end

and soluna_read_list token_acc token_list =
    match token_list with
    | [] -> failwith "Soluna: List was not closed !"
    | ")" :: t -> (List.rev token_acc, t)
    | h :: t -> begin
        let (sexp, rem_tokens) = soluna_read_tokens (h :: t) in
        soluna_read_list (sexp :: token_acc) rem_tokens
    end

let rec soluna_string_of_sexp sexp =
  match sexp with
  | Number n -> Printf.sprintf "%d" n
  | Symbol s -> s
  | Boolean b -> if b then "true" else "false"
  | List expr_list -> begin
      let inner_content = 
          List.map soluna_string_of_sexp expr_list
          |> String.concat " "
      in
      Printf.sprintf "(%s)" inner_content
  end
  | Primitive _ -> Printf.sprintf "Primitive"
  | String h -> Printf.sprintf "%s" h
  | Lambda _ -> Printf.sprintf "Lambda"

let soluna_apply_arithmetic art args =
    let numbers = List.map (fun sexp ->
        match sexp with
        | Number n -> n
        | _ -> failwith "Soluna [ERROR]: Arithmetic operation requires numbers"
    ) args in
    match numbers with
    | [] -> failwith "Soluna [ERROR]: Arithmetic operation requires at least one argument"
    | h :: t -> begin
        let result = List.fold_left art h t in
        Number result
    end


let rec soluna_eval sexp (env: env) =
    match sexp with
    | Number _  | Boolean _ -> sexp 
    | List [Symbol "defvar"; Symbol name; raw_val] -> begin
        let bind = soluna_eval raw_val env in
        begin try
            Hashtbl.add env name bind;
        with
        | Invalid_argument _ -> failwith ("Soluna [ERROR]: Cannot redefine immutable variable: " ^ name)
        end;
        bind
    end
    | List [Symbol "if"; cond_exp; then_exp; else_exp] -> begin
        let eval_cond = soluna_eval cond_exp env in
        (
            match eval_cond with
            | Boolean true -> soluna_eval then_exp env
            | Boolean false -> soluna_eval else_exp env
            | _ -> failwith "Solune [ERROR]: if condition must evaluate to a Boolean"
        )
    end
    | List [Symbol "function"; Symbol name; List params_list; body_sexp] -> begin
        let params_names = List.map (fun p ->
            match p with
            | Symbol s -> s
            | _ -> failwith "Soluna [ERROR]: Function parameters must be symbols"
        ) params_list in
        let named_lambda = Lambda (params_names, body_sexp, env) in
        Hashtbl.replace env name named_lambda;
        named_lambda
    end
    | List [Symbol "lambda"; List params_list; body_sexp] -> begin
        let params_names = List.map (fun p ->
            match p with
            | Symbol s -> s
            | _ -> failwith "Soluna [ERROR]: Lambda function parameters must be symbols"
        ) params_list in
        Lambda (params_names, body_sexp, env)
    end
    | List (h :: t) -> soluna_eval_list_form (List (h :: t)) env
    | List [] -> sexp
    | String s -> String s
    | Symbol s -> (try Hashtbl.find env s with | Not_found -> failwith (Printf.sprintf "Soluna [ERROR]: Unbound symbol: %s" s))
    | _ -> failwith "Soluna [ERROR]: soluna_eval is missing something"

and soluna_eval_list_form sexp env =
    match sexp with
    | List (Symbol "do" :: expr_list) -> soluna_eval_do expr_list env
    | List (h :: t) -> begin
        let op = soluna_eval h env in
        let args = List.map (fun arg -> soluna_eval arg env) t in
        (
            match op with
            | Primitive fn -> fn args
            | Lambda (params, body, lambda_env) -> begin
                if List.length params <> List.length args then
                    failwith "Solune [ERROR]: Wrong number of arguments passed to function"
                else
                    let local_env = Hashtbl.copy lambda_env in
                    List.iter2 (fun name value -> Hashtbl.replace local_env name value) params args;
                    soluna_eval body local_env
            end
            | _ -> failwith (Printf.sprintf "Soluna [ERROR]: Expected a symbol, got: %s" (soluna_string_of_sexp op))
        )
    end
    | _ -> failwith "Soluna [INTERNAL ERROR]: soluna_eval_list_form called with non-list expression"
and soluna_eval_do expr_list env =
    match expr_list with
    | [] -> Symbol "nil"
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
    match args with
    | [sexp] -> begin
        let output = match sexp with
        | String s -> soluna_write_string s
        | _ -> soluna_string_of_sexp sexp in
        if mode then Printf.printf "%s\n" output else Printf.printf "%s" output;
        sexp
    end
    | _ -> failwith "Soluna [ERROR]: 'write' requires exactly one argument"

let soluna_apply_comp op args =
    match args with
    | [Number a; Number b] -> Boolean (op a b)
    | _ -> failwith "Soluna [ERROR]: Comparison requires exactly two numbers."

let soluna_apply_modulo args =
    match args with
    | [Number a; Number b] -> begin 
        if b = 0 then failwith "Soluna [ERROR]: Division by zero"
        else Number (a mod b)
    end
    | _ -> failwith "Soluna [ERROR]: Modulo operation requires two numbers"

let soluna_list_primitive args = List args

let soluna_fst_primitive args =
    match args with
    | [List (h :: _)] -> h
    | [List []] -> failwith "Soluna [ERROR]: 'fst' called on empty list"
    | _ -> failwith "Soluna [ERROR]: 'fst' requires exactly one list as argument"

let soluna_rst_primitive args =
    match args with
    | [List (_ :: t)] -> List t
    | [List []] -> failwith "Soluna [ERROR]: 'rst' called on empty list"
    | _ -> failwith "Soluna [ERROR]: 'rst' requires exactly one list as argument"

let soluna_null_primitive args =
    match args with
    | [List []] -> Boolean true
    | [List (_ :: _)] -> Boolean false
    | _ -> failwith "Soluna [ERROR]: 'null' requires one list argument"

let soluna_cons_primitive args =
    match args with
    | [h; List t] -> List (h :: t)
    | [_; _] -> failwith "Soluna [ERROR]: 'cons' requires a list as its second argument"
    | _ -> failwith "Soluna [ERROR]: 'cons' requires an element and a list"

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
    match args with
    | [f; List lst] -> begin
        let rec map_aux curr_lst =
            match curr_lst with
            | [] -> List []
            | h :: t -> begin
                let res_h = soluna_map_lambda_to_sexp f h in
                let res_t_sexp = map_aux t in
                match res_t_sexp with
                | List l -> List (res_h :: l)
                | _ -> failwith "Soluna [INTERNAL]: map recursion broke (tail result was not a List)" 
            end
        in
        map_aux lst
    end
    | [_; _] -> failwith "Soluna [ERROR]: 'map' requires a function and a list (second argument must be a List)."
    | _ -> failwith "Soluna [ERROR]: 'map' requires exactly two arguments."

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
    let sexp = match Sys.argv with
    | [|_; filename|] -> soluna_read_file filename
    | _ -> Printf.eprintf "Usage: %s <filename.luna>" Sys.argv.(0); exit 1 in

    let parsed_sexp = sexp
    |> String.to_seq
    |> List.of_seq
    |> soluna_tokenizer "" [] in
    let program = soluna_read_program parsed_sexp [] in
    let global_env = soluna_init_env () in

    List.iter (fun sexp -> let _ = soluna_eval sexp global_env in ()) program;
