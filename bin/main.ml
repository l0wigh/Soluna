let soluna_version = "0.4.4"
type soluna_position = { filename: string; line: int; }
type soluna_expr =
    | Number of int * soluna_position
    | Symbol of string * soluna_position
    | Boolean of bool * soluna_position
    | List of soluna_expr list * soluna_position
    | String of string * soluna_position
    | Primitive of (soluna_expr list -> soluna_expr)
    | Dict of (string, soluna_expr) Hashtbl.t * soluna_position
    | Lambda of string list * soluna_expr * (string, soluna_expr) Hashtbl.t
type env = (string, soluna_expr) Hashtbl.t
type soluna_token = { token: string; pos: soluna_position }
let unknown_pos = { filename = "unknown"; line = 0; }
let font_rst = "\x1b[0m"
let font_blue = "\x1b[34m\x1b[1m"
let error_msg = "\x1b[31m\x1b[1mSoluna ERROR\x1b[0m"
let internal_msg = "\x1b[32m\x1b[1mSoluna INTERNAL\x1b[0m"
let user_reset = "\x1b[0m"
let user_red = "\x1b[31m"
let user_green = "\x1b[32m"
let user_yellow = "\x1b[33m"
let user_blue = "\x1b[34m"

open Filename

let soluna_repl = "(function closed_expression (lst) (do(defvar opening (length (filter (lambda (x) (if (= x \"(\") true false)) lst)))(defvar closing (length (filter (lambda (x) (if (= x \")\") true false)) lst)))(if (= opening closing) true false)))(function oracle () (do(defvar prompt \"oracle λ \")(defvar kill-switch true)(defvar show-return true)(defvar unclosed (list))(while kill-switch(do(defvar user-input (input prompt))(case((= user-input \"exit\") (defvar kill-switch false))((= user-input \"hide\") (do (defvar show-return false) (writeln \"Sexp values are now hidden\")))((= user-input \"show\") (do (defvar show-return true) (writeln \"Sexp values are now written\")))(default (try (do(defvar unclosed (cons \" \" unclosed))(defvar sexp (implode (reverse (cons user-input unclosed))))(case ((closed_expression (explode sexp)) (do(defvar sexp (eval sexp))(defvar prompt \"oracle λ \")(if show-return (do(write \"- : \")(defvar sexp_type (type sexp))(case((= sexp_type \"Lambda\") (defvar sexp_type \"\"))(default (defvar sexp_type (implode (list sexp_type \" \")))))(write sexp_type :green)(writeln sexp :green)(defvar unclosed (list)))())))(default (do(defvar prompt \"... \")(defvar unclosed (cons user-input unclosed))))))(e (writeln e)))))))))(oracle)"

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

let soluna_color_of_symbol arg =
    match arg with
    | ":red" -> user_red
    | ":green" -> user_green
    | ":yellow" -> user_yellow
    | ":blue" -> user_blue
    | _ -> user_reset

let rec soluna_read_tokens token_list =
    match token_list with
    | [] -> failwith (Printf.sprintf "[%s] %s:%d%s -> Unexpected EOF" error_msg (font_blue ^ (List.hd token_list).pos.filename) (List.hd token_list).pos.line font_rst)
    | h :: t -> begin
        let pos = h.pos in
        match h.token with
        | "(" -> begin
            let (elements, rem_token) = soluna_read_list [] t in
            (List (elements, pos), rem_token)
        end
        | ")" -> failwith (Printf.sprintf "[%s] %s:%d%s -> Unexpected ')'" error_msg (font_blue ^ pos.filename) pos.line font_rst)
        | _ -> begin
            let atom = soluna_parse_atom h in
            (atom, t)
        end
    end
and soluna_read_list token_acc token_list =
    match token_list with
    | [] -> failwith (Printf.sprintf "[%s] -> Expression was not closed !" error_msg)
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
    | Sys_error msg -> failwith (Printf.sprintf "[%s] -> Cannot open or read file %s" error_msg msg)
    | e -> close_in_noerr (open_in filename); raise e

let rec soluna_get_string sexp str =
    match sexp with
    | '"' :: t -> (str, t)
    | '\\' :: t -> begin
        match t with
        | 'n' :: tt -> soluna_get_string tt (str ^ "\n")
        | 't' :: tt -> soluna_get_string tt (str ^ "\t")
        | '\\' :: tt -> soluna_get_string tt (str ^ "\\")
        | '"' :: tt -> soluna_get_string tt (str ^ "\"")
        | h :: tt -> soluna_get_string tt (str ^ (String.make 1 '\\') ^ (String.make 1 h))
        | [] -> failwith (Printf.sprintf "[%s] -> String ends unexpectedly after escape character" error_msg)
    end
    | h :: t -> soluna_get_string t (str ^ String.make 1 h)
    | _ -> failwith (Printf.sprintf "[%s] -> String needs to be have \" around them" error_msg)

let soluna_get_pos sexp =
    match sexp with
    | Number (_, pos) | Symbol (_, pos) | Boolean (_, pos) | String (_, pos) | List (_, pos) -> pos
    | Primitive _ | Lambda _ | Dict _ -> unknown_pos

let soluna_token_pos args =
    match args with
    | [] -> unknown_pos
    | h :: _ -> soluna_get_pos h

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
  | Dict (h, _) -> begin
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
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Arithmetic operation requires numbers" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    ) args in
    match numbers with
    | [] -> failwith (Printf.sprintf "[%s] %s:%d%s -> Arithmetic operation requires at least one argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | h :: t -> begin
        let result = List.fold_left art h t in
        Number (result, unknown_pos)
    end

let rec soluna_unify pattern value env =
    match pattern, value with
    | Symbol ("_", _), _ -> true
    | Symbol ("default", _), _ -> true
    | Symbol (name, _), _ -> begin
        Hashtbl.replace env name value;
        true
    end
    | Number (a, _), Number (b, _) -> a = b
    | Boolean (a, _), Boolean (b, _) -> a = b
    | String (a, _), String (b, _) -> a = b
    | List ([h; Symbol ("::", _); t], _), List (vh :: vt, pos) -> soluna_unify h vh env && soluna_unify t (List (vt, pos)) env
    | List ([h; Symbol ("::", _); t], _), String (s, pos) when String.length s > 0 -> begin
        let ch = String.make 1 s.[0] in
        let st = String.sub s 1 (String.length s - 1) in
        soluna_unify h (String (ch, pos)) env && soluna_unify t (String (st, pos)) env
    end
    | List ([], _), List ([], _) -> true
    | List ([], _), String ("", _) -> true
    | List (p_elems, _), List (v_elems, _) -> soluna_unify_list p_elems v_elems env
    | _ -> false
and soluna_unify_list p_list v_list env =
    match p_list, v_list with
    | [], [] -> true
    | p :: ps, v :: vs -> soluna_unify p v env && soluna_unify_list ps vs env
    | _ -> false

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
        | Invalid_argument _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Cannot redefine immutable variable: %s" error_msg (font_blue ^ pos.filename) pos.line name font_rst)
        end;
        bind
    end
    | List ([Symbol ("try", _); main_sexp ; List ([Symbol (catch_var, _); handler_sexp], _)], pos) -> begin
        try
            soluna_eval main_sexp env
        with
        | Failure msg -> begin
            let error_sexp = String (msg, pos) in
            let handler_env = Hashtbl.copy env in
            Hashtbl.replace handler_env catch_var error_sexp;
            soluna_eval handler_sexp handler_env
        end
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
            | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Function parameters must be symbols" error_msg (font_blue ^ pos.filename) pos.line font_rst)
        ) params_list in
        let named_lambda = Lambda (params_names, body_sexp, env) in
        Hashtbl.replace env name named_lambda;
        named_lambda
    end
    | List ([Symbol ("lambda", _); List (params_list, _); body_sexp], pos) -> begin
        let params_names = List.map (fun p ->
            match p with
            | Symbol (s, _) -> s
            | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Lambda function parameters must be symbols" error_msg (font_blue ^ pos.filename) pos.line font_rst)
        ) params_list in
        Lambda (params_names, body_sexp, env)
    end
    | List (Symbol ("bind", _) :: Symbol (keeper, _) :: sexp_list, pos) -> soluna_eval_link keeper sexp_list env pos
    | List ([Symbol ("split", _); delimiter_sexp; str_sexp; Symbol (":keep-empty", _)], pos) -> soluna_eval_split delimiter_sexp str_sexp true env pos
    | List ([Symbol ("split", _); delimiter_sexp; str_sexp], pos) -> soluna_eval_split delimiter_sexp str_sexp false env pos
    | List ([Symbol ("each", _); Symbol (var_name, _); lst_sexp; body_sexp], pos) -> soluna_eval_each var_name lst_sexp body_sexp env pos
    | List ([Symbol ("while", _); cond_sexp; body_sexp], pos) -> soluna_eval_while cond_sexp body_sexp env pos
    | List (Symbol ("match", _) :: target_sexp :: clauses_sexp, pos) -> soluna_eval_match target_sexp clauses_sexp env pos
    | List ((h :: t), pos) -> soluna_eval_list_form (List ((h :: t), pos)) env
    | List ([], _) -> sexp
    | String (s, pos) -> String (s, pos)
    | Symbol (s, pos) when String.length s > 1 && s.[0] = ':' -> String (s, pos)
    | Symbol (s, pos) -> (try Hashtbl.find env s with | Not_found -> failwith (Printf.sprintf "[%s] %s:%d%s -> Unbound symbol '%s'" error_msg (font_blue ^ pos.filename) pos.line font_rst s))
    | _ -> failwith (Printf.sprintf "[%s] -> soluna_eval is missing something" internal_msg)
and soluna_eval_match target_sexp clauses_sexp env pos =
    let target = soluna_eval target_sexp env in
    let rec check_clauses clauses =
        match clauses with
        | List (pattern :: action_part, _) :: rest -> begin
            let local_env = Hashtbl.copy env in
            if soluna_unify pattern target local_env then
                begin
                    match action_part with
                    | List ([Symbol ("when", _); guard_sexp], _) :: [body_sexp] -> 
                        begin
                            match soluna_eval guard_sexp local_env with
                            | Boolean (true, _) -> soluna_eval body_sexp local_env
                            | _ -> check_clauses rest
                        end
                    | [body_sexp] -> soluna_eval body_sexp local_env
                    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Invalid pattern in 'match'" error_msg (font_blue ^ pos.filename) pos.line font_rst)
                end
            else
                check_clauses rest 
        end
        | [] -> failwith (Printf.sprintf "[%s] %s:%d%s -> No pattern matched the value" error_msg (font_blue ^ pos.filename) pos.line font_rst)
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Invalid pattern in 'match'" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    in
    check_clauses clauses_sexp
and soluna_eval_link keeper sexp_list env pos =
    match sexp_list with
    | [] -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'link' requires at least an initial value" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | initial :: rest -> begin
        let initial_value = soluna_eval initial env in
        let local_env = Hashtbl.copy env in
        let rec link_aux steps curr_val =
            match steps with
            | [] -> curr_val
            | next :: remaining -> begin
                Hashtbl.replace local_env keeper curr_val;
                soluna_eval next local_env |> link_aux remaining
            end
        in
        link_aux rest initial_value
    end
and soluna_eval_split delimiter_sexp str_sexp keep_empty env pos =
    let delimiter = soluna_eval delimiter_sexp env in
    match delimiter with
    | String (d, _) -> begin
        let str_val = soluna_eval str_sexp env in
        match str_val with
        | String (s, _) -> begin
            let d_c = String.to_seq d |> List.of_seq in
            let d_c = match d_c with
                | h :: _ -> h
                | _ -> ' '
            in
            let str_lst = String.split_on_char d_c s in
            let rec split_aux lst acc =
                match lst with
                | [] -> List (List.rev acc, pos)
                | h :: t -> begin
                    if keep_empty then split_aux t (String (h, pos) :: acc)
                    else match String.length h with
                    | 0 -> split_aux t acc
                    | _ -> split_aux t (String (h, pos) :: acc)
                end
            in
            split_aux str_lst []
        end
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'split' requires a String as last argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'split' delimiter should evaluate as a String" error_msg (font_blue ^ pos.filename) pos.line font_rst)
and soluna_eval_while cond_sexp body_sexp env pos =
    let rec loop () =
        let cond_value = soluna_eval cond_sexp env in
        let res = match cond_value with
            | Boolean (false, _) -> false
            | _ -> true
        in
        if res then begin
            let _ = soluna_eval body_sexp env in
            loop ()
        end else Symbol ("nil", pos)
    in
    loop ()
and soluna_eval_each var_name lst_sexp body_sexp env pos =
    let seq = soluna_eval lst_sexp env in
    match seq with
    | List (elements, _) -> begin
        let rec loop_elements curr_list =
            match curr_list with
            | [] -> Symbol ("nil", pos)
            | h :: t -> begin
                Hashtbl.replace env var_name h;
                let _ = soluna_eval body_sexp env in
                loop_elements t
            end
        in
        loop_elements elements
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Sequence argument in 'each' should be a list" error_msg (font_blue ^ pos.filename) pos.line font_rst)
and soluna_include_file filename pos env =
    let current_dir = dirname pos.filename in
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
    with Sys_error msg -> failwith (Printf.sprintf "[%s] %s:%d%s -> Cannot include file '%s': %s" error_msg (font_blue ^ pos.filename) pos.line path msg font_rst)
and soluna_eval_case clauses pos env =
    match clauses with
    | [] -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'case' should have atlease one clause" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | [List ([Symbol ("default", _); res_expr], _)] -> soluna_eval res_expr env
    | List ([Symbol ("default", _); _], _) :: _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'default' case should be the last" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | List ([test_expr; res_expr], _) :: rst_clauses -> begin
        let eval = soluna_eval test_expr env in
        match eval with
        | Boolean (true, _) -> soluna_eval res_expr env
        | Boolean (false, _) -> soluna_eval_case rst_clauses pos env
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Conditionnal expressions should return a Boolean" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    end
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Invalid 'case' use. Need at least a default case" error_msg (font_blue ^ pos.filename) pos.line font_rst)
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
            | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Expected a symbol. You might need to use 'do'" error_msg (font_blue ^ pos.filename) pos.line font_rst)
        )
    end
    | _ -> failwith (Printf.sprintf "[%s] -> soluna_eval_list_form called with non-list expression" internal_msg)
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

let soluna_write_primitive env mode args =
    let pos = soluna_token_pos args in
    match args with
    | [sexp; color_sexp] -> begin
        let color = match soluna_eval color_sexp env with
            | String (c, _) -> soluna_color_of_symbol c
            | _ -> user_reset
        in
        let output = match sexp with
            | String (s, _) -> soluna_write_string s
            | _ -> soluna_string_of_sexp sexp in
        if mode then Printf.printf "%s%s%s\n" color output user_reset else Printf.printf "%s%s%s" color output user_reset;
        sexp
    end
    | [sexp] -> begin
        let output = match sexp with
            | String (s, _) -> soluna_write_string s
            | _ -> soluna_string_of_sexp sexp in
        if mode then Printf.printf "%s\n" output else Printf.printf "%s" output;
        sexp
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'write' requires exactly one argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_apply_comp op args =
    let pos = soluna_token_pos args in
    match args with
    | [Number (a, _); Number (b, _)] -> Boolean ((op a b), pos)
    | [String (a, _); String (b, _)] -> begin 
        match (String.compare a b) with
        | 0 -> Boolean (true, pos)
        | _ -> Boolean (false, pos)
    end
    | [Boolean (a, _); Boolean (b, _)] -> begin
        match a, b with
        | true, true -> Boolean (true, pos)
        | false, false -> Boolean (true, pos)
        | _ -> Boolean (false, pos)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Comparison requires two arguments of the same type (boolean, string or int)" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_apply_modulo args =
    let pos = soluna_token_pos args in
    match args with
    | [Number (a, _); Number (b, _)] -> begin 
        if b = 0 then failwith (Printf.sprintf "[%s] %s:%d%s -> Division by zero" error_msg (font_blue ^ pos.filename) pos.line font_rst)
        else Number ((a mod b), unknown_pos)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Modulo operation requires two numbers" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_list_primitive args = List (args, unknown_pos)

let soluna_fst_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [List ((h :: _), _)] -> h
    | [List ([], pos)] -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'fst' called on empty list" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'fst' requires exactly one list as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_rst_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [List ((_ :: t), pos)] -> List (t, pos)
    | [List ([], pos)] -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'rst' called on empty list" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'rst' requires exactly one list as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_null_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [List ([], pos)] -> Boolean (true, pos)
    | [List ((_ :: _), pos)] -> Boolean (false, pos)
    | [Dict (h, pos)] -> if Hashtbl.length h = 0 then Boolean (true, pos) else Boolean (false, pos)
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'null' requires a list or a dictionnary argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_num_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [Number _] -> Boolean (true, pos)
    | _ -> Boolean (false, pos)

let soluna_cons_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [h; List (t, pos)] -> List ((h :: t), pos)
    | [_; _] -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'cons' requires a list as its second argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'cons' requires an element and a list" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_map_lambda_to_sexp lambda sexp =
    match lambda with
    | Lambda (params, body, lambda_env) -> begin
        if List.length params <> 1 then
            failwith (Printf.sprintf "[%s] -> Function passed to 'map' must accept exactly one argument" error_msg)
        else
            let local_env = Hashtbl.copy lambda_env in
            Hashtbl.replace local_env (List.hd params) sexp;
            soluna_eval body local_env
    end
    | Primitive fn -> fn [sexp]
    | _ -> failwith (Printf.sprintf "[%s] -> First argument to 'map' must be a function" error_msg)

let soluna_map_primitive args =
    let pos = soluna_token_pos args in
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
                | _ -> failwith (Printf.sprintf "[%s] -> map recursion broke (tail result was not a List)" internal_msg)
            end
        in
        map_aux lst
    end
    | [_; _] -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'map' requires a function and a list (second argument must be a List)" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'map' requires exactly two arguments" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_filter_primitive args =
    let pos = soluna_token_pos args in
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
                    | _ -> failwith (Printf.sprintf "[%s] -> filter recursion broke (tail result was not a List)" internal_msg)
                end
                | Boolean (false, _) -> filter_aux t
                | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Functions passed to filter must return a Boolean" error_msg (font_blue ^ pos.filename) pos.line font_rst)
            end
        in
        filter_aux lst
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'filter' requires exactly a function and a list" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_reverse_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [List (h, p)] -> List (List.rev h, p)
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'reverse' takes a list as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_dict_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [Number (s, pos)] -> begin
        let new_map = Hashtbl.create s in
        Dict (new_map, pos)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'dict' requires a size argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_dict_set_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [Dict (h, _); String (key, _); value_sexp] -> begin
        Hashtbl.replace h key value_sexp;
        value_sexp
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'dict-set' requires a key and a value" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_dict_get_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [Dict (h, pos); String (key, _)] -> begin
        try
            Hashtbl.find h key
        with Not_found ->
            failwith (Printf.sprintf "[%s] %s:%d%s -> Key '%s' not found in dictionnary" error_msg (font_blue ^ pos.filename) pos.line key font_rst)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'dict-get' requires a dictionnary and a key" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_dict_ref_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [Dict (h, _); String (key, _); default] -> begin
        try
            Hashtbl.find h key
        with Not_found -> default
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'dict-ref' requires a dictionnary, a key and a default value" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_explode_primitive env args =
    let pos = soluna_token_pos args in
    match args with
    | [String (s, p)] -> begin
        let res = String.to_seq s
        |> List.of_seq
        |> List.map (fun c -> String (String.make 1 c, p)) in
        List (res, pos)
    end
    | h :: _ -> begin
        let args_sexp = soluna_eval h env in
        match args_sexp with
        | String (s, p) -> begin
            let res = String.to_seq s
            |> List.of_seq
            |> List.map (fun c -> String (String.make 1 c, p)) in
            List (res, pos)
        end
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'explode' requires a string as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'explode' requires a string as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_dict_keys_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [Dict (h, p)] -> List (List.rev (Hashtbl.fold (fun k _ acc -> String (k, p) :: acc) h []), p)
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'dict-keys' requires a valid dictionnary" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_dict_values_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [Dict (h, p)] -> List (List.rev (Hashtbl.fold (fun _ v acc -> v :: acc) h []), p)
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'dict-values' requires a valid dictionnary" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_dict_remove_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [Dict (h, p); String (key, _)] -> begin
        Hashtbl.remove h key;
        Symbol ("nil", p)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'dict-remove' requires a dictionnary and a key as arguments" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_dict_contains_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [Dict (h, p); String (key, _)] -> begin
        let found = Hashtbl.mem h key in
        Boolean (found, p)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'dict-contains' requires a dictionnary and a key as arguments" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_length_primitive env args =
    let pos = soluna_token_pos args in
    match args with
    | [List (h, p)] -> Number (List.length h, p)
    | [String (h, p)] -> Number (String.length h, p)
    | [Dict (h, p)] -> Number (Hashtbl.length h, p)
    | h :: _ -> begin
        let args_sexp = soluna_eval h env in
        match args_sexp with
        | List (h, p) -> Number (List.length h, p)
        | String (h, p) -> Number (String.length h, p)
        | Dict (h, p) -> Number (Hashtbl.length h, p)
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'length' requires a list, a string or a dict argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'length' requires a list, a string or a dict argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let rec soluna_implode_primitive env args =
    let pos = soluna_token_pos args in
    match args with
    | [List (h, p)] -> String (String.concat "" (sexp_list_to_string_list h p), p)
    | h :: _ -> begin
        let args_sexp = soluna_eval h env in
        match args_sexp with
        | List (h, p) -> String (String.concat "" (sexp_list_to_string_list h p), p)
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'implode' requires a string list as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'implode' requires a string list as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)
and sexp_list_to_string_list sexp_list pos =
    List.map (fun sexp ->
        match sexp with
        | String (s, _) -> s
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'implode' requires a string list as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    ) sexp_list

let rec soluna_read_file_primitive env args =
    let pos = soluna_token_pos args in
    match args with
    | [path_sexp] -> begin
        let path = soluna_eval path_sexp env in
        match path with
        | String (p, _) -> begin
            try
                let content = read_file_content p in
                String (content, pos)
            with
            | Sys_error msg -> failwith (Printf.sprintf "[%s] %s:%d%s -> Can't read file '%s': %s" error_msg (font_blue ^ pos.filename) pos.line font_rst p msg)
            | End_of_file -> String ("", pos)
        end
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'read-file' requires a filepath" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'read-file' requires a filepath" error_msg (font_blue ^ pos.filename) pos.line font_rst)
and read_file_content path =
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    content

let soluna_write_file_primitive env args =
    let pos = soluna_token_pos args in
    match args with
    | [path_sexp; content_sexp; mode_sexp] -> begin
        let path = soluna_eval path_sexp env in
        let content = soluna_eval content_sexp env in
        let mode = soluna_eval mode_sexp env in
        match path, content, mode with
        | String (path, _), String (content, _), String (mode, _) -> begin
            let m = match mode with
            | ":append" -> [Open_wronly; Open_creat; Open_append]
            | ":overwrite" -> [Open_wronly; Open_creat; Open_trunc]
            | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Writing mode '%s' invalid" error_msg (font_blue ^ pos.filename) pos.line font_rst mode)
            in
            let oc =
                try
                    open_out_gen m 0o644 path
                with
                | Sys_error msg -> failwith (Printf.sprintf "[%s] %s:%d%s -> Can't open file '%s': %s" error_msg (font_blue ^ pos.filename) pos.line font_rst path msg)
            in
            try
                output_string oc content;
                close_out oc;
                Symbol ("nil", pos)
            with
            | e -> close_out_noerr oc; raise e
        end
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'write-file' requires three arguments: filename, string, mode (:overwrite or :append)" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'write-file' requires three arguments: filename, string, mode (:overwrite or :append)" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_input_primitive env args =
    let pos = soluna_token_pos args in
    match args with
    | [prompt_sexp] -> begin
        let prompt = soluna_eval prompt_sexp env in
        match prompt with
        | String (s, _) -> begin
            print_string s;
            flush stdout;
            try
                let user_input = read_line () in
                String (user_input, pos)
            with
            | End_of_file -> String ("", pos)
        end
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'input' requires a prompt of type String" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'input' requires a prompt of type String" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_eval_primitive env args =
    let pos = soluna_token_pos args in
    match args with
    | [eval_sexp] -> begin
        let sexp_to_eval = soluna_eval eval_sexp env in
        match sexp_to_eval with
        | String (s, _) -> begin
            let parsed_sexp = String.to_seq s |> List.of_seq in
            let parsed_sexp = soluna_tokenizer "" [] parsed_sexp 1 "runtime" in
            let program = soluna_read_program parsed_sexp [] in
            let rec eval_program program =
                match program with
                | [] -> Symbol ("nil", pos)
                | [last_sexp] -> soluna_eval last_sexp env
                | h :: t -> begin
                    let _ = soluna_eval h env in
                    eval_program t
                end
            in
            eval_program program
        end
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'eval' requires a string as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'eval' requires a string as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_type_primitive args =
    let pos = soluna_token_pos args in
    match args with
    | [sexp] -> begin
        match sexp with
        | Number (_, _) -> String ("Number", pos)
        | String (_, _) -> String ("String", pos)
        | Lambda (_, _, _) -> String ("Lambda", pos)
        | List (_, _) -> String ("List", pos)
        | Boolean (_, _) -> String ("Boolean", pos)
        | Primitive _ -> String ("Primitive", pos)
        | Symbol (_, _) -> String ("Symbol", pos)
        | Dict (_, _) -> String ("Dict", pos)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> Type not found" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_int_primitive env args =
    let pos = soluna_token_pos args in
    match args with
    | [String (h, p)] -> begin
        try
            Number (int_of_string h, p)
        with
        | Failure _ -> Number (0, p)
    end
    | h :: _ -> begin
        let sexp = soluna_eval h env in
        match sexp with
        | String (h, p) -> begin
            try
                Number (int_of_string h, p)
            with
            | Failure _ -> Number (0, p)
        end
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'int' primitive requires a String as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'int' primitive requires a String as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

let soluna_str_primitive env args =
    let pos = soluna_token_pos args in
    match args with
    | [Number (h, p)] -> begin
        try
            String (string_of_int h, p)
        with
        | Failure _ -> String ("", p)
    end
    | h :: _ -> begin
        let sexp = soluna_eval h env in
        match sexp with
        | Number (h, p) -> begin
            try
                String (string_of_int h, p)
            with
            | Failure _ -> String ("", p)
        end
        | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'str' primitive requires a Number as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)
    end
    | _ -> failwith (Printf.sprintf "[%s] %s:%d%s -> 'str' primitive requires a Number as argument" error_msg (font_blue ^ pos.filename) pos.line font_rst)

(* The concept of Internal Pure functions come from the fact
    that I suck at coding in OCaml and I want an easier way to extend the language *)
let soluna_pure_reduce_primitive env =
    let code = "(function reduce (f init lst) (do (defvar acc init) (each x lst (defvar acc (f acc x))) acc))"
        |> String.to_seq
        |> List.of_seq
    in
    let parsed_code = soluna_tokenizer "" [] code 1 "Pure-Internal" in
    let program = soluna_read_program parsed_code [] in
    List.iter (fun sexp -> let _ = soluna_eval sexp env in ()) program;
    ()

let soluna_init_env () : env =
    let env = Hashtbl.create 20 in 
    Hashtbl.replace env "+" (Primitive (soluna_apply_arithmetic (+))); 
    Hashtbl.replace env "-" (Primitive (soluna_apply_arithmetic (-))); 
    Hashtbl.replace env "*" (Primitive (soluna_apply_arithmetic ( * )));
    Hashtbl.replace env "/" (Primitive (soluna_apply_arithmetic (/)));
    Hashtbl.replace env "<" (Primitive (soluna_apply_comp (<)));
    Hashtbl.replace env ">" (Primitive (soluna_apply_comp (>)));
    Hashtbl.replace env "=" (Primitive (soluna_apply_comp (=)));
    Hashtbl.replace env "!=" (Primitive (soluna_apply_comp (!=)));
    Hashtbl.replace env ">=" (Primitive (soluna_apply_comp (>=)));
    Hashtbl.replace env "<=" (Primitive (soluna_apply_comp (<=)));
    Hashtbl.replace env "mod" (Primitive soluna_apply_modulo); 
    Hashtbl.replace env "write" (Primitive (soluna_write_primitive env false));
    Hashtbl.replace env "writeln" (Primitive (soluna_write_primitive env true));
    Hashtbl.replace env "list" (Primitive soluna_list_primitive);
    Hashtbl.replace env "fst" (Primitive soluna_fst_primitive);
    Hashtbl.replace env "rst" (Primitive soluna_rst_primitive);
    Hashtbl.replace env "null" (Primitive soluna_null_primitive);
    Hashtbl.replace env "cons" (Primitive soluna_cons_primitive);
    Hashtbl.replace env "map" (Primitive soluna_map_primitive);
    Hashtbl.replace env "num" (Primitive soluna_num_primitive);
    Hashtbl.replace env "filter" (Primitive soluna_filter_primitive);
    Hashtbl.replace env "reverse" (Primitive soluna_reverse_primitive);
    Hashtbl.replace env "explode" (Primitive (soluna_explode_primitive env));
    Hashtbl.replace env "implode" (Primitive (soluna_implode_primitive env));
    Hashtbl.replace env "length" (Primitive (soluna_length_primitive env));
    Hashtbl.replace env "input" (Primitive (soluna_input_primitive env));
    Hashtbl.replace env "eval" (Primitive (soluna_eval_primitive env));
    Hashtbl.replace env "type" (Primitive soluna_type_primitive);
    Hashtbl.replace env "int" (Primitive (soluna_int_primitive env));
    Hashtbl.replace env "str" (Primitive (soluna_str_primitive env));
    Hashtbl.replace env "read-file" (Primitive (soluna_read_file_primitive env));
    Hashtbl.replace env "write-file" (Primitive (soluna_write_file_primitive env));
    Hashtbl.replace env "dict" (Primitive soluna_dict_primitive);
    Hashtbl.replace env "dict-set" (Primitive soluna_dict_set_primitive);
    Hashtbl.replace env "dict-get" (Primitive soluna_dict_get_primitive);
    Hashtbl.replace env "dict-ref" (Primitive soluna_dict_ref_primitive);
    Hashtbl.replace env "dict-keys" (Primitive soluna_dict_keys_primitive);
    Hashtbl.replace env "dict-values" (Primitive soluna_dict_values_primitive);
    Hashtbl.replace env "dict-remove" (Primitive soluna_dict_remove_primitive);
    Hashtbl.replace env "dict-contains" (Primitive soluna_dict_contains_primitive);
    env

let () =
    try
        let filename = match Sys.argv with
        | [|_; "-h"|] -> Printf.eprintf "Usage: %s <filename.luna>" Sys.argv.(0); exit 1
        | [|_; "-v"|] -> Printf.printf "Soluna %s\n" (font_blue ^ soluna_version ^ font_rst); exit 0
        | [|_; filename|] -> filename
        | _ -> "" in

        let parsed_sexp = if String.length filename = 0 then
            soluna_repl
            |> String.to_seq
            |> List.of_seq
        else
            soluna_read_file filename
            |> String.to_seq
            |> List.of_seq in
        let parsed_sexp = soluna_tokenizer "" [] parsed_sexp 1 filename in

        let program = soluna_read_program parsed_sexp [] in
        let global_env = soluna_init_env () in

        soluna_pure_reduce_primitive global_env;

        List.iter (fun sexp -> let _ = soluna_eval sexp global_env in ()) program;
    with
    | Failure msg -> flush stdout; Printf.eprintf "\n%s\n" msg; exit 1
    | e -> flush stdout; Printf.eprintf "\n\n[%s] -> Unexpected exception: %s\n" internal_msg (Printexc.to_string e); exit 1
