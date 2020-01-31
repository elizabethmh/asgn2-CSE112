(* $Id: interp.ml,v 1.8 2020-01-24 11:42:24-08 - - $ *)

open Absyn

exception Unimplemented of string
let no_expr reason = raise (Unimplemented reason)
let no_stmt reason continuation = raise (Unimplemented reason)

let want_dump = ref false

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> (match memref with 
      | Arrayref (ident, expr) -> let indexVal = (int_of_float (eval_expr expr))
       and arrayVal = Hashtbl.find Tables.array_table ident 
       in Array.get arrayVal indexVal
      | Variable (ident) -> Hashtbl.find Tables.variable_table ident)
    | Unary (oper, expr) -> let value = eval_expr expr
      in Hashtbl.find Tables.unary_fn_table oper value
    | Binary (oper, expr1, expr2) -> let op = Hashtbl.find Tables.binary_fn_table oper
      and left = eval_expr expr1
      and right = eval_expr expr2
      in (op left right)

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continuation -> match firstline with
      | _, _, None -> interpret continuation
      | _, _, Some stmt -> (interp_stmt stmt continuation)

and interp_stmt (stmt : Absyn.stmt) (continuation : Absyn.program) =
    match stmt with
    | Dim (ident, expr) -> interp_dim ident expr continuation
    | Let (memref, expr) -> interp_let memref expr continuation
    | Goto label -> interp_goto label continuation
    | If (expr, label) -> no_stmt "If (expr, label)" continuation
    | Print print_list -> interp_print print_list continuation
    | Input memref_list -> interp_input memref_list continuation

and interp_print (print_list : Absyn.printable list)
                 (continuation : Absyn.program) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ());
    interpret continuation                      

and interp_let (memref : Absyn.memref)
               (expr :  Absyn.expr)
               (continuation : Absyn.program) =
     match memref with
     | Arrayref (ident, indexExpr) -> 
       let indexVal = (int_of_float (eval_expr indexExpr))
       and arrayVal = Hashtbl.find Tables.array_table ident 
       and evalExpress = eval_expr expr
       in Array.set arrayVal indexVal evalExpress;
       interpret continuation
     | Variable var -> 
       let evalExpr = eval_expr expr
       in Hashtbl.add Tables.variable_table var evalExpr;
       interpret continuation

and interp_dim (ident : Absyn.ident)
               (expr :  Absyn.expr)
               (continuation : Absyn.program) =
     Hashtbl.replace Tables.array_table ident (Array.make (int_of_float(eval_expr expr)) 0.0);
     interpret continuation

and interp_goto (label : Absyn.label)
     (continuation : Absyn.program) =
     interpret (Hashtbl.find Tables.label_table label)


and interp_input (memref_list : Absyn.memref list)
                 (continuation : Absyn.program)  =
    let input_number memref =
        try  let number = Etc.read_number ()
             in (print_float number; print_newline ())
        with End_of_file -> 
             (print_string "End_of_file"; print_newline ())
    in List.iter input_number memref_list;
    interpret continuation

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)
