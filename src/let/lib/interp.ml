open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds

(** notes to survive the assignment
>>= operator, binding operator
evaluate expression, store result in this variable, and continue computation
*)
    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"
  
    (* creates an empty tree *)
  | IsEmpty (e) -> 
    eval_expr e >>=
    tree_of_treeVal >>= (* checks if the value we got is a tree *)
    fun t -> (* if it is a tree, then we start pattern matching*)
      (match t with
      | Empty -> return (BoolVal true)
      | Node(_, _, _) -> return (BoolVal false)
      )

    (* returns a boolean indicating whether the e is an empty binary tree (should return an error if e does not evaluate to binary tree) *)
  | EmptyTree (_t) -> return (TreeVal(Empty))

    (* creates a new tree with data e1, and subtrees e2 and e3 *)
  | Node(e1, e2, e3) -> 
    eval_expr e1 >>= fun n -> (* e1 -> node, our node data *)
    eval_expr e2 >>= tree_of_treeVal >>= fun l -> (* e2 -> left subtree*)
    eval_expr e3 >>= tree_of_treeVal >>= fun r -> (* e3 -> right subtree*)
    return (TreeVal (Node(n, l, r)))
  
    (* match, but with trees, from my understanding
    e1, which is supposed to be a tree
    if its empty, return e2,
    if its a node(id1, id2, id3), return e3*)
  | CaseT (e1, e2, id1, id2, id3, e3) -> 
    eval_expr e1 >>= tree_of_treeVal >>= fun t -> (* checking if e1 is a tree *)
      (match t with
      | Empty -> eval_expr e2
      | Node(n, l, r) ->
        (* basically, extending the environment and then evaluating e3 *)
        extend_env id1 n >>+
        extend_env id2 (TreeVal l) >>+
        extend_env id3 (TreeVal r) >>+
        eval_expr e3 
      )
    
    (* creates a record with n fields. Field i is assigned the
    expressed value resulting from evaluating expression ei. Reports an error if there are
    duplicate fields. *)
  | Record(fs) -> failwith "Implement me!"

    (* e.id projects field id from the record resulting from evaluating e. Reports an error if
       e does not evaluate to a record or does not have a field named id *)
  | Proj(e, id) -> failwith "Implement me!"
  
  and
  eval_exprs : expr list -> ( exp_val list ) ea_result =
  fun es ->
  match es with
  | [] -> return []
  | h :: t -> eval_expr h >>= fun i ->
  eval_exprs t >>= fun l ->
  return ( i :: l )

  | _ -> failwith "Not implemented yet!"

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  


