open! Core

type t

external col : string -> t = "rust_expr_col"
external int : int -> t = "rust_expr_int"
external float : float -> t = "rust_expr_float"
external sort : t -> descending:bool -> t = "rust_expr_sort"
external head : t -> length:int option -> t = "rust_expr_head"
external filter : t -> predicate:t -> t = "rust_expr_filter"
external sum : t -> t = "rust_expr_sum"
external alias : t -> name:string -> t = "rust_expr_alias"
external equal : t -> t -> t = "rust_expr_eq"
val ( = ) : t -> t -> t
external ( <> ) : t -> t -> t = "rust_expr_neq"
external ( > ) : t -> t -> t = "rust_expr_gt"
external ( >= ) : t -> t -> t = "rust_expr_gt_eq"
external ( < ) : t -> t -> t = "rust_expr_lt"
external ( <= ) : t -> t -> t = "rust_expr_lt_eq"
external not : t -> t = "rust_expr_not"
val ( ! ) : t -> t
external and_ : t -> t -> t = "rust_expr_and"
external or_ : t -> t -> t = "rust_expr_or"
external xor : t -> t -> t = "rust_expr_xor"
val ( && ) : t -> t -> t
val ( || ) : t -> t -> t
val ( lxor ) : t -> t -> t
external add : t -> t -> t = "rust_expr_add"
external sub : t -> t -> t = "rust_expr_sub"
external mul : t -> t -> t = "rust_expr_mul"
external div : t -> t -> t = "rust_expr_div"
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t