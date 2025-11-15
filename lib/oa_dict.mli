module type Dict = sig
  type key = int
  type 'v t

  val make_empty : unit -> 'v t
  val is_empty : 'v t -> bool
  val insert : key -> 'v -> 'v t -> 'v t
  val remove : key -> 'v t -> 'v t
  val find : key -> 'v t -> 'v option
  val mem : key -> 'v t -> bool
  val map : 'v t -> ('v -> 'r) -> 'r t
  val filter : 'v t -> ('v -> bool) -> 'v t
  val fold_left : 'acc -> ('acc -> key -> 'v -> 'acc) -> 'v t -> 'acc
  val fold_right : 'v t -> (key -> 'v -> 'acc -> 'acc) -> 'acc -> 'acc

  (* Monoid operations *)
  val concat : 'v t -> 'v t -> 'v t

  (* Comparison *)
  val equal : ('v -> 'v -> bool) -> 'v t -> 'v t -> bool
  val size : 'v t -> int
  val to_list : 'v t -> (key * 'v) list
  val of_list : (key * 'v) list -> 'v t
end

module OA_Dict : Dict
