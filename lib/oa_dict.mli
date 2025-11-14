module type Dict = sig
  type key = int
  type 'v t

  val empty : 'v t
  val is_empty : 'v t -> bool
  val insert : key -> 'v -> 'v t -> 'v t
  val remove : key -> 'v t -> 'v t
  val map : 'v t -> ('f -> 'r) -> 'v t
  val filter : 'v t -> ('f -> bool) -> 'v t
  val fold_left : 'v -> ('acc -> key -> 'v -> 'acc) -> 'v t
  val fold_right : 'v t -> (key -> 'v -> 'acc -> 'acc) -> 'v t
  val concat : 'v t -> 'v t -> 'v t
end

module OA_Dict : Dict
