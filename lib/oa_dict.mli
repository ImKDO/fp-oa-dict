module type Dict = sig
  type ('k, 'v) t

  val empty : ('k, 'v) t
  val is_empty : ('k, 'v) t -> bool -> ('k, 'v) t
  val insert : ('k, 'v) t -> ('k -> 'v) -> ('k, 'v) t
  val remove : ('k, 'v) t -> 'k -> ('k, 'v) t
  val map : ('k, 'v) t -> ('f -> 'r) -> ('k, 'v) t
  val filter : ('k, 'v) t -> ('f -> bool) -> ('k, 'v) t
  val fold_left : ('k, 'v) t -> ('acc -> 'k -> 'v -> 'acc) -> ('k, 'v) t
  val fold_right : ('k, 'v) t -> ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t
  val concat : ('k1, 'v1) t -> ('k2, 'v2) t -> ('k, 'v) t
end
