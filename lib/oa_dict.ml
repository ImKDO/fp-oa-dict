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
  val concat : 'v t -> 'v t -> 'v t

  (* Comparison *)
  val equal : ('v -> 'v -> bool) -> 'v t -> 'v t -> bool
  val size : 'v t -> int
  val to_list : 'v t -> (key * 'v) list
  val of_list : (key * 'v) list -> 'v t
end

module OA_Dict = struct
  type key = int
  type 'v t = { size : int; entries : (key * 'v) list }

  let make_empty () = { size = 0; entries = [] }
  let is_empty d = d.size = 0

  let rec replace_or_insert k v acc = function
    | [] -> ((k, v) :: acc |> List.rev, false)
    | (k', _) :: rest when k' = k -> (List.rev acc @ ((k, v) :: rest), true)
    | pair :: rest -> replace_or_insert k v (pair :: acc) rest

  let insert k v d =
    let new_entries, replaced = replace_or_insert k v [] d.entries in
    let new_size = if replaced then d.size else d.size + 1 in
    { size = new_size; entries = new_entries }

  let rec remove_helper k acc removed = function
    | [] -> (List.rev acc, removed)
    | (k', _) :: rest when k' = k -> (List.rev_append acc rest, true)
    | pair :: rest -> remove_helper k (pair :: acc) removed rest

  let remove k d =
    let entries, removed = remove_helper k [] false d.entries in
    if removed then { size = d.size - 1; entries } else d

  let find k d =
    match List.find_opt (fun (k', _) -> k' = k) d.entries with
    | None -> None
    | Some (_, v) -> Some v

  let mem k d = Option.is_some (find k d)

  let map d f =
    { size = d.size; entries = List.map (fun (k, v) -> (k, f v)) d.entries }

  let filter d pred =
    let entries = List.filter (fun (_, v) -> pred v) d.entries in
    { size = List.length entries; entries }

  let fold_left acc f d = List.fold_left (fun a (k, v) -> f a k v) acc d.entries

  let fold_right d f acc =
    List.fold_right (fun (k, v) a -> f k v a) d.entries acc

  let concat d1 d2 = fold_left d1 (fun acc k v -> insert k v acc) d2
  let size d = d.size
  let to_list d = d.entries

  let of_list lst =
    List.fold_left (fun acc (k, v) -> insert k v acc) (make_empty ()) lst

  let equal eq d1 d2 =
    if d1.size <> d2.size then false
    else
      fold_left true
        (fun acc k v1 ->
          acc && match find k d2 with Some v2 -> eq v1 v2 | None -> false)
        d1
end
