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
  type 'v slot = None | Tombstone | Pair of (key * 'v)
  type 'v t = { size : int; buckets : 'v slot array }

  let make_empty () = { size = 0; buckets = Array.make 16 None }
  let capacity d = Array.length d.buckets
  let load_factor d = float_of_int d.size /. float_of_int (capacity d)
  let hash key cap = abs key mod cap

  let update_array arr idx value =
    Array.mapi (fun i slot -> if i = idx then value else slot) arr

  let rec find_slot buckets key cap idx first_tombstone =
    if idx >= cap then match first_tombstone with Some i -> i | None -> 0
    else
      match buckets.(idx) with
      | None -> ( match first_tombstone with Some i -> i | None -> idx)
      | Tombstone ->
          let ft =
            match first_tombstone with
            | None -> Some idx
            | Some _ -> first_tombstone
          in
          find_slot buckets key cap ((idx + 1) mod cap) ft
      | Pair (k, _) ->
          if k = key then idx
          else find_slot buckets key cap ((idx + 1) mod cap) first_tombstone

  let resize d =
    let old_buckets = d.buckets in
    let new_cap = max 1 (capacity d * 2) in
    let rec rehash idx acc =
      if idx >= Array.length old_buckets then acc
      else
        match old_buckets.(idx) with
        | Pair (k, v) ->
            let h = hash k new_cap in
            let slot_idx = find_slot acc k new_cap h None in
            let updated = update_array acc slot_idx (Pair (k, v)) in
            rehash (idx + 1) updated
        | _ -> rehash (idx + 1) acc
    in
    let new_buckets = rehash 0 (Array.make new_cap None) in
    { size = d.size; buckets = new_buckets }

  let is_empty d = d.size = 0

  let insert k v d =
    (* Увеличиваем размер при превышении load factor *)
    let d = if load_factor d > 0.7 then resize d else d in

    let cap = capacity d in
    let h = hash k cap in
    let idx = find_slot d.buckets k cap h None in

    let new_buckets = update_array d.buckets idx (Pair (k, v)) in

    let new_size =
      match d.buckets.(idx) with
      | Pair (key, _) when key = k -> d.size
      | _ -> d.size + 1
    in

    { size = new_size; buckets = new_buckets }

  let remove k d =
    let cap = capacity d in
    let h = hash k cap in
    let rec search idx count =
      if count >= cap then d
      else
        match d.buckets.(idx) with
        | None -> d
        | Tombstone -> search ((idx + 1) mod cap) (count + 1)
        | Pair (key, _) ->
            if key = k then
              let new_buckets = update_array d.buckets idx Tombstone in
              { size = d.size - 1; buckets = new_buckets }
            else search ((idx + 1) mod cap) (count + 1)
    in
    search h 0

  let find k d =
    let cap = capacity d in
    let h = hash k cap in
    let rec search idx count =
      if count >= cap then Option.None
      else
        match d.buckets.(idx) with
        | None -> Option.None
        | Tombstone -> search ((idx + 1) mod cap) (count + 1)
        | Pair (key, v) ->
            if key = k then Option.Some v
            else search ((idx + 1) mod cap) (count + 1)
    in
    search h 0

  let mem k d =
    match find k d with Option.Some _ -> true | Option.None -> false

  let map d f =
    let new_buckets =
      Array.init (Array.length d.buckets) (fun idx ->
          match d.buckets.(idx) with
          | Pair (k, v) -> Pair (k, f v)
          | Tombstone -> Tombstone
          | None -> None)
    in
    { size = d.size; buckets = new_buckets }

  let filter d pred =
    let rec filter_slots idx acc =
      if idx >= Array.length d.buckets then acc
      else
        let new_acc =
          match d.buckets.(idx) with
          | Pair (k, v) when pred v -> insert k v acc
          | _ -> acc
        in
        filter_slots (idx + 1) new_acc
    in
    filter_slots 0 (make_empty ())

  let fold_left acc f d =
    let rec fold_slots idx accumulator =
      if idx >= Array.length d.buckets then accumulator
      else
        let new_acc =
          match d.buckets.(idx) with
          | Pair (k, v) -> f accumulator k v
          | _ -> accumulator
        in
        fold_slots (idx + 1) new_acc
    in
    fold_slots 0 acc

  let fold_right d f acc =
    let rec fold_slots idx =
      if idx < 0 then acc
      else
        match d.buckets.(idx) with
        | Pair (k, v) -> f k v (fold_slots (idx - 1))
        | _ -> fold_slots (idx - 1)
    in
    fold_slots (Array.length d.buckets - 1)

  let concat d1 d2 = fold_left d1 (fun acc k v -> insert k v acc) d2
  let size d = d.size
  let to_list d = fold_left [] (fun acc k v -> (k, v) :: acc) d |> List.rev

  let of_list lst =
    List.fold_left (fun acc (k, v) -> insert k v acc) (make_empty ()) lst

  let equal eq d1 d2 =
    if d1.size <> d2.size then false
    else
      fold_left true
        (fun acc k v1 ->
          if not acc then false
          else
            match find k d2 with
            | Option.Some v2 -> eq v1 v2
            | Option.None -> false)
        d1
end
