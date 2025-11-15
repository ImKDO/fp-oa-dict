open Lab2_lib.Oa_dict

module D = OA_Dict

(* Unit Tests *)

let test_empty () =
  let d = D.make_empty () in
  Alcotest.(check bool) "empty dict is empty" true (D.is_empty d);
  Alcotest.(check int) "empty dict has size 0" 0 (D.size d)

let test_insert_single () =
  let d = D.make_empty () in
  let d = D.insert 1 "one" d in
  Alcotest.(check bool) "dict with one element is not empty" false (D.is_empty d);
  Alcotest.(check int) "dict has size 1" 1 (D.size d);
  match D.find 1 d with
  | Some v -> Alcotest.(check string) "found value is 'one'" "one" v
  | None -> Alcotest.fail "key 1 should exist"

let test_insert_multiple () =
  let d = D.make_empty () in
  let d = D.insert 1 "one" d in
  let d = D.insert 2 "two" d in
  let d = D.insert 3 "three" d in
  Alcotest.(check int) "dict has size 3" 3 (D.size d);
  match D.find 2 d with
  | Some v -> Alcotest.(check string) "found value is 'two'" "two" v
  | None -> Alcotest.fail "key 2 should exist"

let test_insert_overwrite () =
  let d = D.make_empty () in
  let d = D.insert 1 "one" d in
  let d = D.insert 1 "ONE" d in
  Alcotest.(check int) "dict has size 1 after overwrite" 1 (D.size d);
  match D.find 1 d with
  | Some v -> Alcotest.(check string) "found value is 'ONE'" "ONE" v
  | None -> Alcotest.fail "key 1 should exist"

let test_remove () =
  let d = D.make_empty () in
  let d = D.insert 1 "one" d in
  let d = D.insert 2 "two" d in
  let d = D.remove 1 d in
  Alcotest.(check int) "dict has size 1 after removal" 1 (D.size d);
  Alcotest.(check bool) "key 1 does not exist" false (D.mem 1 d);
  Alcotest.(check bool) "key 2 exists" true (D.mem 2 d)

let test_find_not_found () =
  let d = D.make_empty () in
  let d = D.insert 1 "one" d in
  match D.find 99 d with
  | Some _ -> Alcotest.fail "key 99 should not exist"
  | None -> ()

let test_map () =
  let d = D.make_empty () in
  let d = D.insert 1 10 d in
  let d = D.insert 2 20 d in
  let d2 = D.map d (fun x -> x * 2) in
  Alcotest.(check int) "mapped dict has same size" 2 (D.size d2);
  match D.find 1 d2 with
  | Some v -> Alcotest.(check int) "value doubled" 20 v
  | None -> Alcotest.fail "key 1 should exist"

let test_filter () =
  let d = D.make_empty () in
  let d = D.insert 1 10 d in
  let d = D.insert 2 20 d in
  let d = D.insert 3 30 d in
  let d2 = D.filter d (fun x -> x > 15) in
  Alcotest.(check int) "filtered dict has size 2" 2 (D.size d2);
  Alcotest.(check bool) "key 1 removed" false (D.mem 1 d2);
  Alcotest.(check bool) "key 2 exists" true (D.mem 2 d2);
  Alcotest.(check bool) "key 3 exists" true (D.mem 3 d2)

let test_fold_left () =
  let d = D.make_empty () in
  let d = D.insert 1 10 d in
  let d = D.insert 2 20 d in
  let d = D.insert 3 30 d in
  let sum = D.fold_left 0 (fun acc _k v -> acc + v) d in
  Alcotest.(check int) "sum of values" 60 sum

let test_fold_right () =
  let d = D.make_empty () in
  let d = D.insert 1 10 d in
  let d = D.insert 2 20 d in
  let d = D.insert 3 30 d in
  let sum = D.fold_right d (fun _k v acc -> acc + v) 0 in
  Alcotest.(check int) "sum of values" 60 sum

let test_concat () =
  let d1 = D.make_empty () in
  let d1 = D.insert 1 "one" d1 in
  let d1 = D.insert 2 "two" d1 in
  let d2 = D.make_empty () in
  let d2 = D.insert 3 "three" d2 in
  let d2 = D.insert 4 "four" d2 in
  let d3 = D.concat d1 d2 in
  Alcotest.(check int) "concatenated dict has size 4" 4 (D.size d3);
  Alcotest.(check bool) "key 1 exists" true (D.mem 1 d3);
  Alcotest.(check bool) "key 3 exists" true (D.mem 3 d3)

let test_concat_override () =
  let d1 = D.make_empty () in
  let d1 = D.insert 1 "one" d1 in
  let d2 = D.make_empty () in
  let d2 = D.insert 1 "ONE" d2 in
  let d3 = D.concat d1 d2 in
  Alcotest.(check int) "concatenated dict has size 1" 1 (D.size d3);
  match D.find 1 d3 with
  | Some v -> Alcotest.(check string) "value from d2 (right argument)" "ONE" v
  | None -> Alcotest.fail "key 1 should exist"

let test_to_list () =
  let d = D.make_empty () in
  let d = D.insert 1 "one" d in
  let d = D.insert 2 "two" d in
  let lst = D.to_list d in
  Alcotest.(check int) "list has 2 elements" 2 (List.length lst);
  Alcotest.(check bool) "list contains (1, 'one')" true (List.mem (1, "one") lst);
  Alcotest.(check bool) "list contains (2, 'two')" true (List.mem (2, "two") lst)

let test_of_list () =
  let lst = [(1, "one"); (2, "two"); (3, "three")] in
  let d = D.of_list lst in
  Alcotest.(check int) "dict has size 3" 3 (D.size d);
  Alcotest.(check bool) "key 1 exists" true (D.mem 1 d);
  Alcotest.(check bool) "key 2 exists" true (D.mem 2 d);
  Alcotest.(check bool) "key 3 exists" true (D.mem 3 d)

let test_equal () =
  let d1 = D.make_empty () in
  let d1 = D.insert 1 10 d1 in
  let d1 = D.insert 2 20 d1 in
  let d2 = D.make_empty () in
  let d2 = D.insert 2 20 d2 in
  let d2 = D.insert 1 10 d2 in
  Alcotest.(check bool) "equal dicts" true (D.equal (=) d1 d2)

let test_not_equal () =
  let d1 = D.make_empty () in
  let d1 = D.insert 1 10 d1 in
  let d2 = D.make_empty () in
  let d2 = D.insert 1 20 d2 in
  Alcotest.(check bool) "not equal dicts" false (D.equal (=) d1 d2)

(* Monoid Tests *)

let test_monoid_left_identity () =
  let d = D.make_empty () in
  let d = D.insert 1 "one" d in
  let d = D.insert 2 "two" d in
  let result = D.concat (D.make_empty ()) d in
  Alcotest.(check bool) "left identity" true (D.equal (=) result d)

let test_monoid_right_identity () =
  let d = D.make_empty () in
  let d = D.insert 1 "one" d in
  let d = D.insert 2 "two" d in
  let result = D.concat d (D.make_empty ()) in
  Alcotest.(check bool) "right identity" true (D.equal (=) result d)

let test_monoid_associativity () =
  let d1 = D.insert 1 "one" (D.make_empty ()) in
  let d2 = D.insert 2 "two" (D.make_empty ()) in
  let d3 = D.insert 3 "three" (D.make_empty ()) in
  let left_assoc = D.concat (D.concat d1 d2) d3 in
  let right_assoc = D.concat d1 (D.concat d2 d3) in
  Alcotest.(check bool) "associativity" true (D.equal (=) left_assoc right_assoc)

(* Property-Based Tests using QCheck *)

let gen_dict =
  QCheck.make
    ~print:(fun d -> "dict with " ^ string_of_int (D.size d) ^ " elements")
    QCheck.Gen.(
      let* pairs = list (pair small_nat (string_size (int_range 1 10))) in
      return (D.of_list pairs)
    )

let prop_insert_increases_size =
  QCheck.Test.make ~count:100 ~name:"insert increases size (or keeps same on overwrite)"
    QCheck.(pair small_nat small_printable_string)
    (fun (k, v) ->
      let d = D.make_empty () in
      let size_before = D.size d in
      let d' = D.insert k v d in
      let size_after = D.size d' in
      size_after >= size_before)

let prop_insert_find =
  QCheck.Test.make ~count:100 ~name:"insert then find returns the value"
    QCheck.(triple small_nat small_printable_string gen_dict)
    (fun (k, v, d) ->
      let d' = D.insert k v d in
      match D.find k d' with
      | Some v' -> v = v'
      | None -> false)

let prop_remove_decreases_size =
  QCheck.Test.make ~count:100 ~name:"remove decreases size (or keeps same if key doesn't exist)"
    QCheck.(pair small_nat gen_dict)
    (fun (k, d) ->
      let size_before = D.size d in
      let d' = D.remove k d in
      let size_after = D.size d' in
      size_after <= size_before)

let prop_monoid_left_identity =
  QCheck.Test.make ~count:100 ~name:"monoid left identity: empty + d = d"
    gen_dict
    (fun d ->
      let result = D.concat (D.make_empty ()) d in
      D.equal (=) result d)

let prop_monoid_right_identity =
  QCheck.Test.make ~count:100 ~name:"monoid right identity: d + empty = d"
    gen_dict
    (fun d ->
      let result = D.concat d (D.make_empty ()) in
      D.equal (=) result d)

let prop_monoid_associativity =
  QCheck.Test.make ~count:100 ~name:"monoid associativity: (a + b) + c = a + (b + c)"
    QCheck.(triple gen_dict gen_dict gen_dict)
    (fun (d1, d2, d3) ->
      let left_assoc = D.concat (D.concat d1 d2) d3 in
      let right_assoc = D.concat d1 (D.concat d2 d3) in
      D.equal (=) left_assoc right_assoc)

let prop_to_list_of_list =
  QCheck.Test.make ~count:100 ~name:"to_list . of_list preserves all keys"
    QCheck.(list (pair small_nat small_printable_string))
    (fun lst ->
      let d = D.of_list lst in
      (* All keys from original list should be in result *)
      List.for_all (fun (k, _) -> D.mem k d) lst)

let prop_map_preserves_size =
  QCheck.Test.make ~count:100 ~name:"map preserves size"
    gen_dict
    (fun d ->
      let d' = D.map d (fun s -> String.length s) in
      D.size d = D.size d')

let prop_filter_decreases_size =
  QCheck.Test.make ~count:100 ~name:"filter decreases or maintains size"
    gen_dict
    (fun d ->
      let d' = D.filter d (fun s -> String.length s > 0) in
      D.size d' <= D.size d)

let prop_equal_reflexive =
  QCheck.Test.make ~count:100 ~name:"equal is reflexive: d = d"
    gen_dict
    (fun d -> D.equal (=) d d)

(* Test Suite *)

let unit_tests = [
  "empty dict", `Quick, test_empty;
  "insert single element", `Quick, test_insert_single;
  "insert multiple elements", `Quick, test_insert_multiple;
  "insert overwrites existing key", `Quick, test_insert_overwrite;
  "remove element", `Quick, test_remove;
  "find non-existent key", `Quick, test_find_not_found;
  "map function", `Quick, test_map;
  "filter function", `Quick, test_filter;
  "fold_left", `Quick, test_fold_left;
  "fold_right", `Quick, test_fold_right;
  "concat two dicts", `Quick, test_concat;
  "concat with override", `Quick, test_concat_override;
  "to_list", `Quick, test_to_list;
  "of_list", `Quick, test_of_list;
  "equal dicts", `Quick, test_equal;
  "not equal dicts", `Quick, test_not_equal;
  "monoid left identity", `Quick, test_monoid_left_identity;
  "monoid right identity", `Quick, test_monoid_right_identity;
  "monoid associativity", `Quick, test_monoid_associativity;
]

let property_tests = 
  List.map QCheck_alcotest.to_alcotest [
    prop_insert_increases_size;
    prop_insert_find;
    prop_remove_decreases_size;
    prop_monoid_left_identity;
    prop_monoid_right_identity;
    prop_monoid_associativity;
    prop_to_list_of_list;
    prop_map_preserves_size;
    prop_filter_decreases_size;
    prop_equal_reflexive;
  ]

let () =
  Alcotest.run "OA_Dict" [
    "Unit Tests", unit_tests;
    "Property-Based Tests", property_tests;
  ]
