open Sized_numbers

module Alcotest = Alcotest.V1

let () =
  let open Alcotest in

  let sized_int (type t) (module I : Unsigned.S with type t = t) : t testable =
    testable I.pp I.equal
  in

  (* let uint8 = sized_int (module UInt8) in *)
  (* let uint16 = sized_int (module UInt16) in *)
  let uint32 = sized_int (module UInt32) in
  let uint64 = sized_int (module UInt64) in
  (* let int8 = sized_int (module Int8) in *)
  (* let int16 = sized_int (module Int16) in *)

  let limit_tests (module I : Unsigned.S) = [
    test_case "wrap max_int successor" `Quick (fun () -> check (sized_int (module I)) "same uint8" I.min_int I.(succ max_int));
    test_case "wrap min_int predecessor" `Quick (fun () -> check (sized_int (module I)) "same uint8" I.max_int I.(pred min_int));
  ] in

  let comparison_tests (module I : Unsigned.S) =
    let open I in
    (* TODO: Property based testing *)
    let n = O.(max_int / (one + one)) in
    [
      test_case "compare n n" `Quick (fun () -> check int "same int" 0 (compare n n));
      test_case "equal n n" `Quick (fun () -> check bool "same bool" true (equal n n));
      test_case "n = n" `Quick (fun () -> check bool "same bool" true O.(n = n));
      test_case "n <> n" `Quick (fun () -> check bool "same bool" false O.(n <> n));
      test_case "n < n" `Quick (fun () -> check bool "same bool" false O.(n < n));
      test_case "n > n" `Quick (fun () -> check bool "same bool" false O.(n > n));
      test_case "n <= n" `Quick (fun () -> check bool "same bool" true O.(n <= n));
      test_case "n >= n" `Quick (fun () -> check bool "same bool" true O.(n >= n));

      test_case "compare n (n + 1)" `Quick (fun () -> check int "same int" (-1) (compare n (succ n)));
      test_case "equal n (n + 1)" `Quick (fun () -> check bool "same bool" false (equal n (succ n)));
      test_case "n = n + 1" `Quick (fun () -> check bool "same bool" false O.(n = succ n));
      test_case "n <> n + 1" `Quick (fun () -> check bool "same bool" true O.(n <> succ n));
      test_case "n < n + 1" `Quick (fun () -> check bool "same bool" true O.(n < succ n));
      test_case "n > n + 1" `Quick (fun () -> check bool "same bool" false O.(n > succ n));
      test_case "n <= n + 1" `Quick (fun () -> check bool "same bool" true O.(n <= succ n));
      test_case "n >= n + 1" `Quick (fun () -> check bool "same bool" false O.(n >= succ n));

      test_case "compare n (n - 1)" `Quick (fun () -> check int "same int" (+1) (compare n (pred n)));
      test_case "equal n (n - 1)" `Quick (fun () -> check bool "same bool" false (equal n (pred n)));
      test_case "n = n - 1" `Quick (fun () -> check bool "same bool" false O.(n = pred n));
      test_case "n <> n - 1" `Quick (fun () -> check bool "same bool" true O.(n <> pred n));
      test_case "n < n - 1" `Quick (fun () -> check bool "same bool" false O.(n < pred n));
      test_case "n > n - 1" `Quick (fun () -> check bool "same bool" true O.(n > pred n));
      test_case "n <= n - 1" `Quick (fun () -> check bool "same bool" false O.(n <= pred n));
      test_case "n >= n - 1" `Quick (fun () -> check bool "same bool" true O.(n >= pred n));
    ]
  in

  run "Sized_numbers" [
    "UInt32.to_string", [
      test_case "zero" `Quick (fun () -> check string "same string" "0" UInt32.(to_string zero));
      test_case "one" `Quick (fun () -> check string "same string" "1" UInt32.(to_string one));
      test_case "min_int" `Quick (fun () -> check string "same string" "0" UInt32.(to_string min_int));
      test_case "max_int" `Quick (fun () -> check string "same string" "4294967295" UInt32.(to_string max_int));
    ];

    "UInt64.to_string", [
      test_case "zero" `Quick (fun () -> check string "same string" "0" UInt64.(to_string zero));
      test_case "one" `Quick (fun () -> check string "same string" "1" UInt64.(to_string one));
      test_case "min_int" `Quick (fun () -> check string "same string" "0" UInt64.(to_string min_int));
      test_case "max_int" `Quick (fun () -> check string "same string" "18446744073709551615" UInt64.(to_string max_int));
    ];

    "UInt32.of_string", [
      test_case "zero" `Quick (fun () -> check uint32 "same uint32" UInt32.zero UInt32.(of_string "0"));
      test_case "one" `Quick (fun () -> check uint32 "same uint32" UInt32.one UInt32.(of_string "1"));
      test_case "min_int" `Quick (fun () -> check uint32 "same uint32" UInt32.min_int UInt32.(of_string "0"));
      test_case "max_int" `Quick (fun () -> check uint32 "same uint32" UInt32.max_int UInt32.(of_string "4294967295"));
      test_case "positive zero" `Quick (fun () -> check uint32 "same uint32" UInt32.zero UInt32.(of_string "+0"));
      test_case "positive one" `Quick (fun () -> check uint32 "same uint32" UInt32.one UInt32.(of_string "+1"));
      test_case "positive min_int" `Quick (fun () -> check uint32 "same uint32" UInt32.min_int UInt32.(of_string "+0"));
      test_case "positive max_int" `Quick (fun () -> check uint32 "same uint32" UInt32.max_int UInt32.(of_string "+4294967295"));
      test_case "digit separators" `Quick (fun () -> check uint32 "same uint32" UInt32.max_int UInt32.(of_string "429__49_672___95_"));
      test_case "unfinished sign" `Quick (fun () -> check_raises "failure" (Failure "UInt32.of_string") (fun () -> ignore UInt32.(of_string "+")));
      test_case "unfinished base" `Quick (fun () -> check_raises "failure" (Failure "UInt32.of_string") (fun () -> ignore UInt32.(of_string "0x")));
      test_case "unfinished sign and base" `Quick (fun () -> check_raises "failure" (Failure "UInt32.of_string") (fun () -> ignore UInt32.(of_string "+0x")));
      test_case "separator at start" `Quick (fun () -> check_raises "failure" (Failure "UInt32.of_string") (fun () -> ignore UInt32.(of_string "_0")));
      test_case "separator after sign" `Quick (fun () -> check_raises "failure" (Failure "UInt32.of_string") (fun () -> ignore UInt32.(of_string "+_0")));
      test_case "separator after base" `Quick (fun () -> check_raises "failure" (Failure "UInt32.of_string") (fun () -> ignore UInt32.(of_string "0x_0")));
    ];

    "UInt64.of_string", [
      test_case "zero" `Quick (fun () -> check uint64 "same uint64" UInt64.zero UInt64.(of_string "0"));
      test_case "one" `Quick (fun () -> check uint64 "same uint64" UInt64.one UInt64.(of_string "1"));
      test_case "min_int" `Quick (fun () -> check uint64 "same uint64" UInt64.min_int UInt64.(of_string "0"));
      test_case "max_int" `Quick (fun () -> check uint64 "same uint64" UInt64.max_int UInt64.(of_string "18446744073709551615"));
      test_case "positive zero" `Quick (fun () -> check uint64 "same uint64" UInt64.zero UInt64.(of_string "+0"));
      test_case "positive one" `Quick (fun () -> check uint64 "same uint64" UInt64.one UInt64.(of_string "+1"));
      test_case "positive min_int" `Quick (fun () -> check uint64 "same uint64" UInt64.min_int UInt64.(of_string "+0"));
      test_case "positive max_int" `Quick (fun () -> check uint64 "same uint64" UInt64.max_int UInt64.(of_string "+18446744073709551615"));
      test_case "digit separators" `Quick (fun () -> check uint64 "same uint64" UInt64.max_int UInt64.(of_string "18446__74407370_9551_______615"));
      test_case "unfinished sign" `Quick (fun () -> check_raises "failure" (Failure "UInt64.of_string") (fun () -> ignore UInt64.(of_string "+")));
      test_case "unfinished base" `Quick (fun () -> check_raises "failure" (Failure "UInt64.of_string") (fun () -> ignore UInt64.(of_string "0x")));
      test_case "unfinished sign and base" `Quick (fun () -> check_raises "failure" (Failure "UInt64.of_string") (fun () -> ignore UInt64.(of_string "+0x")));
      test_case "separator at start" `Quick (fun () -> check_raises "failure" (Failure "UInt64.of_string") (fun () -> ignore UInt64.(of_string "_0")));
      test_case "separator after sign" `Quick (fun () -> check_raises "failure" (Failure "UInt64.of_string") (fun () -> ignore UInt64.(of_string "+_0")));
      test_case "separator after base" `Quick (fun () -> check_raises "failure" (Failure "UInt64.of_string") (fun () -> ignore UInt64.(of_string "0x_0")));
    ];

    "UInt8 limits", limit_tests (module UInt8);
    "UInt16 limits", limit_tests (module UInt16);
    "UInt32 limits", limit_tests (module UInt32);
    "UInt64 limits", limit_tests (module UInt64);
    "Int8 limits", limit_tests (module Int8);
    "Int16 limits", limit_tests (module Int16);
    "Int32 limits", limit_tests (module Int32);
    "Int64 limits", limit_tests (module Int64);

    "UInt8 comparisons", comparison_tests (module UInt8);
    "UInt16 comparisons", comparison_tests (module UInt16);
    "UInt32 comparisons", comparison_tests (module UInt32);
    "UInt64 comparisons", comparison_tests (module UInt64);
    "Int8 comparisons", comparison_tests (module Int8);
    "Int16 comparisons", comparison_tests (module Int16);
    "Int32 comparisons", comparison_tests (module Int32);
    "Int64 comparisons", comparison_tests (module Int64);
  ]
