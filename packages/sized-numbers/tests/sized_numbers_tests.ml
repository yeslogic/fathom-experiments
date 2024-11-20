open Sized_numbers

module Alcotest = Alcotest.V1

let () =
  let open Alcotest in

  let uint8 = testable UInt8.pp UInt8.equal in
  let uint16 = testable UInt16.pp UInt16.equal in
  let uint32 = testable UInt32.pp UInt32.equal in
  let uint64 = testable UInt64.pp UInt64.equal in
  let int8 = testable Int8.pp Int8.equal in
  let int16 = testable Int16.pp Int16.equal in

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
    "UInt8 limits", [
      test_case "wrap max_int successor" `Quick (fun () -> check uint8 "same uint8" UInt8.min_int UInt8.(succ max_int));
      test_case "wrap min_int predecessor" `Quick (fun () -> check uint8 "same uint8" UInt8.max_int UInt8.(pred min_int));
    ];
    "UInt16 limits", [
      test_case "wrap max_int successor" `Quick (fun () -> check uint16 "same uint16" UInt16.min_int (UInt16.(succ max_int)));
      test_case "wrap min_int predecessor" `Quick (fun () -> check uint16 "same uint16" UInt16.max_int (UInt16.(pred min_int)));
    ];
    "UInt32 limits", [
      test_case "wrap max_int successor" `Quick (fun () -> check uint32 "same uint32" UInt32.min_int UInt32.(succ max_int));
      test_case "wrap min_int predecessor" `Quick (fun () -> check uint32 "same uint32" UInt32.max_int UInt32.(pred min_int));
    ];
    "UInt64 limits", [
      test_case "wrap max_int successor" `Quick (fun () -> check uint64 "same uint64" UInt64.min_int UInt64.(succ max_int));
      test_case "wrap min_int predecessor" `Quick (fun () -> check uint64 "same uint64" UInt64.max_int UInt64.(pred min_int));
    ];
    "Int8 limits", [
      test_case "wrap max_int successor" `Quick (fun () -> check int8 "same uint8" Int8.min_int Int8.(succ max_int));
      test_case "wrap min_int predecessor" `Quick (fun () -> check int8 "same uint8" Int8.max_int Int8.(pred min_int));
    ];
    "Int16 limits", [
      test_case "wrap max_int successor" `Quick (fun () -> check int16 "same uint16" Int16.min_int Int16.(succ max_int));
      test_case "wrap min_int predecessor" `Quick (fun () -> check int16 "same uint16" Int16.max_int Int16.(pred min_int));
    ];
    "Int32 limits", [
      test_case "wrap max_int successor" `Quick (fun () -> check int32 "same uint32" Int32.min_int Int32.(succ max_int));
      test_case "wrap min_int predecessor" `Quick (fun () -> check int32 "same uint32" Int32.max_int Int32.(pred min_int));
    ];
    "Int64 limits", [
      test_case "wrap max_int successor" `Quick (fun () -> check int64 "same uint64" Int64.min_int Int64.(succ max_int));
      test_case "wrap min_int predecessor" `Quick (fun () -> check int64 "same uint64" Int64.max_int Int64.(pred min_int));
    ];
  ]
