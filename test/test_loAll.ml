open OUnit2
open InterpreterLambdaBruijn
open Hcons.Hcons

let t1:term = sApp (sLambda (sVar 0)) (sVar 1)
let t2:term = sApp (sLambda (sVar 0)) (sLambda (sVar 0))

let test_lo_1 _ =
  let expected = sVar 1 in
  assert_equal ~printer:Ast.string_of_terme expected (Ast.loAll t1)

let test_lo_2 _ =
  let expected = sLambda (sVar 0) in
  assert_equal ~printer:Ast.string_of_terme expected (Ast.loAll t2)

let suite =
  "test_lo_suite" >::: [
    "test_lo_1" >:: test_lo_1;
    "test_lo_2" >:: test_lo_2;
  ]

let () =
  run_test_tt_main suite