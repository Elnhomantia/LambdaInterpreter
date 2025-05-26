open OUnit2
open InterpreterLambdaBruijn

let t1:Ast.term = Ast.App (Ast.Lambda (Ast.Var 0), Ast.Var 1)
let t2:Ast.term = Ast.App (Ast.Lambda (Ast.Var 0), Ast.Lambda (Ast.Var 0))

let test_lo_1 _ =
  let expected = Ast.Var 1 in
  assert_equal ~printer:Ast.string_of_terme expected (Ast.loAll t1)

let test_lo_2 _ =
  let expected = Ast.Lambda (Ast.Var 0) in
  assert_equal ~printer:Ast.string_of_terme expected (Ast.loAll t2)

let suite =
  "test_lo_suite" >::: [
    "test_lo_1" >:: test_lo_1;
    "test_lo_2" >:: test_lo_2;
  ]

let () =
  run_test_tt_main suite