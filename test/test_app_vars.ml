open OUnit2
open InterpreterLambdaBruijn
open Hcons.Hcons

let t1:term = 
(sApp
  (sLambda
    (sLambda
      (sLambda
        (sApp (sApp (sVar 2) (sVar 1)) (sVar 0))
      )
    )
  )
  (sApp (sVar 0) (sVar 0))
)

(*
this don't works
The same as :
next lo ((L.(L.(L.(2 1 0)))) ((L.0) (L.0)));
which works
????????????????????????????????
*)

let test_lo_1 _ =
  let expected = sVar 1 in
  assert_equal ~printer:Ast.string_of_terme expected (Ast.loAll t1)

let suite =
  "test_lo_suite" >::: [
    "test_lo_1" >:: test_lo_1;
  ]

let () =
  run_test_tt_main suite