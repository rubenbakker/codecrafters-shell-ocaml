open! Base

let%expect_test "words" =
  let open Stdlib.Printf in
  let args = Cmdargs.parse "   echo  w1    w2  " in
  printf "arg count: %d\n\n" (List.length args);
  String.concat ~sep:"\n" args |> Stdlib.print_endline |> ignore;
  [%expect {|
    arg count: 3  

    echo
    w1 
    w2
  |}]

let%expect_test "single quote" =
  let open Stdlib.Printf in
  let args = Cmdargs.parse " echo  'w1    w2' test  " in
  printf "arg count: %d\n\n" (List.length args);
  String.concat ~sep:"\n" args |> Stdlib.print_endline |> ignore;
  [%expect {|
    arg count: 3  

    echo
    w1    w2
    test
  |}]

let%expect_test "adjanced single quotes" =
  let open Stdlib.Printf in
  let args = Cmdargs.parse " echo  'w1    w2'' test' war" in
  printf "arg count: %d\n\n" (List.length args);
  String.concat ~sep:"\n" args |> Stdlib.print_endline |> ignore;
  [%expect {|
    arg count: 3  

    echo
    w1    w2 test
    war
  |}]
