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

let%expect_test "last word" =
  let open Stdlib.Printf in
  let args =
    Cmdargs.parse "echo 'hello     example' 'test''shell' world''script"
  in
  printf "arg count: %d\n\n" (List.length args);
  String.concat ~sep:"\n" args |> Stdlib.print_endline |> ignore;
  [%expect
    {|
    arg count: 4  

    echo
    hello     example 
    testshell 
    worldscript
  |}]

let%expect_test "adjacend single quotes" =
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

let%expect_test "cat files" =
  let open Stdlib.Printf in
  let args =
    Cmdargs.parse "cat '/tmp/owl/f   56' '/tmp/owl/f   54' '/tmp/owl/f   92'"
  in
  printf "arg count: %d\n\n" (List.length args);
  String.concat ~sep:"\n" args |> Stdlib.print_endline |> ignore;
  [%expect
    {|
    arg count: 4  

    cat
    /tmp/owl/f   56
    /tmp/owl/f   54
    /tmp/owl/f   92
  |}]

let%expect_test "backslashes" =
  let open Stdlib.Printf in
  let args =
    Cmdargs.parse
      "cat /tmp/bee/'no slash 99' /tmp/bee/'one slash \41' /tmp/bee/'two \
       slashes \84\'"
  in
  printf "arg count: %d\n\n" (List.length args);
  String.concat ~sep:"\n" args |> Stdlib.print_endline |> ignore;
  [%expect
    {|
    arg count: 4

    cat 
    /tmp/bee/'no slash 99' 
    /tmp/bee/'one slash \41' 
    /tmp/bee/'two slashes \84\'
  |}]
