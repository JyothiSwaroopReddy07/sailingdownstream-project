(* Define an exception for handling errors *)
exception InvalidInput of string

(* Function to process the list *)
let process_list int_list =
    let rec filter lst i = match lst with
    | [] -> []
    | hd :: tl -> 
        if (i mod 2 <> 0) && (i mod 3 <> 0) then hd :: filter tl (i + 1)
        else filter tl (i + 1)
    in
    if List.length int_list mod 10 <> 0 then
        raise (InvalidInput "The list length must be a multiple of 10")
    else
        filter int_list 1

(* Testing function *)
let test_process_list () =
    (* Helper function for tests *)
    let assert_equal expected actual test_name =
        if expected = actual then
            Printf.printf "%s passed\n" test_name
        else
            Printf.printf "%s failed\n" test_name
    in

    (* Length tests *)
    assert_equal [1; 1; 1] (process_list [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]) "Length Test 1";
    begin try
        let _ = process_list [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11] in
        Printf.printf "Length Test 2 Failed - No error for non-multiple of 10\n"
    with
    | InvalidInput _ -> Printf.printf "Length Test 2 Passed\n"
    end;

    (* Content tests *)
    assert_equal [0; 4; 6] (process_list (List.init 10 (fun i -> i))) "Content Test 1";
    assert_equal [20; 24; 26] (process_list (List.init 10 (fun i -> i + 20))) "Content Test 2";

    (* Edge case tests *)
    assert_equal [] (process_list []) "Edge Case Test 1 - Empty list";
    assert_equal [0; 0; 0] (process_list [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]) "Edge Case Test 2 - List of zeros";
    assert_equal [-2; 2; 4] (process_list [-2; -1; 0; 1; 2; 3; 4; 5; 6; 7]) "Edge Case Test 3 - Negative numbers";

    print_endline "All tests completed."

(* Run the tests *)
let () = test_process_list ()
