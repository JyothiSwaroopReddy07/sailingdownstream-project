
(*  Calculator with Efficient Factorial Calculation *)

let multiply arr n =
  let carry = ref 0 in
  for i = 0 to Array.length arr - 1 do
    let product = arr.(i) * n + !carry in
    arr.(i) <- product mod 10;  (* Keep only the last digit at current position *)
    carry := product / 10;      (* Carry over the remaining part *)
  done

let big_factorial n =
  let arr = Array.make (n * 10) 0 in  (* Sufficiently large array *)
  arr.(0) <- 1;  (* Initializing 1 as the first factorial *)

  for i = 1 to n do
    multiply arr i;
  done;

  (* Trimming leading zeros and converting array to string *)
  let rec find_first_non_zero i =
    if i < 0 then 0 else if arr.(i) != 0 then i else find_first_non_zero (i - 1)
  in
  let first_non_zero = find_first_non_zero (Array.length arr - 1) in
  String.init (first_non_zero + 1) (fun i -> char_of_int (arr.(first_non_zero - i) + 48))

let cube_root a =
  a ** (1.0 /. 3.0)  (* Custom cube root function *)

let rec calculator () =
  print_endline "Enter operation (+, -, *, /, ^, sqrt, cbrt, !, or 'exit' to quit):";
  let op = read_line () in
  match op with
  | "exit" -> print_endline "Exiting calculator."
  | "+" ->
    print_endline "Enter two numbers:";
    let a = read_float () in
    let b = read_float () in
    Printf.printf "Result: %f\n" (a +. b); calculator ()
  | "-" ->
    print_endline "Enter two numbers:";
    let a = read_float () in
    let b = read_float () in
    Printf.printf "Result: %f\n" (a -. b); calculator ()
  | "*" ->
    print_endline "Enter two numbers:";
    let a = read_float () in
    let b = read_float () in
    Printf.printf "Result: %f\n" (a *. b); calculator ()
  | "/" ->
    print_endline "Enter two numbers:";
    let a = read_float () in
    let b = read_float () in
    Printf.printf "Result: %f\n" (a /. b); calculator ()
  | "^" ->
    print_endline "Enter base and exponent:";
    let a = read_float () in
    let b = read_float () in
    Printf.printf "Result: %f\n" (a ** b); calculator ()
  | "sqrt" ->
    print_endline "Enter a number:";
    let a = read_float () in
    Printf.printf "Result: %f\n" (sqrt a); calculator ()
  | "cbrt" ->
    print_endline "Enter a number:";
    let a = read_float () in
    Printf.printf "Result: %f\n" (cube_root a); calculator ()
  | "!" ->
    print_endline "Enter a non-negative integer:";
    let a = read_int () in
    Printf.printf "Result: %s\n" (big_factorial a); calculator ()
  | _ ->
    print_endline "Invalid operation"; calculator ()

let () = calculator ()
