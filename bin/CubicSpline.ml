(* Cubic spline interpolation module *)

type t = {
  x : float array;        (* x coordinates (parameter values) *)
  y : float array;        (* y coordinates (function values) *)
  a : float array;        (* spline coefficients *)
  b : float array;
  c : float array;
  d : float array;
}

(* Solve tridiagonal system using Thomas algorithm *)
let solve_tridiagonal (lower : float array) (diag : float array) (upper : float array) (rhs : float array) : float array =
  let n = Array.length diag in
  let c_prime = Array.make n 0.0 in
  let d_prime = Array.make n 0.0 in
  let x = Array.make n 0.0 in

  c_prime.(0) <- upper.(0) /. diag.(0);
  d_prime.(0) <- rhs.(0) /. diag.(0);

  for i = 1 to n - 1 do
    let m = diag.(i) -. lower.(i - 1) *. c_prime.(i - 1) in
    c_prime.(i) <- if i < n - 1 then upper.(i) /. m else 0.0;
    d_prime.(i) <- (rhs.(i) -. lower.(i - 1) *. d_prime.(i - 1)) /. m
  done;

  x.(n - 1) <- d_prime.(n - 1);
  for i = n - 2 downto 0 do
    x.(i) <- d_prime.(i) -. c_prime.(i) *. x.(i + 1)
  done;

  x

(* Create a natural cubic spline *)
let create (x_vals : float array) (y_vals : float array) : t =
  let n = Array.length x_vals in
  if n < 2 then failwith "Need at least 2 points for spline";
  if Array.length y_vals <> n then failwith "x and y arrays must have same length";

  (* Calculate h values (differences between x points) *)
  let h = Array.init (n - 1) (fun i -> x_vals.(i + 1) -. x_vals.(i)) in

  (* Set up tridiagonal system for natural cubic spline *)
  (* Natural spline: second derivative = 0 at endpoints *)
  let lower = Array.make (n - 2) 0.0 in
  let diag = Array.make n 1.0 in
  let upper = Array.make (n - 2) 0.0 in
  let rhs = Array.make n 0.0 in

  (* Interior points *)
  for i = 1 to n - 2 do
    lower.(i - 1) <- h.(i - 1);
    diag.(i) <- 2.0 *. (h.(i - 1) +. h.(i));
    upper.(i - 1) <- h.(i);
    rhs.(i) <- 3.0 *. ((y_vals.(i + 1) -. y_vals.(i)) /. h.(i) -. (y_vals.(i) -. y_vals.(i - 1)) /. h.(i - 1))
  done;

  (* Natural boundary conditions: c_0 = c_n = 0 *)
  diag.(0) <- 1.0;
  diag.(n - 1) <- 1.0;

  (* Solve for c coefficients (second derivatives / 2) *)
  let c = Array.make n 0.0 in
  if n > 2 then begin
    let c_interior = solve_tridiagonal lower (Array.sub diag 1 (n - 2)) upper (Array.sub rhs 1 (n - 2)) in
    Array.blit c_interior 0 c 1 (n - 2)
  end;

  (* Calculate other coefficients *)
  let a = Array.copy y_vals in
  let b = Array.make (n - 1) 0.0 in
  let d = Array.make (n - 1) 0.0 in

  for i = 0 to n - 2 do
    d.(i) <- (c.(i + 1) -. c.(i)) /. (3.0 *. h.(i));
    b.(i) <- (y_vals.(i + 1) -. y_vals.(i)) /. h.(i) -. h.(i) *. (2.0 *. c.(i) +. c.(i + 1)) /. 3.0
  done;

  { x = x_vals; y = y_vals; a; b; c; d }

(* Evaluate spline at a point *)
let eval (spline : t) (x : float) : float =
  let n = Array.length spline.x in

  (* Find the interval containing x *)
  let rec find_interval i =
    if i >= n - 1 then n - 2
    else if x < spline.x.(i + 1) then i
    else find_interval (i + 1)
  in

  let i =
    if x <= spline.x.(0) then 0
    else if x >= spline.x.(n - 1) then n - 2
    else find_interval 0
  in

  (* Evaluate polynomial in this interval *)
  let dx = x -. spline.x.(i) in
  spline.a.(i) +. dx *. (spline.b.(i) +. dx *. (spline.c.(i) +. dx *. spline.d.(i)))

(* Evaluate first derivative of spline at a point *)
let eval_deriv (spline : t) (x : float) : float =
  let n = Array.length spline.x in

  (* Find the interval containing x *)
  let rec find_interval i =
    if i >= n - 1 then n - 2
    else if x < spline.x.(i + 1) then i
    else find_interval (i + 1)
  in

  let i =
    if x <= spline.x.(0) then 0
    else if x >= spline.x.(n - 1) then n - 2
    else find_interval 0
  in

  (* Evaluate derivative: b + 2*c*dx + 3*d*dx^2 *)
  let dx = x -. spline.x.(i) in
  spline.b.(i) +. dx *. (2.0 *. spline.c.(i) +. dx *. 3.0 *. spline.d.(i))
