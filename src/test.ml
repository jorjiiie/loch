open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Exprs
open Phases
open Errors

let t name program input expected =
  name >:: test_run ~args:[] ~std_input:input program name expected

let ta name program input expected =
  name >:: test_run_anf ~args:[] ~std_input:input program name expected

let tgc name heap_size program input expected =
  name
  >:: test_run
        ~args:[ string_of_int heap_size ]
        ~std_input:input program name expected

let tvg name program input expected =
  name >:: test_run_valgrind ~args:[] ~std_input:input program name expected

let tvgc name heap_size program input expected =
  name
  >:: test_run_valgrind
        ~args:[ string_of_int heap_size ]
        ~std_input:input program name expected

let terr name program input expected =
  name >:: test_err ~args:[] ~std_input:input program name expected

let tgcerr name heap_size program input expected =
  name
  >:: test_err
        ~args:[ string_of_int heap_size ]
        ~std_input:input program name expected

let tgc_nb name heap_size program input expected =
  name
  >:: test_run ~no_builtins:true
        ~args:[ string_of_int heap_size ]
        ~std_input:input program name expected

let tgc_nberr name heap_size program input expected =
  name
  >:: test_err ~no_builtins:true
        ~args:[ string_of_int heap_size ]
        ~std_input:input program name expected

let tanf name program input expected =
  name >:: fun _ ->
  assert_equal expected (anf (tag program)) ~printer:string_of_aprogram

let tparse name program expected =
  name >:: fun _ ->
  assert_equal (untagP expected)
    (untagP (parse_string name program))
    ~printer:string_of_program

let teq name actual expected =
  name >:: fun _ -> assert_equal expected actual ~printer:(fun s -> s)

(* let tfvs name program expected = name>:: *)
(*   (fun _ -> *)
(*     let ast = parse_string name program in *)
(*     let anfed = anf (tag ast) in *)
(*     let vars = free_vars_P anfed [] in *)
(*     let c = Stdlib.compare in *)
(*     let str_list_print strs = "[" ^ (ExtString.String.join ", " strs) ^ "]" in *)
(*     assert_equal (List.sort c vars) (List.sort c expected) ~printer:str_list_print) *)
(* ;; *)

let builtins_size = 12

let pair_tests =
  [
    t "tup1"
      "let t = (4, (5, 6)) in\n\
      \            begin\n\
      \              t[0] := 7;\n\
      \              t\n\
      \            end" "" "(7, (5, 6))";
    t "tup2"
      "let t = (4, (5, nil)) in\n\
      \            begin\n\
      \              t[1] := nil;\n\
      \              t\n\
      \            end" "" "(4, nil)";
    t "tup3"
      "let t = (4, (5, nil)) in\n\
      \            begin\n\
      \              t[1] := t;\n\
      \              t\n\
      \            end" "" "(4, <cyclic tuple 1>)";
    t "tup4" "let t = (4, 6) in\n            (t, t)" "" "((4, 6), (4, 6))";
  ]

let oom =
  [
    tgcerr "oomgc1" 4 "(1, (3, 4))" "" "Out of memory";
    tgc "oomgc2" (8 + builtins_size) "(1, (3, 4))" "" "(1, (3, 4))";
    tgc "oomgc4" (4 + builtins_size) "(3, 4)" "" "(3, 4)";
    tgcerr "oomgc5" (3 + builtins_size) "(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)" ""
      "Allocation";
  ]

let gc =
  [
    tgc "gc_lam1" (50 + builtins_size)
      "let f = (lambda: (1, 2)) in\n\
      \             begin\n\
      \               f();\n\
      \               f();\n\
      \               f();\n\
      \               f();f();f();f();f();f()\n\
      \             end" "" "(1, 2)";
  ]

let gc_suite =
  [
    tgcerr "gc1" (0 + builtins_size) "(1,2,3,4,5,6,7,8,9,11,12,13,14)" ""
      "Allocation";
    tgc "gc2" (32 + builtins_size)
      "let x = let y = (1,2,3,4,5,6,7,8,9,10) in 1 in let y = 5 in \
       (1,2,3,4,5,6,7,8,9,10)"
      "" "(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)";
    tgc "gc3" (32 + builtins_size)
      "let f = (lambda : let y = (1,2,3,4,5,6,7,8,9,10) in 1) in \
       f();f();f();f();f();f();f();f()"
      "" "1";
    tgc "gc4" (32 + builtins_size)
      "let f = (lambda :\n\
      \      let y = (1,2,3,4,5,6,7,8,9,10) in let x = (1,2) in x[0] := y; \
       x[1] := y; 1) in f();f();f();f();f();f();f();f();f()"
      "" "1";
    tgc "gc5" (30 + builtins_size)
      "def z(): let f = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) \
       in f[0]:=f; f[1]:=f; 1\n\
      \      let x = z();(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16) in 1" "" "1";
    tgc_nb "gc6" 0 "1" "" "1";
    tgc_nb "gc7" 20
      "def x(): let f = (1,2,3) in f[0]:= f; 1 let z = (1,2,3,4,5,6) in \
       z[0]:=z;z[1]:=z; x(); x();x();x();x();x();x();x(); z"
      "" "(<cyclic tuple 1>, <cyclic tuple 1>, 3, 4, 5, 6)";
    tgc_nb "gc8" 12
      "def x(): let a=1,b=2,c=3,d=4, e = (lambda : a+b+c+d) in 1 \
       x();x();x();x();x();x();x();x();x()"
      "" "1";
    tgc_nb "gc9" 6 "(1,2,3,4,5);(1,2,3,4,5);(1,2,3,4,5);(1,2,3,4,5)" ""
      "(1, 2, 3, 4, 5)";
    tgc_nberr "gc10" 4 "(nil, (13, 1))" "" "Out of memory";
    tgc_nb "gc11" 8 "(nil, (nil, 1))" "" "(nil, (nil, 1))";
    tgc_nb "gc12" 16 "(nil, (nil, 1))" "" "(nil, (nil, 1))";
    tgc_nb "collect_nil_tuple" 4 "(nil, nil, nil); (nil, nil, nil)" ""
      "(nil, nil, nil)";
    tgc_nberr "collect_nil_tuple_oom" 3 "(nil, nil, nil); (nil, nil, nil)" ""
      "needed 4 words";
    tgc_nb "retain_nil_tuple" 8 "let x = (nil, nil, nil) in (nil, nil, nil)" ""
      "(nil, nil, nil)";
    tgc_nberr "retain_nil_tuple_oom" 7
      "let x = (nil, nil, nil) in (nil, nil, nil)" "" "Out of memory";
    tgc_nb "seq_lam" 16 "(lambda (x): x)(1); (lambda (y): y)(2)" "" "2";
    tgc_nberr "seq_lam_oom" 6 "(lambda (x): x)(1); (lambda (y): y)(2)" ""
      "Out of memory";
    tgc_nb "simple_lam_alloc" 4 "(lambda (x): x)(1)" "" "1";
    tgc_nberr "simple_lam_oom" 3 "(lambda (x): x)(1)" "" "1";
    tgc_nb "lam_with_closure" 8
      "let a = 3, f = (lambda (x): x)(1) in (lambda (y): y)(2 + a)" "" "5";
    tgc_nberr "lam_with_closure_oom" 7
      "let a = 3, f = (lambda (x): x)(1) in (lambda (y): y)(2 + a)" ""
      "Out of memory";
    tgc_nb "complex_lam" 8
      "let a = 3, f = (lambda (x): x) in (lambda (y): y)(2 + a) + f(2)" "" "7";
    tgc_nberr "complex_lam_oom" 7
      "let a = 3, f = (lambda (x): x) in (lambda (y): y)(2 + a) + f(2)" ""
      "Out of memory";
    tgc_nb "collect_nested" 12
      "let x = (1, 2) in ((x, x), 2, x); (2, x, 3); let a = (lambda (p): p[0] \
       + p[1]) in a(x)"
      "" "3";
    tgc_nberr "collect_nested_x" 11
      "let x = (1, 2) in ((x, x), 2, x); (2, x, 3); let a = (lambda (p): p[0] \
       + p[1]) in a(x)"
      "" "Out of memory";
    tgc_nb "collect_cyclic" 8
      "let x = (1, 2) in (x, 1, 2); x[1] := x; (x, 1, 2)" ""
      "((1, <cyclic tuple 2>), 1, 2)";
    tgc_nberr "collect_cyclic_oom" 7
      "let x = (1, 2) in (x, 1, 2); x[1] := x; (x, 1, 2)" "" "Out of memory";
    tgc_nb "retain_cyclic" 12
      "let x = (1, 2) in (x, 1, 2); x[1] := x; let y = (x, 1, 2) in (x, y)" ""
      "((1, <cyclic tuple 2>), ((1, <cyclic tuple 4>), 1, 2))";
    tgc_nberr "retain_cyclic_oom" 11
      "let x = (1, 2) in (x, 1, 2); x[1] := x; let y = (x, 1, 2) in (x, y)" ""
      "Out of memory";
    tgc_nb "collect_cyclic_2" 8
      "let x = (1, 2) in (x, 1, 2); x[1] := x; (x, 1, 2); let a = 1 in (1,2,3)"
      "" "(1, 2, 3)";
    tgc_nberr "collect_cyclic_2_oom" 7
      "let x = (1, 2) in (x, 1, 2); x[1] := x; (x, 1, 2); let a = 1 in (1,2,3)"
      "" "Out of memory";
  ]

let input = [ t "input1" "let x = input() in x + 2" "123" "125" ]

let thread_tests =
  [
    t "create_thread" "thread((lambda: 1))" "" "<thread 1>";
    t "get_simple_thread" "let t = thread((lambda : 1)) in start(t); get(t)" ""
      "1";
    t "map_t"
      "def map(f, l):\n\
      \      if (l == nil): nil\n\
      \      else:\n\
      \      let (h, t) = l in\n\
      \        (f(h), map(f, t))\n\
      \      def f(x): x + 1\n\
      \      let x = map((lambda(x) : thread((lambda: f(x)))), (1, (2, (3, (4, \
       (5, nil)))))) in\n\
       map((lambda (t) : start(t)), x);\n\
      \            map((lambda (t) : get(t)), x)" ""
      "(2, (3, (4, (5, (6, nil)))))";
    terr "number_in_thread" "thread(1)" "" "expected lambda for thread, got:";
    terr "boolean_in_thread" "thread(true)" ""
      "expected lambda for thread, got:";
    terr "number_in_start" "start(1)" "" "expected thread for start, got:";
    terr "lambda_in_start" "start((lambda: 1))" ""
      "expected thread for start, got:";
    terr "lambda_in_get" "get((lambda: 1))" "" "expected thread for get, got:";
    terr "number_in_get" "get(3)" "" "expected thread for get, got:";
  ]

let lock_tests =
  [
    t "lock1" "let l = mutex() in lock(l); print(1);unlock(l);2" "" "1\n2";
    t "lock2"
      "def fib(n): if n < 2: n else: fib(n - 1) + fib(n - 2)\n\
      \      let l = mutex() in \n\
      \      let t = thread((lambda : lock(l); print(5); unlock(l); lock(l); \
       print(6); unlock(l); 5)) in\n\
      \      lock(l); start(t); print(fib(20)); unlock(l); get(t)" ""
      "6765\n5\n6\n5";
    terr "number_in_lock" "lock(1)" "" "expected mutex in lock, got:";
    terr "boolean_in_lock" "lock(true)" "" "expected mutex in lock, got:";
    terr "lambda_in_lock" "lock(true)" "" "expected mutex in lock, got:";
    terr "number_in_unlock" "unlock(-123)" "" "expected mutex in unlock, got:";
    terr "boolean_in_unlock" "unlock(true)" "" "expected mutex in unlock, got:";
    terr "lambda_in_unlock" "unlock((lambda: 1))" ""
      "expected mutex in unlock, got:";
    t "lock3" "def inc(l,t): lock(l); t[0] := t[0] + 1; unlock(l)\n\
               def do_n(i,n,f): if i < n: f(); do_n(i+1,n,f) else: 0\n\ 
      \      let l = mutex() in let t = (0,0) in\n\
      \      let t1 = thread((lambda : do_n(0,1000,(lambda : inc(l,t))))) in\n\
      \      let t2 = thread((lambda : do_n(0,1000,(lambda : inc(l,t))))) in\n\
      \      start(t1); start(t2); get(t1); get(t2); t[0]" ""
      "2000"; (* for some reason the below works*)
    t "lock4_bad" "def inc(l,t):  t[0] := t[0] + 1\n\ 
    def do_n(i,n,f): if i < n: f(); do_n(i+1,n,f) else: 0\n\ 
\      let l = mutex() in let t = (0,0) in\n\
\      let t1 = thread((lambda : do_n(0,10000,(lambda : inc(l,t))))) in\n\
\      let t2 = thread((lambda : do_n(0,10000,(lambda : inc(l,t))))) in\n\
       let t3 = thread((lambda : do_n(0,10000,(lambda : inc(l,t))))) in\n\
       let t4 = thread((lambda : do_n(0,10000,(lambda : inc(l,t))))) in\n\
        start(t1); start(t2); start(t3); start(t4); get(t1); get(t2); get(t3); get(t4); t[0]" ""
      "40000";]

let benchmark_tests =
  [
    (* t "benchmark"
       "def fib(n): if n < 2: n else: fib(n - 1) + fib(n - 2) \n\
        let t = thread((lambda: fib(45))) in start(t); fib(46) + get(t)" ""
       "2971215073"; *)
    t "benchmark2"
      "def fib(n): if n < 2: n else: fib(n - 1) + fib(n - 2)\n\
      \      def map(f, l):\n\
      \      if (l == nil): nil\n\
      \      else:\n\
      \      let (h, t) = l in\n\
      \        (f(h), map(f, t))\n\
      \      let x = map((lambda(x) : thread((lambda: fib(x)))), (44, (44, \
       (44, (44, (44, (44, (44, (44, nil))))))))) in\n\
       map((lambda (t) : start(t)), x);\n\
      \            map((lambda (t) : get(t)), x)" ""
      "(701408733, (701408733, (701408733, (701408733, (701408733, (701408733, \
       (701408733, (701408733, nil))))))))";
  ]

let suite = "unit_tests" >::: pair_tests @ oom @ gc @ input @ gc_suite @ thread_tests

let () =
  run_test_tt_main
    ("thread_tests" >::: [ "thread_suite" >::: thread_tests @ lock_tests ])
(* let () = run_test_tt_main ("all_tests" >::: [ suite; input_file_test_suite () ]) *)