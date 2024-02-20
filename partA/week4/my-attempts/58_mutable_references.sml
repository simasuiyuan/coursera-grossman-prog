(* mutable, reference (rarely use) *)
(* syntax: t ref  (t for type) *)
(* functions: 
 * - ref e : init a ref with content of `e` value evaluation 
 * - e1 := e2 : mutate the content e1 with e2
 * - !e : access/retrive the content from ref (! is not the syntax for negation
 * in sml/nj, for negation use `~`)*)
val x = ref 10;
val y = ref 10;
val z = x;
val _ = x := 20;
x;
y;
z;
(* print (x,y,z); *)
