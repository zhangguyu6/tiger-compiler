type a = {
  mutable b:int;
  c :int
}

let incr a =
  a.b <- 1+a.b

let b = {
  b=1;
  c=2
} ;;

let print_and_incr b =
  incr b ; print_int b.b;;
print_int (incr b;b.b);;
print_and_incr b