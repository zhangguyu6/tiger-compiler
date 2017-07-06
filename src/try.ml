let a = [1;2;3];;
let b i j k = i+j+k
let c =b 1
let d =c 2
let e = d 3;;

let da =ref 1
let da2 = (1,da)
let change (_,d) = 
  d:=2;;

print_int (!da);;
change da2;;
print_int (!da);;
