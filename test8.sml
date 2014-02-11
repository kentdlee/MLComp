let val x = ref 0
in
  x := !x + 1;
  println (!x)
end
