let fun map v0 = 
  (fn v1 => 
      (fn (f, nil) => nil 
         | (f, h::t) => (f h) :: ((map f) t)) (v0,v1))
in
  println(map (fn x => x + 1) [1,2,3,4])
end
