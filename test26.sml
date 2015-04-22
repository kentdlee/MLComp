 let fun goofy(L as h::t) = if h=1 then L else goofy t
       | goofy(x) = x

 in
   println
   (goofy [5, 6, 1, 2, 3])
 end


