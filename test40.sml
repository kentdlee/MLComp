let fun append nil L = L
      | append (h::t) L = h :: (append t L) 

    fun appendOne nil = (fn L => L)
      | appendOne (h::t) = (fn L => h :: (appendOne t L))
in 
  println(append [1,2,3] [4]);
  println(appendOne [1,2,3] [4])
end
