let fun append nil L = L
      | append (h::t) L = h :: (append t L) 

    fun appendOne x = (fn nil => (fn L => L)
                        | h::t => (fn L => h :: (appendOne t L))) x
in 
  println(append [1,2,3] [4]);
  println(appendOne [1,2,3] [4])
end
