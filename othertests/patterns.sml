fun factorial 0 = 1
  | factorial n = n * (factorial (n-1));

fun append nil L = L
  | append (h::t) L = h :: (append t L);

fun reverse nil = nil
  | reverse (h::t) = append (reverse t) [h] ;

factorial 5;

append [1,2,3] [4,5];

reverse [1,2,3,4,5]




