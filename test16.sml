let val r = ref(fn x => x)
in
    r := (fn x => x+1);
    !r true
end
