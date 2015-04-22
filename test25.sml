let fun bar x = 
    let fun foo y = x
    in
        foo
    end
in
  bar
end