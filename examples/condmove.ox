let a = 0 in
    let mut b = 5 in 
        (let y = (if true then 1 else (b = a; b=1;b=2;b)) in
            y; b)