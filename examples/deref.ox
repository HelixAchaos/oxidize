let mut x = 3 in
    (let y = &mut x in
        *y = 5; x = 1; x); x = 1; x