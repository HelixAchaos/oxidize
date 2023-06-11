let mut a = [8, 4] in
    let y = (if (1 < 2) then a.1 else 1) in
        let z = &mut a.0 in
            *z = 9; a