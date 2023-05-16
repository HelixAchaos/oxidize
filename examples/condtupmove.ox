let mut a = [8, 4] in
    let y = (if (1 < 2) then &a.0 else &a.1) in
        let z = &mut a.0 in
            *z = 9; a