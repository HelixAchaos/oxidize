let mut a = [1, 2] in
    if 1 < 2 then (
        let b = &mut a.0 in
            let c = &a.1 in
                let d = &a in
                    a = [5, 7]
    ) else (
        unit
    )