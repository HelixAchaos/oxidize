let mut a = [8, -23, 5] in
    let mut b = if true then ([a.1])
            else (
                (let c = &mut a in
                    let d = &a.1 in
                        unit);
                (let e = a in [0])) in
        a.2