let leibniz iter =
    let rec calc i n topterm =
        if i>= iter-1 then n *. 4.0
        else
            let bottomterm = float_of_int(i*2) in
            let term = topterm /. (bottomterm -. 1.0) in
            calc (i+1) (n +. term) (-.topterm)
    in
    calc 2 1.0 (-1.0)

let () =
    let result = leibniz 1000 in
    Printf.printf "Pi: %.10f\n" result