open List

let randZ () =
  let l = map (fun x -> Random.bits x) [();();();();();();();()] in
  let fold = fold_left (fun a b -> let x = Z.shift_left a 30 in
                                   Z.add x (Z.of_int (Random.bits())))
                       (Z.of_int 0) l
  in
  fold


let randG generator group =
  let exponent = randZ () in
  Z.powm_sec generator exponent group


let normalize (n : Z.t) group : Z.t =
  Z.rem (Z.add (Z.rem n group) group) group
