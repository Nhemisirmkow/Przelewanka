(* Autor: Marcin Michorzewski   *)
(* Zadanie : Przelewanka        *)
(* Code review : Dominik Klemba *)


let przelewanka glass =
  let refill k state =
    let t_s = Array.copy state in
    (
      t_s.(k) <- fst glass.(k);
      t_s;
      ) in
  let spill k state =
    let t_s = Array.copy state in
      (
        t_s.(k) <- 0;
        t_s;
        ) in
  let diffuse k state =
    let t_l_s = ref [] in
      (
        let t_s = Array.copy state in
        for i = 0 to (Array.length glass - 1) do
          if (i <> k) then
          (
            let t_s_1 = Array.copy t_s in
            if (fst glass.(i) - t_s.(i) < t_s.(k)) then
              (
                t_s_1.(k) <- t_s.(k) - fst glass.(i) + t_s.(i);
                t_s_1.(i) <- fst glass.(i);
                t_l_s := t_s_1::(!t_l_s);
                )
            else
              (
                t_s_1.(i) <- t_s.(i) + t_s.(k);
                t_s_1.(k) <- 0;
                t_l_s := t_s_1::(!t_l_s);
                )
            )
          done;
        !t_l_s;
        )
  in
  let addable hash_table state dist =
    if not (Hashtbl.mem hash_table state) then
      (
        Hashtbl.add hash_table state dist;
        true
        )
    else false
  in
  let check state =
    fst (Array.fold_left (fun (a, b) x -> if snd glass.(b) = x then (a, b + 1) else (false, b + 1)) (true, 0) state)
  in
  let answer = ref (-1) in
  let state = Array.make (Array.length glass) 0 in
    let hash_table = Hashtbl.create 10007 in
    let q_states = Queue.create () in
      (
        Hashtbl.add hash_table state 0;
        Queue.push state q_states;
        while (not (Queue.is_empty q_states) && !answer = -1) do
          let t_state = Queue.pop q_states in
          let dist = Hashtbl.find hash_table t_state + 1 in
          if (check t_state) then (answer := dist - 1;) else
            (
              for i = 0 to (Array.length glass - 1) do
                let t_1_state = refill i t_state in
                let t_2_state = spill i t_state in
                let t_l_state = diffuse i t_state in
                (
                  if (addable hash_table t_1_state dist) then Queue.push t_1_state q_states;
                  if (addable hash_table t_2_state dist) then Queue.push t_2_state q_states;
                  List.iter (fun x -> if (addable hash_table x dist) then (Queue.push x q_states)) t_l_state;
                  )
              done;
              );
        done;
        !answer;
        );;
