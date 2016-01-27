(* Autor: Marcin Michorzewski   *)
(* Zadanie : Przelewanka        *)
(* Code review : Dominik Klemba *)

(* Stan przechowywany jest jako tablica int *)
(* odpowiedź, jeżeli żaden stan nie będzie równy oczekiwanemu wynosi*)
(* -1 *)
(* wszelkie osiągalne stany są przeglądane poczynając od początkoweg*)
(* optymalizacja: Jeżeli nie istnieje szklanka, gdzie y = x lub y =0*)
(* to stan końcowy nie jest osiagalny                               *)

let przelewanka glass =
  let answer = ref (-1) in
  let s_size = Array.length glass in
  let nwd a b =
    let rec temp a b =
      if b = 0 then a
      else
        temp b (a mod b)
    in
      temp (max a b) (min a b) in
  (* check_nwd - sprawdza, czy każdy ostateczny wynik szklanki *)
  (*             da się otrzymać                               *)
  let check_nwd =
    let nwd = Array.fold_left ( fun a (x, y) -> nwd a x ) 0 glass in
    match nwd with
    | 0 -> true
    | _ -> Array.fold_left
            (fun a (x, y) -> (a && (y mod nwd = 0))) true glass in
  (* check_beg - sprawdza, czy końcowy stan jest w ogóle osiagalny *)
  let check_beg =
    Array.fold_left
      (
        fun a (x, y) -> (a || x = y || y = 0)
        )
      (s_size = 0)
      glass in
  if (check_beg && check_nwd) then
  (
  (* check state - sprawdza czy stan jest szukanym stanem *)
  let check state =
    fst (
          Array.fold_left
            (
              fun (a, b) x ->
                if snd glass.(b) = x then
                  (a, b + 1)
                else (false, b + 1)
              )
            (true, 0)
            state
          )
  in
  (* addable - dodaje stan do tablicy hashy, tylko, gdy jeszcze nie *)
  (*           został on dodany                                     *)
  let addable hash_table state dist =
    if not (Hashtbl.mem hash_table state) then
      (
        Hashtbl.add hash_table state dist;
        true
        )
    else false
  in
  (* refill - dolewa wodę do k-tej szklanki, zwraca końcowy stan    *)
  let refill k state =
    let s_copy = Array.copy state in
    (
      s_copy.(k) <- fst glass.(k);
      s_copy;
      ) in
  (* spill - wylewa wodę z k-tej szklanki, zwraca końcowy stan      *)
  let spill k state =
    let s_copy = Array.copy state in
      (
        s_copy.(k) <- 0;
        s_copy;
        ) in
  (* diffuse - zwraca listę stanów otrzymanych przez przelanie wody *)
  (*           z k-tej szklanki stanu state do kazdej innej         *)
  let diffuse k state =
    let s_list = ref [] in
      (
        let s_copy = Array.copy state in
        for i = 0 to (Array.length glass - 1) do
          if (i <> k) then
          (
            let s_copy_1 = Array.copy s_copy in
            if (fst glass.(i) - s_copy.(i) < s_copy.(k)) then
              (
                s_copy_1.(k) <- s_copy.(k) - fst glass.(i) + s_copy.(i);
                s_copy_1.(i) <- fst glass.(i);
                s_list := s_copy_1::(!s_list);
                )
            else
              (
                s_copy_1.(i) <- s_copy.(i) + s_copy.(k);
                s_copy_1.(k) <- 0;
                s_list := s_copy_1::(!s_list);
                )
            )
          done;
        !s_list;
        ) in
  (* Właściwa część programu - tworzy stan początkowy, tablicę hashy*)
  (* oraz wykonuje algorytm BFS na możliwych do osiągnięcia stanach.*)
  (* Dzieki tablicy hashy, kazdy stan odwiedzimy dokładnie raz      *)
    let state = Array.make (s_size) 0 in
    let hash_table = Hashtbl.create 1000117 in
    let q_states = Queue.create () in
      (
        Hashtbl.add hash_table state 0;
        Queue.push state q_states;
        while (not (Queue.is_empty q_states) && !answer = -1) do
          let s_temp = Queue.pop q_states in
          let dist = Hashtbl.find hash_table s_temp + 1 in
          if (check s_temp) then (answer := dist - 1;) else
            (
              for i = 0 to (s_size - 1) do
                let s_temp_1 = refill i s_temp in
                let s_temp_2 = spill i s_temp in
                let s_list = diffuse i s_temp in
                (
                  if (addable hash_table s_temp_1 dist) then
                    Queue.push s_temp_1 q_states;
                  if (addable hash_table s_temp_2 dist) then
                    Queue.push s_temp_2 q_states;
                  List.iter
                    (fun x ->
                      if (addable hash_table x dist) then
                        (Queue.push x q_states))
                     s_list;
                  )
              done;
              );
        done;
        !answer;
        );
  )
  else !answer;;  (* Jeżeli stan końcowy nie osiągalny *)
