(* Author: Mateusz Dudziński (394171)
   Reviewer: Jan Kolibabski
   Zadanie 6: Przelewanka WPF Sem. 2017Z *)

let przelewanka input_arr = 
  let rec gcd a b = 
    if b = 0 then a else gcd b (a mod b)
  and capacity = Array.map fst input_arr 
  and hash_tab = Hashtbl.create (1000 * 1000);
  and queue = Queue.create ()
  and len = Array.length input_arr
  in
  (* Jesli stan w talibcy arr nie zostal jeszcze odnaleziony, dodaj go 
     do hash-mapy. Dodawana jest KOPIA tablicy arr (nowa tablica). *)
  let try_to_add arr dist = 
    if not (Hashtbl.mem hash_tab arr) then begin
      let copy = Array.copy arr in
      Hashtbl.add hash_tab copy (dist + 1);
      Queue.push copy queue
    end
  in
  (* Funckje zlewania, dolewania i przelewania opisuja wyszstkie mozliwe
     ruchy z przekazanego im stanu. Funkcje te modyfikuja tablice, 
     sprawdza czy nowy stan byl juz osiagniety, ew. go dodaja (kopiujac 
     tablice), a na koniec przywracaja tablice do poczatkowego stanu. *)
  let pour_out arr k dist = 
    let tmp = arr.(k) in  
    arr.(k) <- 0;
    try_to_add arr dist;
    arr.(k) <- tmp
  and refill arr k dist = 
    let tmp = arr.(k) in  
    arr.(k) <- capacity.(k);
    try_to_add arr dist;
    arr.(k) <- tmp
  and transfer arr k j dist =
      let tmp_k, tmp_j = arr.(k), arr.(j) 
      and free_space = capacity.(j) - arr.(j) in
      arr.(j) <- arr.(j) + min free_space arr.(k);
      arr.(k) <- max (arr.(k) - free_space) 0;
      try_to_add arr dist;
      arr.(j) <- tmp_j;
      arr.(k) <- tmp_k
  in
  (* Przetworzenie pojedynczego stanu osiagalnego w dist ruchow. *)
  let process_state curr_state dist = 
    let next_state = Array.copy curr_state in
    for i = 0 to len - 1 do  
      (* Jesli szklanka niepelna to mozna dolac wode. *)
      if next_state.(i) < capacity.(i) then 
        refill next_state i dist;
      (* Jesli niepusta to mozemy zlac wode. *)
      if next_state.(i) > 0 then
        pour_out next_state i dist;
      for j = 0 to len - 1 do
        (* Jesli i-ta szklanka niepusta i j-ta niepelna 
           to przelewamy z i do j. *)
        if i <> j 
        && next_state.(i) > 0 && next_state.(j) < capacity.(j) then
          transfer next_state i j dist
      done
    done
  and common_div = Array.fold_left gcd 0 capacity
  in
  (* Jesli nie ma szklanek lub wszyskie maja pojemosc 0 nie ma co robic. *)
  if len = 0 || common_div = 0 then 0
  (* kazda wielkosc tablicy docelowej dzieli sie przez nwd pojemnosci szklanek 
     oraz music byc jedna szkanka ktora jest pusta/pelna bo musimy skonczyc *)
  else if not (Array.for_all (fun (x, _) -> x mod common_div = 0) input_arr)
  || not (Array.exists (fun (x, y) -> y = 0 || x = y) input_arr) then
    -1
  else begin
    (* Stany poczatkowy i koncowy: *)
    let inicial_state = Array.make len 0 
    and goal_state = Array.map snd input_arr in
    (* Inicializacja struktur danych *)
    Queue.push inicial_state queue;
    Hashtbl.add hash_tab inicial_state 0; 
    (* Wykonujemy BFS. Z wlasnosci tego przeszukiwania wiemy,ze gdy znajdziemy
       poszukiwany wynik bedzie osiagalny optymalnym kosztem. *)
    while not (Queue.is_empty queue) && Queue.top queue <> goal_state do
      let curr_state = Queue.pop queue in
      process_state curr_state (Hashtbl.find hash_tab curr_state)
    done;
    try Hashtbl.find hash_tab goal_state with
    | Not_found -> -1
  end;;
