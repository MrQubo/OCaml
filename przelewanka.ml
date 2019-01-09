(************************************************)
(*                 Zadanie Origami              *)
(*     Autor: Witalis Domitrz (wd393711)        *)
(*    Reviewer: Zuzanna Opała (zo395012)        *)
(************************************************)

(* Funkcja używana do debugowania
let wypisz stan = begin
	Array.iter (fun x -> Printf.printf "%d ; " x;) (stan);
	Printf.printf "\n";
end
*)

(* Naprawienie problemu z kolizjami w Hashtbl *)
let hash2 tab =
	(Array.fold_left (fun acc x -> acc*2333 + x mod 1000000009) 0 tab, tab)

(* Wyjątek podnoszony  *)
exception Solution of int

let przelewanka opis_szklanek = 
	(* Liczba szklanek *)
	let n = Array.length opis_szklanek
	(* Sprawdzenie, czy oczekiwane ilości wody to same 0.
	Zawiera też przypadek gdy mamy dokładnie 0 szklanek *)
	in let czy_same_zera opis_szklanek = 
		Array.fold_left (fun acc (x, _) -> acc && x = 0) true opis_szklanek
	(* Sprawdza, czy spełnione są warunki konieczne do 
	uzyskania stanu końcowego *)
	in let warunki_konieczne opis_szklanek =
		(* Sprawdza, czy jakaś szklanka finalnie będzie pełna, lub pusta *)
		let czy_puste_lub_pelne opis_szklanek = 
			Array.fold_left 
				(fun acc (x, y) -> acc || ((y = 0 || y = x) && x <> 0 ) ) 
				false opis_szklanek
		(* Sprawdzenie, czy ilość wody w każdej szklance na końcy nie będzie 
		względnie pierwsza z nwd wszystkich pojemności szklanek,
		bo tylko takie liczby da się uzyskać *)
		in let czy_nwd opis_szklanek =
			let rec nwd a b = 
				if b = 0 then a else nwd b (a mod b)
			in let nwd_pojemnosci = 
				Array.fold_left (fun acc (x, _) -> nwd acc x) 0 opis_szklanek
			in nwd_pojemnosci = 1 ||
				Array.fold_left 
					(fun acc (_, y) -> acc && nwd nwd_pojemnosci y <> 1)
					true  opis_szklanek
		in czy_puste_lub_pelne opis_szklanek && czy_nwd opis_szklanek
	in if czy_same_zera opis_szklanek then 0 
	else if not (warunki_konieczne opis_szklanek) then -1
	else let bfs v = 
		(* Sprawdzenie, czy dany stan jest stanem końcowym *)
		let czy_koncowy v =
			let czy = ref true in begin
				for i = 0 to (n-1) do
					if v.(i) <> snd opis_szklanek.(i) 
						then czy := false;
				done;
				!czy
			end
		(* Kolejka stanów do kolejności odwiedzania *)
		in let q = Queue.create ()
		(* Mapa z odwiedzonych wierzchołków na odległości od wierzchołka 
		 początkowego *)
		in let odl = Hashtbl.create 2000003
		(* Sprawdzenie, czy jest to wierzchołek końcowy, a jeśli nie, to
		 odowiedzanie wierzchołka jeśli jest nieodwiedzony *)
		in let odwiedz (u : (int array)) (odleglosc : int) =  begin
			if not (Hashtbl.mem odl (hash2 u)) then begin
				if czy_koncowy u then raise (Solution odleglosc);
				Queue.push u q;
				Hashtbl.add odl (hash2 u) odleglosc;
			end;
		end;
		in let przejdz_sasiadow u odleglosc = begin
			(* Operacja pierwsza *)
			for i = 0 to (n-1) do
				(* Wartość w i *)
				let w_i = u.(i) in
				if w_i <> fst opis_szklanek.(i) then begin
					let nowy_u = Array.init n 
						(fun k -> if k = i 
							then fst opis_szklanek.(k) 
							else u.(k))
					in odwiedz nowy_u (odleglosc + 1);
				end;
			done;
			(* Operaca druga *)
			for i = 0 to (n-1) do
				(* Wartość w i *)
				let w_i = u.(i) in
				if w_i <> 0 then begin
					let nowy_u = Array.init n 
						(fun k -> if k = i 
							then 0 
							else u.(k))
					in odwiedz nowy_u (odleglosc + 1);
				end;
			done;
			(* Operacja trzecia
			Przelewanie z i-tego kubka do j-tego *)
			for i = 0 to (n-1) do
				(* Wartość w i-tym kubku *)
				let w_i = u.(i) in
				for j = 0 to (n-1) do 
					(* Wartość w j-tym kubku *)
					let w_j = u.(j) in
					if i <> j && w_i <> 0 && w_j <> fst opis_szklanek.(j) 
					then begin
						(* maksymalna ilość wody w j-tej szklance *)
						let maksymalne_j = fst opis_szklanek.(j) in
						(* ilości wody w i-tej i j-tej szklance po
						 wykonaniu operacji *)
						let nowe_w_i, nowe_w_j =
							if maksymalne_j <= w_i + w_j 
								then (w_i + w_j - maksymalne_j, maksymalne_j)
								else (0, w_i + w_j)
						in let nowy_u = Array.init n 
						(fun k -> 
							if k = i then nowe_w_i
							else if k = j then nowe_w_j
							else u.(k)
						)
						in odwiedz nowy_u (odleglosc + 1);
					end;
				done;
			done;
		end
		in begin
			odwiedz (Array.copy v) 0;
			while not (Queue.is_empty q) do
				let u = Queue.take q
				in let odleglosc =  (Hashtbl.find odl (hash2 u))
				in przejdz_sasiadow u odleglosc;
			done
		end
	in let stan_poczatkowy = Array.make n 0;
	in try (bfs stan_poczatkowy; -1) with
		Solution x -> x