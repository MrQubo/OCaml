(*        Sortowanie Topologicznie          *)
(*     Autor: Witalis Domitrz (wd393711)    *)
(*  Reviever: Tomasz Grześkewicz (tg??????) *)

(* Zakładam istnienie modułu pMap (z treści zadania) *)

(* Typ do oznaczania, czy już opuściliśmy wierzchołek *)
type state = Left | Not_left

exception Cykliczne

let topol l =
	(* Zamiana grafu zapisanego w formacie wejściowym na mapę wierzchłków z
		listami sąsiedztwa *)
	let rec list_of_lists_to_pmap_of_lists l = 
		match l with
		| [] -> PMap.empty
		| (header,list_of_elements)::t -> (
			let old_map = list_of_lists_to_pmap_of_lists t in 
			if PMap.mem header old_map then 
				(* Rozważenie przypadku, gdy jakiś nagłówek występuje na liście
					wejściowej wielokrotnie - nie jest to wykluczone przez 
					specyfikację wejścia *)
				PMap.add header (list_of_elements @ (PMap.find header old_map))
					old_map
			else
				PMap.add header list_of_elements old_map)
	in let graph = list_of_lists_to_pmap_of_lists l
	(* Sprawdzenie przypadku, gdy jakiś wierzchołek (v) jest w grafie (g), a nie
		ma go na liście danej w zadaniu, bo nie ma żadnego syna - 
		jeśli go nie ma, to uważamy, że jest, ale ma pustą listę - 
		nie jest to wykluczone przez specyfikację wejścia *)
	in let sons v g =
		if PMap.mem v g 
			then PMap.find v g
			else []
	(* Zwracana lista *)
	in let out = ref []
	(* Tablica odwiedzonych: 
		brak wartośco - nie odwiedzony,
		Not_left - zaczęte odwiedzanie, ale wierzchołek jeszcze nie opuszczony,
		Left - odwiedzanie rozpoczęte i zakończone *)
	in let visited = ref PMap.empty
	(* Przeszukiwanie grafu wgłąb	*)
	in let rec dfs v = 
		(* Sprawdzenie, czy wierzchołek był już odwiedzony - 
			jeśli tak, to nie wchodzimy tam ponownie *)
		if PMap.mem v !visited then (
			(* Jeśli odwiedzanie się rozpoczęło, a nie zakończyło, 
				to istnieje cykl! *)
			if PMap.find v !visited = Not_left then raise Cykliczne;
		)
		else begin
			(* Zaznaczenie rozpoczęcia odwiedzania *)
			ignore (visited := PMap.add v Not_left !visited);
			(* Przejście po wszystkich synach *)
			List.iter dfs (sons v graph);
			(* Dodanie wierzchołka do wyniku *)
			ignore (out := v::!out);
			(* Zaznaczenie zakończenia odwiedzania *)
			ignore (visited := PMap.add v Left !visited);
		end;
	in begin
		(* Rozpoczęcie przeszukiwania grafu wgłąb z każdego wierzchołka *)
		PMap.iter (fun v _ -> dfs v) graph;
		!out
	end
