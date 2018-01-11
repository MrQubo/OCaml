(************************************************)
(* Zadanie o arytmetyce niedokładnych wartości. *)
(*     Autor: Witalis Domitrz (wd393711)        *)
(*    Reviewer: Michał Kardaś (mk394332)        *)
(************************************************)

(* UWAGA!!! Do nazw zmiennych uzyłem skrótów: ibofx to interval_begin_of_x, a ibofy to interval_begin_of_y *)

(* przedział reprezentuję uporządkowaną parę floatów (a, b). *)
(* Jeśli a<=b to jest to przedział [a, b],  *)
(* w przeciwnym razie jest to suma przedziałów [a, inf) u (-inf, b] *)
type wartosc =
  { interval_begin : float; interval_end : float }

(* is_nan x sprawdza, czy x to nan *)
let is_nan x = ((compare x nan) = 0)

(* are_nans a b sprawdza, czy gdzieś w przedziałach a, b jest nan *)
let are_nans a b =
  if is_nan a.interval_begin || is_nan a.interval_end || is_nan b.interval_begin || is_nan b.interval_end
    then true
  else false

(* zamieniam -0. na 0. w a *)
let minus0to0 a =
  let ib = (if a.interval_begin = (~-.0.) then (0.) else a.interval_begin) in
  let ie = (if a.interval_end = (~-.0.) then (0.) else a.interval_end) in
  { interval_begin = ib; interval_end = ie }

(* porzedzenia procedura *)
let twominus0to0 a b =
  (minus0to0 a, minus0to0 b)

(* nan_interval to u mnie ekwiwalent zbioru pustego *)
let nan_interval = { interval_begin = nan; interval_end = nan }

(* Implicite zakładamy, że wszystkie argumenty typu float są liczbami *)
(* rzeczywistymi, tzn. są różne od infinity, neg_infinity i nan.      *)

(* wartosc_od_do x y = [x;y]         *)
(* war.pocz.: x <= y                 *)
let wartosc_od_do x y =
  { interval_begin = x; interval_end = y }

(* wartosc_dokladnosc x p = x +/- p% *)
(* war.pocz.: p > 0                  *)
let wartosc_dokladnosc x p =
  let a = (x -. (x *. p /. 100.)) and b = (x +. (x *. p /. 100.)) in
  wartosc_od_do (min a b) (max a b)

(* wartosc_dokladna x = [x;x]        *)
let wartosc_dokladna x =
  { interval_begin = x; interval_end = x }

(* in_wartosc w x = x \in w *)
let in_wartosc w x =
  if is_nan w.interval_begin || is_nan w.interval_end then false
  else match w with
  | { interval_begin = ib; interval_end = ie }
    when ib <= ie -> ((ib <= x) && (x <= ie))
  | { interval_begin = ib; interval_end = ie } ->
    not ((ie < x) && (x < ib))

(* min_wartosc w = najmniejsza możliwa wartość w,   *)
(* lub neg_infinity jeśli brak dolnego ograniczenia.*)
let min_wartosc w =
  if is_nan w.interval_begin || is_nan w.interval_end then nan
  else match w with
  | { interval_begin = ib; interval_end = ie } 
    when is_nan ib || is_nan ie -> nan
  | { interval_begin = ib; interval_end = ie }
    when ie < ib -> neg_infinity
  | { interval_begin = ib; _ } -> ib
 

(* max_wartosc w = największa możliwa wartość w,    *)
(* lub infinity jeśli brak górnego ograniczenia.    *)    
let max_wartosc w =
  if is_nan w.interval_begin || is_nan w.interval_end then nan
  else match w with
  | { interval_begin = ib; interval_end = ie }
    when is_nan ib || is_nan ie -> nan
  | { interval_begin = ib; interval_end = ie }
    when ie < ib -> infinity
  | { interval_end = ie; _ } -> ie

(* środek przedziału od min_wartosc do max_wartosc, *)
(* lub nan jeśli min i max_wartosc nie są określone.*)
let sr_wartosc w =
  if is_nan w.interval_begin || is_nan w.interval_end then nan
  else match w with
  | { interval_begin = ib; interval_end = ie }
    when (ib = infinity || ib = neg_infinity) && (ie = infinity || ie = neg_infinity) -> nan
  | { interval_begin = _; interval_end = _ } -> (((min_wartosc w) +. (max_wartosc w) ) /. 2.)

(* sorted_intervals_list usuwa z listy przedziały puste, *)
(* a potem sortuje listę przedziałów najpierw rosnąco po pierwszej współrzędnej, *)
(* a jeśli są one takie same, to rosnąco po drugiej współrzędnej *)
let sorted_intervals_list l =
  (* jest comparatoerm do funkcji List.sort *)
  let interval_compare a b =
    if a.interval_begin < b.interval_begin then -1
    else if a.interval_begin = b.interval_begin then (
      if a.interval_end < b.interval_end then -1
      else if a.interval_end = b.interval_end then 0
      else 1
    )
    else 1 in
  (* usuwa przedziały puste z listy *)
  let rec remove_nans l acc = 
    match l with
    | [] -> acc
    | h::t when is_nan h.interval_begin || is_nan h.interval_end -> remove_nans t acc
    | h::t -> remove_nans t (h::acc)
  in List.sort interval_compare (remove_nans l [])

(* sorted a b = zwraca przedziały w kolejności posortowanej:         *)
(* Empty, Full, begin > end, begin = neg_infinity, end = infinity, _ *)
(* val sorted: wartosc -> wartosc -> wartosc * wartosc               *)
let sorted a b =
  match a, b with  
  | { interval_begin = ibofa; interval_end = ieofa }, _ 
    when is_nan ibofa -> a, b
  | _,  { interval_begin = ibofb; interval_end = ieofb }
    when is_nan ibofb -> b, a
  | { interval_begin = ibofa; interval_end = ieofa }, _ 
    when ibofa > ieofa -> a, b
  | _,  { interval_begin = ibofb; interval_end = ieofb }
    when ibofb > ieofb -> b, a
  | { interval_begin = ibofa; interval_end = ieofa }, _ 
    when ibofa = neg_infinity -> a, b
  | _, { interval_begin = ibofb; interval_end = ieofb }
    when ibofb = neg_infinity -> b, a
  | { interval_begin = ibofa; interval_end = ieofa }, _
    when ieofa = infinity -> a, b
  | _, { interval_begin = ibofb; interval_end = ieofb }
    when ieofb = infinity -> b, a
  | { interval_begin = ibofb; interval_end = ieofb }, _ -> 
    a, b

(* przeciwnosc w = liczby przeciwne do liczb z w *)
(* przeciwnosc w = { -x : in_wartosc w x }       *)
(* val przeciwnosc: wartosc -> wartosc           *)
let inverse w =
  minus0to0 { interval_begin = ((-1.) *. w.interval_end ); interval_end = ((-1.) *. w.interval_begin) }

(* interval_to_list a dzieli a na listę maksymalnie 4 przedziałów  *)
(* Przedziały na tej liście to podprzedziały z (0, inf),  (-inf,0) *)
(* Są tam maksymalnie po dwa podprzedziały z danych przedziałów    *)
let interval_to_list a =
  let rec helper a acc =
    let { interval_begin = ib; interval_end = ie } = a in
    if ib < 0. then
      if (ib <= ie) && (ie <= 0.)
      then ({ interval_begin = ib; interval_end = ie } :: acc)
      else helper { interval_begin = 0.; interval_end = ie }
        ({ interval_begin = ib; interval_end = 0. } :: acc)
    else
      if (ib <= ie)
      then ( { interval_begin = ib; interval_end = ie } :: acc)
      else helper { interval_begin = neg_infinity; interval_end = ie} 
        ({ interval_begin = ib; interval_end = infinity } :: acc)
  in helper a []

(* list_to_interval dostaje listę przedziałów, *)
(*które składają się w 1 przedział i składa je w 1. przedział *)
let list_to_interval l =
  (* dzięki użyciu sorted_intervals_list nie będę miał przedziałów z nan-ami *)
  let l = sorted_intervals_list l in
  (* helper przyjmuje posortowaną listę przedziałów do połączenia, *)
  (* parę przedziałów w których będzie akumulowany w wynik i *)
  (* numer przedziału do którego aktualnie zapisujemy *)
  (* helper zwraza dwa przedziały (a, inf) (-inf, b), albo dwa przedziały (a, b), (nan, nan) *)
  let rec helper l (a, b) numer =
    match l with
    | [] -> (a, b)
    | h::t -> (
      if numer = -1 then helper t (h, b) 0
      else if numer = 1 then helper t (a, { interval_begin = b.interval_begin;
        interval_end = (max b.interval_end h.interval_end) }) 1
      else if a.interval_end < h.interval_begin
        then helper t (a, h) 1
      else helper t ({ interval_begin = a.interval_begin;
        interval_end = (max a.interval_end h.interval_end) }, b) 0
      ) in
  (* finalnie łączy przedziały zwrócone przez procedurę helper *)
  let final_connect (a, b) =
    if is_nan b.interval_begin || is_nan b.interval_end then a
    else if a.interval_end = infinity 
      then { interval_begin = a.interval_begin; interval_end = b.interval_end }
    else { interval_begin = b.interval_begin; interval_end = a.interval_end }
  in final_connect (helper l (nan_interval, nan_interval) (-1))

(* reverse w = liczby odwrotne do liczb z w   *)
(* reverse w = { 1/x : in_wartosc w x }       *)
(* val reverse: wartosc -> wartosc            *)
let reverse a =
  let a = minus0to0 a in
  if a.interval_begin = neg_infinity && a.interval_end = infinity then a else
  if a.interval_end = 0. && a.interval_begin <> 0. 
    then minus0to0 { interval_begin = neg_infinity; interval_end = (1. /. a.interval_begin) }
  else minus0to0 { interval_begin = (1. /. a.interval_end); interval_end = (1. /. a.interval_begin) }

(* mnoży dwa przedziały *)
let multiply a b =
  let a, b = twominus0to0 a b in
  (* float_multipler mnoży mi dwa floaty tak, *)
  (* żeby było mnożenie zachowujące się w spsób określony i *)
  (* niezwracający nan dla 0 i +- inf *)
  let float_multipler f1 f2 =
    if f1 = 0. || f2 = 0. then 0.
    else f1 *. f2 in
  let p0 = float_multipler a.interval_begin  b.interval_begin in
  let p1 = float_multipler a.interval_begin  b.interval_end in
  let p2 = float_multipler a.interval_end b.interval_begin in
  let p3 = float_multipler a.interval_end  b.interval_end in
    { interval_begin = (min (min p0 p1) (min p2 p3)); 
    interval_end = (max (max p0 p1) (max p2 p3)) }

(* Operacje arytmetyczne na niedokładnych wartościach. *)

(* Rozważam wszystkie możliwe przypadki, których liczbę redukuje używając sorted *)
let plus a b =
  let a, b = twominus0to0 a b in
  if are_nans a b then nan_interval 
  else let a, b = sorted a b in
  match a with
  | { interval_begin = ibofa; interval_end = ieofa } when ibofa > ieofa -> (
    match b with
    | { interval_begin = ibofb; interval_end = ieofb } 
      when (ibofb > ieofb) || (ibofb = neg_infinity) || (ieofb = infinity)  ->
      { interval_begin = neg_infinity; interval_end = infinity }
    | { interval_begin = ibofb; interval_end = ieofb } ->
      if ibofa +. ibofb <= ieofa +. ieofb
        then { interval_begin = neg_infinity; interval_end = infinity }
      else { interval_begin = (ibofa +. ibofb); interval_end = (ieofa +. ieofb) }
    )
  | { interval_begin = ibofa; interval_end = ieofa } when ibofa = neg_infinity -> (
    match b with
    | { interval_begin = ibofb; interval_end = ieofb } when ieofb = infinity ->
      { interval_begin = neg_infinity; interval_end = infinity }
    | { interval_begin = ibofb; interval_end = ieofb } ->
      { interval_begin = neg_infinity; interval_end = (ieofa +. ieofb) }
    )
  | { interval_begin = ibofa; interval_end = ieofa } when ieofa = infinity -> (
    match b with
    | { interval_begin = ibofb; interval_end = ieofb } ->
      { interval_begin = (ibofa +. ibofb); interval_end = infinity }
    )
  | { interval_begin = ibofa; interval_end = ieofa } -> (
    match b with
    | { interval_begin = ibofb; interval_end = ieofb } ->
      { interval_begin = (ibofa +. ibofb); interval_end = (ieofa +. ieofb) }
    )

(* Odejmowanie jest odwrotnością dodawania *)
let minus a b =
  let a, b = twominus0to0 a b in
  if are_nans a b then nan_interval
  else plus a (inverse b)

(* Rozbijam przedział a i b na listy przedziałów,                     *)
(* następnie tworzę listę pomnożonych przez siebie przedziałów dla    *)
(* każdych dwóch możliwych przedziałów z obu list,                    *)
(* a na koniec sklejam otrzymaną listę przedziałów w jeden przedział. *)
let razy a b =
  let a, b = twominus0to0 a b in
  if are_nans a b then nan_interval
  else let a, b = (interval_to_list a, interval_to_list b) in
  (* tą funkcją przechodzę po liście przedziałów powstałch z a *)
  let rec for_interval_in_a a acc_of_a =
    match a with
    | [] -> acc_of_a
    | h_of_a::t_of_a ->
      (* tą funkcją przechodzę po liście przedziałów powstałch z b.  *)
      (* Wynikiem tej funkcji jest lista iloczynów przedziału h_of_a *)
      (* i każdego z przedziałów w b *)
      let rec for_interval_in_b b acc_of_b = (
        match b with
        | [] -> acc_of_b
        | h_of_b::t_of_b ->
          for_interval_in_b t_of_b ((multiply h_of_a h_of_b)::acc_of_b)
        )
      in for_interval_in_a t_of_a ((for_interval_in_b b []) @ acc_of_a)
  in list_to_interval (for_interval_in_a a [])

(* Dzielenie jest odwrotnością mnożenia *)
let podzielic a b =
  let a, b = twominus0to0 a b in
  if are_nans a b then nan_interval
  else if b.interval_begin = 0. && b.interval_end = 0.
    then nan_interval
  else razy a (reverse b)
