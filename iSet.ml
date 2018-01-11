(*                  Modyfikacja drzew                         *)
(*              Autor: Witalis Domitrz (wd393711)             *)
(*           Reviewer: Aleksandra Księżny (ak394328)          *)

(* Na bazie pSet oznacza, że warunki początkowe są takie jak w pSet *)

(* Robię nowe ++ i -- w celu uniknięcia problemów z max_int i min_int
Zakładam b >= 0 w (++) *)
let (++) a b =
  let out = a + b in
  if out < a then max_int else out

let (--) a b =
  if b < 0 then (a ++ 1) ++ ((-1)*(b ++ 1)) else
  let out = a - b in
  if out > a then min_int else out

(* typ przedziału (początek, koniec) - przedział domknięty
Zakładam, że początek <= koniec*)
type interval =
  int * int

(* Sprawdzenie, czy liczba x jest w przedziale *)
let czy_w x (a, b) =
  a <= x && b >= x

(* Rozmiar przedziału *)
let interval_size (a, b) =
  b -- a ++ 1

(* Używane do testowania:
(* Przedział zawierający tylko x *)
let inter x = (x, x) *)

(* Sprawdzenie, czy przedziały powinne zostać zamienione na jeden *)
let two_to_one (a, b) (c, d) =
  not ((b ++ 1 < c) || (d < a -- 1))

(* Tak, jak w pSet i jeszcze dodatkowy ostatni element będący liczbą
elemetów w danym drzewie *)
type t =
  | Empty
  | Node of t * interval * t * int * int

(* Na bazie pSet *)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(* Liczba liczb w zbiorze (Analogicznie do height) *)
let size = function
  | Node (_, _, _, _, s) -> s
  | Empty -> 0

(* Na bazie pSet *)
let make l k r = Node (l, k, r, max (height l) (height r) ++ 1,
  (size l) ++ (size r) ++ (interval_size k))

(* Na bazie pSet *)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr ++ 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height lr <= height ll then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hl ++ 2 < hr then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rl <= height rr then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r

(* Na bazie pSet *)
let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(* Na bazie pSet *)
let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "ISet.remove_min_elt"

(* Analogicznie do min_elt -- zwraca największy element w zbiorze *)
let rec max_elt = function
  | Node (_, k, Empty, _, _) -> k
  | Node (_, k, r, _, _) -> max_elt r
  | Empty -> raise Not_found

(* Analogicznie do remove_min_elt -- usuwa największy element w zbiorze *)
let rec remove_max_elt = function
  | Node (l, _, Empty, _, _) -> l
  | Node (l, k, r, _, _) -> bal l k (remove_max_elt r)
  | Empty -> invalid_arg "ISet.remove_max_elt"

(* Kod na bazie pSet *)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
    let k = min_elt t2 in
    bal t1 k (remove_min_elt t2)

(* Konstruktor zbioru pustego *)
let empty = Empty

(* Sprawdzenie, czy zbiór jest pusty *)
let is_empty t =
  t = Empty

(* dodanie (a, b) do t, gdy t po dodaniu (a, b) nie połączy się z innym
przedziałem (zakłada, że t jest już zbalansowane) na bazie pSet,
ale ze zmianami *)
let rec add_one (a, b) t =
  match t with
  | Empty -> make empty (a, b) empty
  | Node (l, p, r, _, _) ->
    if (fst p) > b ++ 1 then bal (add_one (a, b) l) p r
    else if (snd p) < a -- 1 then bal l p (add_one (a, b) r)
    else invalid_arg "ISet.add_one"

(* Łączy l v r, żaden przedział nie połączy się z innym przedziałem
(zakłada, że l i r są już zbalansowane) na bazie pSet *)
let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add_one v r
  | (_, Empty) -> add_one v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh ++ 2 then bal ll lv (join lr v r) else
      if rh > lh ++ 2 then bal (join l v rl) rv rr else
      make l v r

(* Używam nazw zmiennych pb i pe.
Są one utworzone od przedział_begin i przedział_end. *)

(* Działa tak, jak opisane w iSet.mli, na bazie pSet, nieznacznie zmienione *)
let split a t =
  let rec loop = function
    | Empty -> (Empty, false, Empty)
    | Node (l, p, r, _, _) -> let (pb, pe) = p in
      (* jeśli jakiś przedział zawiera a *)
      if czy_w a (pb, pe) then
        let l, r = ((if a > pb then add_one (pb, a -- 1) l else l),
          (if a < pe then add_one (a ++ 1, pe) r else r)) in (l, true, r)
        else if a < pb then
          let (ll, pres, rl) = loop l in (ll, pres, join rl (pb, pe) r)
        else
          let (lr, pres, rr) = loop r in (join l (pb, pe) lr, pres, rr)
  in loop t

(* Dodaje przedział (a, b) do zbioru t, warunki wejścia jak w iSet.mli.
Na bazie pSet, ale bardzo zmodyfikowane.
Dzieli t na 3 części:
Jedna zawiera tylko przedziały liczb mniejszych od a -- 1,
druga, to zbiór przedziałów, które mogę połączyć (a, b),
a trzecia zawiera tylko przedziały liczb większych od b ++ 1
Używam nazw zmiennych ln, lm, rn, rm. Są one od:
left_now, left_max, right_now, right_min *)
let add (a, b) t =
  (* wycinam z t dwie części *)
  let (tl, _, _), (_, _, tr) = (split a t), (split b t) in
    if (tl, tr) = (empty, empty) then make empty (a, b) empty
    else
    (* tu wyliczam cztery (dwie pary par), które są parą par
    Wartości w tych parach to:
    lewe drzewo obcięte do przedziałów, którcych nie mogę połączyć z (a, b),
    liczba - początek przedziału (a, b) po połączeniu go z przedziałami z
    lewego, drzewa (z którymi mogłem go połączyć),
    liczba - koniec przedziału (a, b) po połączeniu go z przedziałami z
    prawego, drzewa (z którymi mogłem go połączyć),
    prawe drzewo obcięte do przedziałów, którcych nie mogę połączyć z (a, b) *)
      let (ln, lm), (rn, rm) =
        (
        (if is_empty tl then (empty, a)
          else let p = max_elt tl in
        if (snd p) ++ 1 >= a then (remove_max_elt tl, (fst p))
        else (tl, a))
        ,
        (if is_empty tr then (empty, b)
          else let p = min_elt tr in
        if (fst p) -- 1 <= b then (remove_min_elt tr, (snd p))
        else (tr, b))
        )
  in join ln (lm, rm) rn

(* Używam nazw zmiennych pb i pe.
Są one utworzone od przedział_begin i przedział_end. *)

(* Bierze przedział i drzewo w całości go zawierające
(bierze przedzial i drzewo, ktore zawiera go w calosci) i
wyrzuca z niego ten przedział *)
let rec remove2 (a, b) t = match t with
  | Node (l, (pb, pe), r, _, _) ->
    if czy_w b (pb, pe) then let l1, r1 =
      ((if pb < a then add (pb, a -- 1) l else l),
      (if pe > b then add (b ++ 1, pe) r else r))
      in merge l1 r1
    else if (pe < a) then bal l (pb, pe) (remove2 (a, b) r)
    else bal (remove2 (a, b) l) (pb, pe) r
  | Empty -> invalid_arg "ISet.remove2"


(* dodaje przedział, a następnie go odejmuje *)
let remove x t =
  let t1 = add x t in remove2 x t1

(* Jak w iSet.ml, na bazie pSet *)
let mem x t =
  let rec loop = function
    | Empty -> false
    | Node (l, (a, b), r, _, _) ->
      if czy_w x (a, b) then true
      else if x < a then loop l
      else loop r
  in loop t

(* Jak w iSet.ml, na bazie pSet *)
let iter f t =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop t

(* Jak w iSet.ml, na bazie pSet *)
let fold f t acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc t

(* Jak w iSet.ml, na bazie pSet *)
let elements t =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] t

(* Używam nazw zmiennych pb i pe.
Są one utworzone od przedział_begin i przedział_end. *)

(* Jak w iSet.mli. Znajdude w drzewie BST największy przedział zawierający
co najmniej jeden element nie mniejszy od x *)
let below x t =
  let rec loop acc t =
    match t with
    | Empty -> acc
    | Node (l, (pb, pe), r, _, _) ->
      if czy_w x (pb, pe) then acc ++ size l ++ interval_size (pb, x)
      else if x < pb then loop acc l
      else loop (acc ++ size l ++ interval_size (pb, pe)) r
  in loop 0 t
