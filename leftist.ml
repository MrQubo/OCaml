(**              Drzewa lewicowe            *)
(**    Autor: Witalis Domitrz (wd393711)    *)
(**   Reviewer: Iwona Kotlarska (ik394380)  *)

type 'a queue  = Null | Node of 'a queue * 'a * 'a queue * int
(** Typ złączalnej kolejki priorytetowej *)
(** Drzewo opisuję jako (dl, a, dr, h) (możliwe, że z indeksami) *)
(** dl - lewe poddrzewo, a - wartość korzenia *)
(** dr - prawe poddrzewo h - wysokość drzewa mierzona *)
(** w liczbie wierzchołków w skrajnej prawej ścierzce*)

let empty = Null
(** Pusta kolejka priorytetowa *)

let single a =
  Node (empty, a, empty, 1)
(** Tworzy jednoelementową kolejkę priorytetową *)

let is_empty d =
  (d = empty)
(** Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *)

exception Empty
(** Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)

let height d =
  match d with
  | Null -> 0
  | Node (_, _, _, h) -> h
(** Zwraca mi wysokość drzewa*)

let rec join d1 d2 =
  match d1, d2 with
  | Null, _ -> d2
  | _, Null -> d1
  (** Łączenie czegoś z pustą listą jest identycznością *)
  | Node (_, a1, _, _), Node (_, a2, _, _) when a1 > a2 -> join d2 d1
  (** Robię zamianę, aby pierwsze drzewo miało mniejszą wartość w korzeniu *)
  | Node (dl1, a1, dr1, _), _ -> let d3 = join dr1 d2 in
    if height dl1 > height d3
    then Node (dl1, a1, d3, (height d3) + 1)
    else Node (d3, a1, dl1, (height dl1) + 1)
    (** Łączę tak, jak jest to opisane w treści zadania *)
(** [join d1 d2] zwraca złączenie kolejek [d1] i [d2] *)

let add a q =
  join q (single a)
(** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e]
    do kolejki [q] *)

let delete_min q =
  match q with
  | Null -> raise Empty
  | Node (d1, a, d2, _) -> (a, (join d1 d2))
(** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
