(************************************************)
(*                 Zadanie Origami              *)
(*     Autor: Witalis Domitrz (wd393711)        *)
(*    Reviewer:                                 *)
(************************************************)


type point = float * float

type kartka = point -> int

(* zamienia dwa wektory (punkty) p1 p2 na ich sumę *)
let suma_wektorow (p1: point) (p2: point) =
  ((fst p2) +. (fst p1), (snd p2) +. (snd p1))

(* zamienia dwa wektory (punkty) p1 p2 na ich różnicę *)
let roznica_wektorow (p1: point) (p2: point) =
  ((fst p2) -. (fst p1), (snd p2) -. (snd p1))

(* zwraca kwadrat modułu wektora *)
let kwadrat_modulu (p1: point) =
  ((fst p1) *. (fst p1) +. (snd p1) *. (snd p1))

(* daj mi punkt symetryczny do x względem prostej p1 p2 *)
let symetryczny (p1: point) (p2: point) ((x, y): point) =
  (* Jeśli jest to prosta pionowa, to robię symetrię względem prostej x = c *)
  if (fst p1) = (fst p2) then
    let c = fst p1 in (2. *. c -. x, y)
  else
  (* Jeśli nie, to przesuwam punkty o wektor -p2, żeby przecieła (0, 0) *)
    let x1, y1 = roznica_wektorow p2 p1 in
    let x, y = roznica_wektorow p2 (x, y) in
  (* I korzystam z wzoru na sumetrie względem prostej y = ax *)
    let a = y1 /. x1 in
    let w1 = ((1. -. (a *. a)) /. (1. +. a *. a)) in
    let w2 = ((2. *. a) /. (1. +. (a *. a))) in
    let x, y = ((w1 *. x) +. (w2 *. y), (w2 *. x) -. (w1 *. y)) in
    (* Znów przesuwam, ale teraz o wektor p2 *)
    suma_wektorow p2 (x, y)

(* Wyrażenie to zwraca mi liczby różnych znaków zależnie od położenia punktu
x względem zadanej dwroma punktami prostej (jeśli jest na prostej to jest 0) *)
let strona_prostej (p1: point) (p2: point) x =
  let x1, y1 = roznica_wektorow p1 p2 in
  let x2, y2 = roznica_wektorow p1 x in
  (x1 *. y2) -. (x2 *. y1)

let prostokat (p1: point) (p2: point) = function (p3: point) ->
  if (fst p3) < (fst p1) then 0
  else if (snd p3) < (snd p1) then 0
  else if (fst p3) > (fst p2) then 0
  else if (snd p3) > (snd p2) then 0
  else 1

let kolko (sr: point) (r: float) = function (p: point) ->
  if ((kwadrat_modulu (roznica_wektorow p sr)) > (r *. r)) then 0
  else 1

(* Składa kartkę k względem prostej przez punkty p1 p2 *)
let zloz (p1: point) (p2: point) (k: kartka) = function (x: point) ->
  let gdzie_lezy = (strona_prostej p1 p2 x) in
  if gdzie_lezy = 0. then k x
  else if gdzie_lezy < 0. then 0
  else (k x) + (k (symetryczny p1 p2 x))

let skladaj l k =
  List.fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) k l
