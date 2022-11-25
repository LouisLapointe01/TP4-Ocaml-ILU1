
let rec hanoi (source, temp, dest) n = if n <= 0
  then [] else hanoi (source, dest, temp) (n-1) @  (source, dest) ::hanoi (temp, source, dest) (n-1);;

let rec map f l = match l with
  | [] -> [] 
  | x :: l -> f(x) :: map f l;; 

let rec inserer x l = match l with
  | [] -> [x]
  | t :: q -> if (x <= t) then x::t::q  else t :: (inserer x q);;

let rec triInsertion l = match l with
  |[] -> []
  | t :: q -> inserer t (triInsertion q) ;; 

let rec partage list = match list with 
  | [] -> [], []
  | x::list' -> let l1, l2 = partage list' in 
      if List.length l1 <= List.length l2 then 
        x::l1, l2 
      else 
        l1, x::l2


let rec merge list1 list2 = match list1 with
  |[] -> list2
  | h::t -> match list2 with 
    |[] -> list1
    |h2::t2 -> if h<=h2 then h::merge t list2 else h2::merge list1 t2

let rec triFusion list = match list with
  | [] -> []
  | [x] -> [x]
  | _ -> let (list1, list2) = partage list in merge (triFusion list1) (triFusion list2) ;;

let fst (a, b) = a;;

let snd (a, b) = b;; 

let estFonction l = 
  let rec liste l = match l with 
    |[] -> []
    |t::q -> (fst t)::(liste q) 
  in let rec diffUnitaire e l = match l with
      |[] -> true
      |t::q -> (e <> t)&& (diffUnitaire e q)
  in let rec diffSuivant l = match l with
      |[] -> true
      |t::q -> (diffUnitaire t q) && (diffSuivant q)
  in diffSuivant (liste l);;

let rec image e l = match l with
  |[] -> failwith("Aucune image")
  |t :: q -> if fst(t) = e then snd(t) else image e q;;

let rec imageEns l f = match l with
  |[] -> []
  |t::q -> (image t f)::(imageEns q f);; 

  

let estInjective l = 
  let rec liste l = match l with 
    |[] -> []
    |t::q -> (snd t)::(liste q) 
  in let rec diffUnitaire e l = match l with
      |[] -> true
      |t::q -> (e <> t)&& (diffUnitaire e q)
  in let rec diffSuivant l = match l with
      |[] -> true
      |t::q -> (diffUnitaire t q) && (diffSuivant q)
  in diffSuivant (liste l);;


let surcharge f1 f2 = 
  let rec parcours_f2 l1 l2 yx = match l2 with
    |[] -> yx
    |t::q -> if (fst t = fst yx) then t else parcours_f2 l1 q yx
  in let rec parcours_f1 l1 l2 = match l1 with
      |[] -> []
      |t::q -> (parcours_f2 l1 l2 t)::(parcours_f1 q l2) 
  in let rec parcours_final final yx = match final with
      |[] -> [yx] 
      |t::q -> if (t = yx) then [] else parcours_final q yx 
  in let rec parcours_f2_rest l2 final = match l2 with
      |[] -> []
      |t::q -> (parcours_f2_rest q final)@(parcours_final final t)
  in (parcours_f1 f1 f2)@(parcours_f2_rest f2 (parcours_f1 f1 f2)) ;;


let rec isDef x f = match f with
  |[] -> false
  |t::q -> (fst t = x)||(isDef x q);;

let rec composition f1 f2 = match f2 with
  |[] -> []
  |t::q -> if (isDef (snd t) f1) 
      then (fst t, image (snd t) f1)::(composition f1 q) 
      else composition f1 q;;

let rec produit f1 f2 =
  let rec sousProduit (x,y) f2 = match f2 with
    | [] -> []
    | (x2,y2) :: f2' -> ((x,x2),(y,y2)) :: sousProduit (x,y) f2' in 
  match f1 with 
  | [] -> []
  | (x1,y1) :: f1' -> 
      surcharge (sousProduit (x1,y1) f2) (produit f1' f2) ;;