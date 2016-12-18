(* 
Arytmetyka przybliżonych wartości - Michał Siwiński
Code review - Bartosz Burny
*)

type wartosc = int * (float * float);; (* 1 - przedział spójny, 2 - przedział "z luką", 0 - nan *)

(*
	dla int = 0 nie obchodzą nas float
	dla int = 1 te dwa float to końce przedziału
	dla int = 2 te dwa float to końce tego "wyjętego" przedziału (otwartego)
	(bo jedynym przedziałem "z luką" będzie (-inf, x] u [y, inf) )
*)

(* FUNKCJE POMOCNICZE *)

let valueType x = fst x ;;

let valueBeg x = fst (snd x) ;;

let valueEnd x = snd (snd x) ;;

let isNan x = 

	if x = x then false else true ;;
	
let betterMin x y = (* zwraca nan tylko dla x = nan, y = nan *)

	if isNan y then x else
	if x <= y then x else y ;;

let betterMax x y =

	if isNan y then x else
	if x >= y then x else y ;;

let minOfFour a b c d = 

	if isNan ( betterMin c d ) then betterMin a b else
	if ( betterMin a b <= betterMin c d ) then betterMin a b else betterMin c d ;;

let maxOfFour a b c d = 

	if isNan ( betterMax c d ) then betterMax a b else
	if ( betterMax a b >= betterMax c d ) then betterMax a b else betterMax c d ;;

let hasZero x = 

	if valueType x = 1 then
		if valueBeg x <= 0. && valueEnd x >= 0. then true else false
	else if valueBeg x < 0. && valueEnd x > 0. then false else true

let reverse x =

	match valueType x with
	
	1 ->
	
	if hasZero x then 
		if valueEnd x = 0. then (1, (neg_infinity, 1. /. valueBeg x) )
		else 
			if valueBeg x = 0. then (1, (1. /. valueEnd x, infinity) )
			else (2, (1. /. valueBeg x , 1. /. valueEnd x ) )
	else
		(1, ( min ( 1. /. valueBeg x ) ( 1. /. valueEnd x ) ,
		      max ( 1. /. valueBeg x ) ( 1. /. valueEnd x ) ) ) |
		
	2 -> 
	
	if hasZero x then
		if hasZero (1, (neg_infinity, valueBeg x) )
		then (2, ( 1. /. valueEnd x , 1. /. valueBeg x ) )
		else (2, ( 1. /. valueBeg x , 1. /. valueEnd x ) )
	else
		(1, ( min ( 1. /. valueBeg x ) ( 1. /. valueEnd x ) ,
		      max ( 1. /. valueBeg x ) ( 1. /. valueEnd x ) ) ) ;;

(* FUNKCJE GŁÓWNE *)

(* Konstruktory *)

let wartosc_od_do x y = (1, (x, y) );;

let wartosc_dokladnosc x p = (1, (x -. (x *. p /. 100.) , x +. (x *. p /. 100.) ) );;

let wartosc_dokladna x = (1, (x, x) );;

(* Selektory *)

let in_wartosc x y =
	
	match valueType x with
	
	0 -> false |
	
	1 -> if ( y >= valueBeg x ) && ( y <= valueEnd x )
				then true else false |
	
	_ -> if ( y <= valueBeg x ) || ( y >= valueEnd x )
				then true else false ;;

let min_wartosc x =

	match valueType x with
	
	0 -> nan |
	
	1 -> valueBeg x |
	
	_ -> neg_infinity ;;
	
let max_wartosc x =
	
	match valueType x with
	
	0 -> nan |
	
	1 -> valueEnd x |
	
	_ -> infinity ;;
	
let sr_wartosc x =
	
	match fst x with
	
	1 -> 
	
	if ( valueBeg x = neg_infinity && valueEnd x = infinity ) then nan
	else
		if valueBeg x = neg_infinity then neg_infinity 
		else
			if valueEnd x = infinity then infinity 
			else ( ( valueBeg x ) +. ( valueEnd x ) ) /. 2. |
	
	_ -> nan ;;

(* Modyfikatory *)

let rec plus a b = 

	if valueType a = 0 || valueType b = 0 then (0, (1., 1.) )
	else
		
		let beg1 = valueBeg a
		and end1 = valueEnd a
		
		and beg2 = valueBeg b
		and end2 = valueEnd b
		
		in 
		
			match (fst a, fst b) with
		
			(1, 1) -> (1, ( (beg1 +. beg2) , (end1 +. end2) ) ) | 
			
			(1, 2) -> 
			
			if (beg2 +. end1) >= (end2 +. beg1) then 
			(1, (neg_infinity, infinity) ) 
			else (2, ( (beg2 +. end1) , (end2 +. beg1) ) ) |
			
			(2, 1) -> plus b a |
			
			(2, 2) -> (1, (neg_infinity, infinity) ) |
			
			(_, _) -> raise (Failure "random type - plus") ;;
		

let minus a b =

	if valueType a = 0 || valueType b = 0 then (0, (1., 1.) )
	else
		let beg2 = valueBeg b and end2 = valueEnd b
		in plus a ( valueType b, ( (-1.) *. end2, (-1.) *. beg2) ) ;;
		

let rec razy a b = 

	if valueType a = 0 || valueType b = 0 then (0, (1., 1.) )
	else if (valueBeg a = 0. && valueEnd a = 0.) ||
	        (valueBeg b = 0. && valueEnd b = 0.) then (1, (0., 0.) )
	else
	
		let beg1 = valueBeg a
		and end1 = valueEnd a
		
		and beg2 = valueBeg b
		and end2 = valueEnd b
		
		in
		
			match (valueType a, valueType b) with
			
			(1, 1) -> 
			
			(1, ( minOfFour ( beg1 *. beg2 ) ( beg1 *. end2 ) 
			                ( end1 *. beg2 ) ( end1 *. end2 ) , 
			      maxOfFour ( beg1 *. beg2 ) ( beg1 *. end2 ) 
			                ( end1 *. beg2 ) ( end1 *. end2 ) ) ) |
			
			(1, 2) -> 
			
			let posBeg = valueEnd ( razy a (1, (neg_infinity, beg2) ) )
			and posEnd = valueBeg ( razy a (1, (end2, infinity) ) )
			in
				if ( posBeg >= posEnd ) then (1, (neg_infinity, infinity) )
				else (2, (posBeg, posEnd) ) |
			
			(* generalnie przedział z luką rozbijam na dwa przedziały
			zwykłe i wykonuje dwa razy mnożenie przedziałów, ale
			już dla przedziałów spójnych, co mam określone *)
			
			(2, 1) -> razy b a |
			
			(2, 2) -> 
			
			let posBeg = max ( valueEnd ( razy (1, (neg_infinity, beg1) ) b ) )
			                   ( valueEnd ( razy (1, (end1, infinity) ) b ) )
			          
			and posEnd = min ( valueBeg ( razy (1, (neg_infinity, beg1) ) b ) )
			                 ( valueBeg ( razy (1, (end1, infinity) ) b ) )
			          
			in
				
				if ( posBeg >= posEnd ) then (1, (neg_infinity, infinity) )
				else (2, (posBeg, posEnd) ) |
			                
			(_, _) -> raise (Failure "random type - razy") ;;

let podzielic a b = 

	if valueType a = 0 || valueType b = 0 then (0, (1., 1.) )
	else if valueBeg b = 0. && valueEnd b = 0. then (0, (1., 1.) )
	else if hasZero a && hasZero b then (0, (1., 1.) )
	else razy a (reverse b) ;;