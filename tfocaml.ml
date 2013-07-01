(*tipos básicos *)
type tcell = { mutable shot: bool; ship: int };; (*Possuí informação sobre uma célula do tabuleiro: se já foi atirada e o tamanho do barco que tem essa célula. Note que “shot” é mutable pois seu valor pode ser alterado após a definição da variável*)
type tboard = { 
	matrix : tcell array array;  (*Matriz com informações sobre o tabuleiro*)
	mutable acts : (int*int) list; (*Lista com as coordenadas já atiradas*)
	mutable alives : int; (*Número de barcos vivos*)
};;

(* Coloca unidade de barco com tamanho v na posição (i, j) da matriz m, retorna se célula tem ou não barco. *)
let fullfill (m, i, j, v) = m.(i).(j) <- {shot=false; ship=v}; if v>0 then 1 else 0;;

(*Verifica se pode colocar barco de tamanho size, orientação h, na posição (i, j) de m. Note que essa função usa recursão.*)
let rec may_put (i, j, h, size, m) =
	if (size>0) then
		((i+(h+1) mod 2<Array.length(m)) && (j+h mod 2<Array.length(m)) && (m.(i).(j).ship == 0) && 
			may_put(i+(h+1) mod 2, j+h mod 2, h, size-1, m))
	else true;;

(*Coloca barco de tamanho size, orientação h, na posição (i, j) de m, retorna tamanho do barco. Usa recursão.*)
let rec put_ship (i, j, h, size, v, m) =
	if ((size!=v) || may_put(i, j, h, size, m)) && (size>0) then
		fullfill(m, i, j, v)+put_ship(i+(h+1) mod 2, j+h mod 2, h, size-1, v, m)
	else 0;;

(*Gera um barco de tamanho size e coloca numa posição (i, j) e orientação aleatórias da matriz.*)
let rec gen_ship(m, size) = 
	while (put_ship(Random.int(Array.length(m)), Random.int(Array.length(m)), Random.int(2), size, size, m)!=size) do () done; size;;

(* Preenche a matriz do tabuleiro a partir de uma lista de tamanho de barcos *)
let ini (bn, ships_sizes) = 
	Array.iteri (fun i line ->
		Array.iteri (fun j vcell ->
					bn.alives<-bn.alives+fullfill(bn.matrix, i, j, 0)) (*inicializa matriz com zero barcos*)
				line)
		bn.matrix;
	List.iter (function s -> bn.alives<-bn.alives+gen_ship(bn.matrix, s)) ships_sizes;; (*coloca barcos*)

exception InvalidUndo;;
(* Desfaz últma ação contida no histórico *)
let undo (bn) = 
	match bn.acts with
		(i, j)::tl -> 
			bn.acts <- tl; (*retira cabeça da lista de atirados*)
			bn.matrix.(i).(j).shot <- false; (*marca item como não atirado *)
			bn.alives<-bn.alives+ (*incrementa número de barcos vivos caso haja barco naquela posição*)
				if bn.matrix.(i).(j).ship>0 then 1 else 0; 
			(i, j)
		| [] 	-> raise (InvalidUndo) ;;

exception OutOfMatrix of int*int;;
exception AlreadyShot of int*int;;
exception Undone of int*int;;

(* função para atirar *)
let shoot (bn, i, j) = 
	if ((i == -1)&&(j == -1)) then
		match undo(bn) with (i, j) -> raise (Undone(i, j))
	else if (i>=Array.length(bn.matrix))||(j>=Array.length(bn.matrix)||(i<0)||(j<0)) then
		raise (OutOfMatrix(i, j)) (*tentou atirar posição inválida*)
	else if bn.matrix.(i).(j).shot then 
		raise (AlreadyShot(i, j)) (* retorna exceção se já foi atirado *)
	else
		bn.acts <- (i, j)::bn.acts; (*coloca na lista dos atirados*)
		bn.matrix.(i).(j).shot <- true; (*marca barco como atirado*)
		bn.alives<-bn.alives- (*decrementa número de barcos vivos*)
			if bn.matrix.(i).(j).ship>0 then 1 else 0; (*se acertou *)
		bn.matrix.(i).(j).ship>0;;

(*Define estrutura para o jogo*)
let bn : tboard = {
		matrix = Array.make_matrix 20 20 {shot=false; ship=0};
		acts = [];
		alives = 0;
	};;

(*Processa o jogo, interagindo com o usuário*)
let doGame () = begin
	ini (bn, [2;2;2;2;3;3;3;5;1;1;1;1;1]);
	while bn.alives>0 do begin	
		Printf.printf "Restam %4d celulas com barcos.\n" bn.alives;
		Printf.printf "    ";
		Array.iteri (fun j v -> Printf.printf "%2d" (j+1)) bn.matrix;
		Printf.printf "\n";
		for i = 0 to Array.length(bn.matrix)-1 do
			(Printf.printf "%2d | " (i+1));
			Array.iteri (fun j vcell ->
				match (vcell.shot, vcell.ship) with
						  (false, n)	-> Printf.printf "W "
						| (true, 0) 	-> print_string "  "
						| (true, n)	-> Printf.printf "%1d " n;
			) bn.matrix.(i);
			print_string "|\n";
		done;
		try print_string (if (shoot(bn, 
			(print_string "--Digite a posição y: ";read_int()-1), 
			(print_string "--Digite a posição x: ";read_int()-1))) then
				"Acertou!\n" else "Errou!\n")
		with 
			OutOfMatrix(i, j) -> Printf.printf "Posição (%2d, %2d) inválida.\n" (i+1) (j+1)
		| 	AlreadyShot(i, j) -> Printf.printf "Posição (%2d, %2d) já foi atirada.\n" (i+1) (j+1)
		|	InvalidUndo	  -> Printf.printf "Não é possível desfazer neste momento.\n"
		|	Undone(i, j)      -> Printf.printf "Celula (%2d, %2d) desfeita.\n" (i+1) (j+1);
		| 	_				-> ();
	end;
	done;
	print_string "Parabéns você ganhou!!\n";
end;;

if (Array.length(Sys.argv)>1) then
	Random.init(int_of_string(Sys.argv.(1)))
else
	(Printf.printf "Digite a semente do jogo: ";
	Random.init(read_int()));
doGame();;
