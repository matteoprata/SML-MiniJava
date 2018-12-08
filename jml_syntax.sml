
(*Sintassi Java per la definizone delle espressioni consentite*)
datatype expr = 
	VAR of string
	| INTEXPR of int
	| BOOLEANEXPR of bool 
	| THIS | SUPER | NULL 
	| NEW of string          				(* creazione di un nuovo oggetto new nomeOggetto() *)
	| FIELD of expr * string 				(* accesso ai campi: istanza.campo *)
	| CALL of expr * string * expr list     (* chiamata ai metodi: istanza.metodo([argomento_1 ... argomento_n]) *)
	| PLUS of expr * expr 
	| TIMES of expr * expr
	| DIV of expr * expr
	| SUB of expr * expr
	| MOD of expr * expr

(*true, false, 1 >=< 0 andalso 1 >=< 0*)
datatype booleanExpr = 
	BOOL of bool  			
	| BOOLEANEXPR of expr * string * expr                (* >, <, == *)
	| BOOLEANOPS of booleanExpr * string * booleanExpr   (* AND, OR *)
	| NOT of booleanExpr; 

(* Rappresenta le istruzioni valide di un metodo *)
datatype validInstruction = 
	ASSIGNVAR of string * expr            								 (* x = y | x = new nomeOggetto() *)
	| ASSIGNFIELD of expr * string * expr        						 (* x.f = y | x.f = new nomeOggetto() *)
	| IF of booleanExpr * validInstruction list * validInstruction list  (* if then else *)
	| WHILE of booleanExpr * validInstruction list
	| RETURN of expr;

(* Sintassi Java base necessaria per la creazione dei costrutti del linguaggio *)
datatype JML_Type     = INT | CLASS of string;
	type varDecl      = JML_Type * string;
	type fieldDecl    = JML_Type * string * expr;
	type methDecl     = JML_Type * string * varDecl list * varDecl list * validInstruction list;
	type class        = string * string * fieldDecl list * methDecl list;
	type JML_program  = class list;
