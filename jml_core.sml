

(* Aggiunge la classe alla lista se non c'è già *)
fun insertClassNode (ClassTreeMap, nil) = ClassTreeMap 
	| insertClassNode(ClassTreeMap: (string, string list)map, (className, extendedClass, _, _)::program) = 
		if className = "" then raise INVALIDCLASSNAME
		else 
			if contains(ClassTreeMap, className) then raise CLASSALREADYEXISTS 
			else 
				(*appendClassTree(ClassTree, className);*)
				insertClassNode(insert(ClassTreeMap, className, []), program)
			    (* ricerca se esiste già nella lista dei nomi di classe *)	

(* Genera l'albero dell'eredità -> creiamo delle dipendenze tra le classi*)
(* ClassTreeMap : associazione "nomeclasse" - [lista sottoclassi] *)
fun inheritanceTree (ClassTreeMap, nil) = ClassTreeMap
	| inheritanceTree(ClassTreeMap, (className, extendedClass, _, _)::program) =
		if extendedClass = "" (*eredita Object*)
		then
			(*inseriamo il nome della classe nella lista degli erediti di Object*)
			inheritanceTree(
				modify(ClassTreeMap, "Object", className::get(ClassTreeMap, "Object")), program)
		else
			(*inseriamo il nome della classe nella lista degli erediti di Object*)			
			inheritanceTree(
				modify(ClassTreeMap, extendedClass, className::get(ClassTreeMap, extendedClass)), program)
		

(*Valuta il programma ricorsivamente secondo la sintassi JML*)

fun jmlProgramEval (ClassTreeMap, nil) = raise UNCHECKED_MAIN   (*qui ci va sia che il corpo del metodo main sia [], sia che il metodo main non esista*)
	| jmlProgramEval (ClassTreeMap, (_,_,_,methodList)::program) =	(*estrae classe per cercare nella lista di metodi il metodo main*)
	let
		(*Funzione di ricerca del main tra i metodi della singola classi analizzata.
		Ritorna la lista delle istruzioni non dichiarative all'interno del main()*)
		fun findMethodMain nil = (nil, nil)
			| findMethodMain ((_,methodName,_,declarInstructions, validInstructionSet)::methodList) =
				if (methodName = "main") then (declarInstructions,  validInstructionSet) 
				else findMethodMain(methodList)	(*se main trovato, ritorna validInstructions*)
		
		(*vengono preservate le istruzioni del main per essere eseguite successivamente*)
		val result = findMethodMain(methodList)
		val mainValidInstructions = #2(result)
		val declarations = #1(result)  
	in
		(*Ricerca ricorsivamente un altro main valido nelle altre classi del programma*)
		if (mainValidInstructions = nil) then jmlProgramEval(ClassTreeMap, program) 
		else   
			let		
				(*E' necessario dichiarare un ambiente e store inizialmente vuoti*)	

				(* Ambiente: nome variabile - locazione*)
				val E: (string, int) map = empty;

				(* Store: locazione - [nome variabile - valore] *)
				val S: (int, (string, int) map) map = empty;

				(* Contesto dei tipi: nome variabile - tipo variabile *)
				val T: (string, string) map = empty;
			    
			    (*Coppia E - T ritornata dalla valutazione delle dichiarazioni nel main stesso*)
				val dataStructures = evalDeclarations(declarations, E, S, T, ClassTreeMap)	
				
				(* viene valutato il metodo secondo l'ambiente E e lo store S(Heap)*)
				val result = (evalMethodEnd(#1(dataStructures), mainValidInstructions, #2(dataStructures), #3(dataStructures), ClassTreeMap) )
				val resultE = (#2(result))
				val resultS = (#3(result))
				val resultT = (#4(result))
				val finalResult = (#1(result))
					handle TYPE_MISMATCH => let val _ = print("Errore: l'assegnamento non tipa\n") in ~1 end
			in
				print("Computazione terminata:\n");
				print("L'ambiente vale:\t" ^ printE(resultE) ^ "\n");
				print("Lo Store vale:\t\t" ^ printS(resultS) ^ "\n");
				print("T vale:\t\t\t\t" ^ printMap(resultT) ^ "\n");
				print("\nE il risultato della computazione è:\n");
				finalResult
			end
	end

(* Funzione che realizza l'interprete con : preCompilatore di classi e valutazione del programma *)
fun jmlInterpreter program = 
	let
	    (* Il ClassTree contiene inizialmente solo l'oggetto Object *)
		val ClassTreeMap: (string, string list) map = insertClassNode(insert(empty, "Object", []), program)
				handle INVALIDCLASSNAME  => let val _ = print("SyntaxError: Nome di classe non valido") in empty end				
					| CLASSALREADYEXIST => let val _ = print("Errore: La classe esiste già") in empty end;
	    val ClassTreeMap = inheritanceTree(ClassTreeMap, program)
	in	
		(*Fase di interpreting vera del programma*)
		jmlProgramEval(ClassTreeMap, program) 
			handle UNCHECKED_MAIN => let val _ = print("SyntaxError: Nessun main trovato\n") in ~1 end
	end
