(* ENVIRONMENTS *)

(*Eccezioni*)
exception NO_REF
exception UNCHECKED_VARIABILE
exception VARIABLE_ALREADY_EXISTS
exception UNCHECKED_MAIN
exception NOMORECLASSES
exception INVALIDCLASSNAME
exception CLASSALREADYEXISTS
exception UNDEFINED_VALUE_HASH
exception TYPE_MISMATCH
exception UNBOUND_REFERENCE
exception NOT_VALID_OPERATOR
exception UNCHECKEDFIELD
exception INVALIDEXPR
exception NOMETHOD
exception NULLPOINTEREXCEPTION

(* rappresenta la locazione attualmente in uso *)
val locInd = ref 0;

(*Funzioni di comodo per l'estrazione di elementi dal coppie o terne di elementi dove
l'operatore # non risulta sufficiente*)
fun coupleFirst(a,_) = a;
fun coupleSecond(_,b) = b;

fun tripleFirst(a,_,_) = a;
fun tripleSecond(_,b,_) = b;
fun tripleThird(_,_,c) = c;

(* Torna il nome della classe sotoforma di stringa *)
fun trueStringType (INT) = "int" 
	| trueStringType (CLASS(className)) = className

(*Ritorna true se il tipo di classe a destra è sottotipo di quello di sinistra secondo la logica di assegnamento*)
fun isSubClassOf((ClassTreeMap), leftType, rightType) =
		let
			val className = #1(getKVCouple(ClassTreeMap));
			val classList = #2(getKVCouple(ClassTreeMap));
			val nextMap = getNextMap(ClassTreeMap)
		in
			if (className = leftType) then
				let 
					fun classListSearch(nil, rightType) = false
					  | classListSearch(head::classList, rightType) = if (head = rightType) then true 
					else classListSearch(classList, rightType)
					
					(* Esito della ricerca nella lista delle sottoclassi figlie *)
					val searchInSubClassList = classListSearch(classList, rightType)
				in
					if searchInSubClassList then true 
					else 
						let
							fun depthSearch (nil) = false
								| depthSearch (newLeftType::classList) = (
									if isEmpty(nextMap) = false then (
										isSubClassOf (nextMap, newLeftType, rightType);
										depthSearch(classList)
										)
								else false
								)		
						in
							depthSearch(classList)
						end
				end
		    else 
		     	if isEmpty(nextMap) = true then false else isSubClassOf(nextMap, leftType, rightType)
		end

(*Determina il tipo del campo della classe in ingresso a partire dalla classe dell'oggetto chiamante*)
fun ftype(nil, field, className) = raise UNCHECKEDFIELD
	| ftype ((className, extendedClass, fieldList, _)::program: JML_program, field, classObj) =
		if className = classObj then 
			let
				fun compareSingleField (nil, field) = ""
					| compareSingleField (classField::fieldList, field) = 
			 			if tripleSecond(classField) = field then trueStringType(tripleFirst(classField)) 
			 			else compareSingleField(fieldList, field)
			 	val fieldType = compareSingleField(fieldList, field)
			in
				(*se il campo non esiste nella classe in analisi, lo si ricerca nella classe estesa*)
				if fieldType = "" then ftype(program, field, extendedClass)
				else 
					fieldType
			end
		else 
			ftype(program, field, classObj)

(*Valuta la singola espressione - la coppia di ritorno rappresenta: (valoreEspressione/hashcode, tipo)*)
and evalExpr (E, INTEXPR(intexpr), S, T, _) = (intexpr, "int")
	| evalExpr (E, VAR(varName), S, T, _) = 
		let 
			(* Cerca nel contesto dei tipi il tipo di varName *)
			val varType = get(T, varName)
		    (* Cerca nell'ambiente l'hashcode di varName *)
			val hashCode = get(E, varName)		
		in
			if varType = "int" then
			    (get(get(S, hashCode), "int"), varType)		
			else
				(*nel caso di oggetti torna l'hashCode al posto del valore*)
				(hashCode, varType)	
		end

	(*torna 0 cosi da sapere all'esterno che si tratta di una NEW*)
	| evalExpr (_, NEW(className), _, _, ClassTreeMap) = 
		if contains(ClassTreeMap, className) then (0, className) 
		else raise INVALIDCLASSNAME

	(*torna un valore intero qualsiasi e la stringa null che servirà fuori*)
	| evalExpr (_, NULL, _, _, _) = (~2, "null") 
	| evalExpr (_, SUPER, _, _, _) = (~3, "super")
	| evalExpr (_, THIS, _, _, _) = (~4, "this")
	| evalExpr (E, FIELD(expr, fieldName), S, T, ClassTreeMap) =
		let
		 	val resultCouple = evalExpr(E, expr, S, T, ClassTreeMap)
		 	val hash = coupleFirst(resultCouple)
		 	val objFieldMap = get(S, hash)
 		in
			(*Ritorna il valore del campo dell'oggetto ricercato*)
		 	(get(objFieldMap, fieldName), ftype(globalProgram, fieldName, #2(resultCouple))) 
		 	(*GESTIRE ECCEZIONI*)
		end
	| evalExpr (E, CALL(objName, methodName, argumentsList), S, T, ClassTreeMap) =
		let
			val ECall : (string, int)map = empty;
			val SCall : (int, (string, int)map)map = empty;
			val TCall : (string, string)map = empty;
			val methodIndex = ref 0;

			val objCouple = evalExpr(E, objName,S, T, ClassTreeMap)	(* coppia hashcode-tipo *)
			val className = #2(objCouple)

			(*Ritorna una lista di coppie valore tipo dalla valutazione delle espressioni passate*)
			fun evalArgument (nil, evaluedArguments:(int*string)list) = evaluedArguments 
				| evalArgument (argument::arguments, evaluedArguments:(int*string)list) = 
					let
						val evaluedArgs = evalExpr(E, argument, S, T, ClassTreeMap)
						val newEvaluedArguments = (evaluedArguments@[evaluedArgs])
					in
						evalArgument(arguments, newEvaluedArguments)
					end

			(* la lista degli argomenti valutati *)
			val evaluedArguments = evalArgument(argumentsList, [])

			fun compareArguments(nil, nil, ECall, SCall, TCall) = (true, ECall, SCall, TCall)  (* se entrambe le liste sono svuotate, vuol dire che tutti gli elementi sono uguali *)
				| compareArguments(argument::argumentsList, parameter::parametersList, ECall, SCall, TCall) = 

					if ((trueStringType(coupleFirst(argument))) = (coupleSecond(parameter))) then (  
						methodIndex := !methodIndex+1;
						let
							val ECall = insert(ECall, coupleSecond(argument), !methodIndex)
							val TCall = insert(TCall, coupleSecond(argument), coupleSecond(parameter))
						in
							if "int" = coupleSecond(parameter) then 
							(*SCall = insert(SCall, !methodIndex, insert(empty, "int", coupleFirst(parameter)))*)
								compareArguments(argumentsList, parametersList, ECall,  
									insert(SCall, !methodIndex, insert(empty, "int", coupleFirst(parameter))), TCall)
							else
							(*passaggio di oggetti - copiamo la mappa dei campi del vecchio obj in quello 
								nuovo dichiarato come argomento del metodo*)
							(*SCall = insert(SCall, !methodIndex, get(S, coupleFirst(parameter)));*)
								compareArguments(argumentsList, parametersList, ECall, 
									insert(SCall, !methodIndex, get(S, coupleFirst(parameter))), TCall)
						end
					)else (false, ECall, SCall, TCall)

			fun searchMethod(nil, _, _, ECall, SCall, TCall) = raise NOMETHOD (* the method "NOMEMETODO()" is undefined for tye type "NOMECLASSE" *)
				| searchMethod((method:(methDecl))::methodsList, methodName, argumentsList:(int*string)list, ECall, SCall, TCall) =
					let
						val comparator = compareArguments((#3(method)), argumentsList, ECall, SCall, TCall)
						val equals = (#1(comparator))
						val ECall = (#2(comparator))
						val SCall = (#3(comparator))
						val TCall = (#4(comparator))
					in
						if ((#2(method) = methodName) 
							andalso ((length argumentsList) = (length (#3(method))))     (* OVERLOAD *)
							andalso equals) then
							(*Ritorna la lista delle valstructions*)
							(

								(* campi e methodEnd *)
								((#1(method)), (#4(method)), (#5(method)), ECall, SCall, TCall)

							)
						else
							(
							searchMethod(methodsList, methodName, argumentsList, ECall, SCall, TCall)
							)
					end
					
			fun executeMethod(nil, instanceType, methodName, ECall, SCall, TCall) = (INT, nil, nil, ECall, SCall, TCall)
				| executeMethod((className, extendedClass, fieldList, methodsList)::globalProgram, instanceType,
				 											methodName, ECall, SCall, TCall) = 
					if className = instanceType then 
						(
						searchMethod(methodsList, methodName, evaluedArguments, ECall, SCall, TCall) (*qui ci vuole il risultato della evalArgument*)
							(*handle NOMETHOD => let val _ = print("Nessun metodo corrispondente\n") in (nil, nil) end*)
						
						(*QUI SI FA LA RICORSIONE PER VEDERE IN ALTRE CLASSI*)
						)
					else 
						(
						executeMethod(globalProgram, instanceType, methodName, ECall, SCall, TCall)
						)
		in
			if (className = "null") then raise NULLPOINTEREXCEPTION else
				let
					val methodContent = executeMethod(globalProgram, className, methodName, ECall, SCall, TCall)
					val returnMethodType = #1(methodContent)
					val ECall = #4(methodContent)
					val SCall = #5(methodContent)
					val TCall = #6(methodContent)
					val E_S_T = evalDeclarations(#2(methodContent), ECall, SCall,TCall, ClassTreeMap)
					val Enew = tripleFirst(E_S_T)
					val Snew = tripleSecond(E_S_T)
					val Tnew = tripleThird(E_S_T)
				in
					(* torna la coppia: valore della valutazione del metodo e il tipo di ritorno del metodo *)
					(#1(evalMethodEnd(Enew, (#3(methodContent)), Snew, Tnew, ClassTreeMap)), trueStringType(returnMethodType))
				end
		end
	| evalExpr(E, PLUS(expr1, expr2), S, T, ClassTreeMap) = 
		let
			val value1 = evalExpr(E, expr1, S, T, ClassTreeMap)
			val value2 = evalExpr(E, expr2, S, T, ClassTreeMap)
		in
			if (#2(value1) = "int" andalso #2(value2) = "int") then ((#1(value1) + #1(value2)), "int") else
			raise INVALIDEXPR 
		end		
	| evalExpr(E, TIMES(expr1, expr2), S, T, ClassTreeMap) = 
		let
			val value1 = evalExpr(E, expr1, S, T, ClassTreeMap)
			val value2 = evalExpr(E, expr2, S, T, ClassTreeMap)
		in
			if (#2(value1) = "int" andalso #2(value2) = "int") then ((#1(value1) * #1(value2)), "int") else
			raise INVALIDEXPR 
		end
	| evalExpr(E, DIV(expr1, expr2), S, T, ClassTreeMap) = 
		let
			val value1 = evalExpr(E, expr1, S, T, ClassTreeMap)
			val value2 = evalExpr(E, expr2, S, T, ClassTreeMap)
		in
			if (#2(value1) = "int" andalso #2(value2) = "int") then ((#1(value1) div #1(value2)), "int") else
			raise INVALIDEXPR 
		end
	| evalExpr(E, SUB(expr1, expr2), S, T, ClassTreeMap) = 
		let
			val value1 = evalExpr(E, expr1, S, T, ClassTreeMap)
			val value2 = evalExpr(E, expr2, S, T, ClassTreeMap)
		in
			if (#2(value1) = "int" andalso #2(value2) = "int") then ((#1(value1) - #1(value2)), "int") else
			raise INVALIDEXPR 
		end
	| evalExpr(E, MOD(expr1, expr2), S, T, ClassTreeMap) = 
		let
			val value1 = evalExpr(E, expr1, S, T, ClassTreeMap)
			val value2 = evalExpr(E, expr2, S, T, ClassTreeMap)
		in
			if (#2(value1) = "int" andalso #2(value2) = "int") then ((#1(value1) mod #1(value2)), "int") else
			raise INVALIDEXPR 
		end

(* Nel momento della creazione del nuovo oggetto la funzione inizializza la mappa dei campi dell'oggetto all'interno dello store*)
and storeFields(searchedClass, hashCode, E, S, T, ClassTreeMap) = 
	let 
		(*confronta ogni campo con quelli già esistenti nella lista finale dei campi dell'oggetto*)
		fun compareSingleField (nil, field) = false
			| compareSingleField (oldField::inheritanceList, field) = 
			 	if oldField = field then true else compareSingleField(inheritanceList, field)

		fun compareFields (inheritanceList, nil) = inheritanceList
			| compareFields (inheritanceList, field::fieldList) = 
				if not (compareSingleField(inheritanceList, field)) then(
					compareFields(inheritanceList @ [field], fieldList)
				)
				else
					compareFields(inheritanceList, fieldList) 

		(* cerca la classe nel program per estrarvi i campi *)
		fun search (nil, _, inheritanceList) = inheritanceList
			| search ((className, extendedClass, fieldList, _)::program, searchedClass, inheritanceList) = 
			(
				if (className = searchedClass) orelse (className = "Object")  then (
					(*Prendiamo la lista dei campi e andiamo a vedere la classe ereditata*)
					search(program, extendedClass, compareFields(inheritanceList, fieldList))
					)
			    else search(program, searchedClass, inheritanceList)
			)

		(* Assegna il singolo campo della lista alla mappa dei campi dell'oggetto*)
		fun assignObjContent (objContent, nil, E, S, T, ClassTreeMap) = objContent
			| assignObjContent (objContent, field::fieldList, E, S, T, ClassTreeMap) =
			     (* #2(field) nome del campo, #3(field) valore del campo *)
			    let
			     	val name = tripleSecond(field)
			     	val value = coupleFirst(evalExpr(E, tripleThird(field), S, T, ClassTreeMap))
			    in		
			     	assignObjContent(insert(objContent, name, value),
			     		fieldList, E, S, T, ClassTreeMap)
			    end		
	in
		let
			val fieldList = search(globalProgram, searchedClass, [])
			val objContentList = assignObjContent(empty, fieldList, E, S, T, ClassTreeMap)
		in	
			insert(S, hashCode, objContentList)
		end
	end

(* Valuta le espressioni booleane e ritorna il booleano che rappresenta il valore della valutazione *)
and evalBooleanExpr (E, BOOL(boolean), S, _, _) = if boolean then true else false
	| evalBooleanExpr (E, BOOLEANEXPR(expr1, operator, expr2), S, T, ClassTreeMap) = 
		let 
			val exp1 = #1(evalExpr(E, expr1, S, T, ClassTreeMap))
			val exp2 = #1(evalExpr(E, expr2, S, T, ClassTreeMap))
		in
			print("STOREVALEXPR: " ^ printS(S) ^ "\n");
			if (operator = "==") then 
				if (exp1 = exp2) 	then true else false(* valuta condizione == *)
			else if (operator = ">") then 
				if (exp1 > exp2) 	then true else false(* valuta condizione > *)
			else if (operator = "<") then 
				if (exp1 < exp2) 	then true else false(* valuta ce*e zionee  *)
			else if (operator = ">=") then 
				if (exp1 >= exp2) then true else false(* valuta condizione >= *)
			else if (operator = "<=") then 
				if (exp1 <= exp2) then true else false(* valuta condizione <= *)	
			else 
				raise NOT_VALID_OPERATOR
		end
	| evalBooleanExpr (E, BOOLEANOPS(booleanExpr1, operator, booleanExpr2), S, T, ClassTreeMap) =
		if (operator = "&&") then 
			(evalBooleanExpr(E, booleanExpr1, S, T, ClassTreeMap) 
				andalso evalBooleanExpr(E, booleanExpr2, S, T, ClassTreeMap))(* valuta operatore && *)
		else if (operator = "||") then 
			(evalBooleanExpr(E, booleanExpr1, S, T, ClassTreeMap) 
				orelse evalBooleanExpr(E, booleanExpr2, S, T, ClassTreeMap)) (* valuta operatore || *)
		else 
			raise NOT_VALID_OPERATOR
	| evalBooleanExpr (E, NOT(booleanExpr), S, T, ClassTreeMap) =
		not (evalBooleanExpr(E, booleanExpr, S, T, ClassTreeMap))

(* Memorizza in T e in E la variabile e la sua nuova locazione  *)
and evalVarDecl (E:(string, int)map, (INT, varName), S, T:(string, string) map, _) = 
		if not (contains(E, varName))then(
			locInd := !locInd +1; 
			(*print ("ESSE: " ^ printS(insert(S, !locInd, insert(empty, "int", 0))) ^ "\n");*)
			(insert(E, varName, !locInd), insert(S, !locInd, insert(empty, "int", 0)), insert(T, varName, "int"))
		)
		else raise VARIABLE_ALREADY_EXISTS
	| evalVarDecl (E:(string, int)map, (CLASS(className), varName), S, T, ClassTreeMap) = 
			if contains(ClassTreeMap, className) then
				if not (contains(E, varName)) then(
					locInd := !locInd + 1;
					(insert(E, varName, !locInd), S, insert(T, varName, className))
				)
				else raise VARIABLE_ALREADY_EXISTS
			else raise INVALIDCLASSNAME

(* Valuta il metodo*)
and evalMethodEnd (E, nil, S, T, _) = (~1, E, S, T)
	| evalMethodEnd (E, ASSIGNVAR(varName, expr)::validInstructionSet, S, T, ClassTreeMap) =
		let 
			val hashCode = get(E, varName) 
			val evalExprCouple = evalExpr(E, expr, S, T, ClassTreeMap)
			    handle INVALIDCLASSNAME => let val _ = 
			    	print("SyntaxError:  Nome di classe non valido") in (~1, "") end
	    in
	    	(* variabile non dichiarata *)
	    	if hashCode = ~1 then raise UNCHECKED_VARIABILE else 
	    	    
	    	    (* se ciò che vogliamo assegnare è di tipo int *)
			    if (get(T, varName) = "int") then 
			    (   
		    		if((#2(evalExprCouple) = "int") andalso not (#2(evalExprCouple) = "null")) then 
		    			evalMethodEnd(E, validInstructionSet, 
		    				modify(S, hashCode, modify(get(S, hashCode), "int", #1(evalExprCouple))), T, ClassTreeMap)
		    				(*modify(S, hashCode, insert(empty, "int", #1(evalExprCouple))), T, ClassTreeMap)*)
		 			else raise TYPE_MISMATCH
			    )
			    else
			    	(*binding di oggetti: x = new Oggetto(); *)
			    	let
			    		val rightType = #2(evalExprCouple)
			    		val leftType = get(T, varName)
			    	in 	
			    		(*Controllo di GARBAGE se viene assegnato NULL ad un reference*)
			    		if rightType = "null" then
			    			let
			    				val E = modify(E, varName, ~2)
			    				val S = remove(S, hashCode, empty)
			    				val T = modify(T, varName, "null")
			    			in
			    				(*rimuovi con garbage*)
			    				evalMethodEnd(E, validInstructionSet, S, T, ClassTreeMap)
			    			end

			    		else 
			    		(* se il tipo desto e quello sinistro coincidono o il tipo a destra è sottotipo del sinistro *)
			    		if (rightType = leftType orelse isSubClassOf(ClassTreeMap, leftType, rightType)) then 
			    			(*Siamo nel caso in cui si crea un nuovo oggetto con la NEW pertanto,
			    				la expr valutata tornerà (0, tipoOggetto) e NON va modificato l'hashcode del 
			    				reference a sinistra*)
			    			if (#1(evalExprCouple) = 0) then 	
			    				evalMethodEnd(E, validInstructionSet, 
			    					storeFields(rightType, get(E, varName), E, S, T, ClassTreeMap), T, ClassTreeMap)			    			
			    			else
			    				(* SIMULAZIONE GARBAGE COLLECTOR su assegnamento di reference*)
			    				let
			    					val E = modify(E, varName, #1(evalExprCouple))
			    					val S = remove(S, hashCode, empty)	
	    					    in
			    					evalMethodEnd(E, validInstructionSet, S, T, ClassTreeMap)
			    				end
			    				(*evalMethodEnd(E, validInstructionSet, S, T, ClassTreeMap)*)
			    		else raise TYPE_MISMATCH
			    	end			    	
		end
	| evalMethodEnd (E, ASSIGNFIELD(expr1, fieldName, expr2)::validInstructionSet, S, T, ClassTreeMap) = 
		let
			val objCouple = evalExpr(E, expr1, S, T, ClassTreeMap)
			val objHash = #1(objCouple)
			val objType = #2(objCouple)
			val fieldType = ftype(globalProgram, fieldName, objType)
			val objCouple2 = evalExpr(E, expr2, S, T, ClassTreeMap)
			val objValue2 = #1(objCouple2)
			val objType2 = #2(objCouple2)
		in
			(*print("VEDIAMOE:" ^ printE(E) ^"\n");
			print("VEDIAMOS: " ^ printS(S) ^"\n");*)
			if (fieldType = objType2) orelse isSubClassOf(ClassTreeMap, fieldType, objType2) then 
					let
						val objFieldList = get(S, objHash)
						(**)
						val newObjFieldList = modify(objFieldList, fieldName, objValue2);
						val newStore = modify(S, objHash, newObjFieldList);
					in
						(*print("OBJFIELD: " ^ printE(objFieldList) ^ "\n");
						print("NEWSTORE"^printS(newStore)^"\n");*)
						evalMethodEnd(E, validInstructionSet, newStore, T, ClassTreeMap)
					end			
		    else raise TYPE_MISMATCH	    
		end
	| evalMethodEnd (E, IF(booleanExpr, thenCommandList, elseCommandList)::validInstructionSet, S, T, ClassTreeMap) =
		let
			val booleanExprValue = evalBooleanExpr(E, booleanExpr, S, T, ClassTreeMap)
				handle NOT_VALID_OPERATOR => let val _ = print("SyntaxError: Operatore non valido\n") in false end
		in
			if booleanExprValue then evalMethodEnd(E, thenCommandList,S, T, ClassTreeMap) 
			else evalMethodEnd(E, elseCommandList, S, T, ClassTreeMap) 
		end
	| evalMethodEnd (E, WHILE(booleanExpr, whileCommandList)::validInstructionSet, S, T, ClassTreeMap) =
		let
			fun execWhile (booleanExprEvaluation, E, S, T) = 
				if not booleanExprEvaluation then (~1, E, S, T)
				else 
					let
						val evaluation = evalMethodEnd(E, whileCommandList, S, T, ClassTreeMap)
						val E = #2(evaluation)
						val S = #3(evaluation)
						val T = #4(evaluation)
						val booleanExp = evalBooleanExpr(E, booleanExpr, S, T, ClassTreeMap)
					in
						print("STORE: " ^ printS(S) ^ "\n");
						execWhile (booleanExp, E, S, T)
							handle NOT_VALID_OPERATOR => let val _ = print("SyntaxError: Operatore non valido\n") in (~1, E, S, T) end	
					end
			val result = execWhile(evalBooleanExpr(E, booleanExpr, S, T, ClassTreeMap), E, S, T)
		in
			evalMethodEnd((#2(result)), validInstructionSet, (#3(result)), (#4(result)), ClassTreeMap)
		end	  
	| evalMethodEnd (E, RETURN expr::_, S, T, ClassTreeMap) =
		let
			val resultExpr = #1(evalExpr(E, expr, S, T, ClassTreeMap))
		in
			print("FINE: " ^ printS(S) ^ "\n");
			(resultExpr, E, S, T)
		end

(* Valuta tutte le dichiarazioni all'interno di un metodo *)
and evalDeclarations (nil, E, S, T, _) = (E, S, T)
	| evalDeclarations (dec::declar, E, S, T, ClassTreeMap) = 
		let 
			val dataStructures = evalVarDecl(E, dec, S, T, ClassTreeMap); 
		in
			evalDeclarations(declar, #1(dataStructures), #2(dataStructures), #3(dataStructures), ClassTreeMap)
		end



