(* File di esempio di codice jml *)

val programma1 = 

([ 
("Impiegato", "", 

		 [(INT, "stipendio", INTEXPR(1)),
		  (INT, "attualeStipendioAnnuo", INTEXPR(0))
		 ],
		 [ 
		 	(INT, "stipendioNegliAnni", [(INT, "anni"), (INT, "stipendioAnnuale")], 

			 	[(INT, "contatore"), 
			 	 (INT, "s")],
			    [
			   		ASSIGNVAR("contatore", INTEXPR(0)),
			   		ASSIGNVAR("s", VAR("stipendioAnnuale")),

			   			WHILE(BOOLEANEXPR(VAR("contatore"), "<", VAR("anni")), 
			   			[  
			   				ASSIGNVAR("s", PLUS(VAR("s"), TIMES(VAR("stipendioAnnuale"), INTEXPR(12)))),
			   				ASSIGNVAR("contatore", PLUS(VAR("contatore"), INTEXPR(1)))
			   			]),

			    	RETURN(VAR("s"))
			 	]
			),

			(INT, "main", nil, 

				[(CLASS("Impiegato"), "p1"), 
				 (CLASS("Impiegato"), "p2"),
				 (INT, "stipendioTot")], 
			    [
			    	ASSIGNVAR("p1", NEW("Impiegato")),
			    	ASSIGNVAR("p2", NEW("Impiegato")),

			    	ASSIGNFIELD(VAR("p1"), "stipendio", INTEXPR(1500)),

			    	ASSIGNVAR("stipendioTot", CALL(VAR("p1"), "stipendioNegliAnni", [INTEXPR(10), FIELD(VAR("p1"), "stipendio")])),
			    	ASSIGNFIELD(VAR("p1"), "attualeStipendioAnnuo", VAR("stipendioTot")),
			    	ASSIGNVAR("p2", VAR("p1")),

			    	RETURN(FIELD(VAR("p2"), "attualeStipendioAnnuo"))
			 	]
		 	)
		]
)])

val globalProgram : JML_program = programma1;
