(*
	JML Interpreter 1.0

	Interprete per linguaggio JML Java-Based.

Definizione delle funzioni necessarie al riconoscimento della
sintassi del linguaggio java minimale ed atte allo svolgimento 
della fase di interpreting del codice.

*)

(* import per semantica e sintatti jml*)

use "jmap.sml";
use "jml_syntax.sml";
use "jml_sample.sml";
use "jml_semantic.sml";
use "jml_core.sml";

(* import file di esempio jml codificato in sml *)

jmlInterpreter(programma1)