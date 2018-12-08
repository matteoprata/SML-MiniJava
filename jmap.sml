(*Tipo astratto polimorfo per la rappresentazione di strutture dati sul modello di Dizionario *)
abstype ('a,'b) map = EMPTY | NODE of ('a,'b) map * 'a * 'b
with
  exception MapException of string

  val empty = EMPTY;
  val locInd = ref 0; (* contatore locazioni *)

  (* inserisce chiave, valore, all'interno di una map *)
  fun insert(m,key,value) = NODE(m,key,value)

  (* ottiene la map successiva presente nella map *)
  fun getNextMap(NODE(m,key,value)) = m

  (* torna true se la mappa è vuota, false altrimenti *)
  fun isEmpty(EMPTY) = true
    | isEmpty(NODE(m,key,value)) = false

  (* ottiene associazione chiave-valore di una map *)
  fun getKVCouple(NODE(m, key, value)) = (key,value)

  (* torna il valore associato alla chiave in input *)
  fun get(EMPTY, _) = raise MapException "Chiave non trovata"
    | get(NODE(m,key,value), searchedKey) = if (searchedKey = key) then value else get(m, searchedKey)

  (*Torna true se la chiave ricercata esiste all'interno della mappa*)
  fun contains (EMPTY, searchedKey) = false
    | contains(NODE(m,key,_), searchedKey) = if (key = searchedKey) then true else contains(m, searchedKey)

  (* appende m1 ad m2 *)
  fun concat(EMPTY,m2) = m2
    | concat(NODE(m1,key,value),m2) = insert(concat(m1,m2),key,value)

  (* modifica il valore della chiave *)
  fun modify (EMPTY,_,_) = raise MapException "Impossibile modificare una mappa vuota"
    | modify (NODE(m,key1,value1),key2,value2) = if (key1=key2) then NODE(m,key1,value2) else NODE(modify(m,key2,value2),key1,value1)

  (* rimuove l'associazione chiave valore dalla mappa*)
  fun remove (EMPTY, searchedKey, newMap) = newMap
    | remove (NODE(m, key, value), searchedKey, newMap) = 
        if (key = searchedKey) then remove(m, searchedKey, newMap) 
        else remove(m, searchedKey, insert(newMap, key, value))
  
  (*Funzioni di stampa della mappa*)

  fun printMap (EMPTY) = "}"
    | printMap (NODE(m,k,value))= "{(" ^ k ^ "," ^ value ^ ")," ^ printMap(m)

  fun printMapStrInt (EMPTY) = ""
    | printMapStrInt (NODE(m,k,value)) = "(" ^ k ^ "," ^ Int.toString(value) ^ ")," ^ printMapStrInt(m)

  fun printE (EMPTY) = "}"
    | printE (NODE(m,k,value)) = "{" ^ printMapStrInt(NODE(m,k,value)) ^ "}"

  fun printS(EMPTY) = "}"
    | printS (NODE(m,k,value)) = "{" ^ Int.toString(k) ^ "," ^ printMapStrInt(value) ^ "]" ^ printS(m)

end;