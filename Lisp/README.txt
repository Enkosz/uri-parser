Componenti del gruppo:
    1) 866163 Avallone Lorenzo
    2) 872783 Giannino Simone
    3) 866147 Biotto Simone

Requisiti:
Per una corretta esecuzione è richiesta almeno la LTS di SBCL 2.1.10

Introduzione:
L'obiettivo del progetto propostoci è la realizzazione di un parser per stringhe
URI. La versione presentata e sviluppata è una semplificazione della specifica 
RFC-3986. Abbiamo sviluppato un semplice top down parser con un carattere di
forehead seguendo l'idea proposta dallo scrittore del libro Paradigms of 
Artificial Intelligence Programming: Case Studies in Common Lisp by Peter Norvig
Non avendo a disposizione le DCG in Lisp abbiamo sviluppato una funzione che ci
permette data una lista di caratteri in input e una lista di delimitatori di
restituire una struttura dati contenente nel seguente ordine:
    1) Valore parsato
    2) Resto dell'input non parsato

In questo modo è stato necessario solamente sviluppare delle funzioni in cascata
che sfruttassero l'output della funzione precedente in modo da costruire un
oggetto URI di questo tipo:
    uri-structure:
        schema
        authority:
            userinfo
            host
            port
        path
        query
        fragment
Ognuno dei seguenti elementi è stato parsato attraverso delle funzioni dedicate.

Abbiamo dunque implementato delle funzioni che andranno a scomporre la stringa
di input in delle sottostrutture per rappresentare l'oggetto composto URI.
Possiamo definire dunque delle funzioni principali:
    - (uri-parse string), restituisce l'oggetto URI, in caso di URI non valido 
      si avrà l'oggetto impostato a NIL.
    - (uri-display uri-structure &optional stream), permette dato uno stream
      opzionale e un oggetto di tipo URI di stampare in modo formattato le 
      singole componenti del nostro URI. In caso lo stream non sia specificato,
      l'output sarà stampato sullo standard-output. 

L'idea di base per la realizzazione del parser è la divisione in più parti della
stringain input tramite una funzione principale di appoggio, ovvero:
    - (identificator% list &optional delimitators banned accumulator), che 
       restituisce due liste, la prima è costituita dai caratteri parsati fino 
       al delimitatore e la seconda da tutti i rimanenti. Questo è possibile 
       grazie all'uso di un accumulator che ci permette di analizzare carattere
       per carattere la lista.
   
Quindi la stringa URI presa in input dalla funzione uri-parse verrà prima
trasformata in una lista e successivamente, grazie alla struttura delle funzioni
a cascata, divisa nei vari campi. La divisione dei campi avviene grazie alla 
grammatica definita dalla consegna.

Inoltre, per garantire il codice più completo possibile, abbiamo implementato 
degli unit test che è possibile eseguire per controllare la correttezza del 
parser.

Infine abbiamo implementato le funzioni:
    - uri-scheme
    - uri-userinfo
    - uri-host
    - uri-port
    - uri-path
    - uri-query
    - uri-fragment
Le quali prendono un Uri-structure e restituiscono il rispettivo campo 
dell'oggetto in input.
