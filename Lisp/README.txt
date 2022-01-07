Componenti del gruppo:
    1) 866163 Avallone Lorenzo
    2) 872783 Giannino Simone
    3) 866147 Biotto Simone

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

Abbiamo dunque implementato dei predicati che andranno a scomporre la stringa di
input in delle sottostrutture per rappresentare l'oggetto composto URI.
Possiamo definire dunque dei predicati principali:
    - (uri-parse String), restituisce true se la stringa in input rappresenta
      un URI valido. 
      Internamente viene costruito un AST dell'oggetto URI tramiteregole private
      Esempio: uri-parse("http://disco.unimib.it", URI).
               URI = uri(http, [], 'disco.unimib.it', 80, [], [], []).
      Non è possibile però ricostruire una stringa a partire da un oggetto
      composto URI.
    - (uri-display uri-structure &optional stream), permette dato uno stream e 
      un oggetto di tipo URI valido di 
      stampare in modo formattato le singole componenti del nostro URI.
