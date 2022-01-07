Componenti del gruppo:
    1) 866163 Avallone Lorenzo
    2) 872783 Giannino Simone
    3) 866147 Biotto Simone

Introduzione:
L'obiettivo del progetto propostoci è la realizzazione di un parser per stringhe
URI. La versione presentata e sviluppata è una semplificazione della specifica 
RFC-3986. L'approccio per lo sviluppo del parser è stato mediante l'uso delle
DCG (https://www.metalevel.at/prolog/dcg) offerte nativamente dal linguaggio
Prolog. Le DCG ci permettono di rappresentare delle grammatiche mediante delle
regole che permettono l'implementazione di un top down parser.

Abbiamo dunque implementato dei predicati che andranno a scomporre la stringa di
input in delle sottostrutture per rappresentare l'oggetto composto URI.
Possiamo definire dunque dei predicati principali:
    - uri_parse/2, restituisce true se la stringa in input rappresenta un URI
      valido. Internamente viene costruito un AST dell'oggetto URI tramite
      regole private.
      Esempio: uri_parse("http://disco.unimib.it", URI).
               URI = uri(http, [], 'disco.unimib.it', 80, [], [], []).
      Non è possibile però ricostruire una stringa a partire da un oggetto
      composto URI.
    - uri_display/2, permette dato uno stream e un oggetto di tipo URI valido di 
      stampare in modo formattato le singole componenti del nostro URI.
    - uri_display/1, permette dato un oggetto di tipo URI la stampa sullo 
      STDOUT.
