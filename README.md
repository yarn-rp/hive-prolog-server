## Informe

## Proyecto de Programacion Declarativa (Prolog)

- Integrantes:
  - Javier Alejandro Valdes Gonzales (C-411)
  - Yansaro Rodriguez Paez (C-412)

### Problema

El problema a resolver es la implementacion del juego `InsectsOnArena`, el juego consiste de 22 piezas (11 por cada integrante). El juego termina cuando se consigue "encarcelar" a la `queen_bee` de la colmena.

### Implementacion de la logica del juego

Lo primero a tener en cuenta es la geometria hexagonal del juego, se intenta establecer un sistema de coordenadas hexagonales para su facil implementacion. Esto se realiza de la siguiente forma:

- Se comienza con (q,r)=(0,0)
- A partir de aqui se forman las siguientes casillas sumando los clasicos vectores de movimiento:
  - (q, r+1) para la posicion norte
  - (q+1, r+1) para la posicion noreste
  - etc...

A cada pieza le corresponde un identificador unico( este es incremental) y se utilizan los diferentes predicados para la realizacion del juego:

- Place (colocar de la mano)
- Move (mover en el tablero)
- NextPlayer (cambia y guarda el jugador que viene), esto se conjuga con el predicado de CurrentPlayer
- GetPossiblePlacements para saber en que posicion puedo colocar las piezas de mi mano, y la correspondiente
- GetPossibleMoves donde dado el id de la pieza obtenemos las posibles direcciones a donde se puede mover la pieza
- GetGameStats, aqui se puede ver el estado e informacion de cada jugador y el estado de la `colmena` o tablero

Para las piezas que usan saltos( se pueden colocar encima de otras), en nuestro caso el escarabajo o `beetle` se lleva un `stack` de piezas en la cual cada una tiene un `level` correspondiente a su altura. En la mano lvl=-1 y luego a partir de cero en el tablero

Tambien se usaron los predicados correspondientes a los movimientos de cada una de las piezas o "insectos" en el archivo `./insects.pl`

### AI

El desarrollo de la AI del juego no ofrece ninguna complejidad. Primero analiza las posibles jugadas que puede realizar en el turno. Para eso usa los predicados de la logica del juego y de los movimientos de los insectos. Luego entre estas jugadas eescoge una de manera aleatoria. En resumen:

- Lista todas las posibles jugadas legales en el turno
- Escoge una de manera aleatoria

En fin, tenemos un AI que en teoria pudiera vencer al campeon del mundo en InsectsOnArena (suck it Google 💪)

### API de comunicacion

Para la comunicacion con la capa de presentacion visual, el frontend se elaboro un API mediante un `http_server` en prolog, los detalles se pueden encontrar en la carpeta `./http/`, esto esta separado en varias capas:

- Routes, aqui se encuentra el mapeo de ruta http a su controlador correspondiente
- Controllers, son los predicados encargados de obtener la data ya en prolog procediente de las `routes` y ejecutar la logica de juego correspondiente
- Server, aqui se levanta el servidor http en el puerto 3000, (NOTA: verifique que no tiene ningun server http levantado en ese puerto)

### Parte visual del proyecto (frontend)

Como presentacion tenemos una aplicacion desarrollada en flutter, ofrecemos el codigo de la aplicacion, este puede ser compilado para todas las plataformas (iOS, Android, Linux, MacOS, Windows).
