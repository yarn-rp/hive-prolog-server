# Proyecto de Programación Declarativa (Prolog)

- Integrantes:
  - Javier Alejandro Valdes Gonzales (C-411)
  - Yansaro Rodriguez Paez (C-412)

## Problema

El problema a resolver es la implementación del juego `InsectsOnArena`, el juego consiste de 22 piezas (11 por cada integrante). El juego termina cuando se consigue "encarcelar" a la `queen_bee` de la colmena.

## Ejecución del proyecto

El proyecto consta de 2 partes, una aplicación backend, donde se maneja toda la lógica del juego a traves de servicios API REST, y una aplicación frontend multiplataforma escrita en Flutter.

Desde la raíz de este proyecto:

```bash
swipl  init.pl
```

Si todo va bien deberia mostrarse el siguiente mensaje:
`Started server at http://localhost:8080/`. Por favor, asegúrese de tener libre el puerto 8080 antes de levantar el servidor.

Para correr la aplicación, primero clone nuestro [repositorio git](https://github.com/yarn-rp/hive_game_client.git), y luego desde la raíz puede:

1. Montar la web previamente construida. Para ello: `cd web-client && python -m http.server <Puerto>`.

2. Instalar Flutter SDK. Levantar la aplicación de cualquiera de las plataformas soportadas por Flutter ejecutando:

```bash
flutter run -d <deviceId>
```

## Implementación de la lógica del juego

Lo primero a tener en cuenta es la geometría hexagonal del juego, se intenta establecer un sistema de coordenadas hexagonales para su fácil implementación. Esto se realiza de la siguiente forma:

- Se comienza con (q,r)=(0,0)
- A partir de aquí se forman las siguientes casillas sumando los clásicos vectores de movimiento:
  - (q, r+1) para la posición norte
  - (q+1, r+1) para la posición noreste
  - etc...

A cada pieza le corresponde un identificador único( este es incremental) y se utilizan los diferentes predicados para la realización del juego:

- Place (colocar de la mano)
- Move (mover en el tablero)
- NextPlayer (cambia y guarda el jugador que viene), esto se conjuga con el predicado de CurrentPlayer
- GetPossiblePlacements para saber en que posición puedo colocar las piezas de mi mano, y la correspondiente
- GetPossibleMoves donde dado el id de la pieza obtenemos las posibles direcciones a donde se puede mover la pieza
- GetGameStats, aquí se puede ver el estado e información de cada jugador y el estado de la `colmena` o tablero

Para las piezas que usan saltos( se pueden colocar encima de otras), en nuestro caso el escarabajo o `beetle` se lleva un `stack` de piezas en la cual cada una tiene un `level` correspondiente a su altura. En la mano lvl=-1 y luego a partir de cero en el tablero

También se usaron los predicados correspondientes a los movimientos de cada una de las piezas o "insectos" en el archivo `./insects.pl`

### AI

El desarrollo de la AI del juego no ofrece ninguna complejidad. Primero analiza las posibles jugadas que puede realizar en el turno. Para eso usa los predicados de la lógica del juego y de los movimientos de los insectos. Luego entre estas jugadas escoge una de manera aleatoria. En resumen:

- Lista todas las posibles jugadas legales en el turno
- Escoge una de manera aleatoria

En fin, tenemos un AI que en teoría pudiera vencer al campeon del mundo en InsectsOnArena (suck it Google 💪)

### API de comunicación

Para la comunicación con la capa de presentación visual, el frontend se elaboro un API mediante un `http_server` en prolog, los detalles se pueden encontrar en la carpeta `./http/`, esto esta separado en varias capas:

- Routes, aquí se encuentra el mapeo de ruta http a su controlador correspondiente
- Controllers, son los predicados encargados de obtener la data ya en prolog procedente de las `routes` y ejecutar la lógica de juego correspondiente
- Server, aquí se levanta el servidor http en el puerto 3000, (NOTA: verifique que no tiene ningún server http levantado en ese puerto)

### Parte visual del proyecto (frontend)

Como presentación tenemos una [aplicación desarrollada en flutter](https://github.com/yarn-rp/hive_game_client.git), ofrecemos el código de la aplicación, este puede ser compilado para todas las plataformas (iOS, Android, Linux, MacOS, Windows).

![captura-1](https://raw.githubusercontent.com/yarn-rp/hive-prolog-server/main/doc/img1.png)
![captura-2](https://github.com/yarn-rp/hive-prolog-server/blob/main/doc/img2.jpg?raw=true)
![captura-3](https://github.com/yarn-rp/hive-prolog-server/blob/main/doc/img3.png?raw=true)
![video-1](https://github.com/yarn-rp/hive-prolog-server/blob/main/doc/gif.gif?raw=true)
