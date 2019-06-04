# xGlade
_Simple console strategy game_  
## Start
`telnet host port`
```
Hi! It's xGlade game.
Authenticate and play or go out)
+===========================+
| Command    | Action       |
+===========================+
| auth Token | authenticate |
+---------------------------+
| exit       | go out       |
+---------------------------+
```
_Token_  can be obtained after registration (_comming soon_)  
```
+======================================================+
| Command   | Action                                   |
+======================================================+
| i         | current player info (rating, money, ...) |
+------------------------------------------------------+
| games     | waiting players and active games         |
+------------------------------------------------------+
| players   | players table with achievements          |
+------------------------------------------------------+
| game W H  | start new game with dimension W x H      |
|           | W - width, H - height (min: 3, max: 10)  |
+------------------------------------------------------+
| play ID   | play game number ID (if there is)        |
+------------------------------------------------------+
| bet G P $ | make a bet: GameID, PlayerID, $um        |
+------------------------------------------------------+
| exit      | leave the server                         |
+------------------------------------------------------+
```

## Battle
A battle glade may has different dimensions from 3 to 10 fields by width or height.   
Initial state of 4x4 battle field:
```
+---+---+---+---+
| A |   |   |   |
+---+---+---+---+
|   |   |   |   |
+---+---+---+---+
|   |   |   |   |
+---+---+---+---+
|   |   |   | B |
+---+---+---+---+
```
Players make moves one by one to the up, right, left or down only.  
The player who has nowhere to make a move loses. For example:
```
+---+---+---+---+
| X | X | X |   |
+---+---+---+---+
| A | X | X |   |
+---+---+---+---+
| B | X | X | X |
+---+---+---+---+
|   |   |   | X |
+---+---+---+---+
```
Player 'A' lost the game.  

### Controllers:  
```
+---+-------+---+
| ↑ | up    | w |
| → | right | d |
| ↓ | down  | s |
| ← | left  | a |
+---+-------+---+
```

## TODO  
- log files  
- save battle moves history  
- registration server (Python, Django, DRF)
- initial random closed fields
- multiplayer mode
- achivements


