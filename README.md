# xGlade
_Simple console strategy game_ 
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
- save battle moves history  
- log files  

