# xGlade
_Simple console strategy game_  
To play the game:  
- Pull down database, regserver and game images:  
`docker pull kavzov/xgladedb`  
`docker pull kavzov/xglade_regserver`  
`docker pull kavzov/xglade_game`  
- Run it:  
`docker run --rm -d -p 5432:5432 --name xgladedb kavzov/xgladedb`  
`docker run --rm -d -p 8000:8000 --link xgladedb:localhost kavzov/xglade_regserver`  
`docker run --rm -dt -p 1234:1234 --link xgladedb:localhost kavzov/xglade_game`  

---

Use next API in your terminal:  
```
"create user": "http://localhost:8000/api/user/create",
"current user details": "http://localhost:8000/api/user/me",
"get token": "http://localhost:8000/api/token/get",
"login": "http://localhost:8000/api/user/login",
"logout": "http://localhost:8000/api/user/logout",
"root": "http://localhost:8000/",
"set password": "http://localhost:8000/api/user/password",
"set username": "http://localhost:8000/api/user/username",
"users list": "http://localhost:8000/api/users"
```  
You may use curl, httpie, etc.   
For example, to register, type:  
`http http://localhost:8000/api/user/create username=your_nickname password=your_password`  
You'll receive a token to authenticate on game server and manipulate your account data on the regserver:  
```
{
    "email":"",
    "username":"your_nickname",
    "id":42,
    "token":"36be62a4d1b2557bef6a826ea90fe73cdacc210f"
}
```  

Next, type in terminal:  
`telnet localhost 1234`  
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
Type:  
`auth 36be62a4d1b2557bef6a826ea90fe73cdacc210f`  
And you'll come to the game server:  
```
+======================================================+
| Command   | Action                                   |
+======================================================+
| me        | current player info (rating, money, ...) |
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
- web interface
- initial random closed fields
- multiplayer mode
- achivements
- permissions
