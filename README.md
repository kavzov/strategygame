# xGlade  
_Simple console strategy game_  

---
- Pull down database server, register server and game server images:  
`docker pull kavzov/xgladedb`  
`docker pull kavzov/xglade_regserver`  
`docker pull kavzov/xglade_game`  
- Run it:  
`docker run --rm -d -p 5432:5432 --name xgladedb kavzov/xgladedb`  
`docker run --rm -d -p 8000:8000 --link xgladedb:localhost kavzov/xglade_regserver`  
`docker run --rm -dt -p 1234:1234 --link xgladedb:localhost kavzov/xglade_game`  

---

### API:  
_Register user:_  
```
+---------+--------------------------------------------+
| url     | http://localhost:8000/api/user/create      |
| method  | POST                                       |
| headers | Content-Type: application/json             |
| data    | username, password                         |
+---------+--------------------------------------------+
```
_Current user details:_  
```
+---------+--------------------------------------------+
| url     | http://localhost:8000/api/user/me          |
| method  | GET                                        |
| headers | Content-Type: application/json             |
|         | Authorization: Token token                 |
| data    |                                            |
+---------+--------------------------------------------+
```
_Login:_  
```
+---------+--------------------------------------------+
| url     | http://localhost:8000/api/user/login       |
| method  | POST                                       |
| headers | Content-Type: application/json             |
| data    | username, password                         |
+---------+--------------------------------------------+
```
_Logout:_  
```
+---------+--------------------------------------------+
| url     | http://localhost:8000/api/user/logout      |
| method  | POST                                       |
| headers | Content-Type: application/json             |
|         | Authorization: Token token                 |
| data    |                                            |
+---------+--------------------------------------------+
```
_Get token:_  
```
+---------+--------------------------------------------+
| url     | http://localhost:8000/api/token/get        |
| method  | POST                                       |
| headers | Content-Type: application/json             |
| data    | username, password                         |
+---------+--------------------------------------------+
```
_Users:_  
```
+---------+--------------------------------------------+
| url     | http://localhost:8000/api/users            |
| method  | POST                                       |
| headers | Content-Type: application/json             |
|         | Authorization: Token token                 |
| data    |                                            |
+---------+--------------------------------------------+
```
_Change username:_  
```
+---------+--------------------------------------------+
| url     | http://localhost:8000/api/user/username    |
| method  | POST                                       |
| headers | Content-Type: application/json             |
|         | Authorization: Token token                 |
| data    | current_password, new_username             |
+---------+--------------------------------------------+
```
_Change password:_  
```
+---------+--------------------------------------------+
| url     | http://localhost:8000/api/user/password    |
| method  | POST                                       |
| headers | Content-Type: application/json             |
|         | Authorization: Token token                 |
| data    | current_password, new_password             |
+---------+--------------------------------------------+
```
  
You may use curl, httpie, Postman, etc.   
  
For example, to register a new user using httpie, type:  
```http http://localhost:8000/api/user/create username=your_nickname password=your_password```  
  
You'll receive a token to authenticate on a game server and manipulate your account data on the regserver:  
```
{
    "email":"",
    "username":"your_nickname",
    "id":42,
    "token":"36be62a4d1b2557bef6a826ea90fe73cdacc210f"
}
```  
## Game server
To get to the game server, type in terminal:  
```telnet localhost 1234```
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
> 
```  
To authenticate, type:  
```> auth 36be62a4d1b2557bef6a826ea90fe73cdacc210f```  
You'll get to the game server.  
  
There will be information about other players waiting for the opponent and you may join to it and start battle.  
  
Also there will be info about the games taking place at the moment and odds for every player, so you can bet on any player and maybe get some money)  
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
To start a new game you may type:  
```> game 4 4```  
and so you'll start a game with field width 4 and height 4 also.  
  
You need to wait until another user joins the game you just started.  
  
To join to other game with ID 5, type:  
```> play 5```  
and so you'll start a battle against another player.  
  
To make a bet $10 on the player ID 7 taking part at the game ID 5, type  
```> bet 7 5 10```  
If your account has enough funds, it will be charged $10.  
  
If the player you bet on wins, you will receive the winnings to your account.
  
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
---
## TODO  
~~registration server (Python, Django, DRF)~~  
~~web interface~~  
log files  
save battle moves history  
initial random closed fields  
multiplayer mode  
achivements  
permissions  
