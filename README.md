# xGlade  
_Simple console strategy game_  
  
[Start](https://github.com/kavzov/xGlade_strategy_game#start)  
[Regserver](https://github.com/kavzov/xGlade_strategy_game#regserver)  
[Game server](https://github.com/kavzov/xGlade_strategy_game#game-server)  
[Battle](https://github.com/kavzov/xGlade_strategy_game#battle)  

---
### Start:  
Pull the database server, the register server, the game server images and run them:  
```docker pull kavzov/xglade_db_server```  
```docker pull kavzov/xglade_reg_server```  
```docker pull kavzov/xglade_game_server```  
  
```docker run --rm -d -p 5432:5432 --name xglade_db_server kavzov/xglade_db_server```  
```docker run --rm -d -p 8000:8000 --link xglade_db_server:localhost kavzov/xglade_reg_server```  
```docker run --rm -dt -p 1234:1234 --link xglade_db_server:localhost kavzov/xglade_game_server```  

---

### Regserver
[GitHub](https://github.com/kavzov/xGlade_regserver)
#### API:  
  
_Register user:_  
```
+---------+--------------------------------------------+
| url     | http://localhost:8000/api/user/create      |
| method  | POST                                       |
| headers | Content-Type: application/json             |
| data    | username, password                         |
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
_A user details:_  
```
+---------+--------------------------------------------+
| url     | http://localhost:8000/api/user/{userID}    |
| method  | GET                                        |
| headers | Content-Type: application/json             |
|         | Authorization: Token token                 |
| data    |                                            |
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
---
## Game server
To get to the game server, type in terminal:  
```telnet localhost 1234```  
You'll see next info:  
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
```> auth Token```  
`Token` -- a token you got after login the regserver  
  
For example,  
```> auth 36be62a4d1b2557bef6a826ea90fe73cdacc210f```  
You'll get to the game server.  
  
There will be info about other players, waiting for opponents, and you may start battle with any of them.  
  
Also there will be info about the games taking place at the moment and odds for every player in the games, so you can bet on a win of any of them and maybe earn some money)  
  
For example,  
```
-----------------------------------------------
Welcome, alex!
You participated in 25 battles. Won in 18.
Your rating - 72. Position in the championship - 7.
-----------------------------------------------

Waiting players:
====================
Game ID: 9
Player: olga
Rating: 88
Board size: 7 x 5
--------------------
Game ID: 12
Player: bill
Rating: 50
Board size: 3 x 4
--------------------

Running games available for bets:
===================================
5. Game 4 x 5
--------------
3. kate. Won 22 battles from 28. Rating: 78, Coef: 1.3700
7. maya. Won 12 battles from 20. Rating: 63, Coef: 1.9265
===================================

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
To **start a new game** you may type:  
```> game 4 4```  
and so you'll start a game with battle glade width 4 and height 4 also.  
  
You need to wait until another user joins the game you just started.  
  
To **join the game** ID 12, type:  
```> play 12```  
and so you'll start a battle against another player.  
  
To **make a bet** $10 on the player ID 7 taking part at the game ID 5, type  
```> bet 7 5 10```  
If your account has enough funds, it will be charged $10.  
  
If the player you bet on wins, you will receive the winnings to your account.
  
---
## Battle
A battle glade has rectangle form and may has different dimensions from 3 to 10 fields by width or height.   
For example, initial state of 4x4 battle field:
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
REFACTORING  
log files  
save battle moves history  
initial random closed fields  
multiplayer mode  
achivements  
bots  
