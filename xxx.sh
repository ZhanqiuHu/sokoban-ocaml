objects:
map - arrays of objects and their positions are related to the indices of the array
  - position of the exit
  - position of the entrance
  - position of the blocks
  - position of the boundary of the map
  - position of the tiles
blocks - record with mutable position
  - mutable complete(whether this block is in its correct position)
holes - record with mutable has_block(whether there is a block in this hole or not 
  ... if there is a block it cannot be removed anymore)
tiles - once a block is pushed into a hole, it turns into a walkable tile
  - record with position 
  - mutable is_player (if position matches player position then print player here instead)
  - mutable is_block (if position matches block position then print block here instead)
player - record with mutable position
  - mutable on_exit(whether player is at the exit or not)


modules:
main - starts game with level 1
  - prints the game to the screen after every input
  - prompts player for input (wasd, quit)
command - Player movements
  -Start/quit "wasd"
state -  state -> command -> state 
  - current level 
  - map
  - player (if is_player is true, print out player. is_player automatically false for blocks etc)
  - blocks (in current room/map) 
player - stores player position
  - emoji printed
  - on_exit
  - get_position
types - type of images other than player
  - blocks
  - tiles
  - obstacles
  - holes







MS1:
Kristi
main - starts game (player at bottom center)
  - prints edges of map to the screen
  - prints player to the screen
  - prompts player for input (default player loads in bottom center)

Summer
command - "wasd"/"quit"

Han
state - 
  - valid moves are moves that dont conflict with the boundaries of the map
  - change player position
  - changes is_player of tiles

Sophia
map.ml file  (10 * 10)
  - hard-coded rooms 
  - boundaries of every map

types.ml file -
  - horizontal boundary
  - vertical boundary
  - tile (walkable location)
  - player (stores player position, on_exit)



- hardcode one map
- print out boundary of map
- print out player onto map
- move player from player input







ob = texture | obstacles | holes 

let start_room  = {
  
  obj_list = [
obstacles = cord= {1,1}
texture = cord= {1,1}
texture = cord= {1,1}
texture = cord= {1,1}
texture = cord= {1,1}
texture = cord= {1,1}
texture = cord= {1,1}
texture = cord= {1,1}
texture = cord= {1,1}
  ]

}