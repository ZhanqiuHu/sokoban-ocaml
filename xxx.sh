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
  - immutable is_player (if position matches player position then print player here instead)
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
  - block 
player - stores player position
  - emoji printed
  - on_exit
  - get_position

