# CS3110-final-project


# Installzation

- Step 1: Download the zip file and upzip it;

- Step 2: In the directory, run [make play] command;

- Step 3: Type in "start" to start the game.
    To move the player, type in:
  "w" to move the player one tile upwards;
  "s" to move the player one tile downwards; 
  "a" to move the player one tile leftwards; 
  "d" to move the player one tile rightwards; 

Note if a move has the player reached the boundary, the move won't take place;
Also if the input command is anything other than the above characters, a 
warnning will be printed and asks you to retype an input. 

- Step 4: Type in "quit" to quit the game. 

# Modules implemented
## Main:
Call functions in other modules to run the game.

## Types: 
Contains all types used in other modules.

## Map:
Contains the maps used for the game.

## State:
Contains functions that store game's current states and update states.

## Command:
Contain functions that parse command line input to a command type.

