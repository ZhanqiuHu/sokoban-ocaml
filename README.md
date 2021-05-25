# CS3110-final-project


# Installation
- Step 0: Install the graphics and camlimages packages; see below for more details

- Step 1: Download the zip file and upzip it;

- Step 2: In the directory, run [make build] and then [make play] command;

- Step 3: Press any key to start the game.

- Step 4: Close the window to stop the game. 

# How to play the game
Once the window is open, press any key to start the game.

### To move player 1, press the following keys on keyboard:

 - "w" to move the player one tile upwards;
 - "s" to move the player one tile downwards; 
 - "a" to move the player one tile leftwards; 
 - "d" to move the player one tile rightwards; 

### To move player 2, press the following keys on keyboard:
  
 - "i" to move the player one tile upwards;
 - "k" to move the player one tile downwards; 
 - "j" to move the player one tile leftwards; 
 - "l" to move the player one tile rightwards; 


Note that a move that causes the player to end up on an obstacle/boundary will not take place.
Also, if the input command is anything other than the above characters, a warning will be printed and ask you to retype an input. 

The objective of the game is to push all blocks (yellow) into holes (black X). 
Once all the holes have been filled, the player (if playing alone) or both players (if playing two player) must step on the exit to teleport to the next level (gate image).
You can push multiple blocks in a row, and other players, but all players will be able to step on the exit simultaneously to go to the next level.
There are breakable objects which are depicted as gray blocks. You can try to "walk" on them twice (press the key that would otherwise cause you to collide with the object) to break them and turn that tile into a walkable tile. You do not need to break all these objects to finish the level.

The last room is a "win" room which has no exit and a win message displayed.

There are two player modes to choose from, and 3 game modes to choose from. Select one player mode and one game mode, then press start to begin. Note that the game will not begin until one button from each category is selected. 

The two player modes include single player and two player. 

The three game modes include normal mode, sliding blocks, and step limit. 

In normal mode, players can push each other, and blocks can push each other. 
Pushing several moveable objects lined up together will result in a cascade effect, with each object moving one space. 

In sliding blocks mode, players can push players, but blocks cannot push players. Upon colliding with a player, the block will stop sliding. Pushed blocks will not stop unless they encounter a solid obstacle (i.e, another player, block, breakeable, or obstacle.)

In step limit mode, the number of steps (legal movements) the player can take in the level is limited. The steps left for the player will be displayed in the lower lefthand corner. Upon running out of steps, the player will lose, even if the blocks are all in the holes. Step limit is shared between all players. 

Click on the "RESET" button to reset the game, restarting from the first level.
Click on the "QUIT" button to quit the game.
Click the yellow arrow at the top of the screen to undo your most recent action.
Click the pause button in the upper right corner to pause the game and return to the main menu. Click the pause button again to return to the game. 

# Getting the GUI to Work
## For Mac:
NOTE: 
You will need to install X11/XQuartz for Graphics support. 
Install xquartz according to the 3110 Canvas page and delete/reinstall opam.
If you installed ocaml with homebrew, it can be done by running

`$ brew install Caskroom/cask/xquartz`

`$ brew reinstall ocaml --with-x11`

- Step 1: create a new switch for the game by running the command 

    `$ opam switch create <name> 4.07.1`

- Step 2: run

    `$ opam install graphics`

    `$ opam update`

    `$ opam upgrade`

    `$ eval $(opam env)`

    NOTE: If the installation of graphics module is successful, then the
    following commands should have you open a XQuartz window:

    `$ ocaml`

    `$ #load "graphics.cma";;`

    `$ Graphics.open_graph "";;`

- Step 3: run 

    `$ opam install -y utop ounit qcheck ocaml-lsp-server ocamlformat yojson ansiterminal csv bisect_ppx-ocamlbuild menhir user-setup`
    
    `$ opam user-setup install`

- Step 4: run 

    `$ brew install libpng`

    `$ opam install camlimages`

NOTE: All of the terminal commands from step 1 to step 4 are included in the script macinstall.sh. Mac users can run this script in their terminal to install all packages needed. 



## For Windows:
- Step 1: create a new switch for the game by running the command 

    `$ opam switch create <name> 4.07.1`

- Step 2: run

    `$ sudo apt install pkg-config`

    `$ opam install graphics`

    `$ opam update`

    `$ opam upgrade`

    `$ eval $(opam env)`  

- Step 3: install Xming available here: [Xming](https://sourceforge.net/projects/xming/)

- Step 3b: check to make sure running the following code opens up a blank window (make sure Xming has a server running):

    `$ ocaml`
    
    `$ #load “graphics.cma”;;`

    `$ Graphics.open_graph “localhost:0.0”;;`

- Step 4: run

    `$ sudo apt-get install libpng-dev`
    
    `$ opam install camlimages`
    
- Step 5: run 

    `$ opam install -y utop ounit qcheck ocaml-lsp-server ocamlformat yojson ansiterminal csv bisect_ppx-ocamlbuild menhir user-setup`
    
    `$ opam user-setup install`

Note: you will need to change open_graph function to be 
`Graphics.open_graph ("localhost:0.0 " ^ (string_of_int map_w) ^ "x" ^ (string_of_int map_h))` 
in the main.ml file inside the open_graph function. This is noted in a comment 
right above the Mac implementation which is what it will be by default.


# Modules implemented
## Main:
Calls functions in other modules to run the game.

## Types: 
Contains all types used in other modules.

## Map:
Contains the maps used for the game.

## State:
Contains functions that store game's current states and update states.

## Command:
Contains functions that parse command line input to a command type.

## Gui:
Contains functions used to print the game to the screen.

## Text:
Contains functions used to print text to the screen.

## Genmap:
Contains functions used to generate maps for the game.

