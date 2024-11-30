Milestone Read Me

Implementations

get-symbol: Cor Layout -> Symbol. Get the Symbol of the Cor of this Layout
convert: Layout Cor Symbol -> Layout. Change the symbol in Layout into the new Symbol and return the new Layout
in-bound?: Cor -> Boolean. Check if the Cor is valid
time-handler: Gamestate -> Gamestate. Renew the round-timer and the count down of the bomb; if the count down of the bomb is 0, use the boom-end function; if the count down of the bomb is 2, use the function boom
boom: Gamestate -> Gamestate. Deal with the chain-explosion, calculate the explosion range, kill the player in the explosion range, merge the new Gamestate
boom-end: remove the bomb which its count down is 0, change the Layout, return the new Gamestate
key-handler: Gamestate Key-event -> Gamestate. Depends on the movement of the player, check if it is possible to move on; if possible, go; if not possible, change the direction. Compares the bombs that players placed and the maximum and check if it is possible to place a new bomb.
end: Gamestate -> Boolean. If press “esc” key or both players are dead or one of the player is dead, return #true; else return #false
final: Gamestate -> Image. If “esc” is pressed, return QUIT-IMAGE; if both player are dead, return TIE-IMAGE; if player 1 is dead, return PLAYER-TWO-WIN; if player 2 is dead, return PLAYER-ONE-WIN. 



Changes

gamestate: add a boom-range field, to redefine the bomb range, when there is an indestructible obstacle, stop there; when there is a destructible obstacle, destroy it and stop there. 
layout: layout is always a vector(vector symbol), no #false
player: no dead?-field, no bomb-count-field
player: add a new direction-field
bomb-state: add an owner field
player: Posn changes to Cor
Symbol: change, delete the ’S, add ‘W1L, ‘W1R, ‘W1U, ‘W1D, ‘B1L, ‘B1R, ‘B1U, ’B1D, ‘W2L, ‘W2R, ‘W2U, ‘W2D, ‘B2L, ‘B2R, ‘B2U, ’B2D, ‘E1L, ‘E1R, ‘E1U, ’E1D, ‘E2L, ‘E2R, ‘E2U, ‘E2D
Layout: use the rules to merge it
Homepage: add homapage