

if (FALSE) {
  remotes::install_github('coolbutuseless/eventloop')
  remotes::install_github('coolbutuseless/nara')
}

library(grid)
library(nara)
library(eventloop)

source("board.R")
source("sprites.R")
source("sound.R")

# set.seed(1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TODO:
#  * Collision detection logic needs another look
#  * I can't figure out how to reliably get audio::play() 
#    to continually play the munch/chomp sound.  Seems to stutter too 
#    much during playback.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Given the row and column pick a random direction to move in
# Ensure that the direction chosen isn't the reverse of the current
# direction
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
choose_direction <- function(row, col, current) {
  dirs <- c('left', 'right', 'up', 'down')
  reverse <- c('right', 'left', 'down', 'up')
  remove <- which(reverse == current)
  idxs <- setdiff(1:4, remove)
  for (i in sample(idxs)) {
    if (moves[[i]][32 - row, col]) break
  }
  dirs[i]
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Entire game state
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
game <- new.env()
game$over     <- FALSE
game$complete <- FALSE

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 'pacman' state information
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
game$pac <- list()
game$pac$dir      <- "rest"
game$pac$next_dir <- "rest"
game$pac$dx       <- 0
game$pac$dy       <- 0
game$pac$row      <- 14
game$pac$col      <- 15

game$score <- 0
game$lives <- 5
game$last_collision <- -Inf

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Current dots in the scene
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
game$dots <- dots

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ghost state information. This will be updated during the game
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
game$gh <- list(
  list(row =  2, col =  2, dx = 0, dy = 0, dir = "down"),
  list(row =  2, col = 27, dx = 0, dy = 0, dir = "down"),
  list(row = 30, col =  2, dx = 0, dy = 0, dir = "down"),
  list(row = 30, col = 27, dx = 0, dy = 0, dir = "down")
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create working buffer where the board will be rendered
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
board_nr <- nr_duplicate(blank_board_nr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 'heartbeat' eventloop callback function
# This function is called every frame.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
update_game <- function(event, frame_num, ...) {

  
  step <- ((frame_num - 1L) %% 8L) + 1L
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Monitor for keypress at every frame to ensure we don't miss it.
  # and just stow the preferred "next direction"
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(event) && event$type == 'key_press') {
    if (event$str == 'Left' || event$str == 'a') {
      game$pac$next_dir <- 'left'
    } else if (event$str == 'Right' || event$str == 'd') {
      game$pac$next_dir <- 'right'
    } else if (event$str == 'Up' || event$str == 'w') {
      game$pac$next_dir <- 'up'
    } else if (event$str == 'Down' || event$str == 's') {
      game$pac$next_dir <- 'down'
    }
  }
  
  
  if (step == 1) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Eat the dot where the pacman is
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    matches <- (game$dots$y == game$pac$row & game$dots$x == game$pac$col)
    game$dots <- game$dots[!matches,, drop = FALSE]
    if (any(matches)) {
      # Bump the score if a dot was consumed
      game$score <- game$score + 10
      if (game$score %% 200 == 0) {
        audio::play(sound$fruit)
      }
    }
    
    if (!game$over && nrow(game$dots) == 0) {
      game$over     <- TRUE
      game$complete <- TRUE
      audio::play(sound$inter)
      Sys.sleep(5)
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Collison with ghosts?
    # Collision if:
    #   * ghost + pacman on the same square
    #   * ghost + pacman one square apart and headed towards each other
    #       (this colliding in the middle of the 8-step move)
    #
    # Give the user a 3 second "free time" after a collision where we don't 
    # check again.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!game$over && frame_num - game$last_collision > 3*50) {
      for (i in seq_along(game$gh)) {
        if (
          (game$gh[[i]]$row == game$pac$row && game$gh[[i]]$col == game$pac$col) ||
          (game$gh[[i]]$row == game$pac$row + 1 && game$gh[[i]]$col == game$pac$col && 
           game$gh[[i]]$dir == 'down' && game$pac$dir == 'up') ||
          (game$gh[[i]]$row == game$pac$row - 1 && game$gh[[i]]$col == game$pac$col && 
           game$gh[[i]]$dir == 'up' && game$pac$dir == 'down') ||
          (game$gh[[i]]$row == game$pac$row && game$gh[[i]]$col == game$pac$col + 1 && 
           game$gh[[i]]$dir == 'left' && game$pac$dir == 'right') ||
          (game$gh[[i]]$row == game$pac$row && game$gh[[i]]$col == game$pac$col - 1 && 
           game$gh[[i]]$dir == 'right' && game$pac$dir == 'left')
        ) {
          game$last_collision <- frame_num
          game$lives <- game$lives - 1L
          game$score <- game$score - 100L
          if (game$lives == 0) {
            game$over <- TRUE
            audio::play(sound$death)
          } else {
            audio::play(sound$ghost)
          }
          break;
        }
      }
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # if the 'next direction' is possible, make this the direction of pacman
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (game$pac$next_dir == 'left' && move_left[32 - game$pac$row, game$pac$col]) {
      game$pac$dir <- 'left'
    } else if (game$pac$next_dir == 'right' && move_right[32 - game$pac$row, game$pac$col]) {
      game$pac$dir <- 'right'
    } else if (game$pac$next_dir == 'up' && move_up[32 - game$pac$row, game$pac$col]) {
      game$pac$dir <- 'up'
    } else if (game$pac$next_dir == 'down' && move_down[32 - game$pac$row, game$pac$col]) {
      game$pac$dir <- 'down'
    }
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # if the pacman's direction is not possible, then put him in 'rest' state
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (game$pac$dir == 'left' && !move_left[32 - game$pac$row, game$pac$col]) {
      game$pac$dir <- 'rest'
    } else if (game$pac$dir == 'right' && !move_right[32 - game$pac$row, game$pac$col]) {
      game$pac$dir <- 'rest'
    } else if (game$pac$dir == 'up' && !move_up[32 - game$pac$row, game$pac$col]) {
      game$pac$dir <- 'rest'
    } else if (game$pac$dir == 'down' && !move_down[32 - game$pac$row, game$pac$col]) {
      game$pac$dir <- 'rest'
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Decide on the deltax and deltay movement for the next 8 frames
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (game$pac$dir == 'left' ) {game$pac$dx = -1; game$pac$dy =  0} else
    if (game$pac$dir == 'right') {game$pac$dx =  1; game$pac$dy =  0} else
    if (game$pac$dir == 'up'   ) {game$pac$dx =  0; game$pac$dy =  1} else
    if (game$pac$dir == 'down' ) {game$pac$dx =  0; game$pac$dy = -1} else
    {game$pac$dx = 0; game$pac$dy = 0}
  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # For each ghost:
    #  - if at a junction, choose a random new direction
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for (i in seq_along(game$gh)) {
      if (junction[32 - game$gh[[i]]$row, game$gh[[i]]$col]) {
        dir <- choose_direction(game$gh[[i]]$row, game$gh[[i]]$col, game$gh[[i]]$dir)
        game$gh[[i]]$dir <- dir
        if (dir == 'left' ) {game$gh[[i]]$dx = -1; game$gh[[i]]$dy =  0} else
        if (dir == 'right') {game$gh[[i]]$dx =  1; game$gh[[i]]$dy =  0} else
        if (dir == 'up'   ) {game$gh[[i]]$dx =  0; game$gh[[i]]$dy =  1} else
        if (dir == 'down' ) {game$gh[[i]]$dx =  0; game$gh[[i]]$dy = -1} else
        {game$gh[[i]]$dx = 0; game$gh[[i]]$dy = 0}
      }
    }
  }
  

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Move pacman/ghosts from one location to the next in 8 steps.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {
    # Copy blank board into our working buffer
    nr_copy_into(board_nr, blank_board_nr)

    # Blit current dots into board
    nr_blit(board_nr, (game$dots$x - 0.5) * 8, (game$dots$y - 0.5) * 8, dot_nr)

    # Blit ghosts into board
    for (i in seq_along(game$gh)) {
      nr_blit(
        board_nr,
        x = game$gh[[i]]$col * 8 - 11 + step * game$gh[[i]]$dx,
        y = game$gh[[i]]$row * 8 - 11 + step * game$gh[[i]]$dy,
        ghost[[i]][[game$gh[[i]]$dir]][[ bitwShiftR(step, 1L) %% 2 + 1L]]
      )
    }

    # Blit pacman into board
    if (!game$over) {
      nr_blit(
        board_nr,
        x = game$pac$col * 8 - 11 + step * game$pac$dx,
        y = game$pac$row * 8 - 11 + step * game$pac$dy,
        pacman[[game$pac$dir]][[ bitwShiftR(step, 1L) %% 4 + 1L]]
      )
      
      # Show Lives remaining
      for (i in seq(game$lives)) {
        nr_blit(board_nr, x = (26 - 2*i) * 8, y = 31 * 8, pacman$right[[2]])
      }
    }
    
    # Show Score
    nr_text(board_nr, paste0("SCORE: ", game$score), x = 2 * 8, y = 31 * 8, 'white', fontsize = 16)
    
    if (game$over) {
      if (game$complete) {
        nr_text(board_nr, "YOU ", x = 12 * 8, y = 16.5 * 8, 'white', fontsize = 16)
        nr_text(board_nr, "WON!", x = 12 * 8, y = 14.5 * 8, 'white', fontsize = 16)
      } else {
        nr_text(board_nr, "GAME", x = 12 * 8, y = 16.5 * 8, 'white', fontsize = 16)
        nr_text(board_nr, "OVER", x = 12 * 8, y = 14.5 * 8, 'white', fontsize = 16)
      }
    }
    
    # Render to screen
    grid.raster(board_nr)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Play intro music the first time the board is drawn
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (frame_num == 1) {
    dev.flush()
    audio::play(sound$intro)
    Sys.sleep(4.5)
  }
  
  
  if (step == 8L) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # After every 8th step, the sprite is at the next location.
    # Update pacman state to new row/col
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!game$over) {
      game$pac$row <- game$pac$row + game$pac$dy
      game$pac$col <- (game$pac$col - 1 + game$pac$dx) %% 28 + 1
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update ghost states to the new row/col
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for (i in seq_along(game$gh)) {
      game$gh[[i]]$row <- game$gh[[i]]$row + game$gh[[i]]$dy
      game$gh[[i]]$col <- (game$gh[[i]]$col - 1 + game$gh[[i]]$dx) %% 28 + 1
    }
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run the game within an event loop
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
eventloop::run_loop(update_game, width = 7, height = 8, fps_target = 50)






