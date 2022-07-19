
library(grid)
library(nara)
library(eventloop)

source("board.R")
source("sprites.R")

set.seed(1)

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 'pacman' state information
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pac <- list()
pac$dir      <- "rest"
pac$next_dir <- "rest"
pac$dx       <- 0
pac$dy       <- 0
pac$row      <- 2
pac$col      <- 2

game$pac <- pac

game$score <- 0
game$lives <- 5

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Current dots in the scene
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
game$dots <- dots

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ghost state information. This will be updated during the game
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
game$gh <- list(
  list(row = 14, col = 10, dx = 0, dy = 0, dir = "down"),
  list(row = 14, col = 19, dx = 0, dy = 0, dir = "down"),
  list(row = 17, col = 10, dx = 0, dy = 0, dir = "down"),
  list(row = 17, col = 19, dx = 0, dy = 0, dir = "down")
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create working buffer where the board will be rendered
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
board_nr <- nr_duplicate(blank_board_nr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
update_game <- function(event, frame_num, ...) {

  step <- ((frame_num - 1L) %% 8L) + 1L
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Monitor for keypress at every frame to ensure we don't miss it.
  # and just stow the preferred "next direction"
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(event) && event$type == 'key_press') {
    if (event$str == 'Left') {
      game$pac$next_dir <- 'left'
    } else if (event$str == 'Right') {
      game$pac$next_dir <- 'right'
    } else if (event$str == 'Up') {
      game$pac$next_dir <- 'up'
    } else if (event$str == 'Down') {
      game$pac$next_dir <- 'down'
    }
  }
  
  
  if (step == 1) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Eat the dot where the pacman is
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    matches <- (game$dots$y == game$pac$row & game$dots$x == game$pac$col)
    if (any(matches)) {
      # Bump the score if a dot was consumed
      game$score <- game$score + 10
    }
    game$dots <- game$dots[!matches,, drop = FALSE]

    
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
    # Inertia: next_dir always defaults to pacmans current direction
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # game$pac$next_dir <- game$pac$dir
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (game$pac$dir == 'left' ) {game$pac$dx = -1; game$pac$dy =  0} else
    if (game$pac$dir == 'right') {game$pac$dx =  1; game$pac$dy =  0} else
    if (game$pac$dir == 'up'   ) {game$pac$dx =  0; game$pac$dy =  1} else
    if (game$pac$dir == 'down' ) {game$pac$dx =  0; game$pac$dy = -1} else
    {game$pac$dx = 0; game$pac$dy = 0}
    
    # if (junction[32 - game$pac$row, game$pac$col]) {
    #   game$pac$dir <- choose_direction(game$pac$row, game$pac$col, game$pac$dir)
    #   if (game$pac$dir == 'left' ) {game$pac$dx = -1; game$pac$dy =  0} else
    #   if (game$pac$dir == 'right') {game$pac$dx =  1; game$pac$dy =  0} else
    #   if (game$pac$dir == 'up'   ) {game$pac$dx =  0; game$pac$dy =  1} else
    #   if (game$pac$dir == 'down' ) {game$pac$dx =  0; game$pac$dy = -1} else
    #   {game$pac$dx = 0; game$pac$dy = 0}
    # }
  
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
    nr_blit(board_nr, dot_nr, (game$dots$x - 0.5) * 8, (game$dots$y - 0.5) * 8)

    # Blit ghosts into board
    for (i in seq_along(game$gh)) {
      nr_blit(
        board_nr,
        ghost[[i]][[game$gh[[i]]$dir]][[ bitwShiftR(step, 1L) %% 2 + 1L]],
        x = game$gh[[i]]$col * 8 - 11 + step * game$gh[[i]]$dx,
        y = game$gh[[i]]$row * 8 - 11 + step * game$gh[[i]]$dy
      )
    }

    # Blit pacman into board
    nr_blit(
      board_nr,
      pacman[[game$pac$dir]][[ bitwShiftR(step, 1L) %% 4 + 1L]],
      x = game$pac$col * 8 - 11 + step * game$pac$dx,
      y = game$pac$row * 8 - 11 + step * game$pac$dy
    )
    
    # Show Score
    nr_text(board_nr, game$score, x = 2 * 8, y = 31 * 8, 'white', fontsize = 16)
    
    # Show Lives remaining
    for (i in seq(game$lives)) {
      nr_blit(board_nr, pacman$right[[2]], x = (26 - 2*i) * 8, y = 31 * 8)
    }
    
    # Render to screen
    grid.raster(board_nr)
  }

  if (step == 8L) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # After every 8th step, the sprite is at the next location.
    # Update pacman state to new row/col
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    game$pac$row <- game$pac$row + game$pac$dy
    game$pac$col <- game$pac$col + game$pac$dx
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update ghost states to the new row/col
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for (i in seq_along(game$gh)) {
      game$gh[[i]]$row <- game$gh[[i]]$row + game$gh[[i]]$dy
      game$gh[[i]]$col <- game$gh[[i]]$col + game$gh[[i]]$dx
    }
  }
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run the game within an event loop
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
eventloop::run_loop(update_game, width = 7, height = 8, fps_target = 50)






