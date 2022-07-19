
source("board.R")
source("sprites.R")

set.seed(1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Open a a double-buffered x11() device.
# - turn antialiasing off (don't need it for pixel rendering)
# - inibit storage of a displaylist. not needed.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x11(type = 'dbcairo', antialias = 'none', width = 7, height = 8)
# dev.control(displaylist = 'inhibit')


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
pac$dir <- "rest"
pac$dx     <- 0
pac$dy     <- 0
pac$row    <- 2
pac$col    <- 2

game$pac <- pac

game$score <- 0

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
for (i in 1:100) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Eat the dot where the pacman is
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  matches <- (game$dots$y == game$pac$row & game$dots$x == game$pac$col)
  if (any(matches)) {
    game$score <- game$score + 10
  }
  game$dots <- game$dots[!matches,, drop = FALSE]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If pacman at a junction, then decide new direction
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (junction[32 - game$pac$row, game$pac$col]) {
    game$pac$dir <- choose_direction(game$pac$row, game$pac$col, game$pac$dir)
    if (game$pac$dir == 'left' ) {game$pac$dx = -1; game$pac$dy =  0} else
    if (game$pac$dir == 'right') {game$pac$dx =  1; game$pac$dy =  0} else
    if (game$pac$dir == 'up'   ) {game$pac$dx =  0; game$pac$dy =  1} else
    if (game$pac$dir == 'down' ) {game$pac$dx =  0; game$pac$dy = -1} else
    {game$pac$dx = 0; game$pac$dy = 0}
  }

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


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Move pacman/ghosts from one location to the next in 8 steps
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (step in 1:8) {

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

    # Draw everything to screen
    dev.hold()
    grid.raster(board_nr)
    dev.flush()

    # regulate drawing speed. Replace with {eventloop} for actual game
    Sys.sleep(0.01)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Advance pacman location to new row/col
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  game$pac$row <- game$pac$row + game$pac$dy
  game$pac$col <- game$pac$col + game$pac$dx

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Advance ghosts to the next row/col
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_along(game$gh)) {
    game$gh[[i]]$row <- game$gh[[i]]$row + game$gh[[i]]$dy
    game$gh[[i]]$col <- game$gh[[i]]$col + game$gh[[i]]$dx
  }
}