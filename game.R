
source("board.R")
source("sprites.R")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Open a a double-buffered x11() device.
# - turn antialiasing off (don't need it for pixel rendering)
# - inibit storage of a displaylist. not needed.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x11(type = 'dbcairo', antialias = 'none')
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
# 'pacman' state information
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pacdir <- ""
dx     <- 0
dy     <- 0
row    <- 2
col    <- 2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Current dots in the scene
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cdots <- dots

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ghost state information. This will be updated during the game
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gh <- list(
  list(row = 14, col = 10, dx = 0, dy = 0, dir = "down"),
  list(row = 14, col = 19, dx = 0, dy = 0, dir = "down"),
  list(row = 20, col = 10, dx = 0, dy = 0, dir = "down"),
  list(row = 20, col = 19, dx = 0, dy = 0, dir = "down")
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
  matches <- (cdots$y == row & cdots$x == col)
  cdots <- cdots[!matches,, drop = FALSE]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If pacman at a junction, then decide new direction
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (junction[32 - row, col]) {
    dir <- choose_direction(row, col, pacdir)
    pacdir <- dir
    if (dir == 'left' ) {dx = -1; dy =  0} else
    if (dir == 'right') {dx =  1; dy =  0} else
    if (dir == 'up'   ) {dx =  0; dy =  1} else
    if (dir == 'down' ) {dx =  0; dy = -1} else
    {dx = 0; dy = 0}
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For each ghost:
  #  - if at a junction, choose a random new direction
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_along(gh)) {
    if (junction[32 - gh[[i]]$row, gh[[i]]$col]) {
      dir <- choose_direction(gh[[i]]$row, gh[[i]]$col, gh[[i]]$dir)
      gh[[i]]$dir <- dir
      if (dir == 'left' ) {gh[[i]]$dx = -1; gh[[i]]$dy =  0} else
      if (dir == 'right') {gh[[i]]$dx =  1; gh[[i]]$dy =  0} else
      if (dir == 'up'   ) {gh[[i]]$dx =  0; gh[[i]]$dy =  1} else
      if (dir == 'down' ) {gh[[i]]$dx =  0; gh[[i]]$dy = -1} else
      {gh[[i]]$dx = 0; gh[[i]]$dy = 0}
    }
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Move pacman/ghosts from one location to the next in 8 steps
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (step in 1:8) {

    # Copy blank board into our working buffer
    nr_copy_into(board_nr, blank_board_nr)

    # Blit current dots into board
    nr_blit(board_nr, dot_nr, (cdots$x - 0.5) * 8, (cdots$y - 0.5) * 8)

    # Blit ghosts into board
    for (i in seq_along(gh)) {
      nr_blit(
        board_nr,
        ghost[[i]][[gh[[i]]$dir]][[ bitwShiftR(step, 1L) %% 2 + 1L]],
        x = gh[[i]]$col * 8 - 11 + step * gh[[i]]$dx,
        y = gh[[i]]$row * 8 - 11 + step * gh[[i]]$dy
      )
    }

    # Blit pacman into board
    nr_blit(
      board_nr,
      pacman[[pacdir]][[ bitwShiftR(step, 1L) %% 4 + 1L]],
      x = col * 8 - 11 + step * dx,
      y = row * 8 - 11 + step * dy
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
  row <- row + dy
  col <- col + dx

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Advance ghosts to the next row/col
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_along(gh)) {
    gh[[i]]$row <- gh[[i]]$row + gh[[i]]$dy
    gh[[i]]$col <- gh[[i]]$col + gh[[i]]$dx
  }
}