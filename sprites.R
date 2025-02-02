
library(grid)
library(nara)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the spritemap for pacman and the ghosts
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# spritemap <- png::readPNG("image/game-sprites.png")
sm <- fastpng::read_png("image/game-sprites.png", type = 'nativeraster')

if (FALSE) {
  dim(spritemap)
  grid.raster(spritemap)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Extract a sprite from the spritemap
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
extract_old <- function(row, col) {
  sprite <- spritemap[1 + (row-1)*16 + 0:15, 457 + (col-1)*16 + 0:15,]
  
  alpha <- sprite[,,1] == 1 | sprite[,,2] == 1 | sprite[,,3] == 1 
  alpha[] <- as.numeric(alpha)
  
  new <- c(sprite, alpha)
  d <- dim(sprite)
  d[3] <- 4
  dim(new) <- d
  
  nara::array_to_nr(new)
}

extract <- function(row, col) {
  sprite <- nara::nr_crop(sm, 456 + (col-1)*16, (row-1)*16, 16, 16)
  nara::nr_replace(sprite, -16777216L, 0L) # replace 'black' with 'transparent'
  sprite
}


if (FALSE) {
  extract (5, 2) |> plot(T)
  extract2(5, 2) |> plot(T)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract pacman sprites
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pacman <- list(
  right = list(extract(1, 1), extract(1, 2), extract(1, 3), extract(1, 2)),
  left  = list(extract(2, 1), extract(2, 2), extract(1, 3), extract(2, 2)),
  up    = list(extract(3, 1), extract(3, 2), extract(1, 3), extract(3, 2)),
  down  = list(extract(4, 1), extract(4, 2), extract(1, 3), extract(4, 2)),
  rest  = list(extract(1, 3), extract(1, 3), extract(1, 3), extract(1, 3))
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract ghost sprites
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ghost1 <- list(
  right = list(extract(5, 1), extract(5, 2)),
  left  = list(extract(5, 3), extract(5, 4)),
  up    = list(extract(5, 5), extract(5, 6)),
  down  = list(extract(5, 7), extract(5, 8))
)

ghost2 <- list(
  right = list(extract(6, 1), extract(6, 2)),
  left  = list(extract(6, 3), extract(6, 4)),
  up    = list(extract(6, 5), extract(6, 6)),
  down  = list(extract(6, 7), extract(6, 8))
)

ghost3 <- list(
  right = list(extract(7, 1), extract(7, 2)),
  left  = list(extract(7, 3), extract(7, 4)),
  up    = list(extract(7, 5), extract(7, 6)),
  down  = list(extract(7, 7), extract(7, 8))
)

ghost4 <- list(
  right = list(extract(8, 1), extract(8, 2)),
  left  = list(extract(8, 3), extract(8, 4)),
  up    = list(extract(8, 5), extract(8, 6)),
  down  = list(extract(8, 7), extract(8, 8))
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine all ghosts
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ghost <- list(
  ghost1 = ghost1, 
  ghost2 = ghost2, 
  ghost3 = ghost3, 
  ghost4 = ghost4
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test plot of a single sprite
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  grid.raster(ghost4$right[[1]], interpolate = FALSE)
  grid.newpage(); dev.hold(); grid.raster(pacman$left[[1]], interpolate = FALSE); dev.flush()
}












