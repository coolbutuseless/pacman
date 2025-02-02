
library(grid)
library(nara)
library(purrr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Helper function: Reverse the characters in a string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
str_rev <- function(x) {
  paste(rev(strsplit(x, '')[[1]]), collapse="")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper function: Swap 2 characters in a string.  
# Dodgy implementation. Good enough for now.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
str_swap <- function(x, s1, s2) {
  x <- gsub(s1 , 'x', x)
  x <- gsub(s2 ,  s1, x)
  x <- gsub('x',  s2, x)
  x
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define the left half of the board
# Board is symmetrical.
# Full board = 31 x 28
#
# Define "obstruction" pieces.  e.g. "1" represents a rounded quarter-circle
# corner in the top-left of a square
#
#   1 2 3
#   4 5 6
#   7 8 9
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
left <- c(
"12222222222223", 
"4............6", 
"4.1223.12223.6", 
"4.6554.45556.6", 
"4.7889.78889.7", 
"4.............", 
"4.1223.13.1222", 
"4.7889.46.7883", 
"4......46....4", 
"788883.47883.4", 
"555556.41229.7", 
"555556.46.....", 
"555556.46.1222",
"888889.79.4555",
"..........4555", # Middle
"888883.13.4555", 
"555556.46.7888", 
"555556.46.....",
"555556.46.1222",
"122229.79.7883",
"4............6",
"4.1223.12223.6",
"4.7834.78889.7",
"4...46........",
"783.46.13.1222",
"129.79.46.7883",
"4......46....6",
"4.1222297223.6",
"4.7888888889.7",
"4.............",
"78888888888888"
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mirror the left of the board and update the tile references
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
right <- purrr::map_chr(left, str_rev)
right <- str_swap(right, '1', '3')
right <- str_swap(right, '4', '6')
right <- str_swap(right, '7', '9')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rearracnge board into a 31*28 character matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
board <- purrr::map2_chr(left, right, ~paste0(.x, .y, collapse = ""))
board <- unlist(stringr::str_split(board, ''))
board <- matrix(board, nrow = 31, ncol = 28, byrow = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Load the maze parts
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sm <- fastpng::read_png("image/game-maze.png", type = 'nativeraster')
# im <- suppressWarnings(png::readPNG("image/game-maze.png"))
if (FALSE) {
  dim(im)
  grid.raster(im)
  
  plot(sm, TRUE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse out the maze sprites
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
s <- vector('list', 9)

row <- 1; col <- 7; s[[1]] <- nr_crop(sm, 225 + col*9, 27 + row*9, 8, 8)
row <- 1; col <- 5; s[[2]] <- nr_crop(sm, 225 + col*9, 27 + row*9, 8, 8)
row <- 1; col <- 6; s[[3]] <- nr_crop(sm, 225 + col*9, 27 + row*9, 8, 8)
row <- 1; col <- 8; s[[4]] <- nr_crop(sm, 225 + col*9, 27 + row*9, 8, 8)
row <- 2; col <-12; s[[5]] <- nr_crop(sm, 225 + col*9, 27 + row*9, 8, 8)
row <- 1; col <- 9; s[[6]] <- nr_crop(sm, 225 + col*9, 27 + row*9, 8, 8)
row <- 1; col <-11; s[[7]] <- nr_crop(sm, 225 + col*9, 27 + row*9, 8, 8)
row <- 1; col <- 5; s[[8]] <- nr_crop(sm, 225 + col*9, 27 + row*9, 8, 8)
row <- 1; col <-10; s[[9]] <- nr_crop(sm, 225 + col*9, 27 + row*9, 8, 8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a montage of the maze pieces
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nr <- nr_new(3*8 + 4, 3*8 + 4, fill = 'white')
nr_blit(dst = nr, src = s[[1]], x = 1 + 0 * 8 + 0, y = 1 + 2 * 8 + 2, vjust = 0, hjust = 0)
nr_blit(dst = nr, src = s[[2]], x = 1 + 1 * 8 + 1, y = 1 + 2 * 8 + 2, vjust = 0, hjust = 0)
nr_blit(dst = nr, src = s[[3]], x = 1 + 2 * 8 + 2, y = 1 + 2 * 8 + 2, vjust = 0, hjust = 0)
nr_blit(dst = nr, src = s[[4]], x = 1 + 0 * 8 + 0, y = 1 + 1 * 8 + 1, vjust = 0, hjust = 0)
nr_blit(dst = nr, src = s[[5]], x = 1 + 1 * 8 + 1, y = 1 + 1 * 8 + 1, vjust = 0, hjust = 0)
nr_blit(dst = nr, src = s[[6]], x = 1 + 2 * 8 + 2, y = 1 + 1 * 8 + 1, vjust = 0, hjust = 0)
nr_blit(dst = nr, src = s[[7]], x = 1 + 0 * 8 + 0, y = 1 + 0 * 8 + 0, vjust = 0, hjust = 0)
nr_blit(dst = nr, src = s[[8]], x = 1 + 1 * 8 + 1, y = 1 + 0 * 8 + 0, vjust = 0, hjust = 0)
nr_blit(dst = nr, src = s[[9]], x = 1 + 2 * 8 + 2, y = 1 + 0 * 8 + 0, vjust = 0, hjust = 0)

if (FALSE) {
  plot(nr, T)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a blank native raster for the board
# 31 squares high. 28 squares wide
# Extra space on top for score + lives
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
blank_board_nr <- nr_new(width = 28 * 8, height = (31 + 2) * 8, fill = 'black')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Copy the appropriate maze piece into the board
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (row in 1:31) {
  for (col in 1:28) {
    val <- board[row, col]
    if (val != '.') {
      idx <- as.integer(val)
      nr_blit(dst = blank_board_nr, src = , s[[idx]], x = (col - 1) * 8 + 1, y = (row - 1) * 8 + 1,
              hjust = 0, vjust = 0)
    }
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Draw the blank board
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  plot(blank_board_nr, TRUE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dots <- arrayInd(which(board == '.'), dim(board))
dots <- as.data.frame(dots)
names(dots) <- c('y', 'x')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a 2x2 pixel nativeraster to represented the dot onscreen
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dot_nr  <- nara::nr_new(2, 2, 'white')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test drawing the dots over the blank board
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  nr <- nr_duplicate(blank_board_nr)  
  nr_blit(dst = nr, src = dot_nr, x = (dots$x - 0.5) * 8, y = (dots$y - 0.5) * 8, hjust = 0, vjust = 0)
  dev.hold()
  plot(nr, TRUE)
  dev.flush()  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create matrices of allowable movements at each '.' location
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
roll_down  <- rbind(board[-1, ], rep(NA, 28)) 
roll_up    <- rbind(rep(NA, 28), board[-nrow(board), ]) 
roll_right <- cbind(board[,-1], board[,1])
roll_left  <- cbind(board[,ncol(board)], board[,-ncol(board)])

move_left  <- board == '.' & roll_left  == '.'
move_right <- board == '.' & roll_right == '.'
move_up    <- board == '.' & roll_up    == '.'
move_down  <- board == '.' & roll_down  == '.'

moves <- list(
  left  = move_left ,
  right = move_right,
  up    = move_up   ,
  down  = move_down 
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# matrix of junctions where ghosts can choose a new direction
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
junction <- (move_left | move_right) & (move_up | move_down) #&
  # (move_left + move_right + move_up + move_down > 2)

mode(move_left)  <- 'integer'
mode(move_right) <- 'integer'
mode(move_up)    <- 'integer'
mode(move_down)  <- 'integer'
mode(junction)   <- 'integer'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot junction locations where ghost movements may be changed
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  coords <- arrayInd(which(junction == 1), dim(junction))
  junction_nr <- nr_duplicate(blank_board_nr)
  nr_rect(junction_nr, coords[,2]*8 - 3, (coords[,1])*8 - 3, 2, 2, 'white')
  plot(junction_nr, TRUE)
}



