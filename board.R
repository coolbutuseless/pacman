
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
im <- suppressWarnings(png::readPNG("image/game-maze.png"))
if (FALSE) {
  dim(im)
  grid.raster(im)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse out the maze sprites
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
s <- vector('list', 9)
row <- 1; col <- 7; s[[1]] <- im[28 + row*9 + 0:7, 226 + col*9 + 0:7,]
row <- 1; col <- 5; s[[2]] <- im[28 + row*9 + 0:7, 226 + col*9 + 0:7,]
row <- 1; col <- 6; s[[3]] <- im[28 + row*9 + 0:7, 226 + col*9 + 0:7,]

row <- 1; col <- 8; s[[4]] <- im[28 + row*9 + 0:7, 226 + col*9 + 0:7,]
row <- 2; col <-12; s[[5]] <- im[28 + row*9 + 0:7, 226 + col*9 + 0:7,]
row <- 1; col <- 9; s[[6]] <- im[28 + row*9 + 0:7, 226 + col*9 + 0:7,]

row <- 1; col <-11; s[[7]] <- im[28 + row*9 + 0:7, 226 + col*9 + 0:7,]
row <- 1; col <- 5; s[[8]] <- im[28 + row*9 + 0:7, 226 + col*9 + 0:7,]
row <- 1; col <-10; s[[9]] <- im[28 + row*9 + 0:7, 226 + col*9 + 0:7,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert from array to nativeraster
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
s <- lapply(s, nara::array_to_nr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a montage of the maze pieces
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nr <- nr_new(3*8 + 3, 3*8 + 3, fill = 'white')
nr_blit(nr, 1 + 0 * 8 + 0, 1 + 2 * 8 + 2, s[[1]])
nr_blit(nr, 1 + 1 * 8 + 1, 1 + 2 * 8 + 2, s[[2]])
nr_blit(nr, 1 + 2 * 8 + 2, 1 + 2 * 8 + 2, s[[3]])

nr_blit(nr, 1 + 0 * 8 + 0, 1 + 1 * 8 + 1, s[[4]])
nr_blit(nr, 1 + 1 * 8 + 1, 1 + 1 * 8 + 1, s[[5]])
nr_blit(nr, 1 + 2 * 8 + 2, 1 + 1 * 8 + 1, s[[6]])

nr_blit(nr, 1 + 0 * 8 + 0, 1 + 0 * 8 + 0, s[[7]])
nr_blit(nr, 1 + 1 * 8 + 1, 1 + 0 * 8 + 0, s[[8]])
nr_blit(nr, 1 + 2 * 8 + 2, 1 + 0 * 8 + 0, s[[9]])

if (FALSE) {
  grid.newpage()
  grid.raster(nr, interpolate = FALSE)
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
    val <- board[31 + 1 - row, col]
    if (val != '.') {
      idx <- as.integer(val)
      nr_blit(blank_board_nr, (col - 1) * 8 + 1, (row - 1) * 8 + 1, s[[idx]])
    }
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Draw the blank board
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  grid.newpage()
  grid.raster(blank_board_nr, interpolate = FALSE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dots <- arrayInd(which(board == '.'), dim(board))
dots[,1] <- 32 - dots[,1] # flip y
dots <- as.data.frame(dots)
names(dots) <- c('y', 'x')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a 2x2 pixel nativeraster to represented the dot onscreen
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dot_mat <- matrix(rep('white', 4), 2, 2)
dot_nr  <- nara::raster_to_nr(dot_mat)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test drawing the dots over the blank board
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  nr <- nr_duplicate(blank_board_nr)  
  nr_blit(nr, (dots$x - 0.5) * 8, (dots$y - 0.5) * 8, dot_nr)
  dev.hold()
  grid.newpage()
  grid.raster(nr, interpolate = FALSE); 
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
  nr_rect(junction_nr, coords[,2]*8 - 3, (32 - coords[,1])*8 - 3, 2, 2, 'white')
  grid.raster(junction_nr, interpolate = FALSE)
}

