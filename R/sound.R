
library(audio)


# Tweak the audio to be compatible with the audio package
# sox -r 8000 -b 8 -e signed-integer -c 1 %s -b 16 %s
# sox %s -b 16 %s

sound <- list(
  intro = audio::load.wave(system.file("./sound/intro.wav", package = "pacman")),
  chomp = audio::load.wave(system.file("./sound/chomp.wav", package = "pacman")),
  death = audio::load.wave(system.file("./sound/death.wav", package = "pacman")),
  ghost = audio::load.wave(system.file("./sound/ghost.wav", package = "pacman")),
  extra = audio::load.wave(system.file("./sound/extra.wav", package = "pacman")),
  fruit = audio::load.wave(system.file("./sound/fruit.wav", package = "pacman")),
  inter = audio::load.wave(system.file("./sound/inter.wav", package = "pacman"))
)

# audio::play(sound$inter)
