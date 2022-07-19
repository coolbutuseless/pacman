
library(audio)


# Tweak the audio to be compatible with the audio package
# sox -r 8000 -b 8 -e signed-integer -c 1 %s -b 16 %s
# sox %s -b 16 %s

sound <- list(
  intro = audio::load.wave("./sound/intro.wav"),
  chomp = audio::load.wave("./sound/chomp.wav"),
  death = audio::load.wave("./sound/death.wav"),
  ghost = audio::load.wave("./sound/ghost.wav"),
  extra = audio::load.wave("./sound/extra.wav"),
  fruit = audio::load.wave("./sound/fruit.wav"),
  inter = audio::load.wave("./sound/inter.wav")
)

# audio::play(sound$inter)