# Gravar audio do mic
arecord -f U8 -r 8000 -t raw audio.raw

# Ouvir o audio
aplay -f U8 -r 8000 -t raw audio.raw
