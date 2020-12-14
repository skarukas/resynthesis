source("extract-notes.R")
source("synthesis.R")

## extract notes to file system
extract_notes('samples/partita_flute.wav', dur=60, min_note_dur = 0.5, max_trans_dur=0.1)

## load notes to synth
load_files()


## amazing grace
midi = c(55, 60, 64, 62, 60, 64, 62, 60, 57, 55)
durs = c(1, 2, 1/2, 1/6, 1/3, 2, 1, 2, 1, 2) 
z = synthesize(midi, durs)
#write_float(z$left, z$right, name="../synthesized/ag-flute.wav", SR=16000, bits=16)

## scale
midi = c(0, 2, 4, 5, 7, 9, 11, 12, 11, 9, 7, 5, 4, 2, 0) + 66
durs = rep(0.5, 20)
## recover samples from file system and play back
z = synthesize(midi, durs)

write_float(z$left, z$right, name="../synthesized/scale-flute.wav", SR=16000, bits=16)

midi = c(0, 2, 4, 5, 7, 9, 11, 12, 11, 9, 7, 5, 4, 2, 0) + 78
durs = rep(0.5, 20)
## recover samples from file system and play back
z = synthesize(midi, durs)

write_float(z$left, z$right, name="../synthesized/scale-flute2.wav", SR=16000, bits=16)