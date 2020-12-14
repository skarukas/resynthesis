# resynthesis
Monophonic audio segmentation and resynthesis--using a recording of a solo instrument to synthesize new audio (through audio splicing). Final Project for B557 Audio Processing.

The report for this project is in `report.pdf`, and the slides are in `presentation.pdf`.

Audio results are available in the `synthesized` folder.

All code is located in the `R` folder. A demo for how to use the project is in `R/demo.R`. Here is the general idea:

Create a file in the same folder, and import the extraction and synthesis code.
```R
source("extract-notes.R")
source("synthesize.R")

## select a file and how long to analyze (put Inf for the whole file)
# this will take a while
# it adds WAV files to R/.sustain and R/.transition
extract_notes('my-wav-file.wav', dur=60)

# load the stored files into the synthesis part
load_files()

midi = # vector of MIDI pitches #
durs = # vector of durations in seconds #
synthesize(midi, durs)

```
