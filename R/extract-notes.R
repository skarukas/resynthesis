## load packages and local libraries
source("yin.R")
source("util.R")
source("transitions.R")
source("display.R")
source("spectrogram.R")

### given a file, partition it into transitions and sustains
###     saved to .transition and .sustain folders
extract_notes = function(file, dur = 30, min_note_dur = 0.1, max_trans_dur = 0.1, N = 1024) {

    # load file and truncate to `dur` seconds
    w = readWave(file)
    x = w@left
    SR = w@samp.rate
    bits = w@bit
    len = min(dur*SR, length(x))
    x = x[1:len]
    x_right = w@right[1:len]
    x_right = x_right/max(abs(x_right))
    x = x/max(abs(x))


    ## get time-amp estimation
    X = stft(x)
    amp = colMeans(Mod(X))
    amp = amp/max(amp)
    amp = resample(amp, length(amp)/length(x))

    ## get time-f0 estimation
    print("Running pyin...")
    f0_bin = yin(x, N=N, SR=SR)
    f0 = resample(f0_bin, length(f0_bin)/len)

    # convert to pitch
    pitch = ftom(f0)
    pitch[!is.finite(pitch)] = 0

    ## detect notes
    print("Detecting notes...")
    min_dur = SR * min_note_dur
    partition = partition.pitch_deviation(pitch, time_thresh=min_dur)
    starts = rep(0, 0)
    ends = rep(0, 0)
    notes = rep(0, 0)
    # filter by amplitude
    for (i in 1:length(partition$starts)) {
        start = partition$starts[i]
        end = partition$ends[i]
        note = partition$notes[i]
        if (mean(amp[start:end]) > 0.01) {
            starts = c(starts, start)
            ends = c(ends, end)
            notes = c(notes, note)
        }
    }
    print(sprintf("Detected %s notes.", length(starts)))

    ## detect transitions
    print("Detecting transitions...")
    max_t_dur = SR * max_trans_dur
    padding = SR * 0.05
    t_starts = rep(0, 0)
    t_ends = rep(0, 0)
    t_names = rep(0, 0)
    for (i in 2:length(starts)) {
        if (starts[i] - ends[i-1] < max_t_dur) {
            t_starts = c(t_starts, max(ends[i-1] - padding, 0))
            t_ends = c(t_ends, min(starts[i] + padding, len))
            name = sprintf("%s->%s", notes[i-1], notes[i])
            t_names = c(t_names, name)
        }
    }
    print(sprintf("Detected %s transitions.", length(t_starts)))


    unlink(".sustain", recursive = T)
    unlink(".transition", recursive = T)

    dir.create(".sustain")
    dir.create(".transition")

    print("Writing notes to .sustain...")
    setwd(".sustain")
    ## write notes
    for (i in 1:length(starts)) {
        rng = starts[i]:ends[i]
        left = x[rng]
        right = x_right[rng]
        midi_pitch = mean(pitch[rng])
        name = sprintf("[%s]@%s.wav", midi_pitch, starts[i])
        write_float(left, right, SR, bits, normalize=F, name=name)
    }

    print("Writing transitions to .transition...")
    setwd("../.transition")
    ## write transitions
    for (i in 1:length(t_starts)) {
        rng = t_starts[i]:t_ends[i]
        left = x[rng]
        right = x_right[rng]
        name = name = sprintf("[%s]@%s.wav", t_names[i], t_starts[i])
        write_float(left, right, SR, bits, normalize=F, name=name)
    }
    setwd("..")


    print("Displaying analysis...")
    
    ## display spectrogram
    spectrogram(x, N)

    # overlay detected sustains
    #rect(starts/len, 0, ends/len, 1, col=rgb(0, 1, 0, 0.2))
    # overlay detected transitions
    #rect(t_starts/len, 0, t_ends/len, 1, col=rgb(1, 1, 0, 0.2))

    # overlay frequency = cyan
    unit_display(f0, max_y=SR/8, col="red")

    # overlay amplitude = red
    #unit_display(amp, max_y=1, col="red")

    # overlay spectral flux = orange
    #flux = spectral_flux(X)
    #flux = resample(flux, length(flux)/len)
    #unit_display(flux, max_y=max(flux), col="orange")

    # overlay delta pitch = yellow
    #dpitch = abs(diff(pitch))
    #unit_display(dpitch, max_y=0.03, col="yellow")


    ## synthesize sinusoidal model
    #delta = 1/SR
    #y = amp * cos(2*pi*delta*cumsum(f0))
    # normalize and play them together
    #playback_float(x, y)
    #playback_float(y)
}