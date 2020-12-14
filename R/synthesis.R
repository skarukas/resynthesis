library(stringr)
library(tuneR)
source("util.R")

transitions = c()
sustains = c()

## load the WAV files from .transition
load_transitions = function() {
    if (!dir.exists(".transition")) stop(sprintf("\n No transition samples found in %s. \n Run extract_notes.R first.", getwd()))  
    
    setwd(".transition")
    transitions = list()
    intervals = NULL
    for (filename in dir()) {
        w = readWave(filename)
        bracket = str_extract(filename, "\\[.*\\]")
        pair_str = str_remove_all(bracket, "[\\[\\]]")
        pair = str_split_fixed(pair_str, n=2, "->")
        p1 = as.numeric(pair[1])
        p2 = as.numeric(pair[2])
        interval = round(p2-p1)
        tran = list(p1=p1, p2=p2, interval=interval, left=w@left, right=w@right, SR=w@samp.rate, bits=w@bit)
        intervals = c(intervals, interval)
        transitions[[length(transitions)+1]] = tran
    }
    print(unique(intervals))
    setwd("..")
    return(transitions)
}

## load the WAV files from .sustain
load_sustains = function() {
    if (!dir.exists(".sustain")) stop(sprintf("\n No sustain samples found in %s.\n Run extract_notes.R first.", getwd()))

    setwd(".sustain")
    sustains = list()
    for (filename in dir()) {
        w = readWave(filename)
        bracket = str_extract(filename, "\\[.*\\]")
        pair_str = str_remove_all(bracket, "[\\[\\]]")
        pitch = str_split_fixed(pair_str, n=2, "->")
        p = as.numeric(pitch[1])
        sus = list(pitch=p, left=w@left, right=w@right, SR=w@samp.rate, bits=w@bit)
        sustains[[length(sustains)+1]] = sus
    }
    setwd("..")
    
    pitches = sapply(sustains,"[[","pitch")
    sustains = sustains[order(pitches)]
    return(sustains)
}

## initialize
load_files = function() {
    transitions <<- load_transitions()
    sustains <<- load_sustains()
}

## add a new note after a stereo file
append_stereo = function(sus1, pitch, dur) {
    tran = get_transition(sus1$pitch, pitch)
    sus2 = get_sustain(pitch, dur)
    return(blend_transition_stereo(sus1, tran, sus2))
}

## crossfade between the first sustain, transition, and second sustain
blend_transition_stereo = function(sus1, tran, sus2) {
    l = length(tran$left) / 2
    left = crossfade(crossfade(sus1$left, tran$left, l), sus2$left, l)
    right = crossfade(crossfade(sus1$right, tran$right, l), sus2$right, l)

    left[is.na(left)] = 0
    right[is.na(right)] = 0

    return(list(
        left=left,
        right=right,
        pitch=sus2$pitch
    ))
}

## recover a transition from the samples
get_transition = function(pitch1, pitch2) {
    interval = round(pitch2 - pitch1)
    matched = c()
    for (tran in transitions) {
        if (tran$interval == interval) {
            left = match_pitch(tran$left, from=tran$p1, to=pitch1)
            right = match_pitch(tran$right, from=tran$p1, to=pitch1)
            return(list(
                left=left,
                right=right,
                interval=tran$interval
            ))
        }
    }
    print("did not find")
    print(interval)
    return(list(
        left=rep(0, 1000),
        right=rep(0, 1000),
        interval=interval
    ))
}

## pitch shift a signal from `from` pitch to `to` pitch
match_pitch = function(z, from, to) {
    ratio = 2**((to-from) / 12)
    return(pitch_shift(z, ratio))
}

## recover a sustain from the samples
get_sustain = function(pitch, dur) {
    i = 1
    while (i <= length(sustains) && pitch > sustains[[i]]$pitch) i = i+1
    idx = c(i-1, i)
    if (i == 1) idx[1] = 1
    else if (i > length(sustains)) idx[2] = i-1
    idx[1] = idx[2]
    notes = sustains[idx]
    note1 = notes[[1]]
    note2 = notes[[2]]

    note1_l = match_pitch(note1$left, from=note1$pitch, to=pitch)
    note1_r = match_pitch(note1$right, from=note1$pitch, to=pitch)
    note2_l = match_pitch(note2$left, from=note2$pitch, to=pitch)
    note2_r = match_pitch(note2$right, from=note2$pitch, to=pitch)

    r = (pitch - note1$pitch) / (note2$pitch - note1$pitch)
    if (is.infinite(r)) r = 0.5

    left = (1-r) * note1_l + r * note2_l
    right = (1-r) * note1_r + r * note2_r
    left = note1_l
    right = note1_r

    left = resize(left, dur)
    right = resize(right, dur)

    return(list(
        left=left,
        right=right,
        pitch=pitch
    ))
}


## either truncate or time stretch the signal
resize = function(z, dur) {
    if (length(z) >= dur) {
        return(z[1:dur])
    } else {
        return(time_stretch(z, dur/length(z)))
    }
}

## recover samples and playback synthesized notes
synthesize = function(pitches, durs) {
    samp_durs = durs * 16000
    z = get_sustain(pitches[1], samp_durs[1])
    for (i in 2:length(pitches)) z = append_stereo(z, pitches[i], samp_durs[i])
    playback_float(z$left, z$right)

    return(z)
}