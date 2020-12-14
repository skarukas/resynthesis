## A greedy algorithm for detecting notes given a vector with a pitch at each sample
#  - continually expand a range as long as the range does not deviate more than
#      `pitch_thresh` away from the mean of the range's pitch.
#  - only notes longer than `time-thresh` will be selected
partition.pitch_deviation <- function(pitch, time_thresh, pitch_thresh=0.5) {
    ends = rep(0, 0)
    starts = rep(0, 0)
    notes = rep(0, 0)
    start = 1
    end = 1
    sm = pitch[1]

    for (i in 2:length(pitch)) {
        k = (end - start) + 1
        avg = sm/k
        if (abs(pitch[i] - avg) >= pitch_thresh || pitch[i] == 0) {
            # long enough, save note
            if (end - start > time_thresh) {
                ends = c(ends, i)
                starts = c(starts, start)
                notes = c(notes, avg)
            }
            start = i
            end = i
            sm = 0
        } else {
            end = end + 1
        }
        sm = sm + pitch[i]
    }
    # add final note (if any)
    if (end - start > time_thresh) {
        ends = c(ends, i)
        starts = c(starts, start)
        notes = c(notes, avg)
    }

    # shift the start position to account for the greedy beginning that
    #    may include transition pitch deviation
    shift = floor(time_thresh*0.1)
    res = list(starts=starts+shift, ends=ends, notes=notes)
    return(res)
}

## select the points where the change in pitch is over a threshold
#  and take these to be transition points
partition.d_pitch <- function(pitch) {
    d_pitch = diff(pitch)
    avg_dpitch = mean(abs(d_pitch))
    #print(avg_dpitch)
    mask = abs(d_pitch) > avg_dpitch
    transitions = which(mask)
    return(transitions)
}
