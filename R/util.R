if (!require("tuneR")) install.packages("tuneR", repos="https://cran.r-project.org")
library(tuneR)

setWavPlayer("afplay")
SR = 16000
bits = 16
N = 1024
H = N/8


# creates a hann window of the specified length
hann = function(len) {
    t = seq(0, to=2*pi, by=2*pi/(len-1))
    hann = (1+cos(t-pi))/2
    return(hann)
}

# plays back a floating-point signal
playback_float = function(left, right=left, SR1=SR, bits1=bits, normalize=T) {
    if (normalize) {
        left = left/max(abs(left)) # normalize
        right = right/max(abs(right)) # normalize
    }
    left[!is.numeric(left)] = 0
    right[!is.numeric(right)] = 0
    #print(left)
    l = min(length(left), length(right))
    left_int = floor(left * 2^(bits-3))[1:l]
    right_int = floor(right * 2^(bits-3))[1:l]
    w = Wave(left_int, right_int, bit=bits1, samp.rate=SR1)
    play(w)
}

# write a floating-point signal
write_float = function(left, right=left, SR, bits, normalize=T, name="untitled.wav") {
    if (normalize) {
        left = left/max(abs(left)) # normalize
        right = right/max(abs(right)) # normalize
    }
    left[!is.numeric(left)] = 0
    right[!is.numeric(right)] = 0
    l = min(length(left), length(right))
    left_int = floor(left * 2^(bits-3))[1:l]
    right_int = floor(right * 2^(bits-3))[1:l]
    w = Wave(left_int, right_int, bit=bits, samp.rate=SR)
    writeWave(w, name)
}

# converts frequency to midi
ftom = function(fr) {
    return(69 + 12*log2(fr/440))
}

# converts midi to freq
mtof = function(mid) {
    return(440*2^((mid-69)/12))
}

# calculate the inverse STFT
istft = function(X) {
    l = ncol(X)
    x = rep(0, l*H + N)
    win = hann(N)
    for (t in 1:l) {
        start = (t-1)*H + 1
        end = start + N - 1
        chunk = fft(X[,t], inverse=T) / N
        x[start:end] = x[start:end] + Re(chunk)*win
    }
    return(x)
}

# calculate the STFT 
stft = function(x) {
    ncol = floor((length(x)-N) / H) + 1
    result = matrix(0, N, ncol)
    win = hann(N)
    for (t in 1:ncol) {
        start = (t-1)*H + 1
        end = start + N - 1
        result[,t] = fft(x[start:end]*win)
    }
    return(result)
}
    
# convert to delta phase STFT
to_dphase = function (X) {
    dp_X = matrix(0, nrow(X), ncol(X))
    for (i in 1:N) {
        row = X[i,]
        diffs = c(Arg(X[i,1]), diff(Arg(row)))
        dp_X[i,] = complex(modulus=Mod(row), argument=diffs)
    }
    return(dp_X)
}

# convert from delta phase STFT
from_dphase = function (dp_X) {
    X = matrix(0, nrow(dp_X), ncol(dp_X))
    for (i in 1:N) {
        row = dp_X[i,]
        sums = cumsum(Arg(row))
        X[i,] = complex(modulus=Mod(row), argument=sums)
    }
    return(X)
}

# convert to delta amp STFT
get_damp = function (X) {
    damp = matrix(0, nrow(X), ncol(X))
    for (i in 1:N) {
        row = X[i,]
        damp[i, ] = c(Mod(X[i,1]), diff(Mod(row)))
       
        #dp_X[i,] = complex(modulus=diffs, argument=Arg(row))
    }
    return(damp)
}

# convert from delta amp STFT
from_damp = function (dp_X) {
    X = matrix(0, nrow(dp_X), ncol(dp_X))
    for (i in 1:N) {
        row = dp_X[i,]
        sums = cumsum(Mod(row))
        X[i,] = complex(modulus=sums, argument=Arg(row))
    }
    return(X)
}
    
# resample `y`, making it play `scale` times its speed
resample = function(y, scale) {
    t = seq(1, length(y), by=scale)
    f = floor(t)
    c = ceiling(t)
    z = (c-t)*y[f] + (t-f)*y[c] + (c==f)*y[t]
    return(z)
}

# resample STFT `Y`, making it play `scale` times its speed
resample_stft = function(Y, scale) {
    dp_Y = to_dphase(Y)
    t = seq(1, ncol(dp_Y), by=scale)
    dp_Z = dp_Y[,ceiling(t)]
    return(from_dphase(dp_Z))    
}

# change freq without changing speed
pitch_shift = function(y, ratio) {
    y = c(rep(0, N), y, rep(0, N))
    Y = stft(y)
    Y_S = resample_stft(Y, 1/ratio)
    y_s = istft(Y)
    y_s = y_s[(N+1):(length(y_s)-N)]
    z = resample(y_s, ratio)
    return(z)
}

# stretch a time-domain signal without changing pitch
#    by a factor of `ratio` (> 1 means longer)
time_stretch = function(y, ratio) {
    Y = stft(y)
    Y_S = resample_stft(Y, 1/ratio)
    y_s = istft(Y_S)
    return(y_s)
}

# equal power crossfade two time-domain signals 
#    with `dur` amount of overlap in samples
crossfade = function(x, y, dur) {
    n = min(dur, length(x), length(y))
    z = c(x[1:(length(x)-n)], rep(0, n), y[(n+1):length(y)])
    t = seq(0, 1, by=1/n)[1:n]
    start = (length(x)-n+1)
    end = (start+n-1)
    z[start:end] = sqrt(1-t)*x[start:end] + sqrt(t)*y[1:n]
    return(z)
}

# loop a time-domain signal with crossfade 
#     until it fills `dur` samples
# **just an experiment, produces poor results because of phasing**
fill_time = function(x, dur) {
    l = length(x) / 2
    z = x
    while (length(z) <= dur) z = crossfade(z, x, l)
    return(z[1:dur])
}

## calculate the spectral flux of an STFT over time
# spectral flux = squared difference between consecutive normalized frames
spectral_flux <- function(STFT) {
    a = Mod(STFT)
    a_norm = scale(a, scale=1/colSums(a))
    diffs = t(diff(t(a_norm)))
    flux = colSums(diffs**2) # diff goes by row
    return(flux)
}