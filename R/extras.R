### some extra code

# use window to average these areas
win_len = 0.1 * SR
sparse_mask = rep(F, length(mask))

for (i in transitions) {
    if (mask[i] && amp[i] > 0.01) {
        trues = which(mask[i:(i+win_len)]) + (i-1)
        avg_line = floor(mean(trues))
        sparse_mask[avg_line] = T
        mask[i:(i+win_len)] = F
    }
}

sparse_transitions = which(sparse_mask)
tran_x = sparse_transitions / len
abline(v=tran_x, col="yellow")


extract_transitions = function() {
    for (idx in sparse_transitions) {
        start = idx-win_len
        end = idx+win_len
        if (start > 0 && end <= len && f0[start] && f0[end]) {
            left = x[start:end]
            right = x_right[start:end]
            write_float(left, right, paste(f0[start], "-", f0[end], ".wav"))
        }
    }
}

# overlay frequency = cyan
alpha = abs(d_f0) < 0.01 & f0[1:length(d_f0)] != 0
display02(f0, max_y=SR/2, col=rgb(1, 1, 0, alpha))



### overlayed spectrograms


### extras for "aligning" spectra

extract_timbre = function(row, freq, num_harms=10) {
    if (freq == 0 || is.na(freq)) return(rep(0, num_harms))
    ## get the amplitudes of the first 10 harmonics
    sd = 2
    N = length(row)
    b = freq*N/SR
    tplt = rep(0, N/2)
    timbre = NULL
    for (h in seq(1, num_harms)) {
        bin = b * h
        lo = floor(max(bin-2, 0))
        hi = floor(min(bin+2, N/2))
        amp = sum(row[lo:hi])
        timbre = c(timbre, amp)
        #tplt = tplt + dnorm(1:(N/2),mean=b*h,sd=sd)  # peak is normal shaped
    }
    return(timbre)
}

num_harms = 10
Y = matrix(nrow=num_harms)
f0_bin = resample(f0_bin, length(f0_bin)/ncol(X))

for (t in 1:ncol(X)) Y = cbind(Y, extract_timbre(Mod(X[,t]), f0_bin[t]))

timbre_to_spectrum = function(timbre, freq, N) {
    num_harms = length(timbre)
    row = rep(0, N)
    b = freq*N/SR
    for (h in 1:num_harms) {
        bin = floor(b*h)
        print(bin)
        row[bin] = timbre[h]
    }
    return(row)
}

Z = matrix(nrow=N)

for (t in 1:ncol(X)) Z = cbind(Z, timbre_to_spectrum(Y[,t], f0_bin[t], N))

spectrogram(Y, num_harms)


get_local_FFT = function(z, start, f, N) {
    if (f == 0 || is.na(f)) return(rep(0, N))
    k = 5 # number of subharmonics of f
    M = floor((SR/f) * k) * 2
    Z = fft(z[start:(start+M-1)])
    print("uh")
    print(M == length(Z))
    res = local_to_fixed(Z, N)
    print(N == length(res))
    return(res)
}

local_to_fixed = function(Z, N) {
    # "convert" to an FFT of size N
    M = length(Z)
    extra = N - M
    if (extra > 0) {
        padding = rep(0, extra)
        return(c(Z[1:(M/2)], padding, Z[(M/2+1):M]))
    } else {
        print("overflow")
        print(M)
        return(c(Z[1:(N/2)], Z[(1+M-N/2):M]))
    }
}

fixed_to_local = function(Z, f) {
    if (f == 0 || is.na(f)) f = 200

    k = 5 # number of subharmonics of f
    M = floor((SR/f) * k) * 2
    N = length(Z)
    extra = (N - M) / 2 # assume N > M
    if (N > M) {
        print("N>M")
        Z_slice = c(Z[1:(M/2)], Z[(1+N-M/2):N])
    } else {
        padding = rep(0, M - N)
        print("N<M")
        return(c(Z[1:(N/2)], padding, Z[(N/2+1):N]))
    }
    print(M==length(Z_slice))
    print(M)
    print(length(Z_slice))
    z = fft(Z_slice, inverse=T)
    return(Mod(z))
}

Z = NULL
for (start in seq(1, len, by=N/4)) Z = cbind(Z, get_local_FFT(x, start, f0[start], 2056))

start = 150000 / (N/4)
end = 200000 / (N/4)
z = NULL

for (t in 1:end) {
    #L = get_local_FFT(x, start, f0[start], 2056)
    #l = fixed_to_local(L, f0[start])
    #print(l)
    i = t * (N/4)
    portion = fixed_to_local(Z[,t], f0[i])
    z = c(z, Mod(portion))
}

stopp;

dur = 0.5 * SR
i = 1
while (i <= length(f0)) {
    start = i
    while (i <= length(f0) && alpha[i]) i = i + 1
    if (i - start >= dur && i <= length(f0)) {
        playback_float(x[start:i])
    }
    i = i + 1
}