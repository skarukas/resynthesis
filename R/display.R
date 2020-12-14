# display the line graph on an existing graph with width/height 1
# (for use with spectrogram)
unit_display = function(z, max_y, col) {
    t = seq(0, 1, by=1/length(z))[1:length(z)]
    lines(t, z/max_y, col=col, lwd=2)
}

# display the points on an existing graph with width/height 1
# (for use with spectrogram)
unit_display_points = function(z, max_y, col) {
    t = seq(0, 1, by=1/length(z))[1:length(z)]
    points(t, z/max_y, col=col, pch=".")
}

# quickly show the FFT of a time-domain signal
#    for debugging
show_fft = function(y, overlay=F) {
    Y = fft(y[1:N]) / (N/2)
    z = Mod(Y[2:(N/2)])
    
    if (overlay) lines(z, col="red")
    else plot(z, type="l")
}