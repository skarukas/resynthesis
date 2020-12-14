## adapted from spectrogram.R by Christopher Raphael

# "spectrogram" is a function getting two arguments: y is the audio data
# and N is the length of the fft.  Functions normally return something,
# however this function simply produces an image using the "image" command

spectrogram = function(y,N) {
  power = .2
  bright = seq(0,1,by=.01)^power
  grey = rgb(bright,bright,bright)        # this will be our color palate --- all grey
  frames = floor(length(y)/N)             # number of "frames" (like in movie)
  spect = matrix(0,frames,N/8)	          # initialize frames x N/2 spectrogram matrix to  0
					  # N/2 is # of freqs we compute in fft (as usual)
  v = seq(from=0,by=2*pi/N,length=N)      # N evenly spaced pts 0 -- 2*pi
  win = (1 + cos(v-pi))/2		  # Our Hann window --- could use something else (or nothing)
  for (t in 1:frames) {
    chunk  = y[(1+(t-1)*N):(t*N)]         # the  frame t of  audio data
    Y = fft(chunk*win)
#    Y = fft(chunk)
    spect[t,] = Mod(Y[1:(N/8)]) 
#    spect[t,] = log(1+Mod(Y[1:(N/2)])/1000)  # log(1 + x/1000) transformation just changes contrast
  }
  image(spect,col=grey)                   # show the image using the color map given by "grey"
  title(main="spectrogram and estimated f0 (pYIN)", xlab="time", ylab="frequency")
}

