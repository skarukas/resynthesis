if (!require("reticulate")) install.packages("reticulate", repo = 'https://mac.R-project.org')
library(reticulate)
source_python("yin.py")
source("util.R")

# get fundamental freq using YIN algorithm
yin = function(x, fmin=60, fmax=3000, SR=SR, N=N) {
    # track_pitch() from yin.py
    f0 = track_pitch(x, sr=SR, frame_length=N, fmin=fmin, fmax=fmax)
    f0[is.nan(f0)] = 0
    return(f0)
}