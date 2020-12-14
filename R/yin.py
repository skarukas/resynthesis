from numpy import array
import librosa

def track_pitch(x, sr, frame_length, fmin=165, fmax=1500):
    x = array(x)
    x_arr = x / max(abs(x))
    return librosa.pyin(x_arr, sr=sr, frame_length=int(frame_length), fmin=fmin, fmax=fmax)[0]