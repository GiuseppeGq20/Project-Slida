#%%
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.signal import welch
directory="./AG2_ramp/DATI/A20 PRE/"
filename=[f"A20_pre_{x}.csv" for x in range(1,11) ]

data=pd.read_csv(directory+filename[0])
data=data.to_numpy()
time=data[:,0]
data=data[:,1:]
#compute fft
datafft=np.fft.rfft(data, axis=0)
timestep=abs(time[1]-time[0])

fsamp=1/timestep

freq=np.fft.rfftfreq(len(data),d=timestep)
#compute energy
temp = np.abs(datafft)**2
temp
e=np.sum(temp, axis=0)
plt.plot(e[1:])
plt.show()

# plt.plot(freq,temp[:,0])
plt.plot(freq,temp[:,1])
plt.xlabel("Hz")
plt.show()
# %%
#PSD of the signals

fS,PxxS=welch(data[:,0],fs=fsamp,return_onesided=True,axis=0)
f,Pxx=welch(data[:,1:],fs=fsamp,return_onesided=True,axis=0)

plt.plot(fS,PxxS)
plt.show()

plt.plot(f,Pxx)
plt.show()
# %%

#spectrogram
plt.specgram(data[:,0],NFFT=256, noverlap=128)
plt.colorbar()
plt.show()
# %%
