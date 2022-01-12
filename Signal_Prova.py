#%%
from numpy.fft import rfft
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
print(fsamp)

freq=np.fft.rfftfreq(len(data),d=timestep)
#compute energy
temp = np.abs(datafft)**2

e=np.sum(temp, axis=0)
e2=np.sum(data**2, axis=0)

plt.plot(e[1:])
plt.plot(e2[1:])
plt.show()

# plt.plot(freq,temp[:,0])
plt.plot(freq,temp[:,0])
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


#spectrogram
plt.specgram(data[:,0],NFFT=60, noverlap=59)

plt.colorbar()
plt.show()
# %%
# prova filtering
#original signal
sig_act=data[:,0]
sig_fft=np.fft.rfft(sig_act)
freq=np.fft.rfftfreq(time.size,d=timestep)
plt.plot(freq,np.abs(sig_fft)**2)
plt.show()

sig_fft[freq>830]=0
sig_fft[freq<370]=0
plt.plot(np.abs(sig_fft))
plt.show()
plt.plot(time,sig_act)
plt.plot(time[:-1],np.fft.irfft(sig_fft))
plt.show()
#%%
#raw
# filtered
sig=data[:,1]
sig_fft=np.fft.rfft(sig)
freq=np.fft.rfftfreq(sig.size,d=timestep)
sig_fft[(freq>800)]=0
sig_fft[(freq<400)]=0
plt.plot(freq,np.abs(sig_fft))
sig_f=np.fft.irfft(sig_fft)
plt.plot(time,data[:,1])
plt.plot(time[:-1],sig_f)
# %%

