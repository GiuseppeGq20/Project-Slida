#%%
from numpy.fft import rfft
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.signal import welch
import scipy.signal as signal
directory="./AG2_ramp/DATI/A21 PRE/"
filename=[f"A21_pre_{x}.csv" for x in range(1,11) ]

# filename="S1_pre_65khz.csv"

data=pd.read_csv(directory+filename[0],skiprows=2)
data=data.to_numpy()
time=data[:,0]
data=data[:,1:]
plt.plot(time,data[:,1:])
plt.show()
#compute fft
datafft=np.fft.rfft(data, axis=0)
timestep=abs(time[3]-time[2])

fsamp=1/timestep
print(fsamp)

freq=np.fft.rfftfreq(len(data),d=timestep)/fsamp
#compute energy
temp = np.abs(datafft)**2

e=np.sum(temp, axis=0) *(freq[1]-freq[0])
e2=np.sum(data**2, axis=0)*timestep


plt.plot(e[1:])
# plt.plot(e2[1:])
plt.show()

# plt.plot(freq,temp[:,0])
plt.plot(freq,temp[:,0])
plt.xlabel("Hz")
plt.legend()
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
plt.plot(freq,np.abs(sig_fft)**2,'-.r')
plt.title(f"freq max = {freq[np.argmax(sig_fft)]}")
plt.show()

sig_fft[freq>830]=0
sig_fft[freq<370]=0
plt.plot(freq,np.abs(sig_fft))
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

# filter attempt 2

sig_act=np.fft.rfft(data[:,0])
freq=np.fft.rfftfreq(time.size,d=timestep)
sig_sens=np.fft.rfft(data[:,1:],axis=0)
norm=np.linalg.norm(sig_act)

sig_filter=np.zeros_like(sig_sens)
for i in range(sig_sens.shape[1]):
    sig_filter[:,i]=sig_act*sig_sens[:,i]/norm

sig_filter= np.fft.irfft(sig_sens,axis=0)
plt.plot(time[:-1],sig_filter)
plt.show()
# %%
# filter attempt 3: band pass filter
fmax=60e3/fsamp
alpha=1e3/fsamp

N, Wn = signal.buttord([fmax-2*alpha, fmax+ 2*alpha],
                       [fmax-10*alpha, fmax+ 10*alpha], 2, 5,)
sos=signal.butter(N,Wn,output='sos',btype='band')
sig_sos = signal.sosfilt(sos, data,axis=0)
plt.plot(time,sig_sos[:,1:])