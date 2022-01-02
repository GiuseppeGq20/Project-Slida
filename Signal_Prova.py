#%%
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

directory="./AG2_ramp/DATI/A20 PRE/"
filename=[f"A20_pre_{x}.csv" for x in range(1,11) ]

data=pd.read_csv(directory+filename[0])
data=data.to_numpy()
time=data[:,0]
data=data[:,1:]
#compute fft
datafft=np.fft.rfft(data, axis=0)
timestep=time[1]-time[0]
freq=np.fft.rfftfreq(len(data),d=timestep)
#compute energy
e=np.sum(
    np.abs(datafft)**2
    , axis=0)
ediff=np.abs(e[0]-e[1:])

# %%
