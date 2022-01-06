"""
script to compute the necessary metrics and convert everything to a dataframe
"""

from os import listdir
import pandas as pd
import numpy as np
from scipy.signal import stft

def sortnames(sensors: list,sensname:str) -> tuple:
    sensors=sensors.copy()
    actuator=sensors.pop( sensors.index(sensname))
    sensors.sort()
    sensors.insert(0,actuator)
    return sensors
    
def computeEnergyMetrics(filename:str):
    """
    compute energy metrics for each .csv
    """
    data=pd.read_csv(filename).to_numpy()
    time=data[:,0]
    data=data[:,1:]
    f_sample= abs(1/(time[1]-time[2]))
    #compute fft
    data_fft=np.fft.rfft(data, axis=0)
    #compute energydiff
    datafft_squared = np.abs(data_fft)**2
    e=np.sum(datafft_squared, axis=0)
    ediff=e[0]-e[1:]

    #compute stft
    f, t, Sxx=stft(data,nperseg=128,fs=f_sample,axis=0) #nperseg critical parameter
    Sxx=np.abs(Sxx)

    # get max index
    index=[]
    for i in range(Sxx.shape[1]):
        flat_index=np.argmax(Sxx[:,i,:])
        a=np.unravel_index(flat_index,np.shape(Sxx[:,i,:]))
        index.append((a[0],i,a[-1]))
    
    # unpack indices
    index_time=[ind[-1] for ind in index]
    index_f=[ind[0] for ind in index]
    index_signal=[ind[1] for ind in index]
    
    #compute ToF
    ToF=t[index_time[1:]]-t[index_time[0]]

    #compute Transmission factor
    Tf=Sxx[index_f[1:],index_signal[1:],index_time[1:]]/Sxx[index[0]]
    
    return ediff, ToF,Tf

#TODO
def computeDistance(act_pos, sens_pos):
    d=[]
    x1=sens_pos[act_pos][0]
    y1=sens_pos[act_pos][1]

    #get a dict with only sens names
    sens_name=sens_pos.copy()
    del sens_name[act_pos]

    b=np.zeros((2,1))

    for key in sens_name.keys():
        x2=sens_pos[key][0]
        y2=sens_pos[key][1]

        A=np.array([[x1,y1,1],[x2,y2,1]])
        sol=np.linalg.lstsq(A,b,rcond=None)
        print(sol)

    return d


# create a dict to contains the information needed
data_dir="AG2_ramp/DATI"
dir_list=listdir(data_dir)

sensors= [20,21,22,24,25]
sensors.extend([x for x in range(42,50)])
#convert to string
sensorsname =[ str(value) for value in sensors]

pairs=[]
for sens in sensorsname:
    
    a={}
    a["actuator"]=sens
    
    #find pre and post dir
    for dirs in dir_list:
        if sens in dirs:
            if  "PRE" in dirs:
                a["pre"]=dirs
            if "POST" in dirs:
                a["post"]= dirs
    
    a["sensorder"]=sortnames(sensorsname,sens)
    pairs.append(a)


#compute the metrics
filename=data_dir+"/"+pairs[1]["pre"]
file_list=listdir(filename)

file_list=[filename+"/"+file for file in file_list]

ediff, Tof, Tf=computeEnergyMetrics(file_list[2])

#Distances

#compute posistions
sens_pos={}
sens_pos["20"]= (0.0,0.0)
#geom data
r=150 #mm
delta_angle= 30 # degree
damage_pos=(81,40)
for i,value in enumerate(sensors[1:]):
    sens_pos[str(value) ]= (r*np.cos(np.deg2rad(-90 - i*delta_angle)),
                            r*np.sin(np.deg2rad(-90 - i*delta_angle)))

computeDistance("20",sens_pos)


"""TODO
- compute the distances
- find a way to properly construct a dataframe like:
    distances | actuator | sensor | pre | post | ediff | Tof | Tf
- organize the script
"""