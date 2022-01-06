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

def computeDistance(actuator:str,sens_pos:dict,damage_pos:tuple):
    
    """
    from lin eq y=mx+q
    cartesian form y -mx -q =0
    and ax+by+c=0
    then a=-m; c=-q; b=1 only if m is finite
    """
    temp=sens_pos.copy()
    x1=temp[actuator][0]
    y1=temp[actuator][1]

    xd=damage_pos[0]
    yd=damage_pos[1]

    del temp[actuator]

    
    d=[]
    for key in temp.keys():

        n=() # a b c vector
        x2=temp[key][0]
        y2=temp[key][1]

        if abs(x1-x2)<1e-4:
            a=0
            b=1
            c=-x2

        elif abs(y1-y2)<1e-3:
            b=0
            a=1
            c=-y2
        else:
            m=(y2-y1)/(x2-x1)
            q=-m*x2 + y2
            a=-m 
            b=1
            c=-q
        
        #distance of the path from the damage
        distance=abs(a*xd +b*yd +c)/ ((a**2 + b**2)**0.5)
        d.append(distance)
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

#evaluate distances
for pair in pairs:

    pair["distances"]=computeDistance(pair["actuator"],sens_pos,damage_pos)



"""TODO

- find a way to properly construct a dataframe like:
    distances | actuator | sensor | pre | post | ediff | Tof | Tf
- organize the script
"""