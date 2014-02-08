# Import all stuff we need
#%pylab inline
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import matplotlib.pylab as pylab
import sys
import os
from os.path import expanduser
import time
import random
import string
import struct

from brian import Network, Equations, NeuronGroup, Connection,\
    SpikeMonitor, raster_plot, StateMonitor, clear, reinit
from brian.stdunits import ms, mV, nS, pA, pF, Hz

for i in range(len(sys.argv)):
  if sys.argv[i] == "-rEE":
    rEE = float(sys.argv[i+1])

#%matplotlib inline
pylab.rcParams['figure.figsize'] = 12, 8  # changes figure size (width, height) for larger images

clear(True, True) 
reinit()# To reinit brain clocks and remove all old brian objects from namespace,
# it's usually a good idea to put this at the beginning of a script



# The equations defining our neuron model

eqs_string = ''' dV/dt = (1.0/tau)*(myu-V) + Isyn : 1
             Isyn =  Isyne - Isyni : hertz
             dIsyne/dt = -(1/tau1)*(Isyne + ye) : hertz
             dye/dt = -(1/tau2)*ye : hertz
             dIsyni/dt = -(1/tau1)*(Isyni + yi) : hertz
             dyi/dt = -(1/tau2)*yi : hertz
             myu : 1
             '''

# Our model parameters
mod = 0.83
mod2 = 0.96

taue = 15.*ms *mod # time constant of membrane excitatory
taui = 10.*ms *mod # time constant of membrane inhibitory
tau1 = 1.*ms *mod
tau2e = 3.*ms *mod
tau2i = 2.*ms *mod
myueMax = 1.2 *mod2
myueMin = 1.1 *mod2
myuiMax = 1.05 *mod2
myuiMin = 1.0 *mod2
syn_delay = 0*ms # synaptic delay between two neurons
V_th = 1 # firing threshold
V_reset = 0 # reset potential
refr_period = 5*ms # absolute refractory period
bump = 500 #value to bump Isyn with on spike
jEE = 0.024
jEI = 0.045
jIE = 0.014
jII = 0.057
pII = 0.5
pEI = 0.5
pIE = 0.5
pEE = 0.2
#rEE = 2.5
pEEaverage = 0.2

# Number of neurons
N_e = 4000 # number of exc neurons
N_i = 1000 # number of inh neurons
uniformClustering = True
N_eClusters = 50 #Number of exc neuron clusters
pEEin = -1
pEEout = -1
if uniformClustering:
    ins = N_e/N_eClusters
    outs = N_e - ins
    pEEout = (pEEaverage*N_e)/(rEE*ins + outs)
    pEEin = rEE*pEEout
    

print "pEEin = ", pEEin, "pEEout = ", pEEout    
    
# Duration of our simulation
duration = 5000*ms

# Let's create an equation object from our string and parameters
model_eqs_e = Equations(eqs_string,
                      tau=taue,
                      tau1=tau1,
                      tau2=tau2e,
                      )

model_eqs_i = Equations(eqs_string,
                      tau=taui,
                      tau1=tau1,
                      tau2=tau2i,
                      )


# Divide the neurons into excitatory and inhibitory ones
neurons_e = NeuronGroup(N=N_e,
                          model=model_eqs_e,
                          threshold=V_th,
                          reset=V_reset,
                          refractory=refr_period,
                          freeze = True,
                          method='Euler',
                          compile=True)

neurons_i = NeuronGroup(N=N_i,
                          model=model_eqs_i,
                          threshold=V_th,
                          reset=V_reset,
                          refractory=refr_period,
                          freeze = True,
                          method='Euler',
                          compile=True)


neurons_e.myu = np.random.uniform(myueMin, myueMax, N_e)
neurons_i.myu = np.random.uniform(myuiMin, myuiMax, N_i)

neurons_e_clusters = []

if uniformClustering:
    numInEachCluster = N_e/N_eClusters
    if N_e%N_eClusters is not 0:
        raise Exception
    baseNeuron = 0
    for i in range(N_eClusters):
        neurons_e_clusters.append(neurons_e[baseNeuron:baseNeuron+numInEachCluster])
        baseNeuron = baseNeuron + numInEachCluster
else:
    raise Exception







conn_ee_clusters = []

for i in range(N_eClusters):
    
    if i != 0 and i != N_eClusters-1:
        conn_forward = Connection(neurons_e_clusters[i],
                                  neurons_e[(i+1)*numInEachCluster:],
                                  'ye',
                                  delay = syn_delay,
                                  weight = -jEE*(1./(tau2e)),
                                  sparseness = pEEout)
        conn_ee_clusters.append(conn_forward)
        conn_backward = Connection(neurons_e_clusters[i],
                                  neurons_e[0:i*numInEachCluster],
                                  'ye',
                                  delay = syn_delay,
                                  weight = -jEE*(1./(tau2e)),
                                  sparseness = pEEout)
        #here???
        conn_ee_clusters.append(conn_backward)
        conn_in = Connection(neurons_e_clusters[i],
                                  neurons_e_clusters[i],
                                  'ye',
                                  delay = syn_delay,
                                  weight = -1.9*jEE*(1./(tau2e)),
                                  sparseness = pEEin)
        conn_ee_clusters.append(conn_in)
        
    if i == 0:
        conn_forward = Connection(neurons_e_clusters[i],
                                  neurons_e[numInEachCluster:],
                                  'ye',
                                  delay = syn_delay,
                                  weight = -jEE*(1./(tau2e)),
                                  sparseness = pEEout)
        
        conn_ee_clusters.append(conn_forward)
        conn_in = Connection(neurons_e_clusters[i],
                                  neurons_e_clusters[i],
                                  'ye',
                                  delay = syn_delay,
                                  weight = -1.9*jEE*(1./(tau2e)),
                                  sparseness = pEEin)
        conn_ee_clusters.append(conn_in)
    
    if i == N_eClusters-1:
        conn_backward = Connection(neurons_e_clusters[i],
                                  neurons_e[:(i-1)*numInEachCluster],
                                  'ye',
                                  delay = syn_delay,
                                  weight = -jEE*(1./(tau2e)),
                                  sparseness = pEEout)
        conn_ee_clusters.append(conn_backward)
        conn_in = Connection(neurons_e_clusters[i],
                                  neurons_e_clusters[i],
                                  'ye',
                                  delay = syn_delay,
                                  weight = -1.9*jEE*(1./(tau2e)),
                                  sparseness = pEEin)
        conn_ee_clusters.append(conn_in)

conn_ei = Connection(neurons_e,
                    neurons_i,
                    'ye',
                    delay=syn_delay,
                    weight = -jIE*1./(tau2e),
                    sparseness=pEI)

conn_ie = Connection(neurons_i,
                    neurons_e,
                    'yi',
                    delay=syn_delay,
                    weight = -jEI*1./(tau2i),
                    sparseness=pIE)

conn_ii = Connection(neurons_i,
                    neurons_i,
                    'yi',
                    delay=syn_delay,
                    weight = -jII*1./(tau2i),
                    sparseness=pII)



# Set the initial membrane potential somewhere between the reversal potential and slightly
# above the firing threshold. This will make some cells fire at t=0.
neurons_e.V = np.random.uniform(V_reset, V_th * 1.1, N_e)
neurons_i.V = np.random.uniform(V_reset, V_th * 1.1, N_i)




# Make some monitors to record spikes of all neurons and the membrane potential of a few
spike_mon_e = SpikeMonitor(neurons_e)
spike_mon_i = SpikeMonitor(neurons_i)
#state_mon_v_e = StateMonitor(neurons_e, 'V', record=[0,1,2])
#state_mon_v_i = StateMonitor(neurons_i, 'V', record=[0,1])
#state_mon_isyn = StateMonitor(neurons_i, 'Isyn', record=[0,1])



# Put everything into the network conn_ii conn_ee_clusters, conn_ei, conn_ie,  
network = Network(neurons_e, neurons_i, spike_mon_e,  conn_ii, conn_ee_clusters, conn_ie, conn_ei, spike_mon_i)#, state_mon_v_i, state_mon_isyn, state_mon_v_e,)




# Let's run our simulation
network.run(duration, report='text')

#plt.figure()
#plt.plot(state_mon_isyn.times,state_mon_isyn.values[0,:])



# Plot spike raster plots, blue exc neurons, red inh neurons
plt.figure()
gs = gridspec.GridSpec(2, 1, height_ratios=[1, 3])
plt.subplot(gs[0])
raster_plot(spike_mon_i, color='r')
plt.title('Inhibitory neurons')
plt.subplot(gs[1])
raster_plot(spike_mon_e)
plt.title('Excitatory neurons')

# Plot the evolution of the membrane potentials
'''plt.figure()
for irun in range(1,6):
    plt.subplot(6,1,irun)
    if irun < 3:
        mon = state_mon_v_i
        idx = irun-1
        color='r'
    else:
        mon = state_mon_v_e
        idx = irun-3
        color='b'

    y =  mon.values[idx,:]
    x = mon.times
    plt.plot(x, y, color)
    plt.yticks([min(y), (max(y)-min(y))/2.0+min(y)]

    if irun == 5:
        plt.xlabel('t in ms')
        plt.ylabel('V in volt')'''

# Show the plots
#plt.show()

def writeSpikesToFile(spike_mons, rEE, duration):
    length = 0
    for spike_mon in spike_mons:
      length = length + len(spike_mon.spikes)
    
    path = getGoodPath(rEE, duration, N_e, N_i, length)
    sl = []
    for spike_mon in spike_mons:
      sl.append(str(spike_mon.spikes))
    with open(path, 'w') as f:
        for s in sl:
            f.write(s)
            f.write("\n")
    assert(f.closed)
    return True

def bWriteSpikesToFile(spike_mons, rEE, duration):
  length = 0
  for spike_mon in spike_mons:
    length = length + len(spike_mon.spikes)
  
  path = getGoodPath(rEE, duration, N_e, N_i, length)
  with open(path, 'wb') as f:
    for i in range(len(spike_mons)):
      for spike in spike_mons[i].spikes:
        if i == 0:
          f.write(struct.pack('>i', int(spike[0])))
          f.write(struct.pack('>d', float(spike[1])))
        elif i == 1:
          f.write(struct.pack('>i', int(spike[0]+N_e)))
          f.write(struct.pack('>d', float(spike[1])))
        else:
          print("ERROR")
  return True

def buildListString(l):
  print("1")
  s = "["
  print("2")
  s.join(string.joinfields(map(lambda x: str(x)+"," , l)))
  print("3")
  return s[:-1] + "]"

def getGoodPath(rEE, duration, N_e, N_i, length):
    filenameRoot = "clusteredNet_"
    filenameRoot = filenameRoot + ("rEE:" + str(float(rEE)) + "_")
    filenameRoot = filenameRoot + ("duration:" + str(float(duration)) + "_")
    filenameRoot = filenameRoot + ("Ne:" + str(int(N_e)) + "_")
    filenameRoot = filenameRoot + ("Ni:" + str(int(N_i)) + "_")
    filenameRoot = filenameRoot + ("len:" + str(length) + "_")
    home = expanduser("~")
    path = home + "/Documents/MNSprojectResults/b/"
    i = 0
    available = not os.path.exists(path+filenameRoot+"("+str(i)+")")
    while (not available):
        i = i + 1
        available = not os.path.exists(path+filenameRoot+"("+str(i)+")")
    return path + filenameRoot + "(" + str(i) + ")"

bWriteSpikesToFile([spike_mon_e, spike_mon_i], rEE, duration)