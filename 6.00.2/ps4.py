# 6.00.2x Problem Set 4

import numpy
import random
import pylab
from ps3b import *

#
# PROBLEM 1
#        
def simulationDelayedTreatment(numTrials,timesteps_1,timesteps_2=150, numViruses=100, maxPop=1000, maxBirthProb=0.1, clearProb=0.05, 
                                resistances = {'guttagonol': False}, mutProb=0.005):
    """
    Runs simulations and make histograms for problem 1.

    Runs numTrials simulations to show the relationship between delayed
    treatment and patient outcome using a histogram.

    Histograms of final total virus populations are displayed for delays of 300,
    150, 75, 0 timesteps (followed by an additional 150 timesteps of
    simulation).

    numTrials: number of simulation runs to execute (an integer)
    """
    
    virus_populatn = []
    for trial in xrange(numTrials):
        viruses = [ResistantVirus(maxBirthProb, clearProb, resistances, mutProb) for i in xrange(numViruses)]
        patient = TreatedPatient(viruses, maxPop)
        for timestep in xrange(timesteps_1):
            patient.update()

        # Post DELAY
        patient.addPrescription('guttagonol')
        for timestep in xrange(timesteps_1, timesteps_1+timesteps_2):
            patient.update()
        
        virus_populatn.append(patient.getTotalPop())
    return virus_populatn
"""
numTrials = 500
bins = 20
pylab.subplot(221)
delay_300 = simulationDelayedTreatment(numTrials,300)
pylab.hist(delay_300, bins = bins)
pylab.title("300 delay")


pylab.subplot(222)
delay_150 = simulationDelayedTreatment(numTrials,150) 
pylab.hist(delay_150, bins = bins)
pylab.title("150 delay")

pylab.subplot(223)
delay_75 = simulationDelayedTreatment(numTrials,75)
pylab.hist(delay_75, bins = bins)
pylab.title("75 delay")

pylab.subplot(224)
delay_0 = simulationDelayedTreatment(numTrials,0)
pylab.hist(delay_0, bins = bins)
pylab.title("0 delay")

pylab.show()
"""


#
# PROBLEM 2
#
def simulationTwoDrugsDelayedTreatment(numTrials,timesteps_2,timesteps_1=150, timesteps_3=150,numViruses=100, maxPop=1000, maxBirthProb=0.1, clearProb=0.05, 
                                        resistances = {'guttagonol': False, 'grimpex': False}, mutProb=0.005):
    """
    Runs simulations and make histograms for problem 2.

    Runs numTrials simulations to show the relationship between administration
    of multiple drugs and patient outcome.

    Histograms of final total virus populations are displayed for lag times of
    300, 150, 75, 0 timesteps between adding drugs (followed by an additional
    150 timesteps of simulation).

    numTrials: number of simulation runs to execute (an integer)
    """
    
    virus_populatn = []
    for trial in xrange(numTrials):
        viruses = [ResistantVirus(maxBirthProb, clearProb, resistances, mutProb) for i in xrange(numViruses)]
        patient = TreatedPatient(viruses, maxPop)
        for timestep in xrange(timesteps_1):
            patient.update()

        # Adding guttagonol
        patient.addPrescription('guttagonol')
        for timestep in xrange(timesteps_1, timesteps_1+timesteps_2):
            patient.update()
        
        # Adding grimpex
        patient.addPrescription('grimpex')
        for timestep in xrange(timesteps_1+timesteps_2, timesteps_1+timesteps_2+timesteps_3):
            patient.update()
                
        virus_populatn.append(patient.getTotalPop())
    return virus_populatn
numTrials = 500
bins = 20
pylab.subplot(221)
delay_300 = simulationTwoDrugsDelayedTreatment(numTrials,300)
pylab.hist(delay_300, bins = bins)
pylab.title("300 delay")


pylab.subplot(222)
delay_150 = simulationTwoDrugsDelayedTreatment(numTrials,150) 
pylab.hist(delay_150, bins = bins)
pylab.title("150 delay")

pylab.subplot(223)
delay_75 = simulationTwoDrugsDelayedTreatment(numTrials,75)
pylab.hist(delay_75, bins = bins)
pylab.title("75 delay")

pylab.subplot(224)
delay_0 = simulationTwoDrugsDelayedTreatment(numTrials,0)
pylab.hist(delay_0, bins = bins)
pylab.title("0 delay")

pylab.show()

print len([i for i in delay_300 if i < 51])/500.0
print len([i for i in delay_150 if i < 51])/500.0
print len([i for i in delay_75 if i < 51])/500.0
print len([i for i in delay_0 if i < 51])/500.0

#0.062
#0.102
#0.49
#0.842

pylab.figure()
pylab.plot([300,150,75,0], [0.062, 0.102, 0.49, 0.842])
                             