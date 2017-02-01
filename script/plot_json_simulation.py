#!/usr/bin/python

import json
from matplotlib.pylab import subplots


# Read simulation data
f = open("simulation.json")
data = json.load(f)

# Repack simulation data
states, particles = list( zip( *((d['state'], d['particles']) for d in data )) )

figure, axes = subplots(1)
figure.show()

from pdb import set_trace
set_trace()

for i in range( len( particles ) ):
    # Plot robot and robot trajectory
    axes.plot(states[0: (i + 1)][0][0], states[0: (i + 1)][0][1], c='r')
    axes.scatter(states[i][0], states[i][1], c='r')
    
    # Repack particle states
    psx, psy = list( zip( * ((p['state'][0], p['state'][1]) for p in particles[i]) ) )
    
    # Plot particles
    axes.scatter(psx, psy, c='b')
    
    # Style and draw
    
    axes.set_ylim([-10, 40])
    axes.set_xlim([-10, 40])

    figure.canvas.draw()

    axes.cla()

