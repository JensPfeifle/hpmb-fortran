import numpy as np
from matplotlib import pyplot as plt
from matplotlib import animation
from collections import namedtuple
import itertools

Balldata = namedtuple('Balldata', ['xpos', 'ypos', 'xvel','yvel'])
ballpatches = [] # holds cirlce patches for animation

def chunks(l, n):
    """Yield successive n-sized chunks from l."""
    for i in range(0, len(l), n):
        yield l[i:i + n]

def split_into_frames(filelines: list):
    """
    splits a list of lines into a list of frames
    note: assumes frames are all same # of lines
    frames begin with 'FRAME #'
    frames end with 'ENDF'
    """
    start = -1
    framesize = 0
    for i in range(0,len(filelines)):
        line = filelines[i]
        if 'FRAME' in line:
            start = i
        if 'ENDF' in line:
            framesize = i + 1 - start
            break         
    return chunks(filelines, framesize)
        
def extract_frame_data(framelines: list):
    # first line is header
    f = framelines[0].split()
    framenum = int(f[1])
    # second line are global observables
    f = framelines[1].split()
    t =       float(f[0])
    kinetic = float(f[1])
    # remaining lines except last are balls
    ballvalues = {}
    for i in range(2,len(framelines)-1):
        f = framelines[i].split()
        ballnum = int(f[0])
        xpos,ypos,xvel,yvel = [float(s) for s in f[1:]]
        ballvalues[ballnum] = Balldata(xpos, ypos, xvel, yvel)
    # make observables a namedtuple?
    return (framenum, t, kinetic, ballvalues)

def init():
    """
    animation initialization function
    """
    for patch in ballpatches:
        ax.add_patch(patch)
    return ballpatches

def animate(frame):
    """
    animation update function
    """
    framedata = extract_frame_data(frame)
    framenum, t, kinetic, balldata = framedata
    for i in range(len(balldata)):
        patch = ballpatches[i]
        x = balldata[i+1].xpos
        y = balldata[i+1].ypos
        patch.center = (x, y)

    return ballpatches # only return updated?

def anim_frames(frame_iterator, interval):
    # interval between animation frames [ms]
    frame_deltat = 0.0001 # [s]
    step = int(interval/1000/frame_deltat)
    return itertools.islice(frame_iterator,0,None,step)

if __name__ == "__main__":
    
    # read data
    with open('pool.out','r') as f:
        lines = f.read().strip().split('\n')
    # process data
    frames = split_into_frames(lines)

    # setup canvas
    fig, ax = plt.subplots()
    ax.set_xlim(0,111.76)
    ax.set_ylim(0,223.52)
    ax.set_aspect('equal')
    # ballpatches is global variable
    for i in range(3): # hardcoded number of balls!
        ballpatches.append(plt.Circle((0,0),5.7/2))

    aframes = list(anim_frames(frames, 16))

    anim = animation.FuncAnimation(fig, animate, 
                                init_func=init, 
                                frames=aframes, 
                                interval=16,
                                repeat=True,
                                blit=True)

    plt.show()