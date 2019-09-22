#!/usr/bin/python
import numpy as np
from matplotlib import pyplot as plt
from matplotlib import animation
from collections import namedtuple
import itertools
import json

Balldata = namedtuple('Balldata', ['xpos', 'ypos', 'xvel', 'yvel'])
Frame = namedtuple('Frame', ['framenum', 'time', 'kinetic', 'balldata'])
ballpatches = []  # holds circle patches for animation


def chunks(l, n):
    """Yield successive n-sized chunks from l."""
    for i in range(0, len(l), n):
        yield l[i:i + n]


def split_into_frames(filelines: list):
    """
    splits a list of lines into a list of frames
    assumes frames are all same # of lines
    frames begin with 'FRAME #'
    frames end with 'ENDF'
    """
    start = -1
    framesize = 0
    for i in range(0, len(filelines)):
        line = filelines[i]
        if 'FRAME' in line:
            start = i
        if 'ENDF' in line:
            framesize = i + 1 - start
            break
    return chunks(filelines, framesize)


def extract_frame_data(framelines: list):
    """
    takes raw frame data in the form of lines from the output file
    and retuns a a frame object
    """
    # first line is header
    f = framelines[0].split()
    framenum = int(f[1])
    # second line are global observables
    f = framelines[1].split()
    t = float(f[0])
    kinetic = float(f[1])
    # remaining lines except last are balls
    balldatas = {}
    for i in range(2, len(framelines)-1):
        f = framelines[i].split()
        ballnum = int(f[0])
        xpos, ypos, xvel, yvel = [float(s) for s in f[1:]]
        balldatas[ballnum] = Balldata(xpos, ypos, xvel, yvel)
    # make observables a namedtuple?
    return Frame(framenum, t, kinetic, balldatas)


def anim_frames(framesr, frame_dt, anim_frame_interval):
    """
    takes an interable of frames and delta t betwen them
    and returns a reduced # appropriate for animation
    """
    # interval: time between animation frames [ms]
    step = int(anim_frame_interval/1000/frame_dt)
    return itertools.islice(frames, 0, None, step)


def init():
    """
    animation initialization function
    """

    for patch in ballpatches:
        ax.add_patch(patch)
    return ballpatches


def animate(framedata: Frame):
    """
    animation update function
    """
    balls = framedata.balldata
    for i, _ in enumerate(balls):
        patch = ballpatches[i]
        # x and y swapped, for horizontal plot
        y = balls[i+1].xpos
        x = balls[i+1].ypos
        patch.center = (x, y)
    return ballpatches


if __name__ == "__main__":

    ms_between_frames = 20
    inputfile = './input.dat'
    outputfile = './pool.out'

    # read variables from input data
    numballs = 0
    width_mm = 0
    height_mm = 0
    dt = 1
    with open(inputfile, 'r') as f:
        lines = f.read().strip().split('\n')
        lines = lines[1:-1]  # drop first and last
        for l in lines:
            # unsafe...
            exec(l.strip())

    # read results from output data
    with open(outputfile, 'r') as f:
        lines = f.read().strip().split('\n')
    # process data
    frames = split_into_frames(lines)
    animationframes = list(anim_frames(frames, dt, ms_between_frames))
    # only extract data from frames that will actually be animated
    framedata = [extract_frame_data(f) for f in animationframes]

    fnum = [f.framenum for f in framedata]
    ftime = [f.time for f in framedata]
    ekin = [f.kinetic for f in framedata]

    # setup canvas
    fig, (ax, ax2) = plt.subplots(2, 1)

    # energy plot
    ax2.plot(ftime, ekin, label='ekin')
    ax2.set_ylabel("total kinetic energy [J]")
    ax2.set_xlabel("simulation time [s]")
    # ax2.legend()

    # animation
    ax.set_ylim(0, width_mm)
    ax.set_xlim(0, height_mm)
    ax.tick_params(axis='both', which='both', bottom=False, top=False, left=False, right=False,
                   labelbottom=False, labeltop=False, labelleft=False, labelright=False)
    ax.set_aspect('equal')
    ax.set_facecolor('darkgreen')

    prop_cycle = plt.rcParams['axes.prop_cycle']
    colors = prop_cycle.by_key()['color']
    colorcycle = itertools.cycle(colors)
    for i in range(numballs):
        if i == 0:
            ballpatches.append(plt.Circle((-10, 0), 5.6/2, color='ivory'))
        else:
            ballpatches.append(plt.Circle((-10, 0), 5.6/2, color=next(colorcycle)))
    anim = animation.FuncAnimation(fig, animate,
                                   init_func=init,
                                   frames=framedata,
                                   interval=ms_between_frames,
                                   repeat=True,
                                   blit=True)

    plt.show()
