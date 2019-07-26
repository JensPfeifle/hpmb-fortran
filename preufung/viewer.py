import numpy as np
from matplotlib import pyplot as plt
from matplotlib import animation

# read data
with open('pool.out','r') as f:
    lines = f.read().split('\n')
split_lines = [l.split() for l in lines if len(l) > 0]

t = np.array([float(column[0]) for column in split_lines])
xdata = np.array([float(column[1]) for column in split_lines])
ydata = np.array([float(column[2]) for column in split_lines])

# setup canvas
fig, ax = plt.subplots()
ax.set_xlim(0,111.76)
ax.set_ylim(0,223.52)
ax.set_aspect('equal')
patch1 = plt.Circle((0,0),5.7/2,fc='y')
patch2 = plt.Circle((0,0),5.7/2,fc='r')

images = []

def init():
    ax.add_patch(patch1)
    ax.add_patch(patch2)
    return patch1,patch2,

def animate(i):
    x = xdata[i]
    y = ydata[i]
    if i%2 == 0:
        patch1.center = (x, y)
    else:
        patch2.center = (x, y)

    return patch1,patch2,

anim = animation.FuncAnimation(fig, animate, 
                               init_func=init, 
                               frames=len(t), 
                               interval=1,
                               blit=True)

plt.show()