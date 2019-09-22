# Molecular Billiards / Billiards Dynamics - SS2019

## Prerequisites

python 3, numpy and matplotlib

```
sudo apt install python3
python -m pip install --user -r requirements.txt
```

## Compiling

Compile with GNU Fortan \
gcc version 9.1.0 (GCC) was used.
```
make
# or
make main
```


## Running

See `run.sh` or `run_interactive.sh`. \
Recommendation: use 1 thread.


## Configuring parameters

Parameters are confured in `input.dat`:
```
&INPUT
 numballs = 22 
 height_mm = 223.52
 width_mm = 111.76
 t_total = 5.
 dt = 0.00001
 qball_vel = 200.
 qball_offcenter = 0.1
 integration_scheme = 2
/
```
* numballs: number of billiard balls on the table
* height_mm = length of the table, a regulation table is 223.52 cm wide
* width_mm: width of the table, a regulation table is 111.76 cm wide
* t_total: total time to simulate
* dt: time step
* qball_vel: initial velocity of queue ball
* qball_offcenter: move the queue ball offcenter to remove symmetry
* integration_scheme: set 1 for Euler time integration, 2 for Stoermer-Verlet

## Author

[Jens Pfeifle](https://github.com/JensPfeifle) \
1704998

## License

This project is licensed under the MIT License - see the [license.md](LICENSE.md) file for details
