README
================
Brian Chiou
April 12, 2021

FORTRAN code *P4CF* computes source and path **P**arameters for(**4**) a
**C**urved **F**ault rupture. This code has been used in PEER’s NGA-West
Projects (Chiou and others, 2008; Ancheta et al., 2014) and
NGA-Subduction Project (Contreras et al. 2021) to calculate source and
path metadata for the respective project database.

# Inputs

Inputs to *P4CF* are entered via a DOS command, as follows: 1.

To terminate calculation, enter *q* for *EQID*.

NOTE: When finite-fault model of an earthquake is unavailable, the
companion FORTRAN code *CCLD* can be used to simulate possible fault
rupture planes according to the known information of hypocenter location
and focal mechanism (strike/dip).

1.  Inputs on command-line, which apply to all earthquakes
2.  Finite-fault model file, whose name has the extension .*TLL* or
    .*TXY*
3.  Station coordinates file, whose name has the extension .*SLL* or
    .*SXY*

## Command-line inputs

## Finite-fault model input file (.*TLL* or .*TXY*)

-   Line 1:
-   Line 2: Hypocenter location in latitude, longitude, and depth
    -   Hypocenter location may be specified as a fraction of fault
        length and a fraction of fault width along-strike and down-dip
        position, respectively. Relative to the upper-left corner of
        rupture plane (that is, the first vertex of rupture trace in the
        finite-fault file),
-   Line 3:
-   Line 4:
-   Line 5:
-   Line 6:

### Important Conventions

## Station coordinates input file (.TLL or .TXY)

One line per station

-   (a5, ….) EQID, Sta\_seq\_no, Sta\_name, Sta\_long, Sta\_lat,
    Sta\_elev (not used)
    -   The first four fields consists of the earthquake identification,
        the fifth field represents the fault identifier
    -   For the .SLL file (coordindates are in longitude/latitude),
        Sta\_long and Sta\_lat are the longitude and latitude of station
    -   For the .SXY file (coordinates are not in longitude/latitude),
        Sta\_long and Sta\_lat are the easting and northing coordinate
        of station.

# Outputs

Outputs of *P4CF* include

-   Station coordinates

    -   (*X*, *Y*): If input is int geographic coordinates
        (longitude/latitude), Cartesian coordinates relative to the
        origin specified in the finite-fault file
    -   (*U*, *T*): Generalized coordinates 2 (GC2) relative to the
        rupture trace specified in the finite-fault file (Spudich and
        Chiou, 2015)
        -   *R*<sub>*y*</sub> and *R*<sub>*y*0</sub> in the NGA database
            are derived from the *U* coordinate as follows:
            -   *R*<sub>*y*</sub> = *U* − 2/*L*, where *L* is the
                rupture length
            -   *R*<sub>*y*0</sub> = ….
        -   *R*<sub>*x*</sub> = \|*T*\|
            -   Negative *T* indicates that station is located on the
                foot-wall side, while positive *T* indicates hangingwall
                side

-   Distance measures

    -   Station-to-point-source distance

        -   *R*<sub>*e**p**i*</sub>: distance between Station and
            Epicenter
        -   *R*<sub>*h**y**p*</sub>: distance between Station and
            Hypocenter

    -   Station-to-finite-rupture distance (distances to the
        Finite-Fault (*FF*) rupture model of earthquake rupture)

        -   *R*<sub>*r**u**p*</sub>: shortest distance to the FF model
        -   *R*<sub>*j**b*</sub>: shortest distance to the surface
            projection of *FF* model
        -   *R*<sub>*r**m**s*</sub>: root-mean-squared distance to *FF*
            model *d**e**f**i**n**i**t**i**o**n*

-   Finite-fault’s strike-parallel direction

    -   *A**v**e**S**t**r**i**k**e*: average of strike-parallel
        directions over the segments of FF model; useful for curved
        fault rupture
    -   *L**o**c**S**t**r**i**k**e*: representing the local
        strike-parallel direction near a station
        (*d**e**f**i**n**i**t**i**o**n*)
    -   *Note*: Both strike-parallel directions can be utilized to
        rotate the two horizontal components of ground-motion time
        series to strike-normal and strike-parallel components. In the
        two NGA Projects, *L**o**c**S**t**r**i**k**e* has been used for
        that purpose.

-   Direct-Point Parameter of rupture directivity (Spudich Chiou, 2013;
    Chiou and Spudich, 2013)

    -   *ĉ*′: approximate isochrone velocity, normalized to a shear-wave
        velocity of 3.2 km/s
    -   *E*: length of the hypocenter-to-direct-point (H2D) line source
    -   *a**v**e**R*: radiation pattern averaged over the H2D line
        source
    -   *D**P**P*: direct-point parameter of rupture directivity

-   
