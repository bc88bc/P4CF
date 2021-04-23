README
================
Brian Chiou
April 12, 2021

Given a station location, FORTRAN code *P4CF* computes source and path
**P**arameters for(**4**) a **C**urved **F**ault rupture. This code has
been used in PEER’s NGA-West2 and NGA-Subduction projects
(*r**e**f**e**r**e**n**c**e*) to calculate source and path metadata for
the respective project database.

# Inputs

Inputs of *P4CF* consists of three parts: command-line inputs; input
file of finite-fault model; input file of station coordinates.

## Command-line inputs

## Input file of finite-fault model (.TLL or .TXY)

-   Line 1:
-   Line 2: Relative to the upper-right corner (that is, the first
    vertex of rupture trace in the finite-fault file), hypocenter
    location may be specified as a fraction of fault length and a
    fraction of fault width for along-strike and down-dip position,
    respectively.
-   Line 3:
-   Line 4:
-   Line 5:
-   Line 6:

### Important Conventions

## Input file of station coordinate (.TLL or .TXY)

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
