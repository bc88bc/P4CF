README
================
Brian Chiou
April 12, 2021

FORTRAN code *P4CF* computes earthquake source and path **P**arameters
for(**4**) a **C**urved **F**ault rupture.

This code has been used in PEER’s NGA-West Projects (Chiou and others,
2008; Ancheta and others, 2014) and NGA-Subduction Project (Contreras
and others, 2021) to provide some of the source and path metadata
included in the respective project database. The path parameters include
commonly used distance measures and the direct-point directivity
parameters (Chiou and Spudich, 2013). The source parameters include the
rupture trace length, ruptured area, strike direction (in terms of both
global average and local variation), and the location of the closest
point on fault.

# Compiling P4CF.for

P4CF.for is (mostly) a standard FORTRAN77 code. It has been successfully
compiled using the following compilers under several operating systems:

1.  Layhey Fortran (release 5.60a, in Windows 10): lf95 p4cf.for -dbl

2.  GNU Fortran (gcc version 4.7.1, in Windows 10): gfortran p4cf.for
    -freal-4-real-8 -o p4cf.exe

3.  GNU Fortran (??, for Linux, Ubuntu subsystem of Windows 10)

4.  GNU Fortran (??, for MacOS)

Windows is the primary OS under which P4CF has been developed, tested,
and applied. As such, discussions below are based on the Windows 10
version of compiled code.

# Running P4CF

## Command-Line Inputs

*P4CF* should be executed in a DOS command prompt with the following
inputs:

-   Line 1: *EQID\_fault\_ID* (character, ‘a5’).

    -   It is a concatenation of *EQID* (first 4 characters) and
        *fault\_ID* (fifth character).
        -   *fault\_ID* is included to track the calculations for each
            individual (disjointed) rupture trace used to approximate
            the co-seismic rupture of an earthquake

-   Line 2: *inLatLong* (logical)

    -   if FALSE, Cartesian coordinates (*X, Y*) are used to specify
        horizontal locations
    -   if TRUE, (Longitude, Latitude) are used to specify locations

-   Line 3: *method* (integer)

    -   To project rupture trace down-dip to form the depth portion of
        rupture surface, two methods have been implemented:
        -   *method = 1*: ….
        -   *method = 2*: projection is perpendicular to the average
            strike direction specified next

-   Line 4 (only if *method* = 2): *AveStrike* (real)

    -   The average strike direction (in units of degree) perpendicular
        to which down-dip projection is performed
    -   If *AveStrike* = -999, it is determined within *P4CF* by
        connecting the first vertex to the last vertex of input rupture
        trace

-   Line 5 (only if *method* = 2): *AveDip* (real)

    -   The dip angle to be used for down-dip projection of rupture
        trace, in units of degree

-   To continue, repeat Line 1 through Line 5 for next earthquake.

-   To terminate, enter ‘q’ on Line 1.

### Notes

\#1: In *P4CF*, calculations are performed in (*X, Y, Z*) coordinate
system.

-   When input locations are in longitude/latitude, they will be
    transformed to (X, Y) coordinate by first using the *haverside*
    formula to compute the (great circle) distance between the
    user-specified origin and the point of interest, then decomposing
    the distance into the east (X) and the north (Y) components.
    -   A transformation of (X, Y) back to longitude/latitude is
        performed when longitude/latitude outputs are required.
-   *Z* is the elevation (in KM) of the point of interest
    -   When a point is below sea level, its *Z* is negative and its *H*
        (depth) is positive

\#2: For batch calculation of multiple earthquakes, command-line inputs
may be typed into a single text file, then *P4CF* can be ran with the
standard input re-directed to that file (for example, *P4CF &lt;
0169.in*)

\#3: In the NGA projects, when finite-fault model of an earthquake is
unavailable, the companion FORTRAN code *CCLD* has been used to produce
a fault plane by simulating 101 plausible rupture planes according to
the known information of hypocenter location and focal mechanism (strike
and dip). The selected fault plane is the one that best matches the
median
![R\_{RUP}](https://latex.codecogs.com/png.latex?R_%7BRUP%7D "R_{RUP}")
of 101 simulations.

## File Inputs

In addition to the command-line inputs, two input files (per earthquake)
are required.

1.  Finite-fault file, whose name must be the concatenation of
    *EQID\_fault\_id* and ‘.TLL’ (*inLatLon* is *TRUE*) or ‘.TXY’
    (*inLatLon* is *FALSE*)

2.  Station file, whose name must be the concatenation of *EQID*
    (without *fault\_id*) and ‘.SLL’ (*inLatLon* is *TRUE*) or ‘.SXY’
    (*inLatLon* is *FALSE*)

-   Because the same set of stations are used for all faults,
    *fault\_id* does not need to be part of station-file name

NOTE: *P4CF* assumes that these two files are located in the directory
where *P4CF* run is conducted.

### Input file of finite-fault model

-   Line 1: Header

-   Line 2: Origin of the (X, Y) coordinate (real)

    -   if *inLatLon* is *FALSE*, (0, 0)

    -   if *inLatLon* is *TRUE*, (latitude, longitude) of the origin

-   Line 3: Hypocenter location (*Hyp\_x, Hyp\_y, Hyp\_z*)

    -   If *inLatLon* is *FALSE*, hypocenter is given in Cartesian
        coordinate (*X, Y, Z*)
    -   If *inLatLon* is *TRUE*, hypocenter is given as (Latitude,
        Longitude, *Z*)
        -   Elevation *Z* is the negative of hypoentral depth
    -   Hypocenter can be specified in terms of along-strike and
        down-dip locations relative to the first vertex of rupture
        trace. To use this option, set *Hyp\_x* to -9999; *Hyp\_y* and
        *Hyp\_z* are then interpreted as the along-strike and down-dip
        locations, in fractions of rupture length and rupture width,
        respectively.

-   Line 4: Number of (connected) rupture segments (*nseg*)

-   Lines 5 to (5 + *nseg*): Coordinates of the (*nseg* + 1) vertices of
    rupture trace.

    -   If *inLatLon* is FALSE, vertices are given in (X, Y) coordinate
    -   if *inLatLon* is TRUE, vertices are given in Latitude and
        Longitude

-   Line *5 + nseg + 1*: (*Dip*(i), i = 1, *nseg*), *H\_Trace*,
    *H\_Top*, *H\_Bot*

    -   (*Dip*(i), i = 1, *nseg*): dip angle (in degree) of each trace
        segment
    -   *H\_Trace*: depth (in KM) to fault trace
        -   *H\_Trace* is deprecated; it is kept for backward
            compatibility
    -   *H\_Top*: depth (in KM) to the top of rupture
        (![Z\_{TOR}](https://latex.codecogs.com/png.latex?Z_%7BTOR%7D "Z_{TOR}")
        in GMPE parlance)
    -   *H\_Bot*: depth (in KM) to the bottom of rupture

-   Line *5+nseg+2*: *Rake*(i), i = 1, *nseg*

    -   Rake angle (in degree) of each rupture segment

#### Notes

1.  Strike of rupture trace dictates the order in which its vertices are
    specified on in Lines 5 to (5+ *nseg*) of the finite-fault input.
    The strike of segment
    ![i](https://latex.codecogs.com/png.latex?i "i") is the direction
    from vertex *i* to vertex *i+1*.

2.  When using *method* 1, depth to top of fault must be equal to depth
    to top of rupture trace (Line *5+nseg+1* of finite-fault input).

### List of stations and their coordinates

One input file per earthquake and, after file header, one line per
recording station

-   *EQID, Sta\_seq\_no, Sta\_name, Sta\_long, Sta\_lat, Sta\_elev* (not
    used)
    -   The first four fields consists of the earthquake identification,
        the fifth field represents the fault identifier
    -   For the .SLL file (coordindates are in longitude/latitude),
        Sta\_long and Sta\_lat are the longitude and latitude of station
    -   For the .SXY file (coordinates are not in longitude/latitude),
        Sta\_long and Sta\_lat are the easting and northing coordinate
        of station.

# *P4CF* Outputs

## Source and Path Parameters

Outputs for individual EQ are in file *EQID\_fault\_id.out*. As a
convenience, outputs of all earthquakes in the same batch run are
aggregated in *P4CF.out*.

For each station-earthquake pair, following columns are included in the
output files.

-   *EQID*

-   *SSN* and *StaName*

-   Station coordinates.

    -   (*StaX, StaY*): Cartesian coordinates (*X, Y*) relative to the
        origin specified in the finite-fault file. If *inLatLon* is
        *FALSE*, they are verbatim of the input coordinates.

    -   (*U, T*): GC2 coordinates (Spudich and Chiou, 2015) with respect
        to the rupture trace specified in the finite-fault file

        -   Negative *T* coordinate indicates that station is located on
            the footwall side, while positive *T* coordinate indicates
            station is on the hanging-wall side

-   Distance measures

    -   Point-source distances

        -   ![R\_{epi}](https://latex.codecogs.com/png.latex?R_%7Bepi%7D "R_{epi}"):
            distance between Station and Epicenter
        -   ![R\_{hyp}](https://latex.codecogs.com/png.latex?R_%7Bhyp%7D "R_{hyp}"):
            distance between Station and Hypocenter

    -   Distances to finite-fault

        -   ![R\_{rup}](https://latex.codecogs.com/png.latex?R_%7Brup%7D "R_{rup}"):
            shortest distance to the finite-fault model
        -   ![R\_{jb}](https://latex.codecogs.com/png.latex?R_%7Bjb%7D "R_{jb}"):
            shortest distance to the surface projection of finite-fault
            model (aka Joyner-Boore distance)
        -   ![R\_{rms}](https://latex.codecogs.com/png.latex?R_%7Brms%7D "R_{rms}"):
            root-mean-squared distance to the finite-fault model
            ![\\Big(\\frac{\\int\_{\\Sigma} R(\\zeta, x)^{-2} \\ d\\Sigma}{A}\\Big)^{1/2}](https://latex.codecogs.com/png.latex?%5CBig%28%5Cfrac%7B%5Cint_%7B%5CSigma%7D%20R%28%5Czeta%2C%20x%29%5E%7B-2%7D%20%5C%20d%5CSigma%7D%7BA%7D%5CBig%29%5E%7B1%2F2%7D "\Big(\frac{\int_{\Sigma} R(\zeta, x)^{-2} \ d\Sigma}{A}\Big)^{1/2}"),
            where
            ![\\zeta](https://latex.codecogs.com/png.latex?%5Czeta "\zeta")
            is the location vector of any point inside the ruptured
            surface
            (![\\Sigma](https://latex.codecogs.com/png.latex?%5CSigma "\Sigma")),
            and
            ![R(\\zeta, x)](https://latex.codecogs.com/png.latex?R%28%5Czeta%2C%20x%29 "R(\zeta, x)")
            is the distance betweehn
            ![\\zeta](https://latex.codecogs.com/png.latex?%5Czeta "\zeta")
            and the station location vector
            ![x](https://latex.codecogs.com/png.latex?x "x") (see, for
            example, Chiou and others, 2000).

    -   Distances derived from (*U, T*)

        -   ![R\_x = \|T\|](https://latex.codecogs.com/png.latex?R_x%20%3D%20%7CT%7C "R_x = |T|"),
            strike-normal distance
        -   ![R\_y = U - 2/L](https://latex.codecogs.com/png.latex?R_y%20%3D%20U%20-%202%2FL "R_y = U - 2/L"),
            where *L* is the rupture length, strike-parallel distance
        -   ![R\_{y0}](https://latex.codecogs.com/png.latex?R_%7By0%7D "R_{y0}")

        ![R\_{y0} = 
            \\begin{cases}
               -U    & U &lt; 0 \\\\
               0     & 0  \\le U \\le L \\\\
               U - L & U &gt; 0
            \\end{cases}
          ](https://latex.codecogs.com/png.latex?R_%7By0%7D%20%3D%20%0A%20%20%20%20%5Cbegin%7Bcases%7D%0A%20%20%20%20%20%20%20-U%20%20%20%20%26%20U%20%3C%200%20%5C%5C%0A%20%20%20%20%20%20%200%20%20%20%20%20%26%200%20%20%5Cle%20U%20%5Cle%20L%20%5C%5C%0A%20%20%20%20%20%20%20U%20-%20L%20%26%20U%20%3E%200%0A%20%20%20%20%5Cend%7Bcases%7D%0A%20%20 "R_{y0} = 
            \begin{cases}
               -U    & U < 0 \\
               0     & 0  \le U \le L \\
               U - L & U > 0
            \end{cases}
          ")

-   Strike-parallel direction

    -   *AveStrike*:
        -   If *method* = 1, the average strike-parallel direction,
            computed as the azimuth from the first vertex to the last
            vertex  
        -   If *method* = 2, verbatim of the input strike direction, if
            it is not -999. If input value is -999, *AveStrike* is
            calculated the same as in *method* = 1.
    -   *LocStrike*: Average strike-parallel direction local to the
        recording station
        -   It is computed as the length-weighted average of strike
            direction over a stretch of 20-km rupture starting at the
            closest point and moving toward the hypocenter
    -   Though either strike-parallel direction can be utilized to
        rotate the two horizontal components of acceleration time series
        to the strike-normal and the strike-parallel components,
        ![LocStrike](https://latex.codecogs.com/png.latex?LocStrike "LocStrike")
        is the one that has been used for the two NGA-West Projects.

-   Rupture directivity parameters (Chiou and Spudich, 2013)

    -   *ChPrime*
        (![\\hat{c}'](https://latex.codecogs.com/png.latex?%5Chat%7Bc%7D%27 "\hat{c}'")):
        the approximate isochrone velocity, normalized to a shear-wave
        velocity of 3.2 km/s
    -   *E*: length of the line source initiating at the hypocenter and
        ending at the direct point
    -   *aveR*: avearge radiation pattern over the
        hypocenter-to-direct-point line source
    -   *DPP*: direct-point parameter of rupture directivity
        (![= ...](https://latex.codecogs.com/png.latex?%3D%20... "= ..."))

-   Location of closest point on fault (*PcLat, PcLon, PcZ*)

## Constructed Finite-Fault Model

A detailed description of the constructed fault rupture is included in
file *EQID\_fault\_id.ref*. Another file (*EQID\_fault\_id.PLL*),
includes just the 3D polygon, used mainly for plotting map of rupture
plane.

# Limitations of *P4CF*

P4CF is developed mainly for the situation of variable strike direction
(curved rupture trace), which is a common geometry complexity of
published finite fault models. It is hence limited for modeling other
aspects of geometry complexity.

1.  P4CF applies only to connected rupture trace (that is, no gaps in
    modeled rupture trace)

-   The first step in dealing with disconnected rupture planes is to try
    simplifying the observed rupture by connecting into a single,
    connected trace.
-   Connection is not always feasible for very complex rupture. For
    example, in the case of the 1995 **M**6.9 Kobe earthquake, the two
    ruptured faults (Suma/Suwayama Fault and Nojima Fault) dip in
    opposite direction, making it impossible to connect them in a reason
    fashion. Another example is the 2002 **M**7.9 Denali earthquake
    (EQID 0169 of the NGA-W2 database). The three ruptured faults cannot
    be satisfactorily connected because their traces overlap
    considerably in horizontal extent. In addition, Susitna Glacial
    Fault is a dipping reverse fault, while the other two (Denali Fault
    and Totschunda Fault) are near vertical strike-slip faults. For
    complex rupture, each individual fault can be modeled separately and
    their P4CF results consolidated afterward.

2.  When using projection method 1, joint variation of strike and dip is
    allowed, though not encouraged. P4CF does not check nor guarantee
    against pathological rupture plane. So, when using multiple dip
    angles, checking the geological adequacy of constructed fault plane
    (.ref output file) is strongly recommend, particularly when there
    are short segments or large change in strike direction.

-   When using projection method 2, this is not an issue because only a
    single dip angle is required (for the purpose of projecting rupture
    trace down-dip).

3.  Depth to the rupture trace must be a constant. This restriction is
    enforced by allowing only a single
    ![H\_{top}](https://latex.codecogs.com/png.latex?H_%7Btop%7D "H_{top}")
    input.

4.  Rake angle is uniform.

-   In P4CF rake is used in computing average radiation pattern.
    Variable rake is allowed and relaxing this restriction is feasible,
    it has not yet been implemented in P4CF. (… Is a single rake used in
    radiation pattern calculation?)

# Example Runs

Directory ‘Examples’ contains examples runs (inputs and outputs) for

-   Single fault, single segment (case1)
-   Single fault, multiple segments
    -   method1 (case2)
    -   method2 (case3)
-   Multiple (disjoint) faults (case4)
