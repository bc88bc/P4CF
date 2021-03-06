README
================
Brian Chiou,
2021-07-01

-   [Compiling *P4CF.for*](#compiling-p4cffor)
-   [Running *P4CF*](#running-p4cf)
    -   [Command-Line Inputs](#command-line-inputs)
    -   [File Inputs](#file-inputs)
-   [*P4CF* Outputs](#p4cf-outputs)
    -   [Constructed Finite-Fault
        Model](#constructed-finite-fault-model)
    -   [Source and Path Parameters](#source-and-path-parameters)
-   [Program Notes](#program-notes)
    -   [1. Coordinate system](#1-coordinate-system)
    -   [2. Down-dip extension](#2-down-dip-extension)
    -   [3. Discretization of rupture
        surface](#3-discretization-of-rupture-surface)
-   [Limitations of *P4CF*](#limitations-of-p4cf)
-   [Example Runs](#example-runs)
-   [References](#references)

FORTRAN code *P4CF* computes earthquake source and propagation path
**P**arameters for(**4**) a **C**urved **F**ault rupture where strike
direction changes along its length.

This code has been used in PEER’s NGA-West Projects (Chiou et al. 2008;
Ancheta et al. 2014) and NGA-Subduction Project (Contreras and others,
2021, submitted to *Earthquake Spectra*) to provide some of the source
and path metadata included in the respective ground-motion database. The
path parameters include common distance measures used in a GMPE and the
direct-point directivity parameters (Chiou and Spudich 2013). The source
parameters include ruptured length, ruptured area, strike directions
(including both global average and local variation), and the location of
the closest point on fault.

# Compiling *P4CF.for*

*P4CF.for* is a FORTRAN77 code. Windows operating system is the primary
environment under which P4CF has been developed, tested, and applied.
This README file is for the Windows environment.

P4CF has been successfully compiled by the following two compilers.

1.  Layhey Fortran (release 5.60a): lf95 p4cf.for -dbl

2.  GNU Fortran (gcc version 4.7.1): gfortran p4cf.for -freal-4-real-8
    -o p4cf.exe

Note that *P4CF* must be complied with the double precision option
turned on.

# Running *P4CF*

## Command-Line Inputs

To run *P4CF*,

-   open a DOS command prompt,

-   switch to the working directory where input files are located,

-   type P4CF (including the proper path to *P4CF.exe* if a copy is not
    included in the working directory),

-   hit \[RETURN\],

-   enter the following inputs

    -   Line 1: *EQID\_faultID* (character, ‘a5’).

        -   A concatenation of *EQID* (first 4 characters) and *faultID*
            (fifth character).
            -   *faultID* is a identifier for the fault member of a
                multiple-fault rupture (see NOTE \#? below).

    -   Line 2: *inLatLong* (logical)

        -   If FALSE, Cartesian coordinates (*X, Y*) are used to specify
            locations on earth’s surface
        -   If TRUE, (*Latitude, Longitude*) are used to specify
            locations
        -   See Note \#1

    -   Line 3: *method* (integer)

        -   Two methods can be used to extend the rupture trace down-dip
            to form the rupture surface at depth:
            -   *method* = 1, the trapezoid method
            -   *method* = 2, the parallelogram method
        -   See Note \#2 for descriptions of these two methods

    -   Line 4 (only when *method* = 2): *AveStrike* (real)

        -   The strike direction (in degrees) perpendicular to which
            down-dip extension is performed
            -   If *AveStrike* = -999, this input value is overridden in
                *P4CF* by the azimuth from the first vertex to the last
                vertex of user-specified rupture trace

    -   Line 5 (only when *method* = 2): *AveDip* (real)

        -   The dip angle (in degrees) to be used to construct the
            down-dip portion of rupture surface

    -   To continue to next earthquake, repeat Line 1 through Line 5

    -   To terminate, enter ‘q’ on Line 1.

## File Inputs

In addition to the above command-line inputs, two input files are
required per earthquake.

1.  Finite-fault file, whose name must be the concatenation of
    *EQID\_faultID* and ‘.tll’ (*inLatLon* is *TRUE*) or ‘.txy’
    (*inLatLon* is *FALSE*)

2.  Station file, whose name must be the concatenation of *EQID*
    (without *faultID*) and ‘.sll’ (*inLatLon* is *TRUE*) or ‘.sxy’
    (*inLatLon* is *FALSE*)

-   The same set of stations is assumed for all fault members, hence
    station-file name does not include *faultID*.

-   *P4CF* assumes these two files are located in the working directory
    where the *P4CF* run is made.

### Input file of finite-fault model

-   Line 1: Header (‘5x, a65’)

(NOTE: Free format is used in *P4CF* to read the rest of this file.)

-   Line 2: Origin of the (X, Y) coordinate

    -   if *inLatLon* is *FALSE*, (0, 0)

    -   if *inLatLon* is *TRUE*, (*Latitude, Longitude*) of the origin

-   Line 3: Hypocenter location (*Hyp\_x, Hyp\_y, Hyp\_z*)

    -   If *inLatLon* is *FALSE*, hypocenter is given in Cartesian
        coordinate (*X, Y, Z*). If *inLatLon* is *TRUE*, hypocenter is
        given as (*Latitude, Longitude*, *Z*)
        -   Note that elevation *Z* is negative if hypocenter is below
            sea level
    -   Hypocenter can also be specified in terms of along-strike and
        down-dip locations relative to the first vertex of rupture trace
        (the upper-left corner of rupture surface when viewed from the
        hangingwall). To use this option, set *Hyp\_x* to -9999;
        *Hyp\_y* and *Hyp\_z* are then interpreted as the along-strike
        and down-dip locations, in fractions of rupture length and
        rupture width, respectively.

-   Line 4: Number of linear segments comprising the rupture trace
    (*nseg*)

-   Lines 5 through (5 + *nseg*): Coordinates of the (*nseg* + 1)
    vertices which define the rupture trace.

    -   Strike of the rupture trace dictates the order in which its
        vertices are specified, and vice versa. Strike of segment
        ![i](https://latex.codecogs.com/png.latex?i "i") is the azimuth
        from vertex *i* to vertex *i+1*.

    -   If *inLatLon* is FALSE, vertices are given in (X, Y) coordinate.
        If *inLatLon* is TRUE, vertices are given in (*Latitude,
        Longitude*).

    -   Depth of the vertices is specified on the next line as *H\_Top*.

-   Line (5 + *nseg* + 1): (*Dip*(i), i = 1, *nseg*), *H\_Trace*,
    *H\_Top*, *H\_Bot*

    -   (*Dip*(i), i = 1, *nseg*): dip angle (in degrees) of each
        rupture segment
    -   *H\_Trace*: depth (in KM) of fault trace
        -   *H\_Trace* is deprecated; it is kept for the sake of
            backward compatibility
    -   *H\_Top*: depth (in KM) of rupture trace
        (![Z\_{TOR}](https://latex.codecogs.com/png.latex?Z_%7BTOR%7D "Z_{TOR}")
        in GMPE parlance); same value for all segments
    -   *H\_Bot*: depth (in KM) to the bottom of rupture surface; same
        value for all segments

-   Line *5+nseg+2*: (*Rake*(i), i = 1, *nseg*)

    -   Rake angle (in degrees) on each rupture segment.
    -   So far, rake is used only in the computation of radiation
        pattern coefficient

### Recording stations and their coordinates

In a station file, following inputs are listed one line per recording
station.

-   *staID*, *StaName*, *Sta\_x*, *Sta\_y* (*(a10, a55, 2f10.4)*)

    -   *staID* is the unique sequence number of the station, usually
        assigned by the ground-motion database developer.

    -   *Sta\_name* is a station description that may include
        information about geographic locality, sensor location, network
        owner, etc. This field is usually taken from the station
        description given by the network operator.

    -   (*Sta\_x*, *Sta\_y*), depending on the value of *inLatLon*, are
        either the Cartesian coordinates (X, Y) or geographic
        coordinates (*Latitude, Longitude*) of the station

# *P4CF* Outputs

## Constructed Finite-Fault Model

Detailed information of the constructed rupture surface is included in
the output file *EQID\_faultID.ref*. Another output file
(*EQID\_faultID.PLL*), which includes only information about rupture
surface location, is also created for ease of use in certain
post-calculation tasks, such as the plotting of rupture surface on a
map.

## Source and Path Parameters

Output parameters are written to file *EQID\_faultID.out*. As a
convenience for post-processing, outputs of all earthquakes in the same
batch run are bundled in file *P4CF.out*.

For each station in an earthquake, following columns are included in the
output files. Note that the output format of every real variable is
‘f15.7’; it will not be repeated below. See also Note \#3.

-   *EQID*: Verbatim (‘a5’) from the input *EQID\_faultID*.

-   *SSN* : Verbatim (‘a10’) from the input *StaID*, right-adjusted

-   *StaName*: Verbatim (‘a55’) from the input ‘Sta\_name,’
    right-adjusted

-   Station coordinates (in KM)

    -   (*StaX, StaY*): Cartesian coordinate (*X, Y*) relative to the
        user-specified origin. If *inLatLon* is *FALSE*, they are
        verbatim from the input coordinate.

    -   (*U, T*): GC2 coordinate (Spudich and Chiou 2015) with respect
        to the user-specified rupture trace.

        -   Station located on the hanging-wall side has a positive *T*

-   Point-source distances (in KM)

    -   ![R\_{epi}](https://latex.codecogs.com/png.latex?R_%7Bepi%7D "R_{epi}"):
        distance between Station and Epicenter
    -   ![R\_{hyp}](https://latex.codecogs.com/png.latex?R_%7Bhyp%7D "R_{hyp}"):
        distance between Station and Hypocenter

-   Distance measures to finite-fault (in KM)

    -   ![R\_{rup}](https://latex.codecogs.com/png.latex?R_%7Brup%7D "R_{rup}"):
        shortest distance to the finite-fault rupture model

    -   ![R\_{jb}](https://latex.codecogs.com/png.latex?R_%7Bjb%7D "R_{jb}"):
        shortest distance to the surface projection of finite-fault
        rupture (the Joyner-Boore distance)

    -   ![R\_{rms}](https://latex.codecogs.com/png.latex?R_%7Brms%7D "R_{rms}"):
        root-mean-squared distance to the finite-fault rupture (see, for
        example, Chiou and others, 2000). It is computed as
        ![\\Big(\\frac{\\int\_{\\Sigma} R(\\zeta, x)^{-2} \\ d\\Sigma}{A}\\Big)^{-1/2}](https://latex.codecogs.com/png.latex?%5CBig%28%5Cfrac%7B%5Cint_%7B%5CSigma%7D%20R%28%5Czeta%2C%20x%29%5E%7B-2%7D%20%5C%20d%5CSigma%7D%7BA%7D%5CBig%29%5E%7B-1%2F2%7D "\Big(\frac{\int_{\Sigma} R(\zeta, x)^{-2} \ d\Sigma}{A}\Big)^{-1/2}"),
        where
        ![R(\\zeta, x)](https://latex.codecogs.com/png.latex?R%28%5Czeta%2C%20x%29 "R(\zeta, x)")
        is the distance between an arbitrary point
        ![\\zeta](https://latex.codecogs.com/png.latex?%5Czeta "\zeta")
        on the ruptured surface
        ![\\Sigma](https://latex.codecogs.com/png.latex?%5CSigma "\Sigma")
        and the station located at
        ![x](https://latex.codecogs.com/png.latex?x "x").
        ![A](https://latex.codecogs.com/png.latex?A "A") is the area of
        ![\\Sigma](https://latex.codecogs.com/png.latex?%5CSigma "\Sigma").

-   Distances derived from (*U, T*)

    -   ![R\_x = \|T\|](https://latex.codecogs.com/png.latex?R_x%20%3D%20%7CT%7C "R_x = |T|"),
        strike-normal distance to rupture trace

    -   ![R\_y = U - L/2](https://latex.codecogs.com/png.latex?R_y%20%3D%20U%20-%20L%2F2 "R_y = U - L/2"),
        strike-parallel distance along rupture trace

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

    -   *L* is the rupture length

-   Strike-parallel direction

    -   *AveStrike*:
        -   If *method* = 1, the average strike-parallel direction is
            taken as the azimuth from the first vertex to the last
            vertex of user-specified rupture trace.
        -   If *method* = 2, verbatim from the input strike direction.
            If input value is -999, *AveStrike* is calculated in *P4CF*
            the same way as for *method* = 1.
    -   *LocStrike*: Average strike-parallel direction local to the
        recording station
        -   It is computed as the length-weighted average of strike
            direction over a stretch of 20-km rupture starting at the
            closest point and extending toward the hypocenter
    -   Though either strike-parallel direction can be utilized to
        rotate the two horizontal components of acceleration time series
        to the strike-normal and the strike-parallel components,
        *LocStrike* is the one that has been used for the two NGA-West
        Projects.

-   Rupture directivity parameters (Chiou and Spudich 2013)

    -   *ChPrime*
        (![\\hat{c}'](https://latex.codecogs.com/png.latex?%5Chat%7Bc%7D%27 "\hat{c}'")):
        the approximate isochrone velocity, normalized to a shear-wave
        velocity of 3.2 km/s
    -   *E*: length of a line source, initiating at the hypocenter and
        ending at the direct point
    -   *aveR*: average S-wave radiation pattern over the
        hypocenter-to-direct-point line source
    -   *DPP*: direct-point parameter of rupture directivity

-   Location of closest point on fault (*PcLat, PcLon, PcZ*)

# Program Notes

## 1. Coordinate system

In *P4CF*, calculations are performed in a Cartesian (*X, Y, Z*)
coordinate system.

-   (*Latitude, Longitude*) will be transformed to (X, Y) coordinate by
    first using the *haverside* formula to compute the great-circle
    distance between the user-specified origin and the point of
    interest, then decomposing the distance into the east component (X)
    and the north component (Y).
    -   (X, Y) is transformed back to (*Latitude, Longitude*) when the
        latter is the preferred output format.
-   *Z* is the elevation (in KM)
    -   For a point below sea level, its *Z* value is negative, while
        depth (*H*) is positive.

## 2. Down-dip extension

-   *method* = 1, the *trapezoid* approach; also called the ‘Segments
    orthogonal’ approach (Hale, Abrahamson, and Borgzonia 2018)

    -   This method involves two steps.

        -   In the first step, simple rectangles are generated for each
            segment of the input rupture trace, taking the segment trace
            as the top edge of rectangle and forming the side edge
            according to the local dip angle, *H\_Top*, and *H\_Bot*.
            Except in the case of vertical rupture, these rectangles are
            non-contiguous, in the sense that two consecutive rectangles
            do not share a common side edge.

        -   In the second step, the side edges are modified so that two
            consecutive rupture segments share a common side edge. The
            common edge is taken as the line segment connecting the
            shared vertex of top edges and the intersection point
            between the two bottom edges. The second step transforms the
            non-contiguous rectangles into contiguous trapezoids.

    -   This approach has been used in several PSHA codes (Hale,
        Abrahamson, and Borgzonia 2018, Page 41). In the 1996 and 2002
        USGS National Seismic Maps, the first step of *method* 1 was
        used, but not the second step.

-   *method* = 2, the *parallelogram* approach; also called the
    ‘Stirling Method’ (Hale, Abrahamson, and Borgzonia 2018)

    -   In this method, only a single global value is required for each
        of the four basic fault parameters (*AveStrike*, *AveDip*,
        *H\_Top*, and *H\_Bot*). Down-dip extension is orthogonal to the
        *AveStrike* direction and following the dip angle *aveDip* for
        all segments. As such, two consecutive rupture segments will
        automatically be contiguous. Each of the created segment is a
        parallelogram.

        -   The ‘Ave’ prefix in *AveStriek* and *AveDip* reflects the
            fact that, in practice, average values are often used as
            inputs. However, other values may also be used as long as
            they are seismologically/geologically sound.

    -   *method* 2 has been used to model complex rupture geometry.

        -   Several PSHA codes use this approach to model the geometry
            of bending fault (Hale, Abrahamson, and Borgzonia 2018,
            Page 41)

        -   OpenSHA calls fault surface created by this method the
            ‘Stirling Fault Surface’
            (<https://opensha.org/Glossary.html#stirling-fault-surface>).

-   So far, NGA GMPE developers’ consensus preference has been *method*
    1, for the purpose of computing source and path metadata. This
    decision is driven mainly by the desire to maintain the standard
    practice that dip direction is perpendicular to the strike
    direction.

## 3. Discretization of rupture surface

*P4CF* does not discretize the constructed rupture surface into a grid
of small rupture areas to facilitate the computation of various
parameters. Linear algebra and, when available, analytical (or
semi-analytical) solutions are used in PC4F. This has the benefits of
faster computation and improved precision. For examples,

-   The optimization approach to solve for point-to-triangle distance
    (Schnieder and Eberly 2002) is used in *P4CF* for the computation of
    ![R\_{RUP}](https://latex.codecogs.com/png.latex?R_%7BRUP%7D "R_{RUP}")
    and
    ![R\_{JB}](https://latex.codecogs.com/png.latex?R_%7BJB%7D "R_{JB}")
-   Semi-analytical method is used for the computation of
    ![R\_{rms}](https://latex.codecogs.com/png.latex?R_%7Brms%7D "R_{rms}")
-   The analytical solution of Chiou and Spudich (2013) is used for the
    computation of *aveR*

# Limitations of *P4CF*

*P4CF* is developed mainly for use with a curved but connected rupture
trace. By ‘curved’ we mean that strike changes along rupture length,
which is a type of geometrical complexity often seen in published
finite-fault models. *P4CF* has limited capabilities dealing with other
types of rupture complexity.

-   When using *method* 1f (trapezoid approach), followings should be
    avoided to reduce the chance of pathological geometry

    -   Short segment
    -   Large change in strike between two consecutive segments
    -   Change in dip angle along rupture length

    If any of the above is present in finite fault input, it is
    important to check the reasonableness of constructed fault surface.

-   The above limitations of *method* 1 usually do not apply to
    *method* 2.

    -   *AveStrike* is used only to determine the direction of down-dip
        extension. Local strike is honored in other parts of the
        calculations by *P4CF*.
    -   The resulting dip angles of the constructed parallelograms,
        however, may differ between segments and differ from the input
        value.

-   Non-constant
    ![H\_{Top}](https://latex.codecogs.com/png.latex?H_%7BTop%7D "H_{Top}")
    is not allowed. This restriction is enforced by allowing only a
    single
    ![H\_{top}](https://latex.codecogs.com/png.latex?H_%7Btop%7D "H_{top}")
    input.

-   Rake angle may be nonuniform.

    -   In *P4CF*, local rake and local strike are used in the
        computation of *aveR*. (**I need to verify this**.)

-   *P4CF* does not always work for geometrically complicated rupture.

    -   When rupture is modestly complex (for example, when there are
        small gaps or overlaps within the two ends of rupture), one can
        try simplifying the rupture by connecting individual pieces into
        a single connected trace, at the expense of some modifications
        to the mapped rupture that do not severely affect the source and
        path parameters. An example of such simplification is the 1992
        **M** 7.2 Landers earthquake in the NGA-W1 Project.

    -   Above approach does not work for very complex rupture, such as
        those with discordant dip direction, long overlapping branches,
        or large offset at depth. As an example, in the case of the 1995
        **M** 6.9 Kobe earthquake, the two ruptured faults
        (Suma/Suwayama Fault and Nojima Fault) dip to opposite
        direction, making it impossible to connect them in a reasonable
        way. Another example is the 2002 **M** 7.9 Denali earthquake.
        The three ruptured faults cannot be satisfactorily connected
        because rupture traces overlap considerably in the direction
        along length. Furthermore, the Susitna Glacial Fault is a
        reverse fault and it dips in the direction away from the near
        vertical Denali Fault. The large separation at depth between the
        two faults prevents them from being connected in a reasonable
        way.

    -   The approach used in NGA Projects to deal with such complex
        ruptures is to first construct several fault models to
        approximate the observed rupture complexity, run *P4CF*
        separately for each fault, and consolidate the multiple results
        afterward. To facilitate this approach, P4CF uses *faultID* to
        track calculations from different faults.

# Example Runs

Directory ‘Examples’ contains several examples runs (inputs and
outputs):

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Ancheta14" class="csl-entry">

Ancheta, Timothy D., Robert B. Darragh, Jonathan P. Stewart, Emel
Seyhan, Walter J. Silva, Brian S.-J. Chiou, Katie E. Wooddell, et al.
2014. “NGA-West2 Database.” *Earthquake Spectra* 30 (3): 989–1005.
<https://doi.org/10.1193/070913EQS197M>.

</div>

<div id="ref-Chiou08" class="csl-entry">

Chiou, Brian, Robert B. Darragh, Nick Gregor, and Walter Silva. 2008.
“NGA Project Strong-Motion Database.” *Earthquake Spectra* 24 (1):
23–44. <https://doi.org/10.1193/1.2894831>.

</div>

<div id="ref-Chiou&Spudich13" class="csl-entry">

Chiou, Brian, and Paul Spudich. 2013. “The Chiou and Spudich NGA-West2
Directivity Predictor DPP.” Pacific Earthquake Engineering Research
Center, Chapter 6 of Final Report of the NGA-West2 Directivity Working
Group, PEER Report 2013/09.

</div>

<div id="ref-Hale18" class="csl-entry">

Hale, Christie, Norman Abrahamson, and Yousef Borgzonia. 2018.
“Probabilistic Seismic Hazard Analysis Code Verification.” Pacific
Earthquake Engineering Research Center, PEER Reprot 2018/03.

</div>

<div id="ref-Eberly02" class="csl-entry">

Schnieder, Philip, and David Eberly. 2002. *Geometric Tools for Computer
Graphics*. First Edition. The Morgan Kaufmann Series in Computer
Graphics. Morgan Kaufmann.

</div>

<div id="ref-Spudich&Chiou15" class="csl-entry">

Spudich, Paul, and Brian Chiou. 2015. “Strike-Parallel and Strike-Normal
Coordinate System Around Geometrically Complicated Rupture Traces—Use by
NGA-West2 and Further Improvements.” U.S. Geological Survey Open-File
Report, 2015-1028. <https://dx.doi.org/10.3133/ofr20151028>.

</div>

</div>
