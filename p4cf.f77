        Program P4CF
c
c Version 2.7 is modfied from version 2.6 to exlcude unnecessary or experimental
c variables from outputs.
c
c This version has been successfully compiled by
c
c 1. Layhey compiler (release 5.60a)
c      lf95 p4cf.for -dbl
c
c 2. GNU Fortran (gcc version 4.7.1)
c      gfortran p4cf.for -freal-4-real-8 -o p4cf.exe
c
c Note: Compile with double precision option turned on
c

        implicit none
        integer mxsg
        parameter(mxsg = 40)

c User Inputs
        character faultID*5, faultName*65, model*2, a1*1
        integer method
        logical inLatLong, inOneFile
        real Org_Lat, Org_Long
        real Hyp_x, Hyp_y, Hyp_z
        integer nseg, GC_Type
        real P(3,4,mxsg), H_Top, H_Bot, H_Trace, Dip(mxsg), Rake(mxsg)
        real AveStrike, AveDip, Rrms

        character StaID*10, StaName*55
        real Sta_x, Sta_y

c Working variables
        real Rrup, Rjb, Repi, Rhyp, Rpd, Pc(3), Pd(3), Pc_Lat, Pc_Lon
        real Sta_U, Sta_T, Sta_SN(mxsg), Sta_SP(mxsg), Sta_W
        real Hyp_U, Hyp_T, Hyp_SN(mxsg), Hyp_SP(mxsg), Hyp_W
        real Rfn, Rfp, Rri, F_RV, F_NM, F_HW, fD(10), lStrike
        real IDP, ct_prime, S, D,  C_cap, S_cap
        real IDP_Pd, ch_prime, S_Pd, E, C_cap_Pd, E_cap_Pd
        real EwC,    EwC2,    XwC2,    EwCinvr,    EwC2invr
        real EwCPhi, EwC2Phi, XwC2Phi, EwCPhiinvr, EwC2Phiinvr
        real F1, F2, F3, F4, F5, F6, F7, DPP, aveR

c Working variables
        character EQID*4
        real SPF(2,(mxsg+1)*2+1), Len(mxsg), Width(mxsg), Strike(mxsg)
        real dtr, tmp1, tmp2
        real Q(3,2,mxsg), sQ2(mxsg+1), sHyp, sPc, sPd
        real Phi, Theta, s2L, d2W
        real EffL, t(mxsg), u(mxsg), tmp_x, tmp_y, tmp_z
        integer i, j, k, iSegHyp, iSegPc, iSegPd, iEdgPc, iEdgPd, nE,
     1          iStrike1, iStrike2

        real TotalTopL, TotalBotL, AveWidth, MaxWidth, TotalArea
        real Ry0
        real HypI_x, HypI_y, HypI_z, sHypI, RepiI, RhypI

c Common block (fault coordinate of station hypo, & fault corners)
        real Sta_Fx(mxsg),  Sta_Fy(mxsg),  Sta_Fz(mxsg),
     1       Hyp_Fx(mxsg),  Hyp_Fy(mxsg),  Hyp_Fz(mxsg),
     1       HypI_Fx(mxsg), HypI_Fy(mxsg), HypI_Fz(mxsg),
     1       P_Fx(4,mxsg),  P_Fy(4,mxsg),  P_Fz(4,mxsg)
        common /faultCoord/Hyp_Fx,  Hyp_Fy,  Hyp_Fz,
     1                     HypI_Fx, HypI_Fy, HypI_Fz,
     1                     P_Fx,    P_Fy,    P_Fz,
     1                     Sta_Fx,  Sta_Fy,  Sta_Fz


        dtr = atan(1.0)/45.0
        GC_Type   = 2

        write (*,1)
1       FORMAT(/,
     , '    *************************************************',/,
     , '    *           Program P4CF, Version 2.7           *',/,
     , '    *                  (Mar, 2021)                  *',/,
     , '    *             Written by B. Chiou               *',/,
     , '    *************************************************',/)

c
c EQ-Fault Loop  .........
c
        call getarg(1, a1)
c        print*, a1
        inOneFile = .FALSE.
        if (a1 .eq. 'Y' .or. a1 .eq. 'y') inOneFile = .TRUE.
        if (inOneFile) then
           print*, 'Outputs will be to a single file.'
          if (inOneFile) open(13, file='p4cf.out')
        endif

1000    write (*,*) 'EQ/FAULT Identifier (a5): '
        read (*, '(a)') faultID
        if (faultID.eq.'q'.or.faultID.eq.'Q') go to 9999

        EQID = faultID(1:4)
        write (*,*) EQID

        open (2,  file=trim(faultID)//'.Out')
        open (3,  file=trim(faultID)//'.Ref')
        open (22, file=trim(faultID)//'.PLL')
        open (30, file='P4CF.Hyp')

        write (*,*) 'Coordinates are in Latitude/Longitude? (t/f) '
        read (*, *) inLatLong

        write (*,*) 'Method 1 (Trapezoid) or 2 (Parallelogram): '
        read (*,*) method

        if (method.eq.2) then
c        method 2
         write (*,*) 'Input average strike; '
         write (*,*)
     1    'if -999, Ave. Strike is formed from the two end vertices :  '
         read (*,*) AveStrike
         if (AveStrike .NE. -999) AveStrike = AveStrike * dtr
         write (*,*) 'Input average dip (0 to 90) :  '
         read (*,*) AveDip
         AveDip = AveDip * dtr
        else
c         method 1
          AveStrike = -999
        endif

        call writeFileHeaders(inOneFile)

c .. Fault geometry inputs
c ..   Strike direction is to be computed from the vertices of fault trace
        call Fault_Inputs(faultID, faultName, inLatLong,
     1                    Org_Lat, Org_Long,
     1                    Hyp_x, Hyp_y, Hyp_z,
     1                    nseg, P,
     1                    Dip, H_Trace, H_Top,
     1                    H_Bot, Rake)

c  .. Dip direction is always to the right of strike direction.
c     Hence, dip angle's sign is ignored.
        do i=1,nseg
           dip(i) = abs(dip(i))
        enddo

c .. Construct down-dip portion of fault rupture

        if (method.eq.1) then

           write (3,*)
           write (3,'(a)') '---- Trapezoids ----'

c  .... Set H_Top = H_Trace to prohibit the creation of new trapezoids

           call Fault_Rectangles(nseg, P, H_Trace, H_Top, H_Bot, Dip,
     1          Len, Width, Strike)
           call Fault_Trapezoids(nseg, P)

c          Compute average strike
c          - For method 1, computed aveerage strike is not used for
c            any computation; it's for checking purpose only
           call distaz(P(1,1,1),    P(2,1,1),    0.0,
     1                 P(1,2,nseg), P(2,2,nseg), 0.0,
     1                 tmp1, AveStrike)

        elseif (method.eq.2) then

           write (3,*)
           write (3,'(a)') '---- Fault Parallelograms ----'

           call Fault_Parallelograms(nseg, P, H_Trace, H_Top, H_Bot,
     1          AveDip, Len, Width, Strike, AveStrike)

c          Compute dip angle of each constructed parallelogram
c          - dip angle of parallelogram is likely different from AveDip
           call DipAngle(nseg, P, Strike, Dip)

        endif

c       Pre-compute unit vectors
        call GetUnitVectors(nseg, Strike, Dip, Rake)

c     . Transform Cartesian coordinates X-Y-Z to fault coordinates Fx-Fy-Fz
c       - Fz is positive for location below the fault plane
        Do i=1,nSeg
          do j=1,4
            call XYZtoUVW(P(1,j,i), P(2,j,i), P(3,j,i), i, P,
     1                    P_Fx(j,i), P_Fy(j,i), P_Fz(j,i))   !! P_Fz should be 0
          enddo
        Enddo

c       Write fault geometry, surface project, and hypocenter to *.ref file

        write (3,*)
        do k=1,nseg
          write (3,'(a10,i5)')   'Segment = ', k
          write (3,'(a9,f10.4)') 'Strike = ', Strike(k)/dtr
          write (3,'(a9,f10.4)') 'Dip    = ', Dip(k)/dtr
          write (3,'(i5)') nseg
          write (3, '(3f10.4)') ((P(i,j,k),i=1,3),j=1,4)
          write (3, '(3f10.4)') ((P(i,j,k),i=1,3),j=1,1)
          write (3,*)
        enddo

        call Surface_Projection_Of_Fault(nseg, P, SPF)

        write (3,*)
        write (3,'(a)') '---- Surface Projection of Fault ----'
        write (3,*)
        do k=1,nseg*2+3
           tmp1 = SPF(1,k)
           tmp2 = SPF(2,k)
           call XY_to_LatLong(Org_Lat, Org_Long, tmp1, tmp2)
           write (3, '(2f10.4)') tmp1, tmp2
        enddo
        write (3,*)
        do k=1,nseg*2+3
           write (3, '(2f10.4)') (SPF(i,k),i=1,2)
        enddo

        write (3,*)
        write (3,'(a)') '---- Hypocenter ----'

        if (inLatLong) then
          tmp1 = Hyp_x
          tmp2 = Hyp_y
          call XY_to_LatLong(Org_Lat, Org_Long, tmp1, tmp2)
          write (3,'(a10,3f10.4)') 'Hyp.LL =', tmp1,  tmp2,  Hyp_z
        else
          write (3,'(a10,3f10.4)') 'Hyp.XY =', Hyp_x, Hyp_y, Hyp_z
        endif

c   Q stores the vertices of auxliary line at hypo depth;
c  ..  Q(i,j,k) -- i=1,3, coordinates of a vertice;
c                      j=1,2, the two vertices defining line segment k
c  ..  sQ2 stores the s-coordinates of the 2nd vertice Q(i,2,k) of segment k
c  ..  sHyp stores the s-coordinate of hypocenter
c      (sHyp and sQ2 are used by subroutines LocalStrikeParrallel
c       and isochrone_directivity.)

        call Hypocenter (faultID, nseg, P, Width, Dip,
     1    H_top, H_bot, Hyp_x, Hyp_y, Hyp_z,
     1    Org_Lat, Org_Long, inLatLong, Q, sQ2, sHyp, s2L, d2W, iSegHyp)

c       Write lat/long of hypo and fault trapezoids/parallelogram to
c        PLL file
        tmp1 = Hyp_x
        tmp2 = Hyp_y
        call XY_to_LatLong(Org_Lat, Org_Long, tmp1, tmp2)
        write (22, '(a11, 3f11.4)') 'Hypocenter:', tmp1, tmp2, Hyp_z

        write (22, '(i5,a9)') nseg, " Segments"
        do k=1,nseg
          write (22,'(a7,i6)')   'Segment', k
          write (22,'(i3)') 5
          do j=1,4
           tmp1 = P(1,j,k)
           tmp2 = P(2,j,k)
           call XY_to_LatLong(Org_Lat, Org_Long, tmp1, tmp2)
           write (22, '(3f11.4)') tmp1, tmp2, P(3,j,k)
          enddo
          tmp1 = P(1,1,k)
          tmp2 = P(2,1,k)
          call XY_to_LatLong(Org_Lat, Org_Long, tmp1, tmp2)
          write (22, '(3f11.4)') tmp1, tmp2, P(3,1,k)
        enddo

c       Compute fault-coordinate and GC2 of input hypocenter location
        do i=1,nSeg
          call XYZtoUVW(Hyp_x,  Hyp_y,  Hyp_z, i, P,
     1                  Hyp_Fx(i), Hyp_Fy(i), Hyp_Fz(i))   !! Hyp_Fz should be 0
        enddo

        call Generalized_Coord(Hyp_x, Hyp_y, nseg, P, Len, Strike,
     1                         GC_Type, Hyp_SN, Hyp_SP, Hyp_U, Hyp_T,
     1                         Hyp_W)

c       Modified hypocenter for use in computing DPP
c       - (HypI_x, HypI_y, HypI_z), (HypI_Fx, HypI_Fy, HypI_Fz)

        write (3,'(a)') '---- Hypocenter for DPP Computation ----'

        call HypocenterDPP(Hyp_x, Hyp_y, Hyp_z, sHyp, s2L, d2W,
     1                     iSegHyp, H_top, H_bot, Width, nseg, P,
     1                     inLatLong, Org_Lat, Org_Long,
     1                     HypI_x, HypI_y, HypI_z)

        do i=1,nSeg
          call XYZtoUVW(HypI_x,  HypI_y,  HypI_z, i, P,
     1                  HypI_Fx(i), HypI_Fy(i), HypI_Fz(i))  !! HypI_Fz should be 0
        enddo

c       Write summary of fault rupture
        write (3,*)
        write (3,'(a)') '---- Dimension of Fault ----'
        call FaultArea(nseg, P,
     1             TotalTopL, TotalBotL, AveWidth, MaxWidth, TotalArea)
        write (3,'(''Total Trace Length  '', f8.2)') TotalTopL
        write (3,'(''Average Fault Width '', f8.2)') AveWidth
        write (3,'(''Total Fault Area    '', f8.1)') TotalArea
        write (3,'(''Average Strike      '', f8.2)') AveStrike/dtr
        write (3,*)

        write (3,'(a)') '---- Auxilary Line at Focal Depth  ----'
        write (3,'(30(20x,i10))') (i,i=1,nseg)
        write (3,'(90f10.4)') (Q(1,1,i),Q(2,1,i),Q(3,1,i),i=1,nseg)
        write (3,'(90f10.4)') (Q(1,2,i),Q(2,2,i),Q(3,2,i),i=1,nseg)
        write (3,'(30(20x,f10.4))') (sQ2(i),i=1,nseg)
        write (3,*)

c
c Station Loop .........
c

        if (inLatLong) then
            open (1, file=trim(EQID)//'.sll', status='old')
        else
            open (1, file=trim(EQID)//'.sxy', status='old')
        endif

        read (1, *)
2000    read (1,'(a10,a55,2f10.4)',end=1000)
     1          staID, StaName, Sta_x, Sta_y

        if (inLatLong) call LatLong_to_XY(Org_Lat, Org_Long,
     1                Sta_x, Sta_y)

        do i = 1, nSeg
          call XYZtoUVW(Sta_x, Sta_y, 0.0, i, P,
     1                  Sta_Fx(i), Sta_Fy(i), Sta_Fz(i))
        enddo

        Repi = (Sta_x-Hyp_x)**2 + (Sta_y-Hyp_y)**2
        Rhyp = sqrt(Repi + Hyp_z**2)
        Repi = sqrt(Repi)

        call Joyner_Boore_Distance(Sta_x, Sta_y, nseg, SPF, Rjb)

        call rmsD(nseg, P, Sta_x, Sta_y, Rrms)

c       Compute the closest point on ruptured fault (Pc)
c       - Rrup: distance from station to Pc
        call Closest_Point_on_Fault(Sta_x, Sta_y, 0.0, nseg, P,
     1          iSegHyp, Rrup, Pc, iSegPc, iEdgPc)

        Pc_Lat = Pc(1)
        Pc_Lon = Pc(2)
        call XY_to_LatLong(Org_Lat, Org_Long, Pc_Lat, Pc_Lon)

c       Pc*: projected Pc on the aux line Q
c       sPc stores the s-coordinate of Pc*

        call xyz_to_s(Pc(1), Pc(2), Pc(3), iSegPc, nseg, P, Hyp_z, sPc)

c       Calculate local strike-parallel direciton,  w.r.t. the aux line Q

        call LocalStrikeParallel(StaName, nSeg, sPc, sHyp, sQ2,
     1    Strike, lStrike, EffL, iStrike1, iStrike2)

c       Caclculate station's GC2
        call Generalized_Coord(Sta_x, Sta_y, nseg, P, Len, Strike,
     1          GC_Type, Sta_SN, Sta_SP, Sta_U, Sta_T, Sta_W)

c       Calculate point-source radiation pattern for
c         hypocenter-station pair
        call RfnRfp(Sta_U, Sta_T, 0.0, Hyp_U, Hyp_T, Hyp_z,
     1          abs(Dip(iSegHyp)), Rake(iSegHyp), Rfn, Rfp)


c      Subroutine PathToDirectPoint
c         1. finds the direct point on fault;
c         2. constructs the path from  Hypo to direct point;
c         3. computes the associated DPP parameters
c      Per Chiou and Spudich (2013),
c         Hypo is moved inward 10% of rupture  length if it is located
c         within 10% of any of the fault edges

        RepiI= (Sta_x-HypI_x)**2 + (Sta_y-HypI_y)**2
        RhypI= sqrt(RepiI + HypI_z**2)
        RepiI= sqrt(RepiI)

        call PathToDirectPoint(FaultID, StaName,
     1        H_Top, HypI_x, HypI_y, HypI_z, Sta_x, Sta_y, RhypI,
     1        nseg, P, Len, Width, Dip, Strike, Rake,
     1        iSegHyp, Sta_SN, .TRUE.,
     1        Pd, iSegPd, iEdgPd, Rpd,
     1        IDP_Pd, ch_prime, E, C_cap_Pd, E_cap_Pd,
     1        EwC,    EwC2,    XwC2,    EwCinvr,    EwC2invr,
     1        EwCPhi, EwC2Phi, XwC2Phi, EwCPhiinvr, EwC2Phiinvr,
     1        F1, F2, F3, F4, F5, F6, F7, DPP,
     1        phi, theta, aveR)

c
c       Subroutine PathToClosestPoint
c          1. constructs a path from hypo to the closest point on fault
c          2. computes associated IDP parameters (Spudich and Chiou 2008)
c
c   Isochone parameters D & s are computed w.r.t. the auxiliary
c   line Q at z = Hyp_z (July-2010)
c
        call PathToClosestPoint(FaultID, StaName,
     1          Hyp_x, Hyp_y, Hyp_z, Pc,
     1          sQ2, sHyp, sPc, iSegHyp, iSegPc, Dip(iSegHyp),
     1          H_Top, Rrup, Rhyp, nseg, P, Rfn, Rfp, Hyp_z,
     1          IDP, ct_prime, s, D, C_cap, S_cap, Rri)

        tmp1 = Rake(iSegHyp)/dtr
        F_RV = 0.0
        F_NM = 0.0
        if (tmp1.ge.30.0.and.tmp1.le.150.0)   F_RV = 1.0
        if (tmp1.gt.-120.0.and.tmp1.lt.-60.0) F_NM = 1.0

        F_HW = 0.0
        if (Sta_T.ge.0.0) F_HW = 1.0

c ... Outputs
        if (Sta_U .le. 0) then
           Ry0 = abs(Sta_U)
        elseif (Sta_U .le. TotalTopL) then
           Ry0 = 0
        else
           Ry0 = Sta_U - TotalTopL
        endif

        write (2, 14) faultID, adjustr(StaID), adjustr(StaName),
     1        Sta_x, Sta_y, Sta_U, Sta_T,
     1        Repi, Rhyp, Rrup, Rjb, Rrms,
     1        abs(Sta_T), Sta_U - TotalTopL/2, Ry0,
     1        AveStrike/dtr, lStrike/dtr,
     1        ch_prime, E, aveR, DPP,
     1        Pc_Lat, Pc_Lon, Pc(3)

        if (inOneFile)
     1   write (13,14) faultID, adjustr(StaID), adjustr(StaName),
     1        Sta_x, Sta_y, Sta_U, Sta_T,
     1        Repi, Rhyp, Rrup, Rjb, Rrms,
     1        abs(Sta_T), Sta_U - TotalTopL/2, Ry0,
     1        AveStrike/dtr, lStrike/dtr,
     1        ch_prime, E, aveR, DPP,
     1        Pc_Lat, Pc_Lon, Pc(3)

14       format(a5, a10, a55, 21f15.7)

        go to 2000

9999    continue
        stop
        end


c-----------------------------------------------------------------------
        subroutine Fault_Inputs(faultID, faultName, inLatLong,
     1        Org_Lat, Org_Long, Hyp_x, Hyp_y, Hyp_z, nseg, P, Dip,
     1        H_Trace, H_Top, H_Bot, Rake)
c-----------------------------------------------------------------------

        implicit none
        integer mxsg
        parameter(mxsg=40)

        character faultID*5
        logical inLatLong

        character faultName*65
        integer nseg
        real Org_Lat, Org_Long, Hyp_x, Hyp_y, Hyp_z
        real P(3,4,mxsg)
        real H_Top, H_Bot, H_Trace, Dip(mxsg), Rake(mxsg)

        integer i
        real dtr
        dtr = atan(1.0)/45.0

c .. Vertices must be ordered according to the strike direciton
c      About strike direction:
c        - When Dip is 90 degrees, strike directions is ambiguous.
c        - User must pick the direction and make sure the ordering
c          of vertices is consistent with the picked direction.
c
        if (inLatLong)
     1     open (1, file=trim(faultID)//'.tll', status='old')
        if (.NOT.inLatLong)
     1     open (1, file=trim(faultID)//'.txy', status='old')

        read (1, '(5x,a65)') faultName
        read (1, *) Org_Lat, Org_Long
        read (1, *) Hyp_x, Hyp_y, Hyp_z
        read (1, *) nseg
        read (1,*) (P(1,1,i), P(2,1,i), i=1,nseg),
     1              P(1,2,nseg), P(2,2,nseg)
c       H_Trace is not used in this program;
c       it is inlcuded as an input for backward compatiblity with ff7.for.
        read (1, *) (Dip(i), i=1,nseg), H_Trace, H_Top, H_Bot
        H_trace = H_Top
        read (1, *) (Rake(i),i=1,nseg)
        close(1)

        do i=1,nseg
c               if(Dip(i).eq.90.0) Dip(i)=89.9
                Dip(i) = Dip(i) * dtr
                Rake(i) = Rake(i) * dtr
        enddo

        write (3,'(a5,5x,a65)') faultID, faultName

c       Convert Lat-Long to X-Y coordinate
        if (inLatLong) then
           if(Hyp_x.ne.-9999)
     1         call LatLong_to_XY(Org_Lat, Org_Long, Hyp_x, Hyp_y)
           do i=1,nseg
               call LatLong_to_XY(Org_Lat, Org_Long, P(1,1,i), P(2,1,i))
           enddo
           call LatLong_to_XY(Org_Lat, Org_Long,
     1                        P(1,2,nseg), P(2,2,nseg))
        endif

        return
        end

c-----------------------------------------------------------------------
        subroutine Fault_Parallelograms(nseg, P, H_Trace, H_Top, H_Bot,
     1        AveDip, Len, Width, Strike, AveStrike)
c-----------------------------------------------------------------------

        implicit none
        integer mxsg
        parameter (mxsg=40)

c .. Input arguments
        integer nseg
        real H_Trace, H_Top, H_Bot, AveDip

c .. Returned arguments
        real P(3,4,mxsg), Strike(mxsg), Len(mxsg), Width(mxsg),
     1       AveStrike

c .. Working variables
        real vx, vy, vz, ww, ss
        real dtr, rtd, pi
        real tmp
        integer i, j, k


        pi = atan(1.0)*4.0
        dtr = pi/180.0
        rtd = 1.0/dtr

c .. This is only for programming convenience
        P(1,1,nseg+1) = P(1,2,nseg)
        P(2,1,nseg+1) = P(2,2,nseg)

        do i=1,nseg
          call distaz(P(1,1,i), P(2,1,i), H_Trace,
     1          P(1,1,i+1), P(2,1,i+1),
     1          H_Trace, Len(i), Strike(i))
          Width(i) = (H_Bot - H_Top) / sin(abs(AveDip))
        enddo

c
c ..  Rupture trace is projected down-dip perpendicular to the average
c     strike direction. The top and bottom of projected parallelograms
c     are at depths of H_Top and Z_Bot, respectively
c
c ..  P(i,j,k): i - coordinate component
c ..            j - apex
c ..            k - segment

        if (AveStrike .eq. -999)
     1     call distaz(P(1,1,1),      P(2,1,1),      0.0,
     1                 P(1,1,nseg+1), P(2,1,nseg+1), 0.0,
     1                 tmp, AveStrike)

        do i=1,nseg
            ww = (H_Top - H_Trace) / sin(abs(AveDip))
            ss = AveStrike + pi/2.0
            vx = cos(AveDip)*sin(ss)
            vy = cos(AveDip)*cos(ss)
            vz = -sin(abs(AveDip))
            P(1,1,i) = P(1,1,i)  + ww * vx
            P(2,1,i) = P(2,1,i)  + ww * vy
            P(3,1,i) = -H_Trace  + ww * vz
            P(1,2,i) = P(1,1,i+1)+ ww * vx
            P(2,2,i) = P(2,1,i+1)+ ww * vy
            P(3,2,i) = -H_Trace  + ww * vz
            ww = (H_Bot - H_Top  ) / sin(abs(AveDip))
            P(1,4,i) = P(1,1,i)  + ww * vx
            P(2,4,i) = P(2,1,i)  + ww * vy
            P(3,4,i) = -H_Trace  + ww * vz
            P(1,3,i) = P(1,1,i+1)+ ww * vx
            P(2,3,i) = P(2,1,i+1)+ ww * vy
            P(3,3,i) = -H_Trace  + ww * vz
        enddo

        P(1,1,nseg+1) = 0
        P(2,1,nseg+1) = 0

        return
        end

c-----------------------------------------------------------------------
        subroutine DipAngle(nseg, P, Strike, Dip)
c-----------------------------------------------------------------------
c This subroutine computes dip angle of a quadrilateral

        implicit none
        integer mxsg
        parameter(mxsg=40)

        integer nseg
        real P(3,4,mxsg), Strike(mxsg), Dip(mxsg)
        real pi, tmp
        integer i

        pi  = atan(1.0)*4

        do i=1,nseg
          tmp  = (P(1,4,i) - P(1,1,i)) * sin(Strike(i)+pi/2.0)  +
     1           (P(2,4,i) - P(2,1,i)) * cos(Strike(i)+pi/2.0)
          Dip(i) = atan((P(3,1,i)-P(3,4,i))/tmp)
        enddo

        return
        end

c-----------------------------------------------------------------------
        subroutine Fault_Rectangles(nseg, P, H_Trace, H_Top, H_Bot,
     1        Dip, Len, Width, Strike)
c-----------------------------------------------------------------------

        implicit none
        integer mxsg
        parameter (mxsg=40)

c  Input arguments
        integer nseg
        real H_Trace, H_Top, H_Bot, Dip(mxsg)

c  Returned arguments
        real P(3,4,mxsg), Strike(mxsg), Len(mxsg), Width(mxsg)

c .. Working variables
        real ww, ss, vx, vy, vz
        real dtr, rtd, pi
        integer i, j, k

        pi = atan(1.0)*4.0
        dtr = pi/180.0
        rtd = 1.0/dtr

        P(1,1,nseg+1) = P(1,2,nseg)
        P(2,1,nseg+1) = P(2,2,nseg)

        do i=1,nseg
          call distaz(P(1,1,i), P(2,1,i), H_Trace,
     1                P(1,1,i+1), P(2,1,i+1),
     1                H_Trace, Len(i), Strike(i))
c          if(Dip(i).le.0.) then
c            Strike(i) = Strike(i) + pi
c            if (Strike(i).gt.2*pi) Strike(i) = Strike(i) - 2*pi
c          ENDif
          Width(i) = (H_Bot - H_Top) / sin(abs(dip(i)))
        enddo

c
c ..  Rupture trace is projected down-dip perpendicular to the strike
c     direction. The top and bottom of projected rectangle
c     are at depths of H_Top and Z_Bot, respectively
c
c ..  P(i,j,k): i - coordinate component
c ..            j - apex
c ..            k - segment

        do i=1,nseg
            ss = strike(i) + pi/2.0
            vx = cos(dip(i))*sin(ss)
            vy = cos(dip(i))*cos(ss)
            vz = -sin(abs(dip(i)))
            ww = (H_Top - H_Trace) / sin(abs(dip(i)))
            P(1,1,i) = P(1,1,i)  + ww * vx
            P(2,1,i) = P(2,1,i)  + ww * vy
            P(3,1,i) = -H_Trace  + ww * vz
            P(1,2,i) = P(1,1,i+1)+ ww * vx
            P(2,2,i) = P(2,1,i+1)+ ww * vy
            P(3,2,i) = -H_Trace  + ww * vz
c            ww = (H_Bot - H_Top) / sin(abs(dip(i)))
            P(1,4,i) = P(1,1,i)  + Width(i) * vx
            P(2,4,i) = P(2,1,i)  + Width(i) * vy
            P(3,4,i) = P(3,1,i)  + Width(i) * vz
            P(1,3,i) = P(1,2,i)  + Width(i) * vx
            P(2,3,i) = P(2,2,i)  + Width(i) * vy
            P(3,3,i) = P(3,2,i)  + Width(i) * vz
c            do j=1,4
c             write (*,*) (P(k,j,i), k=1,3)
c            enddo
        enddo

        P(1,1,nseg+1) = 0
        P(2,1,nseg+1) = 0

        return
        end

c-----------------------------------------------------------------------
        subroutine Fault_Trapezoids(nseg, P)
c-----------------------------------------------------------------------
c .. Turn fault rectangles into trapezoids by connecting bottoms.
c    This approach may create a unrealistic plow where a sharp change
c    in strike direction occurs.
c
        implicit none
        integer mxsg
        parameter (mxsg=40)
        integer nseg
        real P(3,4,mxsg)
        real denom, ss
        integer i

c .. Intersect point from the bottoms of rectangle i and i+1 becomes
c    P(.,3,i) and P(.,4,i+1) of rectangles i and i+1, respectively.
c    (Reference: page 250, Computation Geometry in C, by Joseph O'Rourke)

        do i=1,nseg-1
            denom = P(1,4,i)  *(P(2,3,i+1)-P(2,4,i+1)) +
     1              P(1,3,i)  *(P(2,4,i+1)-P(2,3,i+1)) +
     1              P(1,3,i+1)*(P(2,3,i)  -P(2,4,i)  ) +
     1              P(1,4,i+1)*(P(2,4,i)  -P(2,3,i)  )
            if (denom .eq. 0) then
                write (*,*) ' Error: Neighboring segments are parallel!'
                ss = 1
            else
                ss = (P(1,4,i)  *(P(2,3,i+1)-P(2,4,i+1)) +
     1                P(1,4,i+1)*(P(2,4,i)  -P(2,3,i+1)) +
     1                P(1,3,i+1)*(P(2,4,i+1)-P(2,4,i  ))) / denom
            endif
            P(1,3,i) = P(1,4,i) + ss * (P(1,3,i)-P(1,4,i))
            P(2,3,i) = P(2,4,i) + ss * (P(2,3,i)-P(2,4,i))
            P(3,3,i) = P(3,4,i) + ss * (P(3,3,i)-P(3,4,i))
            P(1,4,i+1) = P(1,3,i)
            P(2,4,i+1) = P(2,3,i)
            P(3,4,i+1) = P(3,3,i)
        enddo

        if (nseg.gt.1) then
            P(1,4,nseg) = P(1,3,nseg-1)
            P(2,4,nseg) = P(2,3,nseg-1)
            P(3,4,nseg) = P(3,3,nseg-1)
        endif

        return
        end

c-----------------------------------------------------------------------
        subroutine Surface_Projection_Of_Fault(nseg, P, SPF)
c-----------------------------------------------------------------------
c .. Create polygon (SPF) of the surface pojected fault

        parameter(mxsg=40)
        integer nseg
        real P(3,4,mxsg), SPF(2,(mxsg+1)*2+1)

        do i=1,nseg
                SPF(1,i) = P(1,1,i)
                SPF(2,i) = P(2,1,i)
        enddo
        SPF(1,nseg+1) = P(1,2,nseg)
        SPF(2,nseg+1) = P(2,2,nseg)
        do i=1,nseg
                SPF(1,nseg+1+i) = P(1,3,nseg-i+1)
                SPF(2,nseg+1+i) = P(2,3,nseg-i+1)
        enddo
        SPF(1,nseg*2+2) = P(1,4,1)
        SPF(2,nseg*2+2) = P(2,4,1)

        SPF(1,nseg*2+3) = P(1,1,1)
        SPF(2,nseg*2+3) = P(2,1,1)

        return
        end

c-----------------------------------------------------------------------
        subroutine GetUnitVectors(nseg, Strike, Dip, Rake)
c-----------------------------------------------------------------------
c This subroutine computes and stores unit vectors in a common block

        implicit none
        integer mxsg
        parameter(mxsg=40)

        integer nseg
        real Strike(mxsg), Dip(mxsg), Rake(mxsg)

        real ux(mxsg), uy(mxsg), uz(mxsg), vx(mxsg), vy(mxsg), vz(mxsg),
     1       wx(mxsg), wy(mxsg), wz(mxsg), rx(mxsg), ry(mxsg), rz(mxsg),
     1       nx(mxsg), ny(mxsg)
        common /unitVectors/ux,uy,uz,vx,vy,vz,wx,wy,wz,rx,ry,rz,nx,ny

        integer i
        real pi, ss, cr, sr


        pi = atan(1.0)*4.0

        do i=1,nseg

c N2M .... along-strike direction; strike-parallel direction (ux, uy)
            ux(i) = sin(Strike(i))
            uy(i) = cos(Strike(i))
            uz(i) = 0.0

c N2M .... strike-normal vector (nx, ny)
            ss = Strike(i) + pi/2.0
            nx(i) = sin(ss)
            ny(i) = cos(ss)

c N2M .... down-dip vector (vx, vy, vz)
            vx(i) = cos(Dip(i))*nx(i)
            vy(i) = cos(Dip(i))*ny(i)
            vz(i) = -sin(abs(Dip(i)))

c N2M .... fault perpendicular vector (wx, wy, wz)
            wx(i) =   (uy(i)*vz(i))-(uz(i)*vy(i))
            wy(i) = -((ux(i)*vz(i))-(uz(i)*vx(i)))
            wz(i) =   (ux(i)*vy(i))-(uy(i)*vx(i))

c N2M .... rake vector (rx, ry, rz)
            cr = cos(Rake(i))
            sr = sin(Rake(i))
            rx(i) = cr*ux(i)+sr*vx(i)
            ry(i) = cr*uy(i)+sr*vy(i)
            rz(i) = cr*uz(i)+sr*vz(i)

        enddo

        return
        end

c-----------------------------------------------------------------------
        subroutine Hypocenter(faultID, nseg, P, Width, Dip, H_top, H_bot
     1                       , Hyp_x, Hyp_y, Hyp_z, Org_Lat, Org_Long,
     1                       inLatLong, Q, sQ2, sHyp, s2L, d2W, iSegHyp)
c-----------------------------------------------------------------------

c  .. Determine hypocenter location (if Hyp_x=-9999) and
c      check if it is on the fault plane.
c

        implicit none
        integer mxsg
        parameter (mxsg=40)

        character faultID*5
        integer nseg
        logical inLatLong
        real P(3,4,mxsg), Width(mxsg), Dip(mxsg), H_top, H_bot,
     1       Hyp_x, Hyp_y, Hyp_z, Org_Lat, Org_Long
        real Q(3,2,mxsg), sQ2(mxsg+1)

        integer iSegHyp, iEdgHyp
        real sHyp

        real s2L, d2W, dmin, Pmin(3)
        integer i, j, k
        real Hyp_Lat, Hyp_Lon

        IF (Hyp_x.eq.-9999) THEN
          dmin = 0.0
          s2L = Hyp_y
          d2W = abs(Hyp_z)

c .. use the first segment to determine hypocenter depth
          Hyp_z = -(d2W * Width(1) * sin(Dip(1)) + H_top)

          call AuxLine(nseg, P, Hyp_z,  Q, sQ2)
c .. follwoing statement must follow immediately the above statement
          sHyp = s2L*sQ2(nseg)

          call s_to_xyz(sHyp, nseg, sQ2, Q,
     1                  Hyp_x, Hyp_y, Hyp_z, iSegHyp)
          Hyp_Lat  = Hyp_x
          Hyp_Lon  = Hyp_y
          call XY_to_LatLong(Org_Lat, Org_Long, Hyp_Lat, Hyp_Lon)

        ELSE

c .. Check if the given Hypo is on the fault
c    If it is not on fault, then replacle it, with its closest point
c    on fault

          call Closest_Point_on_Fault(Hyp_x, Hyp_y, Hyp_z, nseg, P,
     1          iSegHyp, dmin, Pmin, iSegHyp, iEdgHyp)

c         if (dmin.gt.1E-5) then
          if (dmin.gt.1E-1) then     !! FF5 and FF7 used a threshold of 0.1
            write (*,*) 'Input hypo if off fault: '
            write (*,'(3f12.6)') Hyp_x, Hyp_y, Hyp_z
            write (*,'(1x, a16,f12.6)') 'Dist to fault = ', dmin
            write (*,'(1x, a18,3f12.6,i5)') 'Hypo is moved to: ',
     1             Pmin, iSegHyp
            Hyp_Lat  = Pmin(1)
            Hyp_Lon  = Pmin(2)
            call XY_to_LatLong(Org_Lat, Org_Long, Hyp_Lat, Hyp_Lon)
            write (30,'(a5,9f12.6)') faultID, dmin, Hyp_x, Hyp_y, Hyp_z,
     1              Pmin(1), Pmin(2), Pmin(3), Hyp_Lat, Hyp_Lon
            Hyp_x = Pmin(1)
            Hyp_y = Pmin(2)
            Hyp_z = Pmin(3)
          else
            write (30,'(a5,9f12.6)') faultID, dmin, Hyp_x, Hyp_y, Hyp_z
          endif

          call AuxLine(nseg, P, Hyp_z, Q, sQ2)
          call xyz_to_s(Hyp_x,Hyp_y,Hyp_z,iSegHyp, nseg,P,Hyp_z, sHyp)

c .. Move hypo inward by 0.01 km if it is on a side edge
c         IF (sHyp.le.0.01) then
c          sHyp = 0.01
c          call s_to_xyz(sHyp, nseg, sQ2, Q, Hyp_x,Hyp_y,Hyp_z, iSegHyp)
c         ENDIF
c
c        IF (sQ2(nseg)-sHyp.le.0.01) then
c         sHyp = sQ2(nseg)-0.01
c         call s_to_xyz(sHyp, nseg, sQ2, Q, Hyp_x, Hyp_y,Hyp_z, iSegHyp)
c        ENDIF

          s2L = sHyp/sQ2(nseg)
          d2W = (-Hyp_z - H_top)/(H_bot - H_top)

        ENDIF

        if (inLatLong) then
          Hyp_Lat = Hyp_x
          Hyp_Lon = Hyp_y
          call XY_to_LatLong(Org_Lat, Org_Long, Hyp_Lat, Hyp_Lon)
          write (3,'(a10,3f10.4)') 'HypLL =', Hyp_Lat, Hyp_Lon, Hyp_z
        else
          write (3,'(a10,3f10.4)') 'HypXY =', Hyp_x, Hyp_y, Hyp_z
        endif

        write (3,'(a7,3x,3f10.4)') 'Dmin = ', dmin
        write (3,'(a10,3f10.4)'  ) '  s =  ', sHyp
        write (3,'(a10,3f10.4)')   '  d =  ', d2W * Width(iSegHyp)
        write (3,'(a10,1I10  )')   'iSeg = ', iSegHyp
        write (3,'(a10,1f10.4)')   'Hyp.s2L = ', s2L
        write (3,'(a10,1f10.4)')   'Hyp.d2W = ', d2W
        write (3,*)

        return
        end


c-----------------------------------------------------------------------
        subroutine HypocenterDPP(Hyp_x, Hyp_y, Hyp_z, sHyp, s2L, d2W,
     1                           iSegHyp, H_top, H_bot, Width, nseg, P,
     1                           inLatLong, Org_Lat, Org_Long,
     1                           HypI_x, HypI_y, HypI_z)
c-----------------------------------------------------------------------
        implicit none
        integer mxsg
        parameter (mxsg=40)

        integer nseg, iSegHyp
        real Hyp_x, Hyp_y, Hyp_z, s2L, d2W, H_bot, H_top, P(3,4,mxsg)
        real sHyp, Org_Lat, Org_Long, Width(mxsg)
        logical inLatLong

        integer iSegHypI
        real HypI_x, HypI_y, HypI_z, sHypI, s2LI

        real Q(3,2,mxsg), sQ2(mxsg+1), tmp1, tmp2

        if (s2L.lt.0.1.or.s2L.gt.0.9.or.d2W.lt.0.1.or.d2W.gt.0.9) then
               write (*,*) '----- Hypo for DPP has been moved inward!'
          HypI_z = -(min(0.9, max(0.1, d2W)) * (H_bot - H_top) + H_top)
          call AuxLine(nseg, P, HypI_z, Q, sQ2)
          s2LI = min(0.9, max(0.1, s2L))
          sHypI = s2LI * sQ2(nseg)
          call s_to_xyz(sHypI, nseg, sQ2, Q, HypI_x, HypI_y, HypI_z,
     1                  iSegHypI)
          if (iSegHypI.ne.iSegHyp) then
                  write (*,*) 'Hypo for DPP is in a different segment'
                  write (*,'(3f10.3)')  Hyp_x, Hyp_y, Hyp_z
                  write (*,'(3f10.3)')  HypI_x, HypI_y, HypI_z
                  stop
          endif
        else
          HypI_x = Hyp_x
          HypI_y = Hyp_y
          HypI_z = Hyp_z
          sHypI  = sHyp
          iSegHypI = iSegHyp
          s2LI = s2L
        endif

        if (inLatLong) then
          tmp1 = HypI_x
          tmp2 = HypI_y
          call XY_to_LatLong(Org_Lat, Org_Long, tmp1, tmp2)
          write (3,'(a10,3f10.4)') 'HypI.LL =', tmp1, tmp2, HypI_z
        else
          write (3,'(a10,3f10.4)') 'HypI.XY =', HypI_x, HypI_y, HypI_z
        endif

        write (3,'(a10,3f10.4)') '  s =  ', sHypI
        write (3,'(a10,3f10.4)') '  d =  ', d2W * Width(iSegHyp)
        write (3,'(a10,1I10  )') 'iSeg = ', iSegHypI
        write (3,'(a10,1f10.4)') 'Hyp.s2L = ', s2LI
        write (3,'(a10,1f10.4)') 'Hyp.d2W = ',
     1                           (-HypI_z - H_top)/(H_bot - H_top)

c        call AuxLine(nseg, P, Hyp_z, Q, sQ2)
        return
        end

c-----------------------------------------------------------------------
        subroutine SnSp_Coord(x, y, nseg, P, Strk, t, u, Dist)
c-----------------------------------------------------------------------
c  t-u is the 2-D coordiante of strike-normal (t) & strike-parallel (u),
c        relative to the fault trace corners

        implicit none
        integer mxsg
        parameter(mxsg=40)

        integer nseg
        real x, y, P(3,4,mxsg), Strk(mxsg)

        real t(mxsg), u(mxsg), Dist(mxsg+1)

        real dx, dy
        real ux(mxsg), uy(mxsg), uz(mxsg), vx(mxsg), vy(mxsg), vz(mxsg),
     1       wx(mxsg), wy(mxsg), wz(mxsg), rx(mxsg), ry(mxsg), rz(mxsg),
     1       nx(mxsg), ny(mxsg)
        common /unitVectors/ux,uy,uz,vx,vy,vz,wx,wy,wz,rx,ry,rz,nx,ny

        integer i

c ..... (t,u,z) coordinate of point (x,y,0)
        do i = 1, nseg
            dx = (x-P(1,1,i))
            dy = (y-P(2,1,i))
            u(i) = dx*ux(i)+dy*uy(i)
            t(i) = dx*nx(i)+dy*ny(i)
            Dist(i) = sqrt(t(i)*t(i)+u(i)*u(i))
c               write (*,'(i5,5f10.4)') i, t(i), u(i), Dist(i)
c    1                Az(i)/atan(1.0)*45.0,
c    1                Strk(i)/atan(1.0)*45.0
        enddo
        dx = (x-P(1,2,nseg))
        dy = (y-P(2,2,nseg))
        Dist(nseg+1) = sqrt((dx*ux(nseg)+dy*uy(nseg))**2 +
     1                      (dx*nx(nseg)+dy*ny(nseg))**2)

        return
        end

c-----------------------------------------------------------------------
        subroutine XYZtoUVW(x,y,z,iS,P,u,v,w)

        implicit none
        integer mxsg
        parameter(mxsg=40)

        integer iS
        real x, y, z, u, v, w
        real P(3,4,mxsg)

        real dx, dy, dz
        real ux(mxsg), uy(mxsg), uz(mxsg), vx(mxsg), vy(mxsg), vz(mxsg),
     1       wx(mxsg), wy(mxsg), wz(mxsg), rx(mxsg), ry(mxsg), rz(mxsg),
     1       nx(mxsg), ny(mxsg)
        common /unitVectors/ux,uy,uz,vx,vy,vz,wx,wy,wz,rx,ry,rz,nx,ny

        dx = x - P(1,1,iS)
        dy = y - P(2,1,iS)
        dz = z - P(3,1,iS)
        u = dx * ux(iS) + dy * uy(iS) + dz * uz(iS)
        v = dx * vx(iS) + dy * vy(iS) + dz * vz(iS)
        w = dx * wx(iS) + dy * wy(iS) + dz * wz(iS)

        return
        end

c-----------------------------------------------------------------------
        subroutine UVWtoXYZ(u,v,w,iS,P,x,y,z)

        implicit none
        integer mxsg
        parameter(mxsg=40)

        integer iS
        real x, y, z, u, v, w
        real P(3,4,mxsg)

        real ux(mxsg), uy(mxsg), uz(mxsg), vx(mxsg), vy(mxsg), vz(mxsg),
     1       wx(mxsg), wy(mxsg), wz(mxsg), rx(mxsg), ry(mxsg), rz(mxsg),
     1       nx(mxsg), ny(mxsg)
        common /unitVectors/ux,uy,uz,vx,vy,vz,wx,wy,wz,rx,ry,rz,nx,ny

        x = P(1,1,iS) + u * ux(iS) + v * vx(iS) + w * wx(iS)
        y = P(2,1,iS) + u * uy(iS) + v * vy(iS) + w * wy(iS)
        z = P(3,1,iS) + u * uz(iS) + v * vz(iS) + w * wz(iS)

        return
        end

c-----------------------------------------------------------------------
        subroutine Generalized_Coord(x, y, nseg, P, L, Strk,
     1         GC_Type, t, u, UU, TT, W)
c-----------------------------------------------------------------------
c  TT-UU-Z is a generalized coordiante system that
c        warps around the bending fault trace
c
c  Ref: Spudich and Chiou, USGS Open-File Report 2015-1028.

        implicit none
        integer mxsg
        parameter(mxsg=40)

        integer nseg, GC_Type
        real x, y, p(3,4,mxsg), L(mxsg), Strk(mxsg)

        real UU, TT

        real Dist(mxsg+1), t(mxsg), u(mxsg), ClstD(mxsg), ClstD2Top
        real tmp, W, wi
        integer i, iClstD, j

        call SnSp_Coord(x, y, nseg, P, Strk, t, u, Dist)

        if (GC_Type.eq.2) go to 1000

c ........... 2008 definition begins

c ..... ClstD
        do i=1,nseg
                if (u(i) .gt. L(i) .or. u(i) .lt. 0) then
                        ClstD(i) = min(Dist(i), Dist(i+1))
                else
                        ClstD(i) = abs(t(i))
                endif
        enddo
c ..... iClstD
        ClstD2Top = 1.0E+10
        do i=1,nseg
                if (ClstD(i).lt.ClstD2Top) then
                        iClstD = i
                        ClstD2Top = ClstD(i)
                endif
        enddo
c ..... UU
        if (nseg .eq. 1) then
                UU = u(1)
        elseif (nseg .eq. 2) then
                UU = min(u(1), L(1)) + max(u(2), 0.0)
        else
                UU = min(u(1), L(1)) + max(u(nseg), 0.0)
                do i=2,nseg-1
                        UU = UU + min(max(u(i), 0.0), L(i))
                enddo
        endif
c ..... TT
        if (u(1).ge.0.and.u(nseg).le.L(nseg)) then
                TT = sign(ClstD2Top, t(iClstD))
        else
                TT = t(iClstD)
        endif

c ........... 2008 definition ends
        return

1000    continue

c ........... 2012 definition begins

        do i=1,nseg
          if (abs(t(i)).le.1.0E-5.and.u(i).ge.0.0.and.u(i).le.L(i)) then
             TT = 0
             UU = u(i)
             do j=1,i-1
               UU = L(j) + UU
             enddo
             W = 1E+15
             return
          endif
        enddo

        TT = 0.0
        UU = 0.0
        W   = 0.0
        tmp = 0.0
        do i=1,nseg
           if (abs(t(i)).le.1.0E-6) then
             wi  = 1.0 / (u(i) - L(i)) - 1.0 / u(i)
           else
             wi  = (atan((L(i)-u(i))/t(i)) - atan(-u(i)/t(i))) / t(i)
           endif
c           write (*, "(4f10.4)") wi, t(i), u(i), L(i)
           W = W + wi
           TT  = TT + wi * t(i)
           UU  = UU + wi * (tmp + u(i))
           tmp = tmp + L(i)
        enddo
        TT = TT / W
        UU = UU / W

c ........... 2012 definition ends
        return

        end

ccc---------------------------------------------------------------------
        subroutine RfnRfp(Sta_U, Sta_T, Sta_z, Hyp_U, Hyp_T, Hyp_z,
     1                 Dip, Rake, Rfn,  Rfp)
c-----------------------------------------------------------------------
        implicit none
        real Sta_U, Sta_T, Sta_z, Hyp_U, Hyp_T, Hyp_z,
     1       Dip, Rake

        real u_prime, t_prime, z_prime, R, rh
        real s_del, c_del, s_lam, c_lam, s_eta, c_eta, s_psi, c_psi
        real nr, nb, nc, sr, sb, sc

        real Rfn, Rfp

         u_prime = Sta_U - Hyp_U
         t_prime = Sta_T - Hyp_T
         R       = SQRT(u_prime**2+t_prime**2)
         rh      = SQRT(R**2+(Hyp_z)**2)

         s_lam   = sin(Rake)
         c_lam   = cos(Rake)
         s_del   = sin(abs(Dip))  !! Dip is always +
         c_del   = cos(abs(Dip))

         if (R.eq.0) then
                Rfp = c_del * c_lam
                Rfn = -cos(2*abs(Dip)) * s_lam
                return
         endif

         s_eta   = R/rh
         c_eta   = (-Hyp_z)/rh
         s_psi   = t_prime/R
         c_psi   = u_prime/R

         nr = s_eta*s_psi*s_del + c_eta*c_del
         nb = c_eta*s_psi*s_del - s_eta*c_del
         nc = c_psi*s_del
         sr =-c_lam*s_eta*c_psi + s_lam*c_del*s_eta*s_psi -
     1        s_lam*s_del*c_eta
         sb =-c_lam*c_eta*c_psi + s_lam*c_del*c_eta*s_psi +
     1        s_lam*s_del*s_eta
         sc = c_lam*s_psi + s_lam*c_del*c_psi

         Rfp = -c_psi*(nr*sb+nb*sr) + s_psi*(nr*sc+nc*sr)
         Rfn = -s_psi*(nr*sb+nb*sr) - c_psi*(nr*sc+nc*sr)

        return
        end

ccc---------------------------------------------------------------------
        subroutine PathToClosestPoint(faultID, StaName,
     1          Hyp_x, Hyp_y, Hyp_z, Pc,
     1          sQ2, sHyp, sPc, iSegHyp, iSegPc, Dip, H_top, Rrup, Rhyp,
     1          nseg, P, Rfn, Rfp, AuxLine_z,
     1          IDP, ct_prime, s, D, C_cap, S_cap, Rri)
c-----------------------------------------------------------------------
c
c Compute isochrone directivity parameters defined in
c  Spduch and Chiou (2008).
c
c Parameters s and D are w.r.t. the auxiliary line placecd
c  at z = AuxLine_z
c
c Parameter sPc is the s of Pc*, which is the projected Pc
c  on the hypo auxiliary line
c

        implicit none
        integer mxsg
        parameter (mxsg=40)

c .. Input Arguments
        real Hyp_x, Hyp_y, Hyp_z, Hyp_Fy, Pc(3), Pc_Fy, Dip, H_top,
     1       Rrup, Rhyp, P(3,4,mxsg), Rfn, Rfp, AuxLine_z
        integer iSegHyp, iSegPc, nseg
        character faultID*5, StaName*55

c .. Returned Arguments
        real IDP

c .. Working Variables
        character type*3
        real sQ2(mxsg+1)
        real Xp(3), dmin, sPc, sHyp, s, D, ct_prime, C_cap, S_cap, Rri
        real dx, dy, dz, dd, ds, O(3,mxsg), sB, sE, zB, zE, tmp1
        integer nD, iB, iE, i, j, k

        s = abs(sPc - sHyp)

c .. Isochrone parameter D; length of D path from hypo to Pc

        iB = iSegHyp
        iE = iSegPc + 1
        sB    = sHyp
        sE    = sPc
        zB    = Hyp_z
        zE    = Pc(3)
        Type  = 'Hyp'
        if (iSegHyp.gt.iSegPc) then
          iB = iSegPc
          iE = iSegHyp + 1
          sB    = sPc
          sE    = sHyp
          zB    = Pc(3)
          zE    = Hyp_z
          Type  = 'Pc'
        endif
        nD = iE - iB

        O(1,iB)  = Hyp_x
        O(2,iB)  = Hyp_y
        O(3,iB)  = Hyp_z
        O(1,iE)  = Pc(1)
        O(2,iE)  = Pc(2)
        O(3,iE)  = Pc(3)
        if (iSegHyp.gt.iSegPc) then
          O(1,iB)  = Pc(1)
          O(2,iB)  = Pc(2)
          O(3,iB)  = Pc(3)
          O(1,iE)  = Hyp_x
          O(2,iE)  = Hyp_y
          O(3,iE)  = Hyp_z
        endif
        do j=iB+1, iE-1
          dx = (P(1,4,j)-P(1,1,j))
          dy = (P(2,4,j)-P(2,1,j))
          dz = (P(3,4,j)-P(3,1,j))
          tmp1 = zB + (sQ2(j-1) - sB)/(sE - sB) * (zE - zB)
          dd = (tmp1 - P(3,1,j))/(P(3,4,j)-P(3,1,j))
          O(1,j) = P(1,1,j) + dx * dd
          O(2,j) = P(2,1,j) + dy * dd
          O(3,j) = P(3,1,j) + dz * dd
        enddo

        D = 0.0
        do k=iB, iE-1
           D = D + sqrt((O(1,k)-O(1,k+1))**2+(O(2,k)-O(2,k+1))**2+
     1                  (O(3,k)-O(3,k+1))**2)
        enddo

c .. Isochrone parameter ct.prime

        if (D.eq.0) then
            ct_prime = 0.8
        else
            ct_prime = min(4.0, max(1.0/(1/0.8 - (RHyp-Rrup)/D),0.8))
c            ct_prime = 1.0/(1/0.8 - (RHyp-Rrup)/D)
        endif

c       write (3,'(a12,3f10.4)') ' s, D, ct  =', s, d, ct_prime
c       write (*,'(a12,3f10.4)') ' s, D, ct  =', s, d, ct_prime

c .. IDP

        C_cap = (min(ct_prime, 2.45)-0.8)/(2.45-0.8)
        S_cap = log(min(75.0, max(s, abs(abs(Hyp_z)-H_top)/sin(Dip))))
        Rri = max(sqrt(Rfn**2+Rfp**2),0.2)

c       write (3,'(a12,3f10.4)') ' C, S, Rri =', C_cap, S_cap, Rri
c       write (*,'(a12,3f10.4)') ' C, S, Rri =', C_cap, S_cap, Rri

        IDP = C_cap * S_cap * Rri

        return
        end

ccc---------------------------------------------------------------------
        subroutine AuxLine(nseg, p, Hyp_z, Q, sQ2)
c-----------------------------------------------------------------------
        implicit none
        integer mxsg
        parameter (mxsg=40)

        integer nseg
        real P(3,4,mxsg), Hyp_z
        real Q(3,2,mxsg), sQ2(mxsg+1)

        real frct, sum
        integer i, j, k

        do i=1,nseg
           frct = (Hyp_z-P(3,1,i))/(P(3,4,i)-P(3,1,i))
           Q(1,1,i) = (P(1,4,i)-P(1,1,i)) * frct + P(1,1,i)
           Q(2,1,i) = (P(2,4,i)-P(2,1,i)) * frct + P(2,1,i)
           Q(3,1,i) = (P(3,4,i)-P(3,1,i)) * frct + P(3,1,i)

           frct = (Hyp_z-P(3,2,i))/(P(3,3,i)-P(3,2,i))
           Q(1,2,i) = (P(1,3,i)-P(1,2,i)) * frct + P(1,2,i)
           Q(2,2,i) = (P(2,3,i)-P(2,2,i)) * frct + P(2,2,i)
           Q(3,2,i) = (P(3,3,i)-P(3,2,i)) * frct + P(3,2,i)
        enddo

        sum = 0.0
        do i=1,nseg
          sum = sum + sqrt((Q(1,1,i)-Q(1,2,i))**2+
     1           (Q(2,1,i)-Q(2,2,i))**2+(Q(3,1,i)-Q(3,2,i))**2)
          sQ2(i)  = sum
        enddo

        return
        end

ccc---------------------------------------------------------------------
        subroutine s_to_xyz(s, nseg, sQ2, Q, x, y, z, iSeg)
c-----------------------------------------------------------------------
        parameter(mxsg=40)
        integer nseg, iSeg
        real s, sQ2(mxsg+1), Q(3,2,mxsg), x, y, z
        real frct

        if (s.lt.0.0.or.s.gt.sQ2(nseg)) then
          write (*,*) ' s is outside fault area '
          write (*,*) s, sQ2(nseg)
          stop
        endif

        do i=1,nseg
            if (s.le.sQ2(i)) go to 10
        enddo
10      continue
        iSeg = i

        if (iSeg.eq.1) then
          frct = s/sQ2(1)
        else
          frct = (s-sQ2(iSeg-1))/(sQ2(iSeg)-sQ2(iSeg-1))
        endif
        x = (Q(1,2,iSeg) - Q(1,1,iSeg)) * frct + Q(1,1,iSeg)
        y = (Q(2,2,iSeg) - Q(2,1,iSeg)) * frct + Q(2,1,iSeg)
        z = (Q(3,2,iSeg) - Q(3,1,iSeg)) * frct + Q(3,1,iSeg)
        return
        end

ccc---------------------------------------------------------------------
        subroutine xyz_to_s(x, y, z, iSeg, nseg, P, aux_z, s)
c-----------------------------------------------------------------------
c  This subroutine computes the s-coordinate of point (x,y,z).
c   S-coordinate is measured along the aux line at depth aux_z.
c  Point (x,y,z) is on the iSeg-th segment of the fault rupture,
c   it is not necessarily on the aux line.
c
c .. July-03-2012

        implicit none
        integer mxsg
        parameter(mxsg=40)

        real x, y, z, aux_z, P(3,4,mxsg)
        integer nseg, iSeg
        real s

        real sQ2(mxsg+1), Q(3,2,mxsg), dist, frac

        if (iSeg.le.0.or.iSeg.gt.nseg) then
           write (*,*) 'Invalid Segment number!'
           stop
        endif

c .. auxiliar line at depth z
        call AuxLine(nseg, P, z, Q, sQ2)
        dist =
     1   sqrt((x-Q(1,1,iSeg))**2+(y-Q(2,1,iSeg))**2+(z-Q(3,1,iSeg))**2)

        if (iSeg .eq. 1) then
          frac = dist / sQ2(1)
        else
          frac = dist / (sQ2(iSeg)-sQ2(iSeg-1))
        endif

c .. auxiliar line at depth aux_z
        if (z .ne. aux_z) call AuxLine(nseg, P, aux_z, Q, sQ2)

        if (iSeg .eq. 1) then
          s = frac * sQ2(1)
        else
          s = sQ2(iSeg-1) + frac * (sQ2(iSeg)-sQ2(iSeg-1))
        endif

        return
        end

ccc---------------------------------------------------------------------
        subroutine Joyner_Boore_Distance(x, y, nseg, SPF, Rjb)
c-----------------------------------------------------------------------
        parameter(mxsg=40)
        real x, y, SPF(2,(mxsg+1)*2+1), Rjb
        integer nseg, n, ncross, nc
        real xp((mxsg+1)*2+1),yp((mxsg+1)*2+1), dmin((mxsg+1)*2)

        np = (nseg+1)*2+1       !! Check (nseg+1)*2 segments
        nc = ncross(np, SPF, x, y)
        if (mod(nc,2).eq.1) then
            Rjb=0.0
        else
            do i=1,np
              xp(i) = SPF(1,i)-x
              yp(i) = SPF(2,i)-y
            enddo
            do i=1,np-1
              dot = -xp(i)*(xp(i+1)-xp(i))-yp(i)*(yp(i+1)-yp(i))
              cur =  xp(i)*(yp(i+1)-yp(i))-yp(i)*(xp(i+1)-xp(i))
              b   = sqrt((xp(i+1)-xp(i))**2+(yp(i+1)-yp(i))**2)
              a   = sqrt(xp(i)**2+yp(i)**2)
              c   = sqrt(xp(i+1)**2+yp(i+1)**2)
c .. Do not use 'ge' here because fault is vertical
              if (dot.gt.0.0.and.dot.le.b*b) then
                dmin(i) = abs(cur)/b
              else
                dmin(i) = min(a,c)
              endif
            enddo
            Rjb = 1.0E+10
            do i=1,np-1
             if (dmin(i).lt.Rjb) Rjb=dmin(i)
            enddo
        endif
        return
        end

ccc---------------------------------------------------------------------
        integer function ncross(np, p, x, y)
c-----------------------------------------------------------------------
        integer np
        real p(2,np), x, y

        integer cross
        real xp(np), yp(np)

        do i=1,np
                xp(i) = p(1,i)-x
                yp(i) = p(2,i)-y
        enddo

        ncross = 0
        do i=1,np-1
           if((yp(i).gt.0.and.yp(i+1).le.0).OR.
     1        (yp(i).le.0.and.yp(i+1).gt.0)) then
             if((xp(i)*yp(i+1)-xp(i+1)*yp(i))/(yp(i+1)-yp(i)).gt.0) then
                       cross = 1
             else
                       cross = 0
             endif
           else
             cross = 0
           endif
           ncross = ncross + cross
        enddo

        return
        end


ccc---------------------------------------------------------------------
        subroutine PathToDirectPoint(faultID, StaName,
     1          Ztor, Hyp_x, Hyp_y, Hyp_z, Sta_x, Sta_y, Rhyp,
     1          nseg, P, L, W, Dip, Strike, Rake,
     1          iSegHyp, Sta_SN, Vrt,
     1          Pd, iSegPd, iEdgPd, Rpd,
     1          IDP, ch_prime, E, C_cap, E_cap,
     1          EwC,    EwC2,    XwC2,    EwCinvr,    EwC2invr,
     1          EwCPhi, EwC2Phi, XwC2Phi, EwCPhiinvr, EwC2Phiinvr,
     1          F1, F2, F3, F4, F5, F6, F7, DPP,
     1          PhiLast, ThetaLast, aveR)
ccc---------------------------------------------------------------------

        implicit none
        integer mxsg
        parameter(mxsg=40)
c
        character faultID*5, StaName*55
        integer nseg, iSegHyp
        real Ztor, Hyp_x, Hyp_y, Hyp_z, Sta_x, Sta_y, Rhyp,
     1       P(3,4,mxsg), L(mxsg), W(mxsg), Dip(mxsg), Strike(mxsg),
     1       Rake(mxsg), Sta_SN(mxsg)
        logical Vrt
c
        integer iSegPd, iEdgPd, iWN
        real Pd(3), Rpd, IDP, ch_prime, E, C_cap, E_cap,
     1       EwC, EwC2, XwC2, EwCinvr, EwC2Invr,
     1       EwCPhi, EwC2Phi, XwC2Phi, EwCPhiinvr, EwC2Phiinvr,
     1       F1, F2, F3, F4, F5, F6, F7, DPP,
     1       PhiLast, ThetaLast
        real IEwC, IEwCPhi, IEwC2, IEwC2Phi, IXwC2, IXwC2Phi, Ich_prime
        real IEwCinvr, IEwCPhiinvr, IEwC2invr, IEwC2Phiinvr
c
        integer iSegE(mxsg)
        real EPath(3,mxsg), L1(mxsg), L2(mxsg)
        real Phi(mxsg), Theta(mxsg)
        real s1, s2, s3, term1, term2
        integer i, iEdg, iSeg, iS, nE
        real s_x, s_y, s_z, Pd_Fx, Pd_Fy, Pd_Fz, pio2
        real tmp_Fx, tmp_Fy, tmp_Fz, aveR, aveR1
        real p5X, p5Y, p5Z, L3, tmp, Zseis, Prop

        real s2L, d2W

        real ux(mxsg), uy(mxsg), uz(mxsg), vx(mxsg), vy(mxsg), vz(mxsg),
     1       wx(mxsg), wy(mxsg), wz(mxsg), rx(mxsg), ry(mxsg), rz(mxsg),
     1       nx(mxsg), ny(mxsg)
        common /unitVectors/ux,uy,uz,vx,vy,vz,wx,wy,wz,rx,ry,rz,nx,ny

        real Sta_Fx(mxsg), Sta_Fy(mxsg), Sta_Fz(mxsg),
     1       Hyp_Fx(mxsg), Hyp_Fy(mxsg), Hyp_Fz(mxsg),
     1       HypI_Fx(mxsg), HypI_Fy(mxsg), HypI_Fz(mxsg),
     1       P_Fx(4,mxsg), P_Fy(4,mxsg), P_Fz(4,mxsg)
        common /faultCoord/Hyp_Fx,  Hyp_Fy,  Hyp_Fz,
     1                     HypI_Fx, HypI_Fy, HypI_Fz,
     1                     P_Fx,    P_Fy,    P_Fz,
     1                     Sta_Fx,  Sta_Fy,  Sta_Fz

c .. Isochrone parameter E: length of E path, from hypo to Pd

c
c .. Start from the hypocenter segment (!! Hypocetner
c    shouldn't be on fault boundary -- iEdg = 0);
c    If direct point is on interior boundary (iEdgPd=2 or 4), then
c    move it to the next segment and use the cuurent direct point
c    as hypocenter of the next segment.
c    Otherwise, terminate the progression of E-Path.
c .. EPath L1, L2, Phi, & Theta use a different index system
c      differs from the fault segment index
c    I use variable n to remind myself this convention

        pio2 = 3.14159265359/2.0

        nE = 1
        iS = iSegHyp

        if (Vrt) then

          EPath(1,1) = Hyp_x
          EPath(2,1) = Hyp_y
          EPath(3,1) = Hyp_z
          tmp_Fx = HypI_Fx(iS)
          tmp_Fy = HypI_Fy(iS)
          iEdg = 0

11        CONTINUE

          iSegE(nE) = iS
          call Direct_PointVF(faultID, tmp_Fx, tmp_Fy, iEdg,
     1      P_Fx(1,iS), P_Fy(1,iS), Dip(iS), Strike(iS), Rake(iS),
     1      StaName, Sta_Fx(iS), Sta_Fy(iS), Sta_Fz(iS), iWN,
     1      Pd_Fx, Pd_Fy, iEdgPd, L1(nE), L2(nE), Phi(nE), Theta(nE))

           call UVWtoXYZ(Pd_Fx, Pd_Fy, 0.0, iS, P, Pd(1), Pd(2), Pd(3))
           call UVWtoXYZ(Sta_Fx(iS),Sta_Fy(iS),0.0, iS, P, s_x,s_y,s_z)

          if (iEdg.ne.iEdgPd) then
            if (iEdgPd.eq.4.and.iS.ne.1) then
c .. add Pd to EPath
              nE = nE + 1
              call UVWtoXYZ(Pd_Fx,  Pd_Fy,  0.0, iS, P, EPath(1,nE),
     1                      EPath(2,nE), EPath(3,nE))
c .. make Pd (now also EPath(,nE)) the hypocenter of next segment
              iEdg = 2
              iS = iS - 1
              call XYZtoUVW(EPath(1,nE), EPath(2,nE), EPath(3,nE),
     1                      iS, P, tmp_Fx, tmp_Fy, tmp_Fz)
              go to 11
            endif
            if (iEdgPd.eq.2.and.iS.ne.nseg) then
c .. add Pd to EPath
              nE = nE + 1
              call UVWtoXYZ(Pd_Fx,  Pd_Fy,  0.0, iS, P, EPath(1,nE),
     1                      EPath(2,nE), EPath(3,nE))
c .. make Pd (now also EPath(,nE)) the hypocenter of next segment
              iEdg = 4
              iS = iS + 1
              call XYZtoUVW(EPath(1,nE), EPath(2,nE), EPath(3,nE),
     1                      iS, P, tmp_Fx, tmp_Fy, tmp_Fz)
              go to 11
            endif
          endif

c .. if iEdg.eq.iEdgPd, iEdgPd=1, or iEdgPd=3
          nE = nE + 1
          call UVWtoXYZ(Pd_Fx,  Pd_Fy,  0.0, iS, P, EPath(1,nE),
     1                      EPath(2,nE), EPath(3,nE))
          iSegPd = iS

        else

           EPath(1,1) = Hyp_x
           EPath(2,1) = Hyp_y
           EPath(3,1) = Hyp_z
           iEdg = 0

10         CONTINUE

           call Direct_PointDF(faultID, EPath(1,nE), EPath(2,nE),
     1       EPath(3,nE) , Ztor, StaName, Sta_x, Sta_y, nseg, P,
     1       iSeg, Dip(iSeg), Strike(iSeg), Sta_SN(iSeg), iEdg,
     1       Pd, iEdgPd, L1(nE), L2(nE), Sta_FZ(nE), phi(nE), theta(nE))
          if (iEdg.ne.iEdgPd) then
            if (iEdgPd.eq.4.and.iSeg.ne.1) then
              nE = nE + 1
              EPath(1,nE) = Pd(1)
              EPath(2,nE) = Pd(2)
              EPath(3,nE) = Pd(3)
              iSeg = iSeg - 1
              iEdg = 2
              go to 10
            endif
            if (iEdgPd.eq.2.and.iSeg.ne.nseg) then
              nE = nE + 1
              EPath(1,nE) = Pd(1)
              EPath(2,nE) = Pd(2)
              EPath(3,nE) = Pd(3)
              iSeg = iSeg + 1
              iEdg = 4
              go to 10
            endif
          endif

        endif

c .. Initialize the cummulative variables
        E = 0
        EwC = 0
        EwCPhi = 0
        EwC2 = 0
        EwC2Phi = 0
        XwC2 = 0
        XwC2Phi = 0
        EwCinvr     = 0
        EwCPhiinvr  = 0
        EwC2invr    = 0
        EwC2Phiinvr = 0
        F1 = 0
        F2 = 0
        F3 = 0
        F4 = 0
        F5 = 0
        F6 = 0
        F7 = 0
        DPP = 0
        PhiLast = Phi(nE-1)
        ThetaLast = Theta(nE-1)

        do i=1, nE-1

c .. I did not use s3=L2(i)-L1(i) becuase precission error might lead to negative value
          s3   = sqrt((EPath(1,i+1)-EPath(1,i))**2 +
     1                (EPath(2,i+1)-EPath(2,i))**2 +
     1                (EPath(3,i+1)-EPath(3,i))**2)
          iS = iSegE(i)
          call Integrals(L1(i), L2(i), Sta_FZ(iS), s3, Phi(i),
     1       IEwC, IEwCPhi, IEwC2, IEwC2Phi, IXwC2, IXwC2Phi,
     1       IEwCinvr, IEwCPhiinvr, IEwC2invr, IEwC2Phiinvr, Ich_prime,
     1       aveR)
c .. ACUMULATIVE isochrone parameters
          E        = E       + s3
          EwC      = EwC     + IEwC
          EwCPhi   = EwCPhi  + IEwCPhi
          EwC2     = EwC2    + IEwC2
          EwC2Phi  = EwC2Phi + IEwC2Phi
          XwC2     = XwC2    + IXwC2
          XwC2Phi  = XwC2Phi + IXwC2Phi
          EwCinvr     = EwCinvr     + IEwCinvr
          EwCPhiinvr  = EwCPhiinvr  + IEwCPhiinvr
          EwC2invr    = EwC2invr    + IEwC2invr
          EwC2Phiinvr = EwC2Phiinvr + IEwC2Phiinvr

          F1 = F1 + sqrt(IEwCPhi**2 + IEwC2Phi**2)
          F2 = F2 + sqrt(IEwCPhi**2 + IEwC2Phi**2)* Ich_prime
          F3 = F3 + sqrt(IEwCPhi**2 + IXwC2Phi**2)
          F4 = F4 + sqrt(IEwCPhi**2 + IXwC2Phi**2)* Ich_prime

          DPP = DPP +
     1              Ich_prime *
     1              max(s3,0.1*max(L(iS), W(iS))) *
     1              max(aveR, 0.2)

c .. Continue with the modification of F5, F6, and F7 by shallow weak zone
          F5 = F5 + s3
          F6 = F6 + sqrt(IEwCPhi**2 + IXwC2Phi**2)
          F7 = F7 + sqrt(IEwCPhi**2 + IXwC2Phi**2) * Ich_prime

c .. Remove contribution from segment above the seismo zone
          Zseis = 5.0
          Prop  = 1.0

c .. When the entire E-Path is completely within the strong zone, skip to next segment
          if (-Pd(3).ge.Zseis.and.-Epath(3,i).ge.Zseis) then
            go to 1000
          endif

c .. When E-Path straddles the boundary of a upper weak zone and a
c    deeper strong zone, compute the contribution of E-integral
c    in the weak zone

         if ((Pd(3)+Zseis)*(EPath(3,i)+Zseis).lt.0.0) then
           tmp = (-Zseis-Pd(3))/(EPath(3,i)-Pd(3))
           p5X = tmp *(EPath(1,i)-Pd(1)) + Pd(1)
           p5Y = tmp *(EPath(2,i)-Pd(2)) + Pd(2)
           p5Z = tmp *(EPath(3,i)-Pd(3)) + Pd(3)
           L3 = sqrt((p5X-Pd(1))**2 + (p5Y-Pd(2))**2 +
     1               (p5Z-Pd(3))**2) + L1(i)
           if (-Pd(3).gt.Zseis) then
            s3 = sqrt((p5X-EPath(1,i))**2 +
     1                (p5Y-EPath(2,i))**2 +
     1                (p5Z-EPath(3,i))**2)
            call Integrals(L3, L2(i), Sta_FZ(iS), s3, Phi(i),
     1       IEwC, IEwCPhi, IEwC2, IEwC2Phi, IXwC2, IXwC2Phi,
     1       IEwCinvr, IEwCPhiinvr, IEwC2invr, IEwC2Phiinvr, Ich_prime,
     1       aveR1)
           else
            s3 = L3 - L1(i)
            call Integrals(L1(i), L3, Sta_FZ(iS), s3, Phi(i),
     1       IEwC, IEwCPhi, IEwC2, IEwC2Phi, IXwC2, IXwC2Phi,
     1       IEwCinvr, IEwCPhiinvr, IEwC2invr, IEwC2Phiinvr, Ich_prime,
     1       aveR1)
           endif
          endif

c .. Subtract weak zone contributions

          F5 = F5 - s3 * Prop
          F6 = F6 - sqrt(IEwCPhi**2 + IXwC2Phi**2) * Prop
          F7 = F7 - sqrt(IEwCPhi**2 + IXwC2Phi**2) * Prop * Ich_prime

1000    continue
        enddo

        DPP = log(DPP)

c .. This distance is to the global direct point; s3 is distance to the local point
        Rpd = sqrt((Pd(1)-Sta_x)**2+(Pd(2)-Sta_y)**2+(Pd(3))**2)

c .. GLOBAL isochrone parameter ch.prime
        if (E.eq.0) then
            ch_prime = 0.8
        else
c           ch_prime = min(4.0, max(1.0/(1/0.8 - (Rhyp-Rpd)/E),0.8))
            ch_prime = 1.0/(1/0.8 - (Rhyp-Rpd)/E)
        endif

c .. GLOBAL IDP
        E_cap = log(min(75.0,max(E,0.1)))
        C_cap = (min(ch_prime, 2.45)-0.8)/(2.45-0.8)
        IDP = C_cap * E_cap

        return
        end

ccc---------------------------------------------------------------------
        subroutine Integrals(L1, L2, Sta_FZ, s3, Phi,
     1       EwC, EwCPhi, EwC2, EwC2Phi, XwC2, XwC2Phi,
     1       EwCinvr, EwCPhiinvr, EwC2invr, EwC2Phiinvr, ch_prime, aveR)
c-----------------------------------------------------------------------

c .. s1 : direct point distances
c .. s2 : hypocenter distances
c .. s3 : distance between hypo and direct point

c .. EwC : Integral weighted by cosin(Theta)
c .. EwC2: Integral weighted by cosin(2 Theta)
c .. EwCinvr : Integral weighted by cosin(Theta)/r
c .. EwC2invr: Integral weighted by cosin(2 Theta)/r
c .. XwC : Integral weighted by cosin(Theta), accounting for theta vector variation
c .. XwC2: Integral weighted by cosin(2 Theta), accounting for theta vector variation

c .. Sta_FZ's convention affects EwC, EwCphi, but not others

        implicit none

          real L1, L2, Sta_FZ, s3, Phi
          real EwC, EwCPhi, EwC2, EwC2Phi, XwC2, XwC2Phi, ch_prime, aveR
          real EwCinvr, EwCPhiinvr, EwC2invr, EwC2Phiinvr

          real s1, s2, I1, I2, I3, I1a, I1b, I2a, I2b
          real term1, term2

          L1 = max(0.1, L1)
          s1   = sqrt(L1*L1 + Sta_FZ*Sta_FZ)
          L2 = max(0.1, L2)
          s2   = sqrt(L2*L2 + Sta_FZ*Sta_FZ)

          term1 = log(L2+s2) - log(L1+s1)
          term2 = atan(-L2/Sta_FZ) - atan(-L1/Sta_FZ)

          I1a = -Sta_FZ * (L1/s1 - L2/s2)
          I1b = -Sta_FZ * (-term1)
          I2a = -Sta_Fz**2 * (1/s2 - 1/s1)
          I2b =  s2 - s1
          I1  = (-2*I1a + I1b)
          I2  = ( 2*I2a - I2b)
          I3  = (-I1b)

          EwC    =  (-Sta_FZ * term1)
          EwCPhi =  EwC * sin(Phi)
          EwC2   =  -2*Sta_FZ*term2 - (L2-L1)
          EwC2Phi=  EwC2 * cos(Phi)
          XwC2   =  sqrt(I1*I1+I2*I2)
          XwC2Phi=  XwC2 * cos(Phi)

          EwCinvr    =  term2
          EwCPhiinvr =  EwCinvr * sin(Phi)
          EwC2invr   =  2*(L2/s2-L1/s1) - term1
          EwC2Phiinvr=  EwC2invr * cos(Phi)

          if (s3.eq.0) then
           ch_prime = 0.8
           aveR = 0.0
          else
           ch_prime = 1.0/(1/0.8 - (s2-s1)/s3)
           aveR = sqrt(XwC2Phi**2 + EwCPhi**2)/s3
          endif

          return
          end

ccc---------------------------------------------------------------------
        subroutine Direct_PointDF(faultID, Hyp_x, Hyp_y, Hyp_z, Ztor,
     1      StaName, Sta_x, Sta_y,
     1      nseg, P, iSeg, Dip, Strike, T, iEdgHyp,
     1      Pd, iEdgPd, L1, L2, Sta_FZ, phi, theta)
c-----------------------------------------------------------------------
c Find 1. direct point (Pd) on iSeg-th fault segment
c      2. station projection on fault (s_x,s_y,s_z) and Sta_FZ

        implicit none
        integer mxsg
        parameter(mxsg=40)

        character faultID*5, StaName*55
        integer nseg, iSeg, iEdgHyp
        real Hyp_x, Hyp_y, Hyp_z, Sta_x, Sta_y, P(3,4,mxsg),
     1       Dip, Strike, T, Sta_FZ, phi, theta

        integer iEdgPd
        real Pd(3), L1, L2

        integer i, j1, j2
        real pi, tmp, Ztor, vd(2), s_x, s_y, s_z, a, b
        real sinDip, tanDip, ax, ay, az, c1, c2, c3, dot
        real tx, ty, tz, px, py, pz, Rt, Rp, Rsn, Rsp, Rv
        real ux(mxsg), uy(mxsg), uz(mxsg), vx(mxsg), vy(mxsg), vz(mxsg),
     1       wx(mxsg), wy(mxsg), wz(mxsg), rx(mxsg), ry(mxsg), rz(mxsg),
     1       nx(mxsg), ny(mxsg)
        common /unitVectors/ux,uy,uz,vx,vy,vz,wx,wy,wz,rx,ry,rz,nx,ny

        i = iSeg
        pi = 3.14159265359
c       Ztor = -P(3,1,i)

c .. Find the the x-y-z coocrdinate (s_x,s_y,s_z) of projected station
        tmp = Strike + pi/2.0
        vd(1) = sin(tmp)
        vd(2) = cos(tmp)
        sinDip = sin(Dip)
        tanDip = tan(Dip)
        tmp = -(T + Ztor/tan(Dip)) * sinDip * sinDip
        s_x = Sta_x + tmp * vd(1)
        s_y = Sta_y + tmp * vd(2)
        s_z = tmp/tanDip

c .. Sta_Z is + if point is below fault, - if above fault
        Sta_FZ = tmp / sinDip
c       Sta_FY = Sta_FZ / tan(Dip) +  Ztor/sin(Dip)


c .. Find the intersection of (x_s,y_s)-(x_Hyp,y_Hyp)
c     with fault boundary

c .. Hypocenter may be on boundary (iEdgHyp != 0), speical care
c    is needed:
c      If intersection is on the same boundary (iEdgHyp = j1),
c      direct point is forced to colocate with hypo.
c      (Otherwise, the connecting segment is parallel to boundary,
c       which is not intersecting by convention of intersect2D)

       iEdgPd = 0

       Do j1 = 1, 4
        j2 = j1 + 1
        if (j1.eq.4) j2 = 1
        call intersect2D(Hyp_x, Hyp_y, s_x, s_y,
     1                P(1,j1,i), P(2,j1,i), P(1,j2,i), P(2,j2,i), a, b)
        if (a.ge.-1E-10.and.a.le.1.and.b.ge.-1E-10.and.b.le.1) then
          if (iEdgHyp.ne.j1) then
           iEdgPd = j1
           Pd(1) =  P(1,j1,i) + b * (P(1,j2,i) - P(1,j1,i))
           Pd(2) =  P(2,j1,i) + b * (P(2,j2,i) - P(2,j1,i))
           Pd(3) =  P(3,j1,i) + b * (P(3,j2,i) - P(3,j1,i))
            go to 100
          else
c          write (*,'(a11,4f10.4,i5)') 'Same Edge: ', x, y, a, b, j1
           iEdgPd = j1
           Pd(1) = Hyp_x
           Pd(2) = Hyp_y
           Pd(3) = Hyp_z
          endif
        endif
       Enddo
       if (iEdgPd.ne.0) go to 100

c .. Station projection is interior, Pd is set to station projection
       Pd(1) = s_x
       Pd(2) = s_y
       Pd(3) = s_z

100    continue

c .. compute distance from projection point to Pd   -- L1
       L1 = sqrt((s_x-Pd(1))**2+(s_y-Pd(2))**2+(s_z-Pd(3))**2)

c .. compute distance from projection point to Phyp -- L2
        ax = (s_x-Hyp_x)
        ay = (s_y-Hyp_y)
        az = (s_z-Hyp_z)
       L2 = sqrt(ax*ax+ay*ay+az*az)

c .. angle of hypo-to-projected-station, measured counterclockwise from rake vector
        ax = ax/L2
        ay = ay/L2
        az = az/L2
        dot = ax*rx(iSeg)+ay*ry(iSeg)+az*rz(iSeg)
        c1  =   ay*rz(iSeg) - az*ry(iSeg)
        c2  = -(ax*rz(iSeg) - az*rx(iSeg))
        c3  =   ax*ry(iSeg) - ay*rx(iSeg)
        phi = atan2(sign(sqrt(c1**2+c2**2+c3**2), c3), dot)

c .. Zenith angle, measured clockwise from the normal vector
        ax = (Sta_x-Hyp_x)
        ay = (Sta_y-Hyp_y)
        az = (     -Hyp_z)
        tmp= sqrt(ax*ax+ay*ay+az*az)
        ax = ax/tmp
        ay = ay/tmp
        az = az/tmp
        dot = ax*wx(iSeg)+ay*wy(iSeg)+az*wz(iSeg)
        c1  =   ay*wz(iSeg) - az*wy(iSeg)
        c2  = -(ax*wz(iSeg) - az*wx(iSeg))
        c3  =   ax*wy(iSeg) - ay*wx(iSeg)
        theta = atan2(-sign(sqrt(c1**2+c2**2+c3**2), c3), dot)

c .. theta vector (tx,ty) and phi vector (px,py) in a fault rectangular
c     coordindate system where X is the rake direction
        tx = cos(theta)*cos(phi)
        ty = cos(theta)*sin(phi)
        tz = -sin(theta)
        px = -sin(phi)
        py =  cos(phi)
        pz =  0.0

c .. radiation pattern
       Rt = cos(2*theta)*cos(phi)
       Rp = -cos(theta) *sin(phi)

c .. rotate to strike-normal (n) and strike-paralle (u) direction

       Rsn = Rt * (tx*nx(iSeg)+ty*ny(iSeg)) +
     1       Rp * (px*nx(iSeg)+py*ny(iSeg))
       Rsp = Rt * (tx*ux(iSeg)+ty*uy(iSeg)) +
     1       Rp * (px*ux(iSeg)+py*uy(iSeg))
       Rv  = Rt * (tx*ux(iSeg)+ty*uy(iSeg)) +
     1       Rp * (px*ux(iSeg)+py*uy(iSeg))

       return
       end

ccc---------------------------------------------------------------------
        subroutine Direct_PointVF(faultID, Hyp_Fx, Hyp_Fy, iEdgHyp,
     1      P_Fx, P_Fy, Dip, Strike, Rake,
     1      StaName, Sta_Fx, Sta_Fy, Sta_Fz, iWN,
     1      Pd_Fx, Pd_Fy, iEdgPd, L1, L2, Phi, Theta)
c-----------------------------------------------------------------------
c Find the direct point (Pd) of a station

        implicit none
        integer mxsg
        parameter(mxsg=40)

c .. Calling arguments
        character faultID*5, StaName*55
        integer iEdgHyp
        real Hyp_Fx, Hyp_Fy, Sta_Fx, Sta_Fy, Sta_Fz,
     1       P_Fx(4), P_Fy(4), Dip, Strike, Rake

c .. Returned arguments
        integer iEdgPd, iWN
        real Pd_Fx, Pd_Fy, L1, L2, Phi, Theta

        integer j1, j2
        real s_x, s_y, s_z
        real pi, tmp, a, b
        real sinDip, tanDip, ax, ay, az, c1, c2, c3, dot
        real tx, ty, tz, px, py, pz, Rt, Rp, Rsn, Rsp, Rv

        real ux(mxsg), uy(mxsg), uz(mxsg), vx(mxsg), vy(mxsg), vz(mxsg),
     1       wx(mxsg), wy(mxsg), wz(mxsg), rx(mxsg), ry(mxsg), rz(mxsg),
     1       nx(mxsg), ny(mxsg)
        common /unitVectors/ux,uy,uz,vx,vy,vz,wx,wy,wz,rx,ry,rz,nx,ny

      pi = 3.14159265359

c .. Hypocenter may be on boundary (iEdgHyp != 0), speical care
c    is required:
c      If intersection is on the same boundary (iEdgHyp = j1),
c      direct point is forced to colocate with hypo.
c      (Otherwise, the connecting segment is parallel to boundary,
c       which is not intersecting by convention of intersect2D)

      call PULLPOLY(iWN, Sta_Fx, Sta_Fy, P_Fx, P_Fy, 4)

      IF (iWN.eq.0.or.iWN.eq.2) then
c .. iWN = 0 : (Sta_Fx, Sta_Fy) is outside fault polygon
c .. iWN = 2 : (Sta_Fx, Sta_Fy) is on the edge or vertex
         iEdgPd = iEdgHyp
         Pd_Fx = Hyp_Fx
         Pd_Fy = Hyp_Fy
         Do j1 = 1, 4
            if (j1.eq.iEdgHyp) cycle
            j2 = j1 + 1
            if (j1.eq.4) j2 = 1
            call intersect2D(Hyp_Fx, Hyp_Fy, Sta_Fx, Sta_Fy,
     1           P_Fx(j1), P_Fy(j1), P_Fx(j2), P_Fy(j2), a, b)
             if (abs(a).le.1.0D-10)     a=0.0
             if (abs(a-1.0).le.1.0D-10) a=1.0
             If (a.ge.0.0.and.a.le.1.0.and.b.ge.0.0.and.b.le.1.0) then
               iEdgPd = j1
               Pd_Fx =  P_Fx(j1) + b * (P_Fx(j2) - P_Fx(j1))
               Pd_Fy =  P_Fy(j1) + b * (P_Fy(j2) - P_Fy(j1))
               EXIT
            Endif
         Enddo
      ELSEIF (iWN.eq.1.or.iWN.eq.-1) then
c .. iWN = 1 or -1: (Sta_Fx, Sta_Fy) is inside fault polygon
         iEdgPd = iEdgHyp
         Pd_Fx = Sta_Fx
         Pd_Fy = Sta_Fy
      ELSE
         iEdgPd = 0
c         Pd_Fx = Hyp_Fx
c         Pd_Fy = Hyp_Fy
         Pd_Fx = Sta_Fx
         Pd_Fy = Sta_Fy
      ENDIF

c .. compute distance from projection point to Pd   -- L1
       L1 = sqrt((Sta_Fx-Pd_Fx)**2+(Sta_Fy-Pd_Fy)**2)

c .. compute distance from projection point to Phyp -- L2
       ax = (Sta_Fx-Hyp_Fx)
       ay =-(Sta_Fy-Hyp_Fy)
       L2 = sqrt(ax*ax+ay*ay)

c .. angle of hypo-to-projected-station, measured counterclockwise from strike
       Phi = atan2(ay, ax) - Rake

c .. Zenith angle, measured clockwise from the perpendicular vector
       Theta = atan2(L2, Sta_Fz)      ! 0 to 180 degrees

c .. theta vector (tx,ty) and phi vector (px,py) in a fault rectangular
c .. coordindate system where X is the rake direction

        tx = cos(theta)*cos(phi)
        ty = cos(theta)*sin(phi)
        tz = -sin(theta)
        px = -sin(phi)
        py =  cos(phi)
        pz =  0.0

c .. radiation pattern
       Rt = cos(2*theta)*cos(phi)
       Rp = -cos(theta) *sin(phi)

c .. rotate radiation coef to strike-normal (n) and strike-paralle (u) direction

c      Rsn = Rt * (tx*nx(iS)+ty*ny(iS)) +
c    1       Rp * (px*nx(iS)+py*ny(iS))
c      Rsp = Rt * (tx*ux(iS)+ty*uy(iS)) +
c    1       Rp * (px*ux(iS)+py*uy(iS))
c      Rv  = Rt * (tx*ux(iS)+ty*uy(iS)) +
c    1       Rp * (px*ux(iS)+py*uy(iS))


       return
       end

ccc---------------------------------------------------------------------
        subroutine intersect2D(x1, y1, x2, y2, x3, y3, x4, y4, a, b)
c-----------------------------------------------------------------------
c Find the intersection of two lines in 2D

        implicit none
        real x1, y1, x2, y2, x3, y3, x4, y4, r, s
        real denom, a, b

        denom = (y4-y3)*(x2-x1)-(x4-x3)*(y2-y1)
        if (denom .eq. 0) then
c           write (*,*) 'Error: Parallel to Segment Edge'
c           stop
            a = 1E10
            b = 1E10
        else
            a = ((x4-x3)*(y1-y3)-(y4-y3)*(x1-x3))/denom
            b = ((x2-x1)*(y1-y3)-(y2-y1)*(x1-x3))/denom
        endif

        return
        end

ccc---------------------------------------------------------------------
        subroutine Closest_Point_on_Fault(x, y, z, nseg, P, iSegHyp,
     1       Rrup, Pc, iSegPc, iEdgPc )
c-----------------------------------------------------------------------
        parameter(mxsg=40)

        integer nseg, iSegHyp
        real x, y, z, P(3,4,mxsg)

        real Rrup, Pc(3), sPc
        integer iSegPc, iEdgPc

        real*8 dclst(mxsg), P_clst(3,mxsg), dmin
        integer iclst(mxsg)

        do i=1,nseg
          call Closest_Point_on_Trapezoid_3D(
     1          x, y, z, p(1,1,i), dclst(i), P_clst(1,i), iclst(i))
        enddo

        dmin = 1.0E10
        do i=1,nseg
          if(dclst(i).lt.dmin) then
                  dmin   = dclst(i)
                  iEdgPc = iclst(i)
                  Pc(1)  = P_clst(1,i)
                  Pc(2)  = P_clst(2,i)
                  Pc(3)  = P_clst(3,i)
                  iSegPc = i
          endif
        enddo

c .. When Pc is on the side edge of two consecutive trapezoid,
c    resolve the equal min-distance problem by choosing the one
c    that is closer to the hypocenter

        do i=1, nseg

c         if (abs(dmin-dclst(i)).le.1E-10.and.i.ne.iSegPc) then
          if (abs(dmin-dclst(i)).le.1E-05.and.i.ne.iSegPc) then
                 if (iclst(i).ne.2.and.iclst(i).ne.4)
     1                write (*,*) 'Interior Equal-distance point ',
     1                            x, y, dclst(i), dmin, i, iSegPc,
     1                            iclst(i), iclst(iSegPc)

c                if (abs(iSegHyp-i).lt.abs(iSegHyp-iSegPc)) then
c                 dmin   = dclst(i)
c                 iEdgPc = iclst(i)
c                 Pc(1)  = P_clst(1,i)
c                 Pc(2)  = P_clst(2,i)
c                 Pc(3)  = P_clst(3,i)
c                 iSegPc = i
c                endif
          endif

        enddo

c (Aug-11-2011)
        if (iSegPc.ne.1.and.iclst(iSegPc).eq.4.and.
     1      iSegHyp.lt.iSegPc) then
                  iSegPc = iSegPc - 1
                  dmin   = dclst(iSegPc)
                  iEdgPc = iclst(iSegPc)
                  Pc(1)  = P_clst(1,iSegPc)
                  Pc(2)  = P_clst(2,iSegPc)
                  Pc(3)  = P_clst(3,iSegPc)
        endif
        if (iSegPc.ne.nseg.and.iclst(iSegPc).eq.2.and.
     1      iSegHyp.gt.iSegPc) then
                  iSegPc = iSegPc + 1
                  dmin   = dclst(iSegPc)
                  iEdgPc = iclst(iSegPc)
                  Pc(1)  = P_clst(1,iSegPc)
                  Pc(2)  = P_clst(2,iSegPc)
                  Pc(3)  = P_clst(3,iSegPc)
        endif

        Rrup = dmin

        return
        end

ccc---------------------------------------------------------------------
        subroutine Closest_Point_on_Trapezoid_3D
     1        (x, y, z, P, dclst, P_clst, iclst)
c-----------------------------------------------------------------------
c Ref: Schneider and Eberly (2002; page 376)

        implicit none

        real x, y, z, P(3,4)

c .. Double precision is needed for the algorithm
        real*8 dclst, P_clst(3)
        integer iclst

        real*8 B(3), E0(3), E1(3), E_clst(3,4)
        real*8 a1,b1,c1,d1,e11,s1,t1
        real*8 a2,b2,c2,d2,e2,s2,t2
        real*8 detinv, dmin

        real*8 f, dis1, dis2, dis3, dis4
        integer inside

        inside = 0

c .. Top triangle (P1P2P4) of trapezoid
c .. iclst < 0 : interior point
c .. iclst > 0 : boundary point

        B(1)  = P(1,1)
        B(2)  = P(2,1)
        B(3)  = P(3,1)
        E0(1) = P(1,2)-B(1)
        E0(2) = P(2,2)-B(2)
        E0(3) = P(3,2)-B(3)
        E1(1) = P(1,4)-B(1)
        E1(2) = P(2,4)-B(2)
        E1(3) = P(3,4)-B(3)
        a1 = E0(1)*E0(1)+E0(2)*E0(2)+E0(3)*E0(3)
        b1 = E0(1)*E1(1)+E0(2)*E1(2)+E0(3)*E1(3)
        c1 = E1(1)*E1(1)+E1(2)*E1(2)+E1(3)*E1(3)
        d1 = E0(1)*(B(1)-x)+E0(2)*(B(2)-y)+E0(3)*(B(3)-z)
        e11= E1(1)*(B(1)-x)+E1(2)*(B(2)-y)+E1(3)*(B(3)-z)
        detinv = 1.0/(a1*c1-b1*b1)
        s1 = (b1*e11-c1*d1)*detinv
        t1 = (b1*d1-a1*e11)*detinv
        if (s1.ge.0.and.s1.le.1 .and. t1.ge.0.and.t1.le.1 .and.
     1      s1+t1.le.1) then
          inside = 1
          P_clst(1) = B(1)+s1*E0(1)+t1*E1(1)
          P_clst(2) = B(2)+s1*E0(2)+t1*E1(2)
          P_clst(3) = B(3)+s1*E0(3)+t1*E1(3)
          dclst = (P_clst(1)-x)**2+(P_clst(2)-y)**2+(P_clst(3)-z)**2
          dclst = sqrt(dclst)
          iclst = -1
c .. t1 must be tested first so that the upper left point (t1=0, s1=0)
c .. is on edge 4
          if (t1.eq.0) iclst=1
          if (s1.eq.0) iclst=4
          if (s1.eq.1) iclst=2
          return
        endif

c .. Bottom triangle (P3P2P4) of trapezoid
        B(1)  = P(1,3)
        B(2)  = P(2,3)
        B(3)  = P(3,3)
        E0(1) = P(1,2)-B(1)
        E0(2) = P(2,2)-B(2)
        E0(3) = P(3,2)-B(3)
        E1(1) = P(1,4)-B(1)
        E1(2) = P(2,4)-B(2)
        E1(3) = P(3,4)-B(3)
        a2 = E0(1)*E0(1)+E0(2)*E0(2)+E0(3)*E0(3)
        b2 = E0(1)*E1(1)+E0(2)*E1(2)+E0(3)*E1(3)
        c2 = E1(1)*E1(1)+E1(2)*E1(2)+E1(3)*E1(3)
        d2 = E0(1)*(B(1)-x)+E0(2)*(B(2)-y)+E0(3)*(B(3)-z)
        e2 = E1(1)*(B(1)-x)+E1(2)*(B(2)-y)+E1(3)*(B(3)-z)
        detinv = 1.0/(a2*c2-b2*b2)
        s2 = (b2*e2-c2*d2)*detinv
        t2 = (b2*d2-a2*e2)*detinv
        if ((s2.ge.0.and.s2.le.1).and.(t2.ge.0.and.t2.le.1).and.
     1      s2+t2.le.1) then
          inside = 2
          P_clst(1) = B(1)+s2*E0(1)+t2*E1(1)
          P_clst(2) = B(2)+s2*E0(2)+t2*E1(2)
          P_clst(3) = B(3)+s2*E0(3)+t2*E1(3)
          dclst = (P_clst(1)-x)**2+(P_clst(2)-y)**2+(P_clst(3)-z)**2
          dclst = sqrt(dclst)
          iclst = -2
          if (s2.eq.0) iclst = 3
          if (t2.eq.0) iclst = 2
          if (t2.eq.1) iclst = 4
          return
        endif

c .. Closest point is not interior; check the our edges of trapezoid

c .. Edge 1: Top edge (P1P2)
        if (-d1 .lt. 0) then
                f = 0
        elseif (-d1 .gt. a1) then
                f = 1
        else
                f = -d1/a1
        endif
        E_clst(1,1) = P(1,1) + f * (P(1,2)-P(1,1))
        E_clst(2,1) = P(2,1) + f * (P(2,2)-P(2,1))
        E_clst(3,1) = P(3,1) + f * (P(3,2)-P(3,1))

c .. Edge 2: Side edge (P3P2)
        if (-d2 .lt. 0) then
                f = 0
        elseif (-d2 .gt. a2) then
                f = 1
        else
                f = -d2/a2
        endif
        E_clst(1,2) = P(1,3) + f * (P(1,2)-P(1,3))
        E_clst(2,2) = P(2,3) + f * (P(2,2)-P(2,3))
        E_clst(3,2) = P(3,3) + f * (P(3,2)-P(3,3))

c .. Edge 3: Bottom edge (P3P4)
        if (-e2 .lt. 0) then
                f = 0
        elseif (-e2 .gt. c2) then
                f = 1
        else
                f = -e2/c2
        endif
        E_clst(1,3) = P(1,3) + f * (P(1,4)-P(1,3))
        E_clst(2,3) = P(2,3) + f * (P(2,4)-P(2,3))
        E_clst(3,3) = P(3,3) + f * (P(3,4)-P(3,3))

c .. Edge 4: Side edge (P1P4)
        if (-e11.lt. 0) then
                f = 0
        elseif (-e11.gt. c1) then
                f = 1
        else
                f = -e11/c1
        endif
        E_clst(1,4) = P(1,1) + f * (P(1,4)-P(1,1))
        E_clst(2,4) = P(2,1) + f * (P(2,4)-P(2,1))
        E_clst(3,4) = P(3,1) + f * (P(3,4)-P(3,1))

c .. Find closest point on the 4 edges of Trapezoid.
c    Search is in the order of edge 2, 4, 1, and 3.
c    This helps determine if closest point is on the side edges.

        dis1 = (E_clst(1,1)-x)**2+(E_clst(2,1)-y)**2+(E_clst(3,1)-z)**2
        dis2 = (E_clst(1,2)-x)**2+(E_clst(2,2)-y)**2+(E_clst(3,2)-z)**2
        dis3 = (E_clst(1,3)-x)**2+(E_clst(2,3)-y)**2+(E_clst(3,3)-z)**2
        dis4 = (E_clst(1,4)-x)**2+(E_clst(2,4)-y)**2+(E_clst(3,4)-z)**2

c .. Check edges 2 and 4 first, then 1 and 3 so that the vertices
c    are regarded as part of edges 2 and 4.
        dmin  = 1E10
        iclst = 0
        if (dis2 .lt. dmin ) then
          iclst = 2
          dmin  = dis2
        endif
        if (dis4 .lt. dmin ) then
          iclst = 4
          dmin  = dis4
        endif
        if (dis1 .lt. dmin ) then
          iclst = 1
          dmin  = dis1
        endif
        if (dis3 .lt. dmin ) then
          iclst = 3
          dmin  = dis3
        endif

        dclst = dsqrt(dmin )
        P_clst(1) = E_clst(1,iclst)
        P_clst(2) = E_clst(2,iclst)
        P_clst(3) = E_clst(3,iclst)

        return
        end

ccc---------------------------------------------------------------------
        subroutine Closest_Point_on_Line_Segment(x, y, z, P1, P2,
     1        dmin, Pmin)
c-----------------------------------------------------------------------
        implicit none
        real x, y, z, P1(3), P2(3)
        real dmin, Pmin(3)
        integer between, k
        real*8 base, d1, d2, p, area, hc, frct, tmp

        base = (P2(1)-P1(1))**2+(P2(2)-P1(2))**2+(P2(3)-P1(3))**2
        base = sqrt(base)

        d1   = (x-P1(1))**2+(y-P1(2))**2+(z-P1(3))**2
        d1   = sqrt(d1)

        d2   = (x-P2(1))**2+(y-P2(2))**2+(z-P2(3))**2
        d2   = sqrt(d2)

        p    = (base+d1+d2)/2.0
        area = sqrt(abs(p*(p-d1)*(p-d2)*(p-base)))

        hc   = 2.0 * area / base

        if ((d2*d2)>(base*base+d1*d1).or.(d1*d1)>(base*base+d2*d2)) then
                between = 0
        else
                between = 1
        endif

        if (between .eq. 1) then
                dmin = hc
                frct = sqrt(max(d1*d1-hc*hc, 0.0)) / base
                Pmin(1) = frct*(P2(1)-P1(1))+P1(1)
                Pmin(2) = frct*(P2(2)-P1(2))+P1(2)
                Pmin(3) = frct*(P2(3)-P1(3))+P1(3)
        elseif (d1 < d2) then
                dmin = d1
                Pmin(1) = P1(1)
                Pmin(2) = P1(2)
                Pmin(3) = P1(3)
        else
                dmin = d2
                Pmin(1) = P2(1)
                Pmin(2) = P2(2)
                Pmin(3) = P2(3)
        endif
c       write (*,1) between, d1, d2, base, hc, frct, dmin, Pmin
1       format(1x,i5,9f9.6)
        return
        end

ccc---------------------------------------------------------------------
        subroutine DistAz(x1, y1, z1, x2, y2, z2, Dist, Az)
c-----------------------------------------------------------------------
        real x1, y1, z1, x2, y2, z2
        real Dist, Az
        real dx, dy, dz, pi

        pi = 3.14159265359
        dx = (x2-x1)
        dy = (y2-y1)
        dz = (z2-z1)
        Dist = sqrt(dx**2+dy**2+dz**2)
        if (dx .eq. 0) then
                if (dy .lt. 0) then
                     Az = pi
                else
                     Az = 0.0
                endif
        else
                if (dy .eq. 0) then
                        if (dx .lt. 0) then
                                Az = -pi/2.0
                        else
                                Az = pi/2.0
                        endif
                else
                        Az = atan2(dx,dy)
                endif
        endif
        if (Az .lt. 0)  Az = Az + 2 * pi
        return
        end

ccc---------------------------------------------------------------------
        subroutine LatLong_to_XY(orgLat, orgLong, Lat, Long)
c-----------------------------------------------------------------------
c Lat and Long are returned as x- and y-coordinate, respectively
c
c  Transofmration of coordinate is based on haversine formula
c     Reference: http://www.movable-type.co.uk/scripts/latlong.html

        implicit none
        real orgLat, orgLong, Lat, Long, lam1,lam2,phi1,phi2,x,y,dtr,
     ,    Rearth, a, c, dphi,dlam,theta,dist

        Rearth=6371.0
        dtr=asin(1.0)/90.0


        IF(orglong .lt. 0.0) then
          lam1=(orglong+360.0)*dtr
         ELSE
          lam1=orglong*dtr
        ENDIF

        phi1=orglat*dtr


        IF(long .lt. 0.0) then
          lam2=(long+360.0)*dtr
         ELSE
          lam2=long*dtr
        ENDIF

        phi2=lat*dtr

        dphi=phi2-phi1
        dlam=lam2-lam1

        a = sin(dphi/2.0)*sin(dphi/2.0) +
     ,       cos(phi1)*cos(phi2)*sin(dlam/2.0)*sin(dlam/2.0)
        c = 2.0*atan2(sqrt(a),sqrt(1.0-a))

        dist = Rearth*c

        IF(dist .eq. 0.) then
          theta = 0.
         ELSE
          theta = atan2(sin(dlam)*cos(phi2),cos(phi1)*
     ,          sin(phi2)-sin(phi1)*cos(phi2)*cos(dlam))
        ENDIF

        x = dist*sin(theta)
        y = dist*cos(theta)

        lat=x
        long=y
        return
        end

ccc---------------------------------------------------------------------
        subroutine XY_to_LatLong(orgLat, orgLong, x, y)
c-----------------------------------------------------------------------
c X- and Y-coordinates are returned as Lat and Long, respectively
c
c  Transofmration of coordinate is based on haversine formula
c     Reference: http://www.movable-type.co.uk/scripts/latlong.html

        implicit none

        real orgLat, orgLong, Lat, Long, lam1,lam2,phi1,phi2,x,y,dtr,
     ,    Rearth, a, c, dphi,dlam,theta,delta,dist

        Rearth=6371.0
        dtr=asin(1.0)/90.0

        IF(orglong .lt. 0.0) then
          lam1=(orglong+360.0)*dtr
         ELSE
          lam1=orglong*dtr
        ENDIF

        phi1=orglat*dtr

        dist = sqrt(x*x + y*y)
        delta = dist/Rearth
        theta = atan2(x,y)

        phi2 = asin(sin(phi1)*cos(delta)+
     ,       cos(phi1)*sin(delta)*cos(theta))

        lam2 = lam1 + atan2(sin(theta)*sin(delta)*cos(phi1),
     ,                      cos(delta)-sin(phi1)*sin(phi2) )

        lat  = phi2/dtr

        long = lam2/dtr

        IF(long.gt.180.0) long=long-360.0

        x=Lat
        y=Long
        return
        end

c-----------------------------------------------------------------------
      subroutine FaultArea(nseg, P,
     1   TotalTopL, TotalBotL, aveWidth, maxWidth, TotalArea)
c-----------------------------------------------------------------------

      implicit none
      integer mxsg
      parameter(mxsg=40)

      integer nseg
      real P(3,4,mxsg)
      real TotalTopL, TotalBotL, TotalArea, aveWidth, maxWidth

      integer i
      real BotL(mxsg), TopL(mxsg), hc(mxsg), Area(mxsg)
      real dtr

      dtr = atan(1.0)/45.0
      TotalTopL = 0.0
      TotalBotL = 0.0
      TotalArea = 0.0
      aveWidth  = 0.0
      maxWidth  = 0.0

      DO i=1,nseg

c... Length of top base

        TopL(i) = sqrt(
     1            (P(1,1,i)-P(1,2,i))**2+
     1            (P(2,1,i)-P(2,2,i))**2+
     1            (P(3,1,i)-P(3,2,i))**2)

c... Length of bottom base

        BotL(i) = sqrt(
     1            (P(1,3,i)-P(1,4,i))**2+
     1            (P(2,3,i)-P(2,4,i))**2+
     1            (P(3,3,i)-P(3,4,i))**2)

        TotalTopL = TotalTopL + TopL(i)
        TotalBotL = TotalBotL + BotL(i)

c... Altitude; Width = Altitude

        call getHC(P(1,3,i),P(2,3,i),P(3,3,i),
     1             P(1,2,i),P(2,2,i),P(3,2,i),
     1             P(1,1,i),P(2,1,i),P(3,1,i), hc(i))

        Area(i) = (TopL(i)+ BotL(i))*hc(i)/2.0
        TotalArea  = TotalArea + Area(i)

        maxWidth = max(maxWidth, hc(i))
        aveWidth = aveWidth + hc(i)

      ENDDO

      aveWidth = aveWidth / float(nseg)

      return
      end

ccc---------------------------------------------------------------------
        subroutine getHC(sx,sy,sz,x1,y1,z1,x2,y2,z2,hc)
c-----------------------------------------------------------------------

        real d(2)

        base=sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)+(z2-z1)*(z2-z1))
        d(1)=sqrt((sx-x1)*(sx-x1)+(sy-y1)*(sy-y1)+(sz-z1)*(sz-z1))
        d(2)=sqrt((sx-x2)*(sx-x2)+(sy-y2)*(sy-y2)+(sz-z2)*(sz-z2))
c       print*,' triangle ',d,base
        p=(d(1)+d(2)+base)/2.0
        area=sqrt(abs(p*(p-d(1))*(p-d(2))*(p-base)))
        hc=2.0*area/base
        return
        end

ccc---------------------------------------------------------------------
      subroutine LocalStrikeParallel(StaName, nseg, sPc, sHyp, sQ2,
     1    Strike, lStrike, EffL, iStrike1, iStrike2)
c-----------------------------------------------------------------------
c
c Local strike parallel direction is computed as the (weighted)
c   average of strike directions over a strech of 20 km fault defined
c   on the aux line at the focal depth
c   starting from the closest point and toward the hypocenter.

        implicit none
        integer mxsg
        parameter(mxsg=40)

        character StaName*55
        integer nseg
        real sPc, sHyp, sQ2(mxsg+1), Strike(nseg)

        real lStrike, EffL
        integer iStrike1, iStrike2

        real xx, yy, pi, RupL(nseg)
        integer i, i1, i2
        logical cross0

        pi = 3.14159265359

        RupL(1) = sQ2(1)
        do i=2,nseg
          RupL(i) = sQ2(i) - sQ2(i-1)
        enddo

c N2M ..
        if (sHyp.lt.sPc) then
                xx = sPc - 20.
                yy = sPc
        else
                xx = sPc
                yy = sPc + 20.
        endif

        if (xx.le.0.0) xx = 0.0
        if (xx.gt.sQ2(nseg)) xx = sQ2(nseg)
        if (xx.le.sQ2(1)) then
             iStrike1 = 1
        else
             do i=2,nseg
                if (xx.ge.sQ2(i-1).and.xx.le.sQ2(i)) exit
             enddo
             iStrike1 = i
        endif

        if (yy.le.0.0) yy = 0.0
        if (yy.gt.sQ2(nseg)) yy = sQ2(nseg)
        if (yy.le.sQ2(1)) then
             iStrike2 = 1
        else
             do i=2,nseg
                if (yy.ge.sQ2(i-1).and.yy.le.sQ2(i)) exit
             enddo
             iStrike2 = i
        endif

        if (iStrike1.eq.iStrike2) then
                lStrike = Strike(iStrike1)
                EffL = yy + xx
        else
                xx =  sQ2(iStrike1) - xx
                yy =  yy - sQ2(iStrike2-1)

                i1 = 0
                i2 = 0
                cross0 = .false.
                DO i = iStrike1, iStrike2
                        if (Strike(i).le.pi/2.0)     i1=i1+1
                        if (Strike(i).ge.pi/2.0*3.0) i2=i2+1
                ENDDO
                if (i1.ne.0.and.i2.ne.0) cross0 = .true.

                i1 = 0
                i2 = 0
                if (cross0.and.Strike(iStrike1).lt.pi/2.0) i1 = 1
                if (cross0.and.Strike(iStrike2).lt.pi/2.0) i2 = 1
                lStrike = xx * (Strike(iStrike1)+2.0*pi*float(i1))
     1                  + yy * (Strike(iStrike2)+2.0*Pi*float(i2))

                EffL = xx + yy
                do i = iStrike1+1, iStrike2-1
                 EffL = EffL + RupL(i)
                 if (cross0.and.Strike(i).lt.pi/2.0) then
                  lStrike = lStrike + RupL(i)*(Strike(i)+2.0*pi)
                 else
                  lStrike = lStrike + RupL(i) * Strike(i)
                 endif
                enddo
                lStrike = lStrike / EffL

                if (lStrike.gt.2.0*pi) lStrike = lStrike-2.0*pi

c                if (abs(EffL-20.).gt.0.001)
c     1           write (*,'(a26,2x,a35,24f10.3)')
c     1                   'WARNING: EffL is not 20 km',
c     1                   StaName, xx,yy,
c     1                  (RupL(i),i=iStrike1+1,iStrike2-1), EffL

        endif

        return
        end

ccc---------------------------------------------------------------------
      subroutine rmsD(nseg, P, X, Y, Rrms)
c-----------------------------------------------------------------------
c
c This subroutine was modifed from subroutine Rrms in FF7.FOR
c This modification has been verified for the case of a single segment fault
c Verification for the multi-segment case has not been done yet.
       implicit none
       integer mxsg
       parameter (mxsg=40)

       integer nsnsg(mxsg), nseg
       real delta, X, Y, Rrms, hc(mxsg), P(3,4,mxsg)

       real*8 x1, y1, z1, x2, y2, z2
       real*8 dx1, dy1, dz1, dx2, dy2, dz2
       real*8 rinvsq, sum, SiteX, SiteY

       integer nfd, iseg, i1, j
       SiteX = dble(X)
       SiteY = dble(Y)
       sum = 0.0
       delta = 0.1      !! need more experiment with this paramter
c      nfd = depth/delta
       do i1=1,nseg
        call getHC(P(1,1,i1),P(2,1,i1),P(3,1,i1),
     1             P(1,2,i1),P(2,2,i1),P(3,2,i1),
     1             P(1,3,i1),P(2,3,i1),P(3,3,i1), hc(i1))
        nfd = hc(i1)/delta + 1
        dx1 = (P(1,4,i1)-P(1,1,i1))/float(nfd)
        dy1 = (P(2,4,i1)-P(2,1,i1))/float(nfd)
        dz1 = (P(3,4,i1)-P(3,1,i1))/float(nfd)
        dx2 = (P(1,3,i1)-P(1,2,i1))/float(nfd)
        dy2 = (P(2,3,i1)-P(2,2,i1))/float(nfd)
        dz2 = (P(3,3,i1)-P(3,2,i1))/float(nfd)
        x1 = P(1,1,i1) - dx1/2.0
        y1 = P(2,1,i1) - dy1/2.0
        z1 = P(3,1,i1) - dz1/2.0
        x2 = P(1,2,i1) - dx2/2.0
        y2 = P(2,2,i1) - dy2/2.0
        z2 = P(3,2,i1) - dz2/2.0
        DO j=1,nfd
          x1 = x1 + dx1
          y1 = y1 + dy1
          z1 = z1 + dz1
          x2 = x2 + dx2
          y2 = y2 + dy2
          z2 = z2 + dz2
          call dbl_rsq(SiteX, SiteY, 0.0D+01,
     1                 x1, y1, z1, x2, y2, z2, rinvsq)
           sum  = sum + rinvsq/(nfd*nseg) ! numerically more stable
        ENDDO
       enddo
c      rrmd = 1.0/sqrt(sum/float(nfd*nseg))
       Rrms = sqrt(1/sum)

      return
      end

c---------------------------------------------------
      subroutine dbl_rsq(xo,yo,zo,xa,ya,za,xb,yb,zb,rinvsq)
c-----------------------------------------------------------------------
c
c This code integrate 1.0/[(x-xo)**2+(y-yo)**2+(z-zo)**2]
c between points a and b
c
      implicit none
      real*8 xo, yo, zo, xa, ya, za, xb, yb, zb
      real*8 rinvsq
      real*8 A, B, C, D, tmp1, tmp2
c
c Ananltyical integration
c
        A = (xa-xo)**2+(ya-yo)**2+(za-zo)**2
        B = (xa-xo)*(xb-xa)+(ya-yo)*(yb-ya)+(za-zo)*(zb-za)
        C = (xb-xa)**2+(yb-ya)**2+(zb-za)**2
        D = A * C - B * B
        if (D.gt.0) then
c         write (*,*) ' Type 1'
          tmp1 = sqrt(D)
          tmp2 = (atan((C+B)/tmp1) - atan(B/tmp1))/tmp1
        else
          write (*,*) ' Type 2'   !! should have never reached this line
          write (*,'(9f10.1)') xo,yo,zo,xa,ya,za,xb,yb,zb
          stop
          tmp1 = sqrt(-D)
          tmp2 = dlog((C+B-tmp1)/(C+B+tmp1))/2.0/tmp1
        endif
c       write (*,*) 1.0/sqrt(tmp2)
        rinvsq = tmp2
        return
        end

        subroutine cross(x1, y1, z1, x2, y2, z2, xn, yn, zn)
        real x1, y1, z1, x2, y2, z2, xn, yn, zn
        real area

         xn = y1 * z2 - z1 * y2
         yn = x1 * z2 - z1 * x2
         zn = x1 * y2 - y1 * x2

         area = sqrt(xn*xn+yn*yn+zn*zn)
         xn = xn / area
         yn = yn / area
         zn = zn / area

         return
         end

c----------------------------------------------------------------------
      subroutine PULLPOLY(inside,xf,yf,xp,yp,nxp)

c This version was modified to use separate arrays for x and y coordinates

      implicit none
c      integer mxsg
c      parameter (mxsg=40)
      integer nxp
c      real xp(mxsg), yp(mxsg), xf, yf
      real xp(nxp), yp(nxp), xf, yf
      integer inside

      integer i, numwind, isicr

c   begin search
        inside=0
         do 200 i=1,nxp-1
          isicr=numwind(xp(i)-xf, yp(i)-yf, xp(i+1)-xf, yp(i+1)-yf)
          if(isicr.eq.4) go to 600
          inside=inside+isicr
200      CONTINUE

        isicr=numwind(xp(nxp)-xf, yp(nxp)-yf, xp(1)-xf, yp(1)-yf)
        if (isicr.eq.4) go to 600
        inside = (inside+isicr)/2.
        return

600     inside = 2
        return
        END

ccc---------------------------------------------------------------------
      function NUMWIND(x1,y1,x2,y2)
c
      if(y1*y2.gt.0.0) then
        numwind=0
        return
      ENDif
      if(x1*y2.ne.x2*y1 .or. x1*x2.gt.0.0) then
        if(y1*y2.lt.0.0) then
          if(y1.gt.0.0) then
            if(y1*x2.ge.x1*y2) then
              numwind=0
             else
              numwind=-2
            ENDif
           else
            if(x1*y2.ge.y1*x2) then
              numwind=0
             else
              numwind=2
            ENDif
          ENDif
         else
          if(y2.eq.0.0) then
            if(y1.eq.0.0 .or. x2.gt.0.0) then
              numwind=0
             else
              if(y1.gt.0.0) then
                numwind=-1
               else
                numwind=1
              ENDif
            ENDif
           else
            if(x1.gt.0.0) then
              numwind=0
             else
              if(y2.gt.0.0) then
                numwind=1
               else
                numwind=-1
              ENDif
            ENDif
          ENDif
        ENDif
       else
        numwind=4
      ENDif
      return
      END

ccc---------------------------------------------------------------------
        subroutine BardieR(Sta_x, Sta_y, Hyp_x, HYp_y, Hyp_z,
     1      iseg, P, L, W, Strike, Dip, Rake, ksiP, ksiQ)
ccc---------------------------------------------------------------------

        implicit none
        integer mxsg
        parameter(mxsg=40)

        integer iSeg
        real Strike, Dip, Rake, L, W, P(3,4,mxsg), Hyp_x, Hyp_y, Hyp_z,
     1       Sta_x, Sta_y

        real ksiP, ksiQ

        integer i, j, ns, nd, N
        real sub_x, sub_y, sub_z, tmp
        real sx, sy, sz, px, py, pz, qx, qy, qz

        real ux(mxsg), uy(mxsg), uz(mxsg), vx(mxsg), vy(mxsg), vz(mxsg),
     1       wx(mxsg), wy(mxsg), wz(mxsg), rx(mxsg), ry(mxsg), rz(mxsg),
     1       nx(mxsg), ny(mxsg)
        common /unitVectors/ux,uy,uz,vx,vy,vz,wx,wy,wz,rx,ry,rz,nx,ny

        ns = ceiling(L)
        nd = ceiling(W)
        N = ns * nd

c N2M  .... .... s (unit slip vector = rake vector)
c    Sign?
          sx = sin(abs(Rake))*cos(Dip)*cos(Strike) -
     1         cos(abs(Rake))*sin(Strike)
          sy =-sin(abs(Rake))*cos(Dip)*cos(Strike) -
     1         cos(abs(Rake))*cos(Strike)
          sz =-sin(abs(Rake))*sin(Dip)

        do i=1,ns
        do j=1,nd

c N2M  .... .... subfault location
          sub_x = P(1,1,iSeg)+ux(iSeg)*(i-0.5)+vx(iSeg)*(j-0.5)
          sub_y = P(2,1,iSeg)+uy(iSeg)*(i-0.5)+vy(iSeg)*(j-0.5)
          sub_z = P(3,1,iSeg)+uz(iSeg)*(i-0.5)+vz(iSeg)*(j-0.5)

c N2M  .... .... p  (unit vector: hypo to subfault)
          px = Hyp_x - sub_x
          py = Hyp_y - sub_y
          pz = Hyp_z - sub_z
          tmp = sqrt(px**2+py**2+pz**2)
          px = px/tmp
          py = py/tmp
          pz = pz/tmp

c N2M  .... .... q  (unit vector: subfault to site)
          qx = sub_x - Sta_x
          qy = sub_y - Sta_y
          qz = sub_z
          tmp = sqrt(qx**2+qy**2+qz**2)
          qx = qx/tmp
          qy = qy/tmp
          qz = qz/tmp

c N2M  .... .... ksiP
          ksiP = ksiP + px * qx + py * qy + pz * qz

c N2M  .... .... ksiQ
          ksiQ = ksiQ + sx * qx + sy * qy + sz * qz

        enddo
        enddo

        ksiP = ksiP/N
        ksiQ = ksiQ/N

        return
        end


ccc---------------------------------------------------------------------
        subroutine writeFileHeaders(inOneFile)
ccc---------------------------------------------------------------------

        logical inOneFile, WriteOnce

        save WriteOnce

c EQID.out
        write (2,11) 'EQID', '  SSN', 'StaName',
     1        'StaX', 'StaY', 'U', 'T',
     1        'Repi', 'Rhyp', 'Rrup', ' Rjb', 'Rrms',
     1        'Rx', 'Ry', 'Ry0',
     1        'AveStrike', 'LocStrike',
     1        'ChPrime', 'E', 'aveR', 'DPP',
     1        "PcLat", "PcLon", 'PcZ'

11      format((1x,a4), (5x,a5), (48x,a7),
     1        2(11x,a4), 2(14x,a1),
     1        5(11x,a4),
     1        3(12x,a3),
     1        2(6x,a9),
     1        4(8x,a7),
     1        3(10x,a5))

c p4cf.out
        if (inOneFile .and. .not.WriteOnce)
     1    write (13,11) 'EQID', '  SSN', 'StaName',
     1        'StaX', 'StaY', 'U', 'T',
     1        'Repi', 'Rhyp', 'Rrup', ' Rjb', 'Rrms',
     1        'Rx', 'Ry', 'Ry0',
     1        'AveStrike', 'LocStrike',
     1        'ChPrime', 'E', 'aveR', 'DPP',
     1        "PcLat", "PcLon", 'PcZ'

        WriteOnce = .TRUE.

        return
        end

