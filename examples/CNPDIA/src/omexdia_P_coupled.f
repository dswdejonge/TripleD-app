!==========================================================================
! THE OMEXDIA model with P, implemented in FORTRAN
! version with dynamic water concentrations
! Karline Soetaert, nioz-yerseke
!==========================================================================

!==========================================================================
! initialisation = same as omexdiamod - see file omexdia_P.f
!==========================================================================

!==========================================================================
!==========================================================================
! subroutine calculating the rate of change of
! the omexdia model - here the quantities in the common blocks are named
!==========================================================================
!==========================================================================

      SUBROUTINE omexdiamodbw (neq, t, Conc, dConc, yout, ip)

      USE commonCNPDIA
      IMPLICIT NONE

!......................... declaration section.............................
      INTEGER           :: neq, ip(*), i

      DOUBLE PRECISION  :: t,Conc(11*Np1),dConc(11*Np1),yout(*)
      
      DOUBLE PRECISION  :: FdetBW,SdetBW,FePBW,CaPBW,O2BW,NO3bw,NH3bw,          &
     &                     ODUbw,PO4bw,DICbw,PadsBW
      DOUBLE PRECISION  :: dFdetBW,dSdetBW,dO2BW,dNO3BW,dNH3BW,dODUBW,          &
     &                     dPO4BW,dFePBW,dCaPBW,dDICBW,dPadsBW

      DOUBLE PRECISION :: pF, FDETdepo, SDETdepo, FePdepo, CaPdepo
      CHARACTER(len=120) msg
!............................ statements ..................................

!     check memory allocated to output variables
      IF (ip(1) < nout)  CALL rexit("nout not large enough") 

! from Conc to fdet, sdet, o2,...
      DO I = 1, N
        Fdet(I) = Conc(      1+I)
        Sdet(I) = Conc(  Np1+1+I)
        O2(I)   = Conc(2*Np1+1+I)
        NO3(I)  = Conc(3*Np1+1+I)
        NH3(I)  = Conc(4*Np1+1+I)
        ODU(I)  = Conc(5*Np1+1+I)
        DIC(I)  = Conc(6*Np1+1+I)
        PO4(I)  = Conc(7*Np1+1+I)
        FeP(I)  = Conc(8*Np1+1+I)
        CaP(I)  = Conc(9*Np1+1+I)
        Pads(I)  = Conc(10*Np1+1+I)
      ENDDO
      FdetBW = Conc(      1)
      SdetBW = Conc(  Np1+1)
      O2BW   = Conc(2*Np1+1)
      NO3BW  = Conc(3*Np1+1)
      NH3BW  = Conc(4*Np1+1)
      ODUBW  = Conc(5*Np1+1)
      DICBW  = Conc(6*Np1+1)
      PO4BW  = Conc(7*Np1+1)
      FePBW  = Conc(8*Np1+1)
      CaPBW  = Conc(9*Np1+1)
      PadsBW  = Conc(10*Np1+1)
!      write(MSG,'(5(F8.3, 1X))') O2BW, NO3BW, NH3bw, FDETbw, SDETbw
!      CALL rexit(MSG) 
      
! --------------------------------------------------------------------------
      FDETFLux = Carbonflux*pFast
      SDETflux = Carbonflux - FDETflux

      FDETdepo  = FdetBW*Cfall
      SDETdepo  = SdetBW*Cfall
      FePdepo   = FePBW*FePfall
      CaPdepo   = CaPBW*CaPfall
      
      CALL CNPDIAtransolid(FDETdepo,SDETdepo,FePdepo,CaPdepo)

      if (Hwater > 0) THEN
        dFDETBW = (FDETflux - FDETdepo)/ Hwater
        dSDETBW = (SDETflux - SDETdepo)/ Hwater
        dFePBW  = (FePflux  - FePdepo )/ Hwater
        dCaPBW  = (CaPflux  - CaPdepo )/ Hwater
      ELSE
        dFDETBW = 0.D0
        dSDETBW = 0.D0
        dFePBW  = 0.D0
        dCaPBW  = 0.D0
      ENDIF
      dPadsBW = 0.D0
      CALL CNPDIAtranliquid(O2BW, NO3bw, NH3bw, ODUbw, PO4bw, DICbw)
      
      if (Hwater > 0) THEN
        dO2BW   = (bwO2  -  O2BW) * relax - O2flux  /Hwater
        dNO3BW  = (bwNO3 - NO3BW) * relax - NO3flux /Hwater
        dNH3BW  = (bwNH3 - NH3BW) * relax - NH3flux /Hwater
        dODUBW  = (bwODU - ODUBW) * relax - ODUflux /Hwater
        dPO4BW  = (bwPO4 - PO4BW) * relax - PO4flux /Hwater
        dDICBW  = (bwDIC - DICBW) * relax - DICflux /Hwater
      ELSE
        dO2BW   = (bwO2  -  O2BW) * relax
        dNO3BW  = (bwNO3 - NO3BW) * relax
        dNH3BW  = (bwNH3 - NH3BW) * relax
        dODUBW  = (bwODU - ODUBW) * relax
        dPO4BW  = (bwPO4 - PO4BW) * relax
        dDICBW  = (bwDIC - DICBW) * relax
      ENDIF
      CALL CNPDIAbiochem 

      CALL CNPDIAout (yout)
!      write(MSG,'(3(A4, 2(F8.3, 1X)))') "O2 ", O2BW, O2flux, "NO3 ",              &
!     &  NO3BW, NO3flux, "NH3 ", NH3BW, NH3flux
!      CALL rexit(MSG) 
      
! --------------------------------------------------------------------------

! from dfdet, dsdet, do2,... to dconc
      DO I = 1, N
         dConc(      1+I)  =  dFdet(I)
         dConc(  Np1+1+I)  =  dSdet(I) 
         dConc(2*Np1+1+I)  =  dO2(I)  
         dConc(3*Np1+1+I)  =  dNO3(I) 
         dConc(4*Np1+1+I)  =  dNH3(I) 
         dConc(5*Np1+1+I)  =  dODU(I)
         dConc(6*Np1+1+I)  =  dDIC(I)
         dConc(7*Np1+1+I)  =  dPO4(I)
         dConc(8*Np1+1+I)  =  dFeP(I)
         dConc(9*Np1+1+I)  =  dCaP(I)
         dConc(10*Np1+1+I) =  dPads(I)
      ENDDO
      dConc(      1)  =  dFdetBW
      dConc(  Np1+1)  =  dSdetBW 
      dConc(2*Np1+1)  =  dO2BW  
      dConc(3*Np1+1)  =  dNO3BW 
      dConc(4*Np1+1)  =  dNH3BW 
      dConc(5*Np1+1)  =  dODUBW
      dConc(6*Np1+1)  =  dDICBW
      dConc(7*Np1+1)  =  dPO4BW
      dConc(8*Np1+1)  =  dFePBW
      dConc(9*Np1+1)  =  dCaPBW
      dConc(10*Np1+1) =  dPadsBW
 
      RETURN
      END SUBROUTINE
