\name{CNPDIAperturb}
\alias{CNPDIAperturb}
\alias{depositLiquid}
\alias{depositSolid}
\alias{mixLiquid} 
\alias{mixSolid}
\alias{erodeLiquid}
\alias{erodeSolid}
\title{
  Dynamic solution of C, N, P and O2 dynamics in the sediment, including perturbation events.
}
\description{
  \code{CNPDIAperturb} dynamically runs the CNPDIA model with event-like perturbations.
}
\usage{
  CNPDIAperturb (parms = list(), times = 0:365, spinup = NULL, yini = NULL, 
               gridtype = 1, Grid = NULL, porosity = NULL, bioturbation = NULL, 
               irrigation = NULL, surface = NULL, diffusionfactor = NULL,   
               dynamicbottomwater = FALSE, perturbType = "mix", 
               perturbTimes =  seq(from = 0, to = max(times), by = 365), 
               perturbDepth = 5, concfac = 1, 
               CfluxForc   = NULL, FePfluxForc = NULL, CaPfluxForc = NULL, O2bwForc   = NULL, NO3bwForc   = NULL, 
               NH3bwForc   = NULL, ODUbwForc   = NULL, PO4bwForc   = NULL, DICbwForc  = NULL, wForc       = NULL, 
               biotForc    = NULL, irrForc     = NULL, rFastForc   = NULL, rSlowForc  = NULL, pFastForc   = NULL, 
               MPBprodForc = NULL, gasfluxForc = NULL, HwaterForc  = NULL, ratefactor = NULL, verbose = FALSE, ...)
  
  mixSolid      (C, depth = 5, grid = setup.grid.1D(N = 100, L  = 30), por = 0.5) 
  mixLiquid     (C, depth = 5, grid = setup.grid.1D(N = 100, L  = 30), por = 0.5, BW) 
  erodeSolid    (C, depth = 5, grid = setup.grid.1D(N = 100, L  = 30), por = 0.5) 
  erodeLiquid   (C, depth = 5, grid = setup.grid.1D(N = 100, L  = 30), por = 0.5)
  depositSolid  (C, depth = 5, grid = setup.grid.1D(N = 100, L  = 30), por = 0.5, concfac = 1)  
  depositLiquid (C, depth = 5, grid = setup.grid.1D(N = 100, L  = 30), por = 0.5, BW) 
}
\arguments{

 \item{parms}{ A list with parameter names and values. Possible parameters are:
 
\code{Cflux, pFast, FePflux, CaPflux, rFast, rSlow, NCrFdet, NCrSdet, PCrFdet, PCrSdet}, 
\code{BCupLiq, BCdownLiq, O2bw, NO3bw, NH3bw, ODUbw, PO4bw, DICbw, O2dw, NO3dw, NH3dw}, 
\code{ODUdw, PO4dw, DICdw, w, biot, mixdepth, mixatt, irr, gasflux, MPBprod, kMPB, ksDIN} 
\code{ksPO4 ksDIC NH3Ads, rnit, ksO2nitri, rODUox, ksO2oduox, ksO2oxic, ksNO3denit}, 
\code{kinO2denit, kinNO3anox kinO2anox, pdepo, rdepo, temperature, salinity, TOC0}, 
\code{rFePdesorp, rFePadsorp, rCaPprod, rCaPdiss, CPrCaP, por0, pordeep, porcoeff}, 
\code{dilution, Hwater, Cfall, FePfall, CaPfall}
    
    See details of \link{CNPDIAsolve}.
  }

 \item{gridtype}{ Type of grid: \code{1} for cartesian, \code{2} for cylindrical, \code{3} for spherical.
  }

 \item{times}{ Output times for the dynamic run
  }

 \item{spinup}{ Spinput times for the dynamic run; not used for output; the outputted simulation starts from the final values of the spinup run.
  }

 \item{CfluxForc, FePfluxForc, CaPfluxForc}{ \code{NULL} or a list, detailing the forcing function for the deposition fluxes of organic carbon, FeP and CaP.     
    If \code{NULL} then the corresponding parameter value (Cflux, CaPflux, FePflux) is used. 
    If a \code{list}, it should contain either a data time series (\code{list(data = )}) or parameters determining the periodicity of the seasonal signal (defined as \code{list(data = NULL, amp = 0, period = 365, phase = 0, pow = 1, min = 0)}. see details. 
  }

 \item{O2bwForc, NO3bwForc, NH3bwForc, ODUbwForc, PO4bwForc, DICbwForc}{ \code{NULL} or a list, detailing the forcing function for the boundary concentrations. 
    If \code{NULL} then the corresponding parameter value (O2bw, NO3bw, NH3bw, ODUbw, PO4bw, DICbw) is used. 
    If a \code{list}, it should contain either a data time series (\code{list(data = )}) or parameters determining the periodicity of the seasonal signal (defined as \code{list(data = NULL, amp = 0, period = 365, phase = 0, pow = 1, min = 0)}. see details. 
  }  

 \item{wForc, biotForc, irrForc}{ \code{NULL} or a list, detailing the forcing function forthe advection, bioturbation and irrigation rates (units of cm/d, cm2/d and /d).     
    If \code{NULL} then the corresponding parameter value (w, biot, irr) is used. 
    If a \code{list}, it should contain either a data time series (\code{list(data = )}) or parameters determining the periodicity of the seasonal signal (defined as \code{list(data = NULL, amp = 0, period = 365, phase = 0, pow = 1, min = 0)}. see details. 
  }  

 \item{rFastForc, rSlowForc, pFastForc, MPBprodForc}{ \code{NULL} or a list, detailing the forcing function forthe decay rate of fast and slow detritus, the fraction of fast detritus in organic flux and the maximal microphytobenthos production rate (units of /d, /d, - and mmolO2/m3/d). 
    If \code{NULL} then the corresponding parameter value (rFast, rSlow, pFast, MPBprod) is used. 
    If a \code{list}, it should contain either a data time series (\code{list(data = )}) or parameters determining the periodicity of the seasonal signal (defined as \code{list(data = NULL, amp = 0, period = 365, phase = 0, pow = 1, min = 0)}. see details. 
  }  

 \item{gasfluxForc}{ \code{NULL} or a list, detailing the forcing function for the intensity of exchange with the air (units of cm/d). 
    If \code{NULL} then the corresponding parameter value (gasflux) is used. 
    If a \code{list}, it should contain either a data time series (\code{list(data = )}) or parameters determining the periodicity of the seasonal signal (defined as \code{list(data = NULL, amp = 0, period = 365, phase = 0, pow = 1, min = 0)}. see details. 
    A gasflux>0 represents exchange rate for O2 and DIC, not for other dissolved substances. There can still be deposition of C, FeP and CaP.
  }  

 \item{HwaterForc}{ \code{NULL} or a list, detailing the forcing function for the height of the water above the sediment - if \code{dynamicbottomwater} = TRUE. 
    If \code{NULL} then the corresponding parameter value (Hwater) is used. 
    If a \code{list}, it should contain either a data time series (\code{list(data = )}) or parameters determining the periodicity of the seasonal signal (defined as \code{list(data = NULL, amp = 0, period = 365, phase = 0, pow = 1, min = 0)}. see details. 
  }  

 \item{dynamicbottomwater}{ If TRUE, then the concentrations in the water overlying the sediment will also be dynamically described, 
     and with water height equal to \code{Hwater}. Note that this will slow down the simulation.   
  }
 \item{ratefactor}{ \code{NULL} or a list, detailing the forcing function for the biogeochemical rate multiplication factor. If not specified (or \code{NULL}), then it is assumed to be 1 and constant. 
    If a \code{list}, it should contain either a data time series (\code{list(data = )}) or parameters determining the periodicity of the seasonal signal (defined as \code{list(data = NULL, amp = 0, period = 365, phase = 0, pow = 1, min = 0)}. see details. 
  }  


 \item{Grid}{ If specified: either an object, as returned by \code{setup.grid.1D} from the package \code{ReacTran}, 
   a vector of length 101 with the transport distances (from mid to mid of layers, upper interface = diffusive boundary layer), 
   or one number with the constant layer thickness. 
   If \code{NULL}, it is defined as \code{setup.grid.1D(x.up = 0, dx.1 = 0.01, N = 100, L = 100)}, i.e. the total length is 100 cm, 
   the first layer is 0.01 cm thick and layers are increasing with depth for 100 layers.  
  } 

 \item{porosity}{ If specified, either an object with porosities ([-]) as returned by \code{setup.prop.1D} from the package \code{ReacTran}, 
   a vector of length 101 with the porosities defined at the layer interfaces, or one number with the constant porosity. 
   If \code{NULL}, it is defined by the parameters \code{por0}, \code{pordeep} and \code{porcoeff} as: 
   \code{(pordeep + (por0 - pordeep) * exp(-pmax(0, x.int)/porcoeff))}, where \code{x.int} is the distance, from the surface of the layer interface. 
   Note that the porosity values should be consistent withe the \code{Grid} - and should be inbetween 0 and 1.
  }

 \item{bioturbation}{ If specified, either an object with bioturbation rates (units [cm2/d]) as returned by \code{setup.prop.1D} from the package \code{ReacTran}, 
   a vector of length 101 with the bioturbation defined at the layer interfaces, or one number with the constant bioturbation. 
   If \code{NULL}, it is defined by the parameters \code{biot}, \code{mixdepth} and \code{mixatt} as: 
   \code{biot * exp(-pmax(0, (x.int-mixL)/mixatt)}, where \code{x.int} is the distance, from the surface of the layer interface. 
   Note that the bioturbation values should be consistent withe the \code{Grid}.
  }

 \item{irrigation}{ If specified, either an object with irrigation rates (units [/d]) as returned by \code{setup.prop.1D} from the package \code{ReacTran}, 
   a vector of length 100 with the irrigation rates defined at the layer centres, or one number with the constant rates. 
   If \code{NULL}, it is defined by the parameters \code{irr}, \code{mixdepth} and \code{mixatt} as: 
   \code{irr * exp(-pmax(0, (x-mixdepth)/mixxatt))}, where \code{x} is the distance, from the surface, of the layer centres. 
   Note that the irrigation values should be consistent withe the \code{Grid}.
  }

 \item{surface}{ If specified, either an object with surface areas (units [cm2]) as returned by \code{setup.prop.1D} from the package \code{ReacTran}, 
   a vector of length 101 with the surface areas defined at the layer interfaces, or one number with the constant surface area. 
   If \code{NULL}, it is defined by the parameter \code{gridtype}, and the \code{Grid} as: 
   \code{surface = rep(1, 101)} for \code{gridtype == 1}, \code{surface = rev(2*pi*Grid$x.int)} for \code{gridtype == 2}  and \code{surface = rev(pi*(Grid$x.int)^2)} for \code{gridtype == 3}.

   Note that the surface values should be consistent withe the \code{Grid}.
  }

 \item{diffusionfactor}{ The multiplication factor necessary to go from molecular diffusion to effective sediment diffusion, i.e. that takes into account tortuosity.
   If specified, either an object with these factors ([-]) as returned by \code{setup.prop.1D} from the package \code{ReacTran}, 
   a vector of length 101 with these factors defined at the layer interfaces, or one number with the constant factor. 
   If \code{NULL}, it is set equal to the porosity. Note that the factors should be consistent withe the \code{Grid}.
  }

 \item{yini}{ The condition at which to inialise the dynamic simulation, i.e. a vector or matrix, with the values of 
   \code{FDET, SDET, O2, NO3, NH3, ODU, PO4, FeP, CaP, DIC}, each in 100 layers, and in that order. 
   If \code{NULL}, the default, then the steady-state solution based on the mean forcing is used (and obtained with \link{CNPDIAsolve}) .
  } 
  
 \item{perturbType}{ how to perturb, one of "mix", "deposit", "erode".  Several options can be chosen, in which case the perturbations will be done in the order defined.
  } 
  
 \item{perturbTimes}{ times at which the perturbations should take place. Either one value, or a vector of the same length as \code{perturbType}.
  }
  
 \item{perturbDepth, depth}{ the depth of the perturbation, in cm.
  }
  
 \item{concfac}{ only when perturbType = "deposit": the factor at which the available concentration should be increased or decreased.
  } 
  
 \item{C}{ The concentration vector to be perturbed; a vector that is compatible with \code{grid} (i.e. has the same length as \code{grid$x.mid}).
  } 
  
 \item{BW}{ The concentration in the overlying water, one value.
  } 
  
 \item{grid}{ a list as generated with \link{setup.grid.1D}[ReacTran] that contains the grid definition
  }
  
 \item{por}{ the sediment porosity; either as generated with \link{setup.prop.1D}[ReacTran], 
    or a vector of length = \code{length(C)}, or one value. 
  }
 \item{verbose}{ If TRUE, will write progession to the screen .
  } 
  
 \item{...}{ Any argument passed to the dynamic solver (\link{ode.1D}[deSolve])
  }
}

\value{
 \code{CNPDIAperturb} returns a matrix of type \code{deSolve}, as generated by the 
 solver \link{ode.1D} from R-package \code{deSolve}.
 It contains the columns:
 \itemize{
   \item \code{time}, the output time, days
   \item \code{FDET,SDET,O2,NO3,NH3,ODU,PO4,FeP,CaP,DIC}, the state variables.
   \item \code{O2flux, O2deepflux, NO3flux, NO3deepflux, NH3flux, NH3deepflux, 
      ODUflux, ODUdeepflux, PO4flux, PO4deepflux, DICflux, DICdeepflux, 
      FDETflux, FDETdeepflux, SDETflux, SDETdeepflux, FePdeepflux, CaPdeepflux, 
      OrgCflux, OrgNflux, OrgPflux}, the sediment-water and burial fluxes, in nmol/cm2/d.
   \item \code{DINDIPflux, DINDIPmean, DINDIPdeep}, the dissolved nitrogen to phosphorus ratio of flux, sediment concentrations and deep (burial) concentration.
   \item \code{TotMin, TotOxic, TotDenit, TotAnoxic}, total mineralisation, total oxic mineralisation, denitrification and anoxic mineralisation, in nmol/cm2/d.
   \item \code{PartOxic, PartDenit, PartAnoxic}, the fraction of mineralisation due to oxic, denitrification and anoxic mineralisation.
   \item \code{TotNitri, TotODUoxid, TotFePprod, TotCaPprod, TotFePdesorp, TotCaPdiss, TotNprod, TotPprod, TotNH3ads}, integrated rates, nmol/cm2/d.
   \item \code{PartPremoved, PartNremoved},  the total P and N removed, relative to its production.
   \item \code{TOC}, the Total organic carbon concentration profile, \%.
   \item \code{Cprod,Nprod,Pprod,Oxicmin,Denitrific,anoxicmin,nitri,oduox,odudepo,FePadsorp,CaPprod}, rate profiles, nm/cm3 liquid/d.
   \item \code{FePdesorp,CaPdiss}, rate profiles, nm/cm3 solid/d.
 }
 Functions \code{depositLiquid, depositSolid, mixLiquid, mixSolid, erodeLiquid, erodeSolid} return
 a list, with the values:
 \itemize{
   \item \code{C}, the altered concentration profile
   \item \code{Flux}, the instantaneous gain in mass, estimated as (integral(Cnew) - integral(C)).
 } 

}

\author{
  Karline Soetaert
}
\details{
  Several paramaters can also be described as forcing functions. They are:
  \code{Cflux, FePflux, CaPflux, O2bw, NO3bw, NH3bw, ODUbw, PO4bw, DICbw, w,   
             biot, irr, rFast, rSlow, pFast, MPBprod, gasflux, Hwater}.
             
  The forcing functions are prescribed as a \code{list} that either contains a data series or specifies a periodic signal.
  The list is defined as: \code{list(data = NULL, amp = 0, period = 365, phase = 0, pow = 1, min = 0)}
  
  \itemize{
  \item Forcing functions a data series are set with item \code{data} contains time series for the parameter - a matrix with times (first column) and values, second column. The values should be in the same units as the parameters.
  
   The time series should embrace both arguments \code{times} and \code{spinup}.  
  
  \item if a periodic signal, the list should contain \code{amp, period, phase, pow} and \code{min}: parameters determining the periodicity of the seasonal signal in the same units as the parameters. 
   From this the forcing function time series is estimated, e.g. for Cflux as: \code{max(min, Cflux*(1 + (amp*sin((times-phase)/period*2*pi))^pow)}, where \code{Cflux} is the parameter value. Used only if \code{data} is \code{NULL}.   If \code{amp} = 0, or \code{pow} = 0, then the forcing will be kept constant and equal to the parameter value. 
  }

}

\note{
  The model application starts by estimating the steady-state condition
  of the model. This steady-state condition is then used as a starting
  condition for a dynamic simulation, with perturbations as provided in \code{pertTimes}.

  Mixing will homogenise the perturbed depth of the sediment (\code{perturbType = "mix"}).
  Erosion will remove the perturbed depth of the sediment (\code{perturbType = "erode"}. 
  Deposition will add a layer of sediment (\code{perturbType = "deposit"}.
 
  All these events can be combined; the sequence of events is as provided, i.e.
  \code{perturbType = c("mix", "erode")} will not give the same results as \code{perturbType = c("erode", "mix")}.
}


\examples{

# ========================================
# One perturbation
# ========================================

  out <- CNPDIAperturb()
  par(mar = c(3,3,3,3))
  
  image(out, ylim = c(20, 0), legend = TRUE, grid = CNPDIAdepth(out), 
    mfrow = c(4, 3))
    
  out2 <- CNPDIAperturb(perturbTime = c(0, 100, 200, 300),      
    perturbType = "mix", perturbDepth = 10)

  image(out2, ylim = c(20, 0), legend = TRUE, grid = CNPDIAdepth(out), 
    mfrow = c(4, 3))

  CNPDIAbudgetO2(out)  

  out3 <- CNPDIAperturb(perturbTime = c(0, 100, 200, 300), 
    perturbType = "erode", perturbDepth = 5)
  image(out3, ylim = c(20, 0), legend = TRUE, grid = CNPDIAdepth(out), 
    mfrow = c(4, 3))

# ========================================
# Several subsequent perturbations
# ========================================

  out4 <- CNPDIAperturb(perturbTime = c(0, 100, 200, 300), 
     perturbType = c("mix", "erode"), perturbDepth = c(10, 5))
  image(out4, ylim = c(20, 0), legend = TRUE, grid = CNPDIAdepth(out), 
    mfrow = c(4, 3))

# ========================================
# Perturbation profiles
# ========================================

# create reference solution
  pm <- par(mfrow = c(1, 2))
  DIA <- CNPDIAsolve()

# perturb solid fraction
  SDET <- DIA$y[,"SDET"]
  Grid <- DIA$Grid
  por <- DIA$porosity
  # perturb it
  mix <-  mixSolid(SDET, grid = Grid, por = por)
  deposit <- depositSolid(SDET, grid = Grid, por = por)
  erode <- erodeSolid(SDET, grid = Grid)
  matplot(x = cbind(SDET, mix$C, erode$C, deposit$C), main = "Solid",
     y = Grid$x.mid, type = "l", lwd = 3, ylim = c(20, 0))
  legend("bottomright", col = 1:4, lty = 1:4,
     legend = c("default", "mix", "erode", "deposit"))
  cat("instantaneous fluxes, nmol/cm2\n", "mixing: ", mix$Flux, 
    "erosion: ", erode$Flux, "deposition: ", deposit$Flux, "\n")

  NO3 <- DIA$y[,"NO3"]
  Grid <- DIA$Grid
  por <- DIA$porosity
  # perturb it
  mix <-  mixLiquid(NO3, grid = Grid, BW = 10)
  deposit <- depositLiquid(NO3, grid = Grid, BW = 10)
  erode <- erodeLiquid(NO3, grid = Grid)
  matplot(x = cbind(NO3, mix$C, erode$C, deposit$C), main = "Liquid", 
     y = Grid$x.mid, type = "l", lwd = 3, ylim = c(20, 0))
  legend("bottomright", col = 1:4, lty = 1:4,
     legend = c("default", "mix", "erode", "deposit"))
  cat("instantaneous fluxes, nmol/cm2\n", "mixing: ", mix$Flux, 
    "erosion: ", erode$Flux, "deposition: ", deposit$Flux)

  par(mfrow = pm)
  
}
\references{
  Soetaert K, PMJ Herman and JJ Middelburg, 1996a.
  A model of early diagenetic processes from the shelf to abyssal depths.
  Geochimica Cosmochimica Acta, 60(6):1019-1040.

  Soetaert K, PMJ Herman and JJ Middelburg, 1996b.
  Dynamic response of deep-sea sediments to seasonal variation: a model.
  Limnol. Oceanogr. 41(8): 1651-1668.
}

\keyword{ utilities }
