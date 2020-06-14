TripleD_text <- function(){
  return(
    paste(
      "The NIOZ Royal Netherlands Institute for Sea Research owns a special dredge called the TripleD (Deep Digging Dredge)",
      "to sample megafauna from sedimentary habitats.",
      "The TripleD can sample relatively large areas of seabed to quantitatively study the distribution of megafauna that generally occur in low abundances.",
      "The dredge contains a blade that is pushed into the sediment and consequently towed over the seabed.",
      "The sediment that is exised from the seabed passes through a net (mesh size of 7 mm) to collect fauna.",
      "The fauna in the net (generally representing a total area of 20 m2) are brought aboard for identification and measurements."
    )
  ) 
}

package_text <- function(){
  return(
    paste(
      "All data collected by the TripleD are archived in the Data Archiving System (DAS) of NIOZ as CSV files.",
      "The CSV files from DAS can be processed by the TripleD R-package, which contains default workflows",
      "for cleaning the data and performing calculations.",
      "The TripleD R-package produces a database-like table that can be read by this Shiny App to visually interact",
      "with the data.",
      "For more information, please review the documentation from the TripleD package at",
      "github.com/dswdejonge/TripleD."
    )
  )
}
