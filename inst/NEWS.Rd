\name{NEWS}
\title{ausplotsR News}
\encoding{UTF-8}

\section{Changes in version 2.0.0}{
  \subsection{INSTALLATION}{
    \itemize{
      
      \item Package is now tested for compatibility with \R{} v4. 
          
        }
      } %installation
      
      \subsection{NEW FEATURES}{
        \itemize{
          
          \item Filter site downloads in \code{get_ausplots} based on unique site/visit codes as well as partial code match/wildcard feature. Previously, only the \code{site_location_name} code could be used to filter by text match.
          
          \item Due to the increasing size of the vegetation database, \code{get_ausplots} now only returns the site table by default. Users can still select to extract vegetation point-intercept or vouchers as well as all the other modules available in ausplotsR.
          
          \item Survey visit dates are now presented as R-readable dates and the sites table is appended with revisit numbers for each plot (i.e., 1, 2, 3 and so on) to simplify identification of revisits and their chronological order.
          
          \item
            
            
    }
  } % new features

  \subsection{BUG FIXES}{
    \itemize{

      \item Corrected plot order in ausplots_visual. Some plots and sets of visits had missing calculations that upset order and formatting of plots. This have been corrected.

      \item 

     

    }
  } % bug fixes

  \subsection{DEPRECATED}{

    \itemize{

      \item 

      \item 

    }
  } % deprecated
} % 2.6.0

