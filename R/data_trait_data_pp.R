#' Pre-processed plant trait data used in ausplotsR
#'
#' A static dataset containing pre-processed plant trait information used
#' internally by \code{\link{species_traits}} and
#' \code{\link{community_traits}}.
#'
#' The dataset brings together trait information for Australian plant
#' species from multiple sources and standardisation steps used within
#' \code{ausplotsR}. It includes continuous traits, categorical traits,
#' and supporting metadata used to derive community-weighted means,
#' proportions of categorical states, and functional diversity metrics.
#'
#' Trait values are harmonised to accepted taxonomic names on the Australian
#' Plant Census and may include direct observations, curated external trait 
#' sources, or derived trait assignments. Photosynthetic pathway information
#' includes confirmed assignments from peer-reviewed literature or TERN
#' stable isotope analysis, as well as lineage-inferred assignments where
#' appropriate.
#'
#' @format A data frame with one row per taxon and trait information used
#' by \code{ausplotsR}. Common variables include:
#'
#' \describe{
#'
#' \item{species_name}{Scientific name of the species matching the accepted name 
#' on the Australian Plant Census.}
#'
#' \item{family_name}{Family name to which the species belong.}
#'
#' \item{leaf_length}{Leaf length extracted from AusTraits (Falster et al. (2021) and if not available there, 
#' when possible gap-filled with the methodology reported in Andrew et al. (2021). Units are centimeters (cm), but
#' the value has been log-transformed.}
#'
#' \item{leaf_width}{Leaf width extracted from AusTraits (Falster et al. (2021) and if not available there, 
#' when possible gap-filled with the methodology reported in Andrew et al. (2021). Units are centimeters (cm), but
#' the value has been log-transformed.}
#' 
#' \item{leaf_area}{Leaf area extracted from AusTraits (Falster et al. (2021) and if not available there, 
#' when possible gap-filled with the methodology reported in Andrew et al. (2021). Units are squared millimeters (mm2), but
#' the value has been log-transformed.}
#'
#' \item{leaf_mass_per_area}{Leaf mass per area extracted from AusTraits (Falster et al. (2021) and if not available there, 
#' when possible gap-filled with the methodology reported in Andrew et al. (2021). Units are grams per squared meter (g/m2), 
#' but the value has been log-transformed.}

#' \item{max_height}{Maximum plant height extracted from AusTraits (Falster et al. (2021) and if not available there, 
#' when possible gap-filled with the methodology reported in Andrew et al. (2021). Units are meters (m), but
#' the value has been log-transformed.}
#'
#' \item{seed_dry_mass}{Seed dry mass extracted from AusTraits (Falster et al. (2021) and if not available there, 
#' when possible gap-filled with the methodology reported in Andrew et al. (2021). Units are milligrams (mg), but 
#' the value has been log-transformed.}
#' 
#' \item{seed_length}{Seed length extracted from AusTraits (Falster et al. (2021) and if not available there, 
#' when possible gap-filled with the methodology reported in Andrew et al. (2021). Units are millimeters (mm), but 
#' the value has been log-transformed.}
#'
#' \item{photosynthetic_pathway}{photosynthetic pathway  assignment integrating confirmed 
#' evidence (i.e. using peer-reviewed literature or TERN stable isotope analysis) and 
#' lineage-based inference (i.e. based on genus- or family-level inference where
#' species-level testing is unavailable but lineage-level assignment is
#' widely supported; see Munroe et al. (2022)).
#' A value of \code{"U"} denotes that the pathway remains unknown.
#' A value of \code{"-CAM"} denotes that the pathway could be Crassulacean Acid
#' Metabolism (CAM) photosynthesis, but has not been tested.}

#'
#' \item{d13C}{Leaf stable carbon isotope composition (d13C). d13C is calculated as the ratio of, 13C/12C in 
#' a sample to 13C/12C in a standard, minus 1. The result is expressed in parts per thousand, commonly referred to as per mille. 
#' A more negative value for delta13C indicates the sample has a lower ratio of 13C/12C relative to the standard.}
#' }
#'
#' Variable names in the internal dataset may differ slightly from the
#' descriptive labels shown above depending on preprocessing choices.
#'
#' @section Notes on data interpretation:
#'
#' \itemize{
#' \item species name have been supplied by state herbaria and subsequently aligned 
#' to the accepted names on the Australian Plant Census by using the APCalign R package.
#'
#' \item Stable isotope analyses were performed on available plant
#' material, usually leaf tissue where possible.
#'
#' \item Photosynthetic pathway assignments may be based on direct
#' evidence, lineage inference, or both, depending on data availability.
#'
#' \item This dataset is used internally to provide the trait lookups
#' behind \code{species_traits()} and \code{community_traits()}, and is
#' not intended to represent a raw trait database export.
#' }
#'
#' @references
#' Andrew, SC, Mokany, K, Falster, DS, Wenk, E, Wright, IJ, Merow, C, 
#' Adams, V, Gallagher, RV (2021) Functional diversity of the Australian flora: 
#' strong links to species richness and climate. Journal of Vegetation Science 32, e13018. 

#' Besnard G., Muasya A. M., Russier F., Roalson E. H., Salamin N. and
#' Christin P. A. (2009). Phylogenomics of C4 photosynthesis in sedges
#' (Cyperaceae): multiple appearances and genetic convergence.
#' \emph{Molecular Biology and Evolution}, 26(8), 1909--1919.
#'
#' Bohley K., Joosa O., Hartmann H., Sage R., Liede-Schumann S. and
#' Kadereit G. (2015). Phylogeny of Sesuvioideae (Aizoaceae):
#' biogeography, leaf anatomy and the evolution of C4 photosynthesis.
#' \emph{Perspectives in Plant Ecology, Evolution and Systematics},
#' 17, 116--130.
#'
#' Bruhl J. J. and Wilson K. L. (2007). Towards a comprehensive survey
#' of C3 and C4 photosynthetic pathways in Cyperaceae.
#' \emph{Aliso}, 23(1), 99--148.
#'
#' Caddy-Retalic S. (2017). \emph{Quantifying responses of ecological
#' communities to bioclimatic gradients}. PhD thesis, University of
#' Adelaide.
#'
#' Carolin R., Jacobs S. W. L. and Vesk M. (1982). The chlorenchyma of
#' some members of the Salicornieae (Chenopodiaceae).
#' \emph{Austral Journal of Botany}, 30, 387--392.
#'
#' Clayton W. D., Vorontsova M. S. and Williamson K. T. H. (2006).
#' GrassBase - The online world grass flora.
#'
#' D'Andrea R. M., Andreo C. S. and Lara M. V. (2014). Deciphering the
#' mechanisms involved in \emph{Portulaca oleracea} (C4) response to
#' drought. \emph{Physiologia Plantarum}, 152(3), 414--430.
#'
#' Ehleringer J. R. and Monson R. K. (1993). Evolutionary and ecological
#' aspects of photosynthetic pathway variation.
#' \emph{Annual Review of Ecology and Systematics}, 24, 411--439.
#'
#' Falster, D., Gallagher, R., Wenk, E.H. et al. (2021) AusTraits, a curated plant trait 
#' database for the Australian flora. Scientific Data 8, 254.
#' 
#' Feodorova T. A., Voznesenskaya E. V., Edwards G. E. and Roalson E. H.
#' (2010). Biogeographic patterns of diversification and the origins of
#' C4 in \emph{Cleome}. \emph{Systematic Botany}, 35, 811--826.
#'
#' Gallagher R. et al. (in review). AusTraits - a curated plant trait
#' database for the Australian flora. \emph{Scientific Data}.
#'
#' Guillaume K., Huard M., Gignoux J., Mariotti A. and Abbadie L. (2001).
#' Does the timing of litter inputs determine natural abundance of 13C in
#' soil organic matter? \emph{Oecologia}, 127(2), 295--304.
#'
#' Hancock L. P., Holtum J. A. M. and Edwards E. J. (2019). The evolution
#' of CAM photosynthesis in Australian \emph{Calandrinia}.
#' \emph{Integrative and Comparative Biology}, icz089.
#'
#' Holtum J. A., Hancock L. P., Edwards E. J., Crisp M. D., Crayn D. M.,
#' Sage R. and Winter K. (2016). Australia lacks stem succulents but is
#' it depauperate in plants with CAM? \emph{Current Opinion in Plant
#' Biology}, 31, 109--117.
#'
#' Holtum J. A., Hancock L. P., Edwards E. J. and Winter K. (2017).
#' Facultative CAM photosynthesis in four species of \emph{Calandrinia}.
#' \emph{Photosynthesis Research}, 134, 17--25.
#'
#' Horn J. W. et al. (2014). Evolutionary bursts in \emph{Euphorbia} are
#' linked with photosynthetic pathway. \emph{Evolution}, 68(12),
#' 3485--3504.
#'
#' Kadereit G., Borsch T., Weising K. and Freitag H. (2003). Phylogeny
#' of Amaranthaceae and Chenopodiaceae and the evolution of C4
#' photosynthesis. \emph{International Journal of Plant Sciences},
#' 164(6), 959--986.
#'
#' Kattge J. et al. (2020). TRY plant trait database - enhanced coverage
#' and open access. \emph{Global Change Biology}, 26, 119--188.
#'
#' Kock K. E. and Kennedy R. A. (1982). Crassulacean acid metabolism in
#' \emph{Portulaca oleracea} under natural environmental conditions.
#' \emph{Plant Physiology}, 69, 757--761.
#'
#' Lauterbach M. et al. Evolution of leaf anatomy in arid environments -
#' a case study in southern African \emph{Tetraena} and \emph{Roepera}.
#' \emph{Molecular Phylogenetics and Evolution}, 97, 129--144.
#'
#' Llano C. (2009). Photosynthetic pathways, spatial distribution,
#' isotopic ecology, and implications for pre-Hispanic human diets in
#' central-western Argentina. \emph{International Journal of
#' Osteoarchaeology}, 19(2), 130--143.
#'
#' Markovska Y. K. and Dimitrov D. S. (2001). The effect of leaf age on
#' gas exchange and malate accumulation in C3-CAM plant
#' \emph{Marrubium frivaldszkyanum}. \emph{Photosynthetica}, 39,
#' 191--195.
#'
#' Metcalfe C. R. (1960). \emph{Anatomy of the monocotyledons.
#' I. Gramineae}. Oxford University Press.
#' 
#' Munroe, S. E. M., Guerin, G. R., McInerney, F. A., Martín-Forés, I., Welti, N., 
#' Farrell, M., Atkins, R., & Sparrow, B. (2022). A vegetation carbon isoscape for Australia 
#' built by combining continental-scale field surveys with remote sensing. Landscape Ecology, 37(8), 
#' 1987-2006. https://doi.org/10.1007/s10980-022-01476-y
#'
#' Osborne C. P. et al. (2014). A global database of C4 photosynthesis
#' in grasses. \emph{New Phytologist}, 204, 441--446.
#'
#' Rao I. M., Swamy P. M. and Das V. S. R. (1979). Some characteristics
#' of Crassulacean acid metabolism in five nonsucculent scrub species.
#' \emph{Zeitschrift fur Pflanzenphysiologie}, 94(3), 201--210.
#'
#' Sage R. F. (2016). A portrait of the C4 photosynthetic family on the
#' 50th anniversary of its discovery. \emph{Journal of Experimental
#' Botany}, 68(2), e11--e28.
#'
#' Sage R. F., Sage T. L., Pearcy R. W. and Borsch T. (2007). The
#' taxonomic distribution of C4 photosynthesis in Amaranthaceae sensu
#' stricto. \emph{American Journal of Botany}, 94(12), 1992--2003.
#'
#' Sayed O. H. (2001). Crassulacean acid metabolism 1975--2000, a check
#' list. \emph{Photosynthetica}, 39, 339--352.
#'
#' Schmidt S. and Stewart G. R. (2003). d15N values of tropical savanna
#' and monsoon forest species reflect root specialisations and soil
#' nitrogen status. \emph{Oecologia}, 134, 569--577.
#'
#' Taylor S. H., Hulme S. P., Rees M., Ripley B. S., Woodward F. I. and
#' Osborne C. P. (2010). Ecophysiological traits in C3 and C4 grasses.
#' \emph{New Phytologist}, 185, 780--791.
#'
#' Thiede J. and Eggli U. (2007). Crassulaceae. In: Kubitzki K. (ed.),
#' \emph{Flowering Plants-Eudicots, The Families and Genera of Vascular
#' Plants}, Vol. 9, pp. 83--118. Springer.
#'
#' Ting I. P. (1989). Photosynthesis of arid and subtropical succulent
#' plants. \emph{Aliso}, 12, 387--406.
#'
#' Voznesenskaya E. et al. (2008). Structural, biochemical, and
#' physiological characterization of photosynthesis in two C4
#' subspecies of \emph{Tecticornia indica}. \emph{Journal of Experimental
#' Botany}, 59, 1715--1734.
#'
#' Watson L. and Dallwitz M. J. (1992 onwards). The grass genera of the
#' world: descriptions, illustrations, identification, and information
#' retrieval.
#'
#' Watson L. and Dallwitz M. J. (1992 onwards). The families of flowering
#' plants: descriptions, illustrations, identification, and information
#' retrieval.
#'
#' Winter K. (2019a). Ecophysiology of constitutive and facultative CAM
#' photosynthesis. \emph{Journal of Experimental Botany}, erz002.
#'
#' Winter K., Wallace B. J., Stocker G. F. and Roksandic Z. (1983).
#' Crassulacean acid metabolism in Australian vascular epiphytes.
#' \emph{Oecologia}, 57, 129--141.
#'
#' Winter K., Garcia M., Virgo A. and Holtum J. A. M. (2019c). Operating
#' at the very low end of the CAM spectrum: \emph{Sesuvium
#' portulacastrum}. \emph{Journal of Experimental Botany}, 70,
#' 6561--6570.
#'
#' Winter K., Sage R. F., Edwards E. J., Virgo A. and Holtum J. A. M.
#' (2019b). Facultative crassulacean acid metabolism in a C3-C4
#' intermediate. \emph{Journal of Experimental Botany}, 70, 6571--6579.
#'
#' @source
#' Compiled and standardised trait dataset used internally by
#' \code{ausplotsR}, drawing on AusTraits, pathway-specific literature,
#' TERN stable isotope analysis, and associated taxonomic harmonisation.
"trait_data_pp"