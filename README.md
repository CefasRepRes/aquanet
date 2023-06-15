# aquanet
R package containing the functions required to run AquaNet-Mod (The Aquaculture Disease Network Model).

Scripts for AquaNet-Mod located at https://github.com/CefasRepRes/aquanet-mod.

## Citation
If using this package, please cite the package, scripts and accompanying paper. 

To cite the package:
> Millard, R., Alewijnse, S., Ryder, D. and Guilder, J. Functions to Execute AquaNet-Mod (The Aquaculture Disease Network Model) github.com/CefasRepRes/aquanet

To cite the scripts:
> Alewijnse, S., Millard, R., Ryder, D. and Guilder, J. R scripts for AquaNet-Mod (The Aquaculture Disease Network Model) github.com/CefasRepRes/aquanet-mod

To cite the paper:
> Guilder, J., Ryder, D., Taylor, N. G. H., Alewijnse, S. R., Millard, R. S., Thrush, M. A., Peeler, E. J. & Tidbury, H. J. The aquaculture disease network model (AquaNet-Mod): a simulation model to evaluate disease spread and controls for the salmonid industry in England and Wales. In press.

## Installation

Use remotes package to install from GitHub (ensure installed from CRAN if not already). NOTE: insert personal access token at `PAT` to allow download whist repo is set to private for development.

````
library("remotes")
remotes::install_github("CefasRepRes/aquanet", auth_token = "PAT")
library("CefasRepRes/aquanet")
````
