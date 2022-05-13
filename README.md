# aquanet
R package containing the functions required to run AquaNet-Mod (The Aquaculture Disease Network Model).

Scripts for AquaNet-Mod located at https://github.com/CefasRepRes/aquanet-mod.


## Installation

Use remotes package to install from GitHub (ensure installed from CRAN if not already). NOTE: insert personal access token at `PAT` to allow download whist repo is set to private for development.

````
library("remotes")
remotes::install_github("CefasRepRes/auqanet", auth_token = "PAT")
library("CefasRepRes/aquanet")
````
