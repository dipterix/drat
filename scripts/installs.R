# This script makes sure the desired packages can be installed or updated

remotes::install_github(c(
  "dipterix/dipsaus",
  "dipterix/threeBrain",
  "beauchamplab/raveio",
  "dipterix/rutabaga@develop", 
  "beauchamplab/rave",
  "dipterix/ravebase",
  "beauchamplab/ravebuiltins@migrate2"
), dependencies = TRUE, upgrade = 'never')
