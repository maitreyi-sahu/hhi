# Steps:
# 1: read county pop data
# 2: read HUD crosswalk files
# 3: crosswalk county pop to zip-level pop
# 3: aggregate population at state-HRR and HRR levels

# Inputs:
#   1) Total county pop
#   2) HUD census to zip crosswalk files: https://www.huduser.gov/portal/datasets/usps_crosswalk.html

# Outputs:
#   1) pop_hrr: aggregated population at HRR level
#   2) pop_state_hrr: aggregated population at state-HRR level

# Note, this is the opposite of DEX's "ZIPFIX"

# TO DO: Need to do some checks of interpolation
#        Also note, we have 12% missing zip codes with populations
# Note that problem is coming from HUD data