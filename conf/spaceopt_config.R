# configuration file

# set the possible values for parameters
STORE_STATE_LEVELS <- c("QLD","NSW","VIC","TAS","SA","WA","NT")
STORE_LOCATION_LEVELS <- c("METRO","URBAN","REGIONAL")
ALLOCATED_SPACE_LEVELS <- seq(0,10)

# error and warning messages
ERROR_MSG <- list(
  TX_MISMATCH_LENGTH="Length of modifying values should be same as curve values",
  TX_INVALID_TRANSFORM="Valid transformation types include - ADD, MUL, EXP, LOG"
)

# Global Parameters
# Parameters for transfomration functions
TX_PARAMS <- list(ADD="ADD",
                  MUL="MUL",
                  EXP="EXP",
                  LOG="LOG")

# color palette to use in graphs
# COLOR_PALETTE <- "YlGnBu"
COLOR_PALETTE <- "Set2"
ONE_FILL_COLOR <- "steelblue"
BORDER_COLOR <- "grey"
