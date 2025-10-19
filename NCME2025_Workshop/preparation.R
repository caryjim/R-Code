# Preparation for the R Exercises ----

## Install required packages ----

### Note: these packages depend on many other packages (especially blavaan),
### so running the code below will likely install additional packages.
### This may take a while! Go get some tea or coffee.
install.packages(c("psych", "lavaan",
                   "corrplot", "tidyverse", "blavaan", "semTools", "remotes"))
remotes::install_github("falkcarl/ockhamSEM")

library()
