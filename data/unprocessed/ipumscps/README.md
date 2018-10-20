#  Data Processing Directions for IPUMS CPS

# Links/Citations:

[ipums data set](https://cps.ipums.org/cps-action/extract_requests/download)

# How To Read into R: 

### CSV Format

For csv files, simply use `read_csv()` from `tidyverse`.

### DAT Format

You will need to install `ipumsr` package if you downloaded .dat or fixed-width format files.

```{r}
# Load necessary packages
library(ipumsr)

# Read data
ddi_test_00001 <- read_ipums_ddi(ddi_file = "data/unprocessed/ipumscps/cps_00001.xml")
data_test_00001 <- read_ipums_micro(ddi = ddi_test_00001)
```

