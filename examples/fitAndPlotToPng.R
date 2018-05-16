library(grDevices)
library(plotteR);

# define the number of plots
n           <- 8L;
total       <- 1L + n;

# get a reasonable arrangement and figure size
arrangement <- plots.arrange(total);
size        <- plots.size(rows=arrangement[1],
                          columns=arrangement[2],
                          single.minWidth.in=3);
resolution  <- 256;

# decide for png output
png(filename = "examples/fitAndPlot.png",
    width = size[1] * resolution,
    height = size[2] * resolution,
    res = resolution,
    units = "px",
    pointsize = 12,
    bg = "white")
# run the example code
source("examples/fitAndPlot.R");
# close the png output
dev.off();
