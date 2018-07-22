library(grDevices);
library(plotteR);
library(utilizeR);

# define the number of plots
logger <- makeLogger(TRUE);
n           <- 7L;
total       <- 1L + n;
logger("We will do ", n, " fitting result diagrams, i.e., ",
    total, " diagrams in total (with the original data plot).");

# get a reasonable arrangement and figure size
arrangement <- plots.arrange(total, portrait=TRUE);
size        <- plots.size(rows=arrangement[1],
                          columns=arrangement[2],
                          single.minWidth.in=3.5,
                          single.minHeight.in=3.5);
resolution  <- 256;
w.px <- as.integer(round(size[1] * resolution));
h.px <- as.integer(round(size[2] * resolution));
# we want to put the figues next to each other: the original data/function and
# fitting results at five quality levels
logger("In order to facilitate an image with ", arrangement[1],
    " rows and ", arrangement[2],
    " columns for the ", total,
    " diagrams, we set the width to ",
    size[1], "in and the height to ",
    size[2], "in, which corresponds to ",
    w.px, "px * ", h.px,
    "px at a resolution of ", resolution,
    "dpi.");

# decide for png output
png(filename = "examples/fitAndPlot.png",
    width = w.px, height = h.px,
    res = resolution,
    units = "px",
    pointsize = 12,
    bg = "white");
# run the example code
source("examples/fitAndPlot.R");
# close the png output
dev.off();
