################################################################################
# Transmission Electron Microscopy (TEM) Size and Size Distribution Analysis
# Author: Eric Teeman
#
# For use in determining the size and size distribution of superparamagnetic
# nanoparticles using Fiji and psa_r12 macro. 
#
################################################################################


# Import packages ---------------------------------------------------------

my.packages <- c("rChoiceDialogs", "ggplot2", "scales", "magrittr", "dplyr", "tidyr", "gdata", "mgcv", "gridExtra", "viridis")
lapply(my.packages, require, character.only = TRUE)


# Import data and set initial values --------------------------------------

setwd("/Users/ericteeman/Google Drive/Research/Data/TEM/")
setwd(rchoose.dir(caption = "Select Directory")) # Asks user to choose directory containing data files

files <- list.files(pattern = "resultstable")

combined_files <- do.call("rbind", lapply(files, function(i) {
  read.csv(i, header = TRUE, sep = "\t")
}))

dat <- combined_files$Diameter

# histogram the data
bin <- seq(1, 60, by = .01)
hst <- hist( dat,
             breaks = "FD",
             plot = FALSE,
             warn.unused = FALSE)

x <- bin

# For base R  log-normal parameters
df = as.data.frame(cbind(hst$mids, hst$density))
colnames(df) = c("d", "density")

d.start = median(dat)
sgma.start = 0.05

fn <-
  nls(
    density ~ 1 / (sgma * d * sqrt(2 * pi)) * exp(-log(d / d0) ^ 2 / (2 * sgma ^
                                                                        2)),
    data = df,
    start = list(d0 = d.start, sgma = sgma.start),
    nls.control(maxiter = 1e5, tol = 1e-3),
    trace = TRUE
  )

fit <- predict(fn, list(d = x))


# Important export values -------------------------------------------------

d = round(coef(fn)[1], 1)
sigma = round(coef(fn)[2], 2)
n = length(dat)

export = data.frame(sep = "-", values = c(d, sigma, n))
colnames(export) = c("","")
rownames(export) = c("d", "\u03C3", "n")


# Plots -------------------------------------------------------------------

theme_new <- function (base_size=24, base_line_size=1) {
  theme_bw(base_size=base_size,
           base_family="") %+replace%
    theme(
      axis.text.x = element_text(size=base_size, margin = margin(t=0.75*base_size,b=0.25*base_size), color="black"),
      axis.text.y = element_text(size=base_size, margin=margin(r=0.75*base_size,l=0.25*base_size), color="black"),
      axis.title=element_text(size=base_size, color="black"),
      axis.line=element_line(size=base_line_size, lineend="square", color="black"),
      axis.ticks=element_line(size=base_line_size, lineend="square", color="black"),
      axis.ticks.length=unit(-8, "pt"),
      panel.border=element_blank(),
      panel.grid=element_blank(),
      aspect.ratio = 1,
      legend.background=element_rect(fill="transparent", colour=NA),
      legend.position=c(1, 1),
      legend.justification=c("right", "top"),
      legend.direction="vertical",
      legend.title=element_text(size=0.75*base_size),
      legend.title.align=0.5,
      legend.text=element_text(size=0.75*base_size),
      legend.text.align=0
    )
}

df.hst = as.data.frame(cbind(hst$mids, hst$density))
colnames(df.hst) = c("d", "density")

df.max = max(df.hst$density)

df.fit = as.data.frame(cbind(x, fit))
colnames(df.fit) = c("d", "fit")

xmid = df.hst$d[which(df.hst$density == max(df.hst$density))]
xmin = floor(xmid - 0.2*xmid)
xmax = ceiling(xmid + 0.25*xmid)
xann = xmid + 0.07*xmid
ymax = max(df.hst$density) - 0.14*max(df.hst$density)
ymin = max(df.hst$density) - 0.19*max(df.hst$density)

xlab = expression(paste(d[0]," [nm]"))
ylab = "Density [%]"

p1 = ggplot(df.hst, aes(x = d)) +
  geom_bar(aes(y = density),
           stat = "identity",
           na.rm = TRUE,
           fill = "white",
           color = "black",
           size = 0.5) +
  geom_line(data = df.fit,
            aes(x = d, y = fit),
            stat = "identity",
            size = 1,
            color = "red3",
            na.rm = TRUE) +
  annotation_custom(tableGrob(export,theme=ttheme_minimal(base_size = 18, parse = TRUE)), 
                    xmin=xann, xmax=xmax, ymin=ymin, ymax=ymax) +
  theme_new() +
  guides(color = guide_legend()) +
  labs(x = xlab, y = ylab) +
  scale_x_continuous(breaks = pretty_breaks(3), limits = c(xmin, xmax)) +
  scale_y_continuous(breaks = pretty_breaks(3))


# Export and Display Plots ------------------------------------------------

main.directory = getwd()
export.directory = "export"
dir.create(file.path(main.directory, export.directory), showWarnings = FALSE)
setwd(file.path(main.directory, export.directory))

write.csv(export, "export.csv", row.names = FALSE)
write.csv(df.hst, "hist.csv", row.names = FALSE)
write.csv(df.fit, "fit.csv", row.names = FALSE)

sc = 1

ggsave("hist.png",
       p1,
       width = 4.5,
       height = 4.5,
       dpi = "retina")

export
p1