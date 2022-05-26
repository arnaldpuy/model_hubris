## ----setup, include=FALSE--------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "tikz", cache = TRUE)


## ---- results="hide", message=FALSE, warning=FALSE, cache=FALSE------------------

# PRELIMINARY ------------------------------------------------------------------

# Function to read in all required packages in one go:
loadPackages <- function(x) {
  for(i in x) {
    if(!require(i, character.only = TRUE)) {
      install.packages(i, dependencies = TRUE)
      library(i, character.only = TRUE)
    }
  }
}

theme_AP <- function() {
  theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent",
                                           color = NA),
          legend.margin=margin(0, 0, 0, 0),
          legend.box.margin=margin(-7,-7,-7,-7), 
          legend.key = element_rect(fill = "transparent",
                                    color = NA), 
          strip.background = element_rect(fill = "white"))
}

# Load the packages
loadPackages(c("data.table", "tidyverse", "cowplot", "scales", "patchwork"))

# Set checkpoint
dir.create(".checkpoint")
library("checkpoint")

checkpoint("2022-05-20", 
           R.version ="4.2.0", 
           checkpointLocation = getwd())


## ----simulations_dimensions, fig.height=2.5, fig.width=2.5, warning=FALSE--------

# EXPLOSION OF THE UNCERTAINTY SPACE -------------------------------------------

# Define dimensions -------------------------------
k <- 2:10

# Compute number of interactions-------------------
x <- sapply(k, function(k) 2^k - k - 1)

a <- data.table(cbind(x, k)) %>%
  ggplot(., aes(k, x)) +
  geom_line(color = "blue") + 
  labs(x = "$k$", y = "Nº of interactions") +
  theme_AP()

a

# Curse of dimensionality -----------------------------------------------------
N <- 10 # Sample density

out <- N^k 

b <- cbind(k, out) %>%
  data.table() %>%
  ggplot(., aes(k, out)) +
  geom_line(color = "blue") + 
  labs(x = "$k$", y = "Nº of points") +
  theme_AP()

inset.plot <- b + 
  scale_x_continuous(limits = c(2, 5), 
                     breaks = pretty_breaks(n = 3)) + 
  scale_y_continuous(limits = c(1e+02, 1e+05), 
                     breaks = pretty_breaks(n = 3)) + 
  labs(x = "", y = "") + 
  labs(x = "$k$", y = "Nº of points")

b <- b + 
  inset_element(inset.plot, 0.05, 0.15, 0.8, 0.8) 

b

# Ratio of the hypersphere to the hypercube ------------------------------------

sphere_to_cube <- function(x) pi ^ ((x) / 2) * (0.5) ^ (x) / gamma(1 + x / 2)

out <- sapply(k, function(x) sphere_to_cube(x))

c <- data.table(cbind(k, out)) %>%
  .[, corner:= 1 - out] %>%
  ggplot(., aes(k, corner)) +
  geom_line(color = "blue") +
  labs(x = "$k$", y = "Fraction corners") +
  theme_AP()

c


## ----plot_curse, dependson="simulations_dimensions", fig.height=2.3, fig.width=6, warning=FALSE----

# MERGE PLOTS ------------------------------------------------------------------

plot_grid(b, c, a, ncol = 3, labels = "auto", 
          rel_widths = c(0.47, 0.28, 0.28), align = "tb")


## ----code_lines, fig.height=2.5, fig.width=2.5-----------------------------------

# LINES OF CODE ----------------------------------------------------------------

code <- fread("lines_code.csv")
colNames <- colnames(code)[-1]
code[, (colNames):= lapply(.SD, function(x) x * 1000), .SDcols = (colNames)]

# Plot -------------------------------------------------------------------------
code.plot <- code %>%
  ggplot(., aes(KLOC)) +
  geom_histogram() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", scales::math_format(10^.x))) + 
  coord_cartesian(clip = "off") +
  labs(x = "Lines of code", y = "Counts") +
  theme_AP()

code.plot


## ----cyclomatic, fig.height=2.5, fig.width=3-------------------------------------

# CYCLOMATIC COMPLEXITIES ------------------------------------------------------

cyclomatic <- fread("cyclomatic_complexity.csv")
colNames <- colnames(cyclomatic)[-1]
new_colNames <- c("Low risk", "Moderate risk", "High risk", "Untestable")
cyclomatic[, total:= rowSums(.SD), .SDcols = colNames]
fraction <- cyclomatic[, lapply(.SD, function(x) x / total), .SDcols = colNames]
colnames(fraction) <- new_colNames

# Plot ----------------------------------
cyclomatic.plot <- melt(fraction, measure.vars = new_colNames, 
     variable.name = "Cyclomatic \n complexities") %>%
  ggplot(., aes(value, fill = `Cyclomatic \n complexities`)) +
  scale_fill_manual(values = c("green", "yellow", "orange", "red")) +
  labs(x = "Fraction of model", y = "") +
  geom_histogram(alpha = 0.4, position = "identity", color = "black") +
  theme_AP() + 
  theme(legend.position = c(0.55, 0.6))

cyclomatic.plot


## ----merge_cyclomatic, dependson=c("code_lines", "cyclomatic"), fig.height=2.5, fig.width=5----

# MERGE PLOTS ------------------------------------------------------------------

plot_grid(code.plot, cyclomatic.plot, ncol = 2, labels = "auto", 
          rel_widths = c(0.45, 0.55))



transistors <- fread("transistors-per-microprocessor.csv")
supercomputers <- fread("supercomputer-power-flops.csv")

a <- transistors %>%
  ggplot(., aes(Year, `Transistors per microprocessor`)) +
  geom_line() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  annotation_logticks(sides = "l") +
  geom_point(size = 0.8) +
  theme_AP()

b <- supercomputers %>%
  ggplot(., aes(Year, `Floating-Point Operations per Second`)) +
  geom_line() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  annotation_logticks(sides = "l") +
  labs(x = "Year", y = "FLOPS per second") +
  geom_point(size = 0.8) +
  theme_AP()

plot_grid(a, b, ncol = 2, labels = "auto")




watts <- fread("watts.txt", col.names = c("Year", "Typical power (Watts)"), 
               colClasses = c("numeric", "numeric")) 
cores <- fread("cores.txt", col.names = c("Year", "Number of logical cores"), 
               colClasses = c("numeric", "numeric"))
frequency <- fread("frequency.txt", col.names = c("Year", "Frequency (MHz)"), 
                   colClasses = c("numeric", "numeric"))
specint <- fread("specint.txt", 
                 col.names = c("Year", "Single-thread performance \n (SpecINT x $10^3$)"), 
                 colClasses = c("numeric", "numeric"))
transistors <- fread("transistors.txt", col.names = c("Year", "Transistors (thousands)"), 
                     colClasses = c("numeric", "numeric"))

list_dt <- list(watts, cores, frequency, specint, transistors)

all <- Reduce(function(...) merge(..., all = TRUE), list_dt)

colNames_dt <- colnames(all)[-1]

microprocessor.data <- melt(all, measure.vars = colNames_dt) %>%
  ggplot(., aes(Year, value, color = variable)) + 
  geom_point() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  annotation_logticks(sides = "l") +
  labs(x = "Year", y  = "") +
  scale_color_discrete(name = "") +
  theme_AP() + 
  theme(legend.position = c(0.25, 0.75))

rev(nm_sizes)

dark_silicon <- fread("dark_silicon.csv")
nm_sizes <- dark_silicon$Size



dark_silicon[, Size:= factor(Size, levels = rev(nm_sizes))]
dark_silicon[, Size:= paste(Size, "nm", sep = "")]
dark_silicon[, `Dark silicon`:= 1 - Capacitance] %>%
  .[, Year:= NULL]
setnames(dark_silicon, "Capacitance", "Active")


dark.silicon.plot <- melt(dark_silicon, measure.vars = colnames(dark_silicon)[-1]) %>%
  ggplot(., aes(Size, value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme_AP()

ggpubr::ggarrange(microprocessor.data, dark.silicon.plot, ncol = 2, 
                  labels = "auto", widths = c(0.65, 0.45))


plot_grid(microprocessor.data, dark.silicon.plot, ncols = 2, labels = "auto", 
          rel_width = c(0.65, 0.45))
