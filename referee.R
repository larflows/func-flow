# Written beginning October 30, 2019 by Daniel Philippus for the LA River Environmental Flows project
# at the Colorado School of Mines.
#
# This program is an R interface to the (environmental) functional flows calculator (hence "r-eff" -> "referee").
# It uses Reticulate to use functions from the EFF's Python codebase, with the intended workflow of putting in
# a data frame and getting out a data frame.  Since EFF works through CSV files, the easiest way to do this is
# to write a CSV, run EFF on it, and then read the resulting CSVs.


library(reticulate)
library(dplyr)
library(tidyr)


# EFF_DIR <- file.path("Z:", "adit", "Desktop", "LARFlows", "code", "func-flow")
EFF_DIR <- "/home/daniel/research/func-flow"
INPUT_DIR <- "user_input_files"
OUTPUT_DIR <- "user_output_files"
MATRIX_EXT <- "_annual_flow_matrix.csv"
RESULT_EXT <- "_annual_flow_result.csv"
DRH_EXT <- "_drh.csv"
# Must be Python 3
PYTHON_PATH <- file.path("/usr/bin/python3")
# Replace "virtualenv" with whatever the virtual env is called
VENV_PATH <- file.path(EFF_DIR, "virtual")


# If using virtual env, comment use_python and uncomment use_virtualenv.
# use_python(PYTHON_PATH, required = T)
use_virtualenv(VENV_PATH, required = T)

example_gagedata <- function(startdate = "2009/10/01", stopdate = "2019/10/01", mean = 100, sd = 50, gages = 1:10) {
  result <- data.frame()
  dates <- format(as.Date(as.Date(startdate):as.Date(stopdate), origin = "1970/01/01"), format = "%m/%d/%Y")
  nflows <- length(dates)
  for (gage in gages) {
    gn = as.character(gage)
    gmean <- rnorm(1, mean, sd)
    gmean <- if (gmean > 0) gmean else 0
    df <- data.frame(gage = gn, date = dates)
    flows <- rnorm(nflows, gmean, sd)
    flows <- vapply(flows, function(x) {if (x > 0) x else 0 }, 1)
    df$flow <- flows
    result <- rbind(result, df)
  }
  result$gage <- as.character(result$gage)
  result
}

# ----------------------------------------------------
# Functionality: upload files and read the result CSVs.
make_input_df <- function(dates, flows) {
  # Dates as mm/dd/yyyy
  # Flows in cfs
  # Make a data frame in the correct format to be written as a CSV
  data.frame(date = dates, flow = flows)
}

format_input_df <- function(indf, gagecol = 1, datecol = 2, flowcol = 3,
                            usegage = T, gagename = NULL,
                            datetype = as.Date, origin = "1970/01/01",
                            flowconvert = function(x) x) {
  # Prepare an appropriately-formatted data frame given arbitrary input types
  # Columns can be specified as strings or as column numbers
  # If there is not a gage column, set 'usegage' to false.  If gagename is specified, it will add
  # a gage column containing gagename.
  # datetype tells the function how to convert dates, with format(datetype(date, origin = origin), format = ...).
  # If the dates are in the correct format (string mm/dd/yyyy), then use datetype = NULL.
  # The default flowconvert function assumes flows in CFS.  If flows are not in CFS, provide a conversion
  # function.
  getcol <- function(cid) {
    indf[[cid]]
  }
  
  gage <- if (usegage) getcol(gagecol) else gagename
  date <- if (is.null(datetype)) getcol(datecol)
    else vapply(
      getcol(datecol),
      function(x) format(datetype(x, origin = origin), format = "%m/%d/%Y"),
      "s"
    )
  flow <- vapply(
    getcol(flowcol),
    flowconvert,
    1
  )
  outdf <- make_input_df(date, flow)
  if (!is.null(gage)) outdf$gage <- gage
  
  outdf
}

write_input_df <- function(gagename, input_df, basepath = EFF_DIR, indir = INPUT_DIR) {
  # Write the input data frame (assumed to be in suitable format) to <gagename>.csv in
  # basepath/indir. (usually user_input_files)
  write.csv(input_df, file.path(basepath, indir, paste0(gagename, ".csv")), row.names = F, quote = F)
}

get_output_df <- function(gagename, basepath, outdir, ext) {
  # Read a result data frame.  Intended as a building block for wrapper functions.
  # Header = F makes manipulation easier
  read.csv(file.path(basepath, outdir, paste0(gagename, ext)), header = F, strip.white = T,
           na.strings = c("None", "nan", "NA"))
}

get_annual_flow_matrix <- function(gagename, basepath = EFF_DIR, outdir = OUTPUT_DIR, mext = MATRIX_EXT) {
  # gagename = first part of gage input filename, e.g. "gage10.csv" = "gage10"
  # Read the annual flow matrix file.
  # Format is columns are titled by year and each row is a daily flow (this is just the input, restructured)
  # (I think)
  get_output_df(gagename, basepath, outdir, mext)
}

get_annual_flow_result <- function(gagename, basepath = EFF_DIR, outdir = OUTPUT_DIR, rext = RESULT_EXT) {
  # gagename = first part of gage input filename, e.g. "gage10.csv" = "gage10"
  # Read the annual flow result file.
  # Format is titled rows with a bunch of metrics.  This is probably the relevant result.
  res <- get_output_df(gagename, basepath, outdir, rext) %>%
    t %>%
    as_tibble
  # Set correct column names
  colnames(res) <- res[1,]
  # Remove first row which was column names
  res <- res[2:length(res),]
  # for some reason it ends up with a bunch of NA in it
  res <- res[!is.na(res$Year), ]
  res %>% mutate_all(as.numeric)
}

get_drh <- function(gagename, basepath = EFF_DIR, outdir = OUTPUT_DIR, dext = DRH_EXT) {
  # gagename = first part of gage input filename, e.g. "gage10.csv" = "gage10"
  # Read the DRH file.
  # Rows, not columns, are titled--need to switch
  # Format is several percentiles, then min and max
  get_output_df(gagename, basepath, outdir, dext)
}

upload_files <- function(gagenames, basepath = EFF_DIR, indir = INPUT_DIR, start_date = "10/1", flow_class = 3) {
  # Upload all files.
  # Note: use of lapply and not vapply because a length-1 vector will be treated by Python as a string
  uf <- import_from_path("utils.upload_files", basepath)
  files <- lapply(gagenames, function(x) file.path(basepath, indir, paste0(x, ".csv")))
  uf$upload_files(start_date, files, flow_class, basepath)
}

upload_gagedata <- function(gagedata, basepath = EFF_DIR, indir = INPUT_DIR, start_date = "10/1", flow_class = 3) {
  # Convert gage data into files and upload it.
  # gage data should have the columns gage, date (mm/dd/yyyy), and flow.
  # Bug: upload_files is writing everything under the gage name of the second item after a "/", reason unclear
  # A temporary fix is to just use the relative path as that is what the Python code seems to expect
  gages <- unique(gagedata$gage)
  for (gage in gages) {
    data <- gagedata[gagedata$gage == gage,]
    prepared_data <- make_input_df(data$date, data$flow)
    write_input_df(gage, prepared_data, basepath, indir)
  }
  upload_files(gages, basepath, indir, start_date, flow_class)
}

process_gages <- function(gagedata, basepath = EFF_DIR, indir = INPUT_DIR, outdir = OUTPUT_DIR, start_date = "10/1",
                          flow_class = 3) {
  # Accepts gage data with gage (character), date (mm/dd/yyyy), and flow (cfs)
  # Returns a list of data frames, one for each gage
  # If check: allows the use of data for a single gage without specified gage name
  if ("gage" %in% colnames(gagedata)) {
    gages <- unique(gagedata$gage)
    upload_gagedata(gagedata, basepath, indir, start_date, flow_class)
    lapply(gages, function(g) {get_annual_flow_result(g, basepath, outdir)})
  } else process_gage(gagedata, basepath, indir, outdir, start_date, flow_class)
}

format_and_process <- function(gagedata, basepath = EFF_DIR, indir = INPUT_DIR, outdir = OUTPUT_DIR, start = "10/1",
                               gagecol = 1, datecol = 2, flowcol = 3, usegage = T, gagename = NULL,
                               datetype = as.Date, origin = "1970/01/01", flowconvert = function(x) x) {
  # Wraps process_gages to run stuff through the formatter first
  format_input_df(
    gagedata, gagecol, datecol, flowcol, usegage = "gage" %in% colnames(gagedata), gagename,
    datetype, origin, flowconvert
  ) %>%
    process_gages(basepath, indir, outdir, start)
}

process_gage <- function(gagedata, 
                         gagename = "gage",
                         ...) {
  # Like process_gages, but for just one gage (name optional)
  gagedata$gage <- gagename
  process_gages(gagedata, ...)[[1]]
}

gage_mean <- function(gage) {
  # Return mean annual data
  colMeans(gage, na.rm = T)
}

# ----------------------------------------------------