

# Feeds and speeds
# See https://zero-divide.net/?shell_id=151&article_id=4209_general-speeds-and-feeds-formulas
# Imperial Speed and Feed Calculator
# SpindleSpeed (RPM)       =	12 x <SurfaceSpeed> (SFM)/ pi/<Diameter>(inch)
# Feed Rate (inch/min)     = <Spindle Speed>(RPM) x <N teeth>  x <ChipLoad> inch


# Metric Speed and Feed Calculator
# SpindleSpeed (RPM) = 1000 x <SurfaceSpeed> (M/min)/ pi/ <Diameter> (mm)
# Feed Rate (mm/min) = <SpindleSpeed> (RPM) x <N teeth> x <ChipLoad> (mm)


# Calculating Spindle Speed (RPM) Using Imperial Cutting Speed:

#' A Gear Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords 'gear ratio' 'continuous fraction'
#' @export
#' @examples
#' spindleSpeed()

spindleSpeed <- function(SFM, toolDiameter, units = "Metric")
{
  if(units == "Metric")
    speed = 12 * SFM / pi / toolDiameter
  else {
    speed = 1000 * SFM / pi / toolDiameter
  }
}

# Calculating Feed Rate :
# Feed_Rate=RPM x N(teeth) x CL(chip load)
# Definitions
# RPM = Revolutions Per Minute, Spindle Speed, Number of revolutions of spindle per minute
# SFM = Imperial. Surface Feet Per minute, Cutting speed, Speed at which tip of tool travels through material
# SMM= Metric. Surface Meters Per minute, Cutting speed, Speed at which tip of tool travels through material
# Feed_Rate = Inches Per Minute or Millimeters Per Minute, Feed Rate, Distance in inches or millimeters the tool the will travel through work piece per minute.
# N = Number of teeth on the cutter
# CL= Chip Load per tooth, This is the advancement of the cutter per revolution per each cutting edge. Otherwise generally explained as thickness of material each tooth takes per each revolution
# Examples:
# Calculate Speeds and Feeds for 1/2" (0.5 inch) 2 flute end mill in Mild Steel at cutting speed = 100(ft/min), Chip Load=0.001(inch per tooth)

# Calculating Spindle Speed (RPM):

# Code
# RPM=	12 x SFM
# 3.14 x Diameter
# =

# RPM=	12 x 100(ft/min)	=764(revs/min-1)
# 3.14 x 0.5(in)
# For turning applications we do not need this formula since Spindle Speed is usually given in Constant Surface Speed (CSS), which uses SFM value directly. But if you still want to use RPM formula, then diameter value is the actual diameter of work piece.

# Calculating Feed Rate in Inches Per Minute (IPM)
# Formula used for milling and drilling applications. Please note that some tool manufacturers provide their recommended feed rate as feed per revolution. In such case do not multiply by number of teeth.

# Code
# Feed Rate = RPM x N x CL

# Feed Rate = 800(RPM) x 2(teeth) x  0.001("chip load) =1.600 (in/min)
