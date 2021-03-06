% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{StarrettTapDrills}
\alias{StarrettTapDrills}
\title{Starrett Inch Tap Drill Sizes}
\format{A numeric data frame.}
\source{
Generated from \code{
decimal <- c(
             0.0135, 0.0145, 0.0156, 0.0160, 0.0180, 0.0200,
             0.0210, 0.0225, 0.0240, 0.0250, 0.0260, 0.0280,
             0.0292, 0.0310, 0.0312, 0.0320, 0.0330, 0.0350,
             0.0360, 0.0370, 0.0380, 0.0390, 0.0400, 0.0410,
             0.0420, 0.0430, 0.0465, 0.0469, 0.0520, 0.0550,
             0.0595, 0.0625, 0.0635, 0.0670, 0.0700, 0.0730,
             0.0760, 0.0781, 0.0785, 0.0810, 0.0820, 0.0860,
             0.0890, 0.0935, 0.0938, 0.0960, 0.0980, 0.0995,
             0.1015, 0.1040, 0.1065, 0.1094, 0.1100, 0.1110,
             0.1130, 0.1160, 0.1200, 0.1250, 0.1285, 0.1360,
             0.1405, 0.1406, 0.1440, 0.1470, 0.1495, 0.1520,
             0.1540, 0.1562, 0.1570, 0.1590, 0.1610, 0.1660,
             0.1695, 0.1719, 0.1730, 0.1770, 0.1800, 0.1820,
             0.1850, 0.1875, 0.1890, 0.1910, 0.1935, 0.1960,
             0.1990, 0.2010, 0.2031, 0.2040, 0.2055, 0.2090,
             0.2130, 0.2188, 0.2210, 0.2280, 0.2340, 0.2344,
             0.2380, 0.2420, 0.2460, 0.2500, 0.2570, 0.2610,
             0.2656, 0.2660, 0.2720, 0.2770, 0.2810, 0.2812,
             0.2900, 0.2950, 0.2969, 0.3020, 0.3125, 0.3160,
             0.3230, 0.3281, 0.3320, 0.3390, 0.3438, 0.3480,
             0.3580, 0.3594, 0.3680, 0.3750, 0.3770, 0.3680,
             0.3906, 0.3970, 0.4040, 0.4062, 0.4130, 0.4219,
             0.4375, 0.4531, 0.4688, 0.4844, 0.5000, 0.5156,
             0.5312, 0.5469, 0.5625, 0.5781, 0.5938, 0.6094,
             0.6250, 0.6406, 0.6562, 0.6719, 0.6875, 0.7031,
             0.7188, 0.7344, 0.7500, 0.7656, 0.7812, 0.7969,
             0.8125, 0.8281, 0.8438, 0.8594, 0.8750, 0.8906,
             0.9062, 0.9219, 0.9375, 0.9531, 0.9688, 0.9844,
             1.0000, 1.0469, 1.1094, 1.1250, 1.1719, 1.2188,
             1.2500, 1.2969, 1.3438, 1.3750, 1.4219, 1.5000
             )
drills <- (
             80    , 79     , 1/64   , 78   , 77     , 76,
             75    , 74     , 73     , 72   , 71     , 70,
             69    , 68     , 1/32   , 67   , 66     , 65,
             64    , 63     , 62     , 61   , 60     , 59,
             58    , 57     , 56     , 3/64 , 55     , 54,
             53    , 1/16   , 52     , 51   , 50     , 49,
             48    , 5/64   , 47     , 46   , 45     , 44,
             43    , 42     , 3/32   , 41   , 40     , 39,
             38    , 37     , 36     , 7/64 , 35     , 34,
             33    , 32     , 31     , 1/8  , 30     , 29,
             28    , 9/64   , 27     , 26   , 25     , 24,
             23    , 5/32   , 22     , 21   , 20     , 19,
             18    , 11/64  , 17     , 16   , 15     , 14,
             13    , 3/16   , 12     , 11   , 10     , 9,
             8     , 7      , 13/64  , 6    , 5      , 4,
             3     , 7/32   , 2      , 1    , A      , 15/64,
             B     , C      , D      , 1/4 E, F      , G,
             17/64 , H      , I      , J    , K      , 9/32,
             L     , M      , 19/64  , N    , 5/16   , O,
             P     , 21/64  , Q      , R    , 11/32  , S,
             T     , 23/64  , U      , 3/8  , V      , W,
             25/64 , X      , Y      , 13/32, Z      , 27/64,
             7/16  , 29/64  , 15/32  , 31/64, 1/2    , 33/64,
             17/32 , 35/64  ,  9/16  , 37/64, 19/32  , 39/64,
             5/8   , 41/64  , 21/32  , 43/64, 11/16  , 45/64,
             23/32 , 47/64  , 3/4    , 49/64, 25/32  , 51/64,
             13/16 , 53/64  , 27/32  , 55/64, 7/8    , 57/64,
             29/32 , 59/64  , 15/16  , 61/64, 31/32  , 63/64,
             1     , 1 3/64 , 1 7/64 , 1 1/8, 1 11/64, 1 7/32,
             1 1/4 , 1 19/64, 1 11/32, 1 3/8, 1 27/64, 1 1/2
            )
TapSizeInch <- c(
                        ,           ,           ,           ,            ,            ,
                        ,           ,           ,           ,            ,            ,
                        ,           ,           ,           ,            ,            ,
                        ,           ,           ,           ,            ,            ,
                        ,           ,           , "0-80"    ,            ,            ,
             "1-64,1-72",           ,           ,           , "2-56,2-64",            ,
                        ,           , "3-48"    ,           , "3-56"     ,            ,
             "4-40"     , "4-48"    ,           ,           ,            ,            ,
             "5-40"     , "5-44"    , "6-32"    ,           ,            , 0.1110     ,
             "6-40"     ,           ,           ,           ,            , "8-32,8-36",
                        ,           ,           ,           , "10-24"    , 0.1520     ,
                        ,           ,           , "10-32"   ,            ,            ,
                        ,           ,           , "12-24"   ,            , "12-28"    ,
                        ,           ,           ,           ,            ,            ,
                        , "1/4-20"  ,           ,           ,            ,            ,
             "1/4-28"   ,           ,           ,           ,            ,            ,
                        ,           ,           ,           , "5/16-18"  ,            ,
                        ,           , "5/16-24" ,           ,            ,            ,
                        ,           ,           ,           , "3/8-16"   ,            ,
                        ,           , "3/8-24"  ,           ,            ,            ,
                        ,           , "7/16-14" ,           ,            ,            ,
             "7/16-20"  ,           ,           ,           ,            , "1/2-13"   ,
                        , "1/2-20"  ,           , "9/16-12" ,            , "9/16-18"  ,
             "5/8-11"   ,           ,           , "5/8-18"  ,            ,            ,
                        ,           , "3/4-10"  ,           , "3/4-16"   ,            ,
                        ,           ,           , "7/8-9"   ,            ,            ,
             "7/8-14"   ,           ,           ,           , "1-8"      ,            ,
                        , "1-12"    , "1-14"    ,           ,            , "1 1/8-7"  ,
                        , "1 1/8-12", "1 1/4-7" ,           , "1 1/4-12" , "1 3/8-6"  ,
                        , "1 3/8-12", "1 1/2-6" ,           , "1 1/2-12" ,           
             )
 InchTapDrills <- data.frame(Decimal,
                             Drills, 
                             inchTapSize, 
                             stringsAsFactors = FALSE)
}

Generated from \code{
DrillSize <- (
              11/32,  7/16  , 37/64,   23/32 , 59/64  , 1 5/32, 1 1/2,
              1 3/4, 2 7/32, 2 21/32, 3 1/4, 3 3/4, 4 1/4
              )
ThreadSize <- c(
                "1/8"  , "1/4", "3/8"  , "1/2", "3/4"  , "1", "1 1/4", 
                "1 1/2", "2"  , "2 1/2", "3"  , "3 1/2", "4"            
                )
ThreadPitch <- c(
                 27  , 18, 18  , 14, 14  , 11.5, 11.5, 
                 11.5, 11.5  , 8, 8  , 8, 8            
                )
 PipeThreadDrill <- data.frame(Decimal,
                             Drills, 
                             inchTapSize, 
                             stringsAsFactors = FALSE)
}
}
\description{
A few numbers from \code{rnorm()}.

A few numbers from \code{rnorm()}.
}
\examples{
data(InchTapDrills)
str(InchTapDrills)
data(PipeThreadDrill)
str(PipeThreadDrill)
}
