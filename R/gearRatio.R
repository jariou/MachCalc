# Using continuous fractions to obtain the best possible
# approximating ratio from a pair of gears


#' A Gear Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE
#' @keywords 'gear ratio' 'continuous fraction'
#' @export
#' @examples
#' bestGearRatio()
bestGearRatio <-
  function (
    m,
    maxDepth = 10,
    maxGear  = 127,
    maxError = 0.1
  )
  {
    best      = list()
    bestCount = 0

    for( i in 1:maxDepth)
    {
      tmp = gearRatio(m,1,i)
      gg  = max(tmp$gears)

      if(!is.nan(gg))
        if(gg <= maxGear)
          if(
            abs(tmp$percentError)
            <=
            maxError
          )
          {
            bestCount =
              bestCount + 1

            best[[bestCount]] =
              append(
                tmp,
                list(depth = i)
              )
          }
    }

    class(best) = "gearRatioList"
    best
  }

#' A Gear Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords 'gear ratio' 'continuous fraction'
#' @export
#' @examples
#' gearRatio()
gearRatio <-
  function (
    m,
    n     = 1,
    depth = 20
  )
  {
    if(m>n)
    {
      top    = m
      bottom = n
    }
    else
    {
      top    = n
      bottom = m
    }

    goal         = top / bottom
    factorVec    = double(depth + 2)
    remainderVec = double(depth + 2)

    for( i in 1:(depth + 2))
    {
      factorVec[i]    = top%/%bottom
      remainderVec[i] = top%%bottom
      top             = bottom
      bottom          = remainderVec[i]
    }

    gearVec    = c(0, 1, rev(factorVec[1:depth]) )

    for( i in 2 + (1:depth))
    {
      gearVec[i] = gearVec[i] * gearVec[i - 1] + gearVec[i - 2]
    }

    ratio = gearVec[depth + 2] / gearVec[depth + 1]

    tmp = list(
      gears        = c(gearVec[depth + 2], gearVec[depth + 1]),
      ratio        = ratio,
      goal         = goal,
      percentError = 100 * (ratio - goal) / goal
    )

    class(tmp) <- "gearRatio"
    tmp
  }

#' A Gear Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords 'gear ratio' 'continuous fraction'
#' @export
#' @examples
#' print.gearRatio()
print.gearRatio <-
  function (
    x,
    ...
  )
  {
    cat(
      "\nGears  :  ",
      x$gears[1],
      ":",
      x$gears[2],
      "\n"
    )

    cat(
      "Ratio  :  ",
      format(x$ratio, digits = 4),
      "[",
      format(x$goal, digits = 4),
      "]\n"
    )

    cat(
      "Error  :  ",
      paste(
        format(
          x$percentError,
          digits = 3
        ),
        "%",
        sep=""
      ),
      "\n\n"
    )
    invisible(x)
  }

#' A Gear Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords 'gear ratio' 'continuous fraction'
#' @export
#' @examples
#' print.gearRatioList()
print.gearRatioList <- function(
  x, ...)
{
  for(i in 1:length(x))
    print.gearRatio(x[[i]])
  invisible(x)
}

# Finding the size bore of a bore in which 3 pins are cotangent
# as well as tangent to the circle
# See https://pballew.blogspot.com/2014/10/the-kiss-precise-soddys-circle-theorem.html
# The formula is as follows

# (1/r1)^2 + (1/r2)^2 + (1/r3)^2 + (1/r4)^2 = (1/2) * (1/r1 + 1/r2 + 1/r3 + 1/r4)^2

# If you let the bend bi = 1/ri, the formula becomes
# b1^2 + b2^2 + b3^2 + r4^2 = (1/2) * (b1 + b2 + b3 + b4)^2



# A different but perhaps more interesting problem is to find the
# size of the smallest bore that will contain 3 pins of given sizes

#' A Gear Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords 'gear ratio' 'continuous fraction'
#' @export
#' @examples
#' bore3P()
bore3P <-
  function(
    d1,
    d2,
    d3
  )
  {
    r1 = d1/2
    r2 = d2/2
    r3 = d3/2

    rProd       = r1 * r2 * r3
    rSum        = r1 + r2 + r3
    sumOf2Prods = r1*r2 +
      r1*r3 +
      r3*r2

    boreRad = (
      rProd /
        (
          2 *
            sqrt(
              rProd *
                rSum
            ) -
            sumOf2Prods
        )
    )

    myTitle = "Bore for 3 pins"
    pins = paste(
      d1,
      d2,
      d3,
      sep = ", "
    )

    plotCircle(
      boreRad,
      main = paste(
        myTitle,
        "(",
        pins,
        ") => ",
        format(
          2 *
            boreRad,
          3
        )
      )
    )
    linesCircle(
      d1/2,
      0,
      (d1/2-boreRad)
    )


    linesCircle(
      d2/2,
      -d1/1.5,
      0
    )

    linesCircle(
      d3/2,
      -d1/1.5,
      -1.5
    )

    return(2 * boreRad)
  }

#' A Gear Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords 'gear ratio' 'continuous fraction'
#' @export
#' @examples
#' plotCircle()
plotCircle <-
  function(
    radius  = 1,
    main    = "",
    centerX = 0,
    centerY = 0,
    color   = "black",
    npoints = 100
  )
  {
    theta <- c(0:npoints,0) *
      2 * pi /
      npoints

    x = centerX +
      radius * cos(theta)
    y = centerY +
      radius * sin(theta)

    plot(
      x,
      y,
      main = main,
      type = "l"
    )
  }

#' A Gear Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords 'gear ratio' 'continuous fraction'
#' @export
#' @examples
#' linesCircle()
linesCircle <-
  function(
    radius  = 1,
    centerX = 0,
    centerY = 0,
    color.  = "black",
    npoints = 100
  )
  {
    theta <- c(0:npoints,0) *
      2 * pi /
      npoints

    x = centerX +
      radius * cos(theta)

    y = centerY +
      radius * sin(theta)

    lines(
      x,
      y,
      type = "l"
    )
  }


# Examples
#bore3P(3, 2, 1)
#bore3P(3, 2, 2)
#bore3P(3, 1, 1)
#bore3P(3, 3, 3)
#bore3P(.4445, .44465 you, .50)


#bestGearRatio(1.357842)

#bestGearRatio(1.27)

