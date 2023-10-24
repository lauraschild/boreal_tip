#paleospec functions
MakeEquidistant <- function (t.x, t.y, dt = NULL, time.target = seq(from = t.x[1], 
                                                 to = t.x[length(t.x)], by = dt), dt.hres = NULL, bFilter = TRUE, 
          k = 5, kf = 1.2, method.interpolation = "linear", method.filter = 2) 
{
  index <- !is.na(t.x)
  t.x <- t.x[index]
  t.y <- t.y[index]
  if (is.null(dt)) 
    dt <- mean(diff(time.target))
  if (is.null(dt.hres)) {
    dt.hres <- dt/10
    dt.x <- diff(t.x)
    minTimeStep <- min(dt.x[dt.x > 0])
    if (dt.hres > minTimeStep) 
      dt.hres = minTimeStep
  }
  if (dt.hres > min(diff(t.x), na.rm = TRUE)) 
    warning("dt.hres is lower\n            than the minimum timestep")
  index <- (!is.na(t.y))
  time.hres <- seq(from = FirstElement(t.x), to = LastElement(t.x), 
                   by = dt.hres)
  data.hres <- approx(t.x[index], t.y[index], time.hres, method = method.interpolation)
  index <- !is.na(data.hres$y)
  data.hres$x <- data.hres$x[index]
  data.hres$y <- data.hres$y[index]
  filterLength = round(k * (dt/(2 * dt.hres)))
  if ((filterLength%%2) == 0) 
    filterLength = filterLength + 1
  if (bFilter) {
    f.lowpass <- Lowpass(1/(2 * dt) * kf, filterLength, sample = 1/dt.hres)
    meanvalue <- mean(data.hres$y)
    data.hres.filtered <- ApplyFilter(data.hres$y - meanvalue, 
                                      f.lowpass, method = method.filter) + meanvalue
  }
  else {
    data.hres.filtered <- data.hres$y
  }
  index <- !is.na(data.hres.filtered)
  data.target <- approx(data.hres$x[index], data.hres.filtered[index], 
                        time.target, method = method.interpolation)
  return(ts(data.target$y, start = FirstElement(data.target$x), 
            deltat = diff(data.target$x)[1]))
}

Lowpass <- function (omega.c, n = 9, sample = 1, convergence = TRUE) 
{
  if ((n%%2) == 0) 
    stop("N must be odd, this function calculates only symmetrical = phase preserving filters")
  omega.c <- omega.c/sample
  if (omega.c >= 0.5) 
    stop("frequency higher or equal then Nyquist")
  n.side <- (n - 1)/2
  g.u <- numeric(length = n)
  u <- 1:n.side
  g.u[u + n.side + 1] <- sin(u * omega.c * 2 * pi)/(pi * u)
  g.u[n.side + 1] <- omega.c * 2
  g.u[1:n.side] <- g.u[n:(n.side + 2)]
  if (convergence) {
    delta <- 4 * pi/n
    u <- c((-1 * n.side):-1, 1:n.side)
    g.u[u + n.side + 1] <- g.u[u + n.side + 1] * sin(u * 
                                                       delta/2)/(u * delta/2)
  }
  return(g.u)
}

ApplyFilter <- function (data, filter, method = 0, na.rm = FALSE) 
{
  if (!method %in% (0:4)) 
    stop("Unknown method; only 0 : 4 available.")
  result <- rep(NA, length(data))
  x <- c(zoo::na.trim(data))
  n <- length(x)
  if (na.rm) {
    x <- stats::approx(1:n, x, 1:n)$y
  }
  circular = FALSE
  if (method == 0 | method == 4) {
    if (method == 4) {
      circular = TRUE
    }
    xf <- stats::filter(x, filter, circular = circular)
  }
  else {
    N <- floor(length(filter)/2)
    if (method == 1) {
      before <- rep(mean(x), N)
      after <- rep(mean(x), N)
    }
    else if (method == 2 | method == 3) {
      before <- x[N:1]
      after <- x[n:(n - N + 1)]
      if (method == 3) {
        before <- x[1] - (before - mean(before))
        after <- x[n] - (after - mean(after))
      }
    }
    xf <- stats::filter(c(before, x, after), filter, circular = circular)
    xf <- xf[(N + 1):(N + n)]
  }
  i <- seq(match(x[1], data), by = 1, length.out = n)
  result[i] <- xf
  return(ts(result, frequency = frequency(data)))
}

FirstElement <- function (x) 
{
  return(x[1])
}

LastElement <- function (x) 
{
  return(x[length(x)])
}