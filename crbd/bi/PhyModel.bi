class PhyModel < Model {
  // Constants to be used in yields (e. g. "yield impossible" or "resumeYields(); yield accumulated")
  impossible:Real <- -inf;
  accumulated:Real <- 0.0;

  // State variables
  paused:Boolean <- false;
  finished:Boolean <- false;
  infiniteMode:Boolean <- false;

  // Accumulated weights
  acw:Real <- 0.0;
  lastAcw:Real <- 0.0;

  function hasFinished() -> Boolean {
    return finished;
  }

  function pauseYields() {
    paused <- true;
  }

  function resumeYields() {
    paused <- false;
  }

  function setInfiniteMode() {
    infiniteMode <- true;
  }

  function clearInfiniteMode() {
    infiniteMode <- false;
  }

  fiber run() -> Real;

  fiber simulate() -> Real {
    finished <- false;
    acw <- 0.0;
    w:Real! <- run();
    while w? {
      acw <- acw + w!;
      if !paused {
        yield acw;
        lastAcw <- acw;
        acw <- 0.0;
      }
    }
    finished <- true;
    if infiniteMode {
      while true {
        yield 0.0;
      }
    }
  }
}
