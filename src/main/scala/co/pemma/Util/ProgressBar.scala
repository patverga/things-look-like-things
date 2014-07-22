package co.pemma.Util

/**
 * Created by pv on 7/22/14.
 */
class ProgressBar(total : Int)
{
  val totalIterations = total
  var currentIteration = 0

  def increment()
  {
    currentIteration += 1
    if (currentIteration == totalIterations)
      println(" Done ")
    else if (totalIterations >= 100)
    {
      val roundedTotal = Math.floor(totalIterations / 100)
      if ( (currentIteration % roundedTotal) == 0 )
      {
        if (currentIteration / roundedTotal % 10 == 0)
          printf("%.0f", currentIteration / roundedTotal)
        else
          print(".")
      }
    }
    else
    {
      val ratio = currentIteration/totalIterations
      val percentComplete = (100*ratio)
      var i = 0
      while (i < ratio)
      {
        print(".")
        i += 1
      }
      if (percentComplete % 10 == 0)
        printf("%d", percentComplete)
      else
        print(".")
    }
  }
}
