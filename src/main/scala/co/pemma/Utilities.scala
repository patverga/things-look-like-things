package co.pemma

/**
 * Created by pv on 7/4/14.
 */
object Utilities
{

  /**
   * prints the percentage complete in nice format for i of k iterations
   * @param iteration current iteration
   * @param total number of iterations
   * @return incremented iteration
   */
  def printPercentProgress(iteration : Double, total : Double) : Double =
  {
    val it = iteration + 1
    if (it == total)
      println(" Done ")
    else if (total >= 100)
    {
      val roundedTotal = Math.floor(total / 100)
      if ( (it % roundedTotal) == 0 )
      {
        if (it / roundedTotal % 10 == 0)
          printf("%.0f", it / roundedTotal)
        else
          print(".")
      }
    }
    else
    {
      val percentComplete = (100*(it/total))
      var i = 0
      while (i < (it/total))
      {
        print(".")
        i += 1
      }
      if (percentComplete % 10 == 0)
        printf("%d", percentComplete)
      else
        print(".")
    }
    it
  }


}
