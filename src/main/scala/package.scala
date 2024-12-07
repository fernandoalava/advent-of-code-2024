object Utils {
  def withExecutionTime[T](block: => T): (T, Double) = {
    val startTime = System.nanoTime()
    val result = block
    val endTime = System.nanoTime()
    val elapsedTime = (endTime - startTime) / 1e6
    (result, elapsedTime)
  }
}
