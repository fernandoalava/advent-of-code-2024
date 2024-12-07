object Utils {
  def withExecutionTime[T](block: => T): (T, Double) = {
    val startTime = System.nanoTime()
    val result = block
    val endTime = System.nanoTime()
    val elapsedTime = (endTime - startTime) / 1e6
    (result, elapsedTime)
  }

  def memoize[K, V](f: K => V): K => V = {
    val cache = scala.collection.mutable.Map.empty[K, V]
    (key: K) => cache.getOrElseUpdate(key, f(key))
  }
}
