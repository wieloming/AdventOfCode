val containersData = "50\n44\n11\n49\n42\n46\n18\n32\n26\n40\n21\n7\n18\n43\n10\n47\n36\n24\n22\n40"
val containers = containersData.split('\n').toList.map(_.toInt)
def countContainers(liters: Int, containers: List[Int]): List[Int] = {
  if (liters == 0) List(1)
  else if (liters < 0) List()
  else if (containers.isEmpty && liters > 0) List()
  else
    countContainers(liters, containers.tail) ++ countContainers(liters - containers.head, containers.tail).map(_ + 1)
}
val res = countContainers(150, containers)
res.count(_ == res.min)