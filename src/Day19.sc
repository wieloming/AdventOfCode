//brute force
case class Replacer(from: String, to: String) {
  def replace(string: String): List[String] = {
    string.toList match {
      case x :: Nil => List(string.replaceFirst(from, to))
      case x :: xs => string.replaceFirst(from, to) :: replace(string.tail).map(x + _)
    }
    from.r.findAllIn(string).matchData.map(f => {
      val parts = string.splitAt(f.start)
      parts._1 + to ++ parts._2.drop(from.length)
    }).toList.distinct
  }
}
val inputDict = "Al => ThF\nAl => ThRnFAr\nB => BCa\nB => TiB\nB => TiRnFAr\nCa => CaCa\nCa => PB\nCa => PRnFAr\nCa => SiRnFYFAr\nCa => SiRnMgAr\nCa => SiTh\nF => CaF\nF => PMg\nF => SiAl\nH => CRnAlAr\nH => CRnFYFYFAr\nH => CRnFYMgAr\nH => CRnMgYFAr\nH => HCa\nH => NRnFYFAr\nH => NRnMgAr\nH => NTh\nH => OB\nH => ORnFAr\nMg => BF\nMg => TiMg\nN => CRnFAr\nN => HSi\nO => CRnFYFAr\nO => CRnMgAr\nO => HP\nO => NRnFAr\nO => OTi\nP => CaP\nP => PTi\nP => SiRnFAr\nSi => CaSi\nTh => ThCa\nTi => BP\nTi => TiTi\ne => HF\ne => NAl\ne => OMg"
val inputTarget = "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr"
val start = List("e")
val dicts = inputDict
  .split('\n')
  .map(_
    .split(" => ")
    .toList match { case List(from, to) => Replacer(from, to)
  })
  .toList
var iterations = 0
var result = start
var d = dicts
while (!result.contains(inputTarget)) {
  result = result
    .flatMap { el =>
      val p = d
        .flatMap(_.replace(el))
        .filterNot(_ == el)
        .distinct
      d = d.filterNot(d => d.from == el || d.to == el)
      p
    }
  println(result)
  iterations += 1
}
iterations


// clever answer from reddit
import scala.util.Random
import scala.collection.breakOut

val inputDictionary = "Al => ThF\nAl => ThRnFAr\nB => BCa\nB => TiB\nB => TiRnFAr\nCa => CaCa\nCa => PB\nCa => PRnFAr\nCa => SiRnFYFAr\nCa => SiRnMgAr\nCa => SiTh\nF => CaF\nF => PMg\nF => SiAl\nH => CRnAlAr\nH => CRnFYFYFAr\nH => CRnFYMgAr\nH => CRnMgYFAr\nH => HCa\nH => NRnFYFAr\nH => NRnMgAr\nH => NTh\nH => OB\nH => ORnFAr\nMg => BF\nMg => TiMg\nN => CRnFAr\nN => HSi\nO => CRnFYFAr\nO => CRnMgAr\nO => HP\nO => NRnFAr\nO => OTi\nP => CaP\nP => PTi\nP => SiRnFAr\nSi => CaSi\nTh => ThCa\nTi => BP\nTi => TiTi\ne => HF\ne => NAl\ne => OMg"
val mol = "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr"
var reps: List[(String,String)] = inputDictionary
  .split('\n')
  .map(_
    .split(" => ")
    .toList match { case List(from, to) => (from, to)}
   )(breakOut)
var target = mol
var mutations = 0
while (target != "e") {
  val tmp = target
  for ((a, b) <- reps) {
    if (!target.contains(b)) {
      target = target.replaceFirst(a, b)
      mutations += 1
      println(mutations)
    }
  }
  if (tmp == target)
    target = mol
    mutations = 0
    reps = Random.shuffle(reps)
}
println(mutations)

