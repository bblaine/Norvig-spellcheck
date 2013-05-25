import scala.io.Source._

def words(text: String) = "[a-z]+".r.findAllIn(text.toLowerCase).toList

val alphabet = "abcdefghijklmnopqrstuvwxyz"

def train(features: List[String]): Map[String, Int] = features.groupBy(identity).mapValues(_.size)

val nwords = train(words(fromFile("big.txt").mkString))

def edits1(word: String) = {
   val splits = for(i <- 0 to word.size) yield word.splitAt(i)
   val deletes = for((a,b) <- splits if b != "") yield a + b.tail
   val transposes = for((a,b) <- splits if b.size > 1) yield a + b(1) + b.takeRight(b.size - 2)
   val replaces = for((a,b) <- splits; c <- alphabet; if b != "") yield a + c + b.takeRight(b.size -1)
   val inserts = for((a,b) <- splits; c <- alphabet) yield a + c + b
   (deletes ++ transposes ++ replaces ++ inserts).toList
}

def knownEdits(word: String) = (for(e1 <- edits1(word); e2 <- edits1(e1) if nwords.contains(e2)) yield e2).toSet

def known(words: List[String]) = (for (w <- words if nwords.contains(w)) yield w).toSet

def correct(word: String) = {
  val candidates = List(known(List(word)), known(edits1(word)), knownEdits(word), Set(word)).filterNot(_.isEmpty).head
  candidates.maxBy(nwords.get(_))
}
