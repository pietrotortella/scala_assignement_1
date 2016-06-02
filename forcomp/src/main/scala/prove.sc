//import forcomp.Anagrams._

object prove {

	def wordOccurrences(w: String): List[(Char, Int)] = {
    if (w.isEmpty) List()
    else{
        for (pair <- w.toLowerCase.groupBy { x => x })
          yield (pair._1, pair._2.length)
        }.toList.sorted
  }                                               //> wordOccurrences: (w: String)List[(Char, Int)]
  
  def combinations(occurrences: List[(Char, Int)]): List[List[(Char, Int)]] = {
    
    val amico = for {
    	(c, n) <- occurrences
    } yield {for {
    k <- 0 to n
    } yield (c, k)}.toList
    amico
  }                                               //> combinations: (occurrences: List[(Char, Int)])List[List[(Char, Int)]]
  
  val primo = "Caco"                              //> primo  : String = Caco
  val secondo = primo.toLowerCase                 //> secondo  : String = caco
  val mappo = secondo.groupBy(c => c)             //> mappo  : scala.collection.immutable.Map[Char,String] = Map(a -> a, c -> cc, 
                                                  //| o -> o)
  
  val terzo = {for ((car, cari) <- mappo)
  	yield (car, cari.length)}.toList          //> terzo  : List[(Char, Int)] = List((a,1), (c,2), (o,1))
  	
  val quarto = terzo.sorted                       //> quarto  : List[(Char, Int)] = List((a,1), (c,2), (o,1))
  
  wordOccurrences(primo)                          //> res0: List[(Char, Int)] = List((a,1), (c,2), (o,1))
  combinations(wordOccurrences(primo))            //> res1: List[List[(Char, Int)]] = List(List((a,0), (a,1)), List((c,0), (c,1), 
                                                  //| (c,2)), List((o,0), (o,1)))
}