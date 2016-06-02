//import forcomp.Anagrams._

object prove {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(260); 

	def wordOccurrences(w: String): List[(Char, Int)] = {
    if (w.isEmpty) List()
    else{
        for (pair <- w.toLowerCase.groupBy { x => x })
          yield (pair._1, pair._2.length)
        }.toList.sorted
  };System.out.println("""wordOccurrences: (w: String)List[(Char, Int)]""");$skip(213); 
  
  def combinations(occurrences: List[(Char, Int)]): List[List[(Char, Int)]] = {
    
    val amico = for {
    	(c, n) <- occurrences
    } yield {for {
    k <- 0 to n
    } yield (c, k)}.toList
    amico
  };System.out.println("""combinations: (occurrences: List[(Char, Int)])List[List[(Char, Int)]]""");$skip(24); 
  
  val primo = "Caco";System.out.println("""primo  : String = """ + $show(primo ));$skip(34); 
  val secondo = primo.toLowerCase;System.out.println("""secondo  : String = """ + $show(secondo ));$skip(38); 
  val mappo = secondo.groupBy(c => c);System.out.println("""mappo  : scala.collection.immutable.Map[Char,String] = """ + $show(mappo ));$skip(81); 
  
  val terzo = {for ((car, cari) <- mappo)
  	yield (car, cari.length)}.toList;System.out.println("""terzo  : List[(Char, Int)] = """ + $show(terzo ));$skip(32); 
  	
  val quarto = terzo.sorted;System.out.println("""quarto  : List[(Char, Int)] = """ + $show(quarto ));$skip(28); val res$0 = 
  
  wordOccurrences(primo);System.out.println("""res0: List[(Char, Int)] = """ + $show(res$0));$skip(39); val res$1 = 
  combinations(wordOccurrences(primo));System.out.println("""res1: List[List[(Char, Int)]] = """ + $show(res$1))}
}
