package patmat
import Huffman._

object prove {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(103); 
	def string2Chars(str: String): List[Char] = str.toList;System.out.println("""string2Chars: (str: String)List[Char]""");$skip(531); 
	
	def times(chars: List[Char]): List[(Char, Int)] = {
    def addOne(c: Char, counter: List[(Char, Int)]): List[(Char, Int)] = {
      counter match {
        case List() => List((c, 1))
        case y::ys => if (y._1 == c) {(y._1, y._2 + 1)::ys} else {y::addOne(c, ys)}
      }
    }
    
    def addMany(clist: List[Char], counter: List[(Char, Int)]): List[(Char, Int)] = {
      clist match{
        case List() => counter
        case c::cs => addMany(cs, addOne(c, counter))
      }
    }
    
    addMany(chars, List())
  };System.out.println("""times: (chars: List[Char])List[(Char, Int)]""");$skip(981); 
  
	def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
	  def minCount(l: List[(Char, Int)]): (Char, Int) = {
	    def minGuess(l: List[(Char, Int)], guess: (Char, Int)): (Char, Int) = {
	      l match {
	        case List() => guess
	        case l::ls => if (l._2 > guess._2) minGuess(ls, l) else minGuess(ls, guess)
	      }
	    }
	    minGuess(l, ('.', -1))
	  }
	  
	  def remove(elem: (Char, Int), lista: List[(Char, Int)]): List[(Char, Int)] = {
	    lista match {
	      case List() => List()
	      case p::ps => if (p._1 == elem._1) ps else p::remove(elem, ps)
	    }
	  }
	  
	  def loopedOrder(unordered: List[(Char, Int)], ordered: List[Leaf]): (List[(Char, Int)], List[Leaf]) = {
	    unordered match {
	      case List() => (List(), ordered)
	      case p::ps => loopedOrder(remove(minCount(unordered), unordered),
	          Leaf(minCount(unordered)._1, minCount(unordered)._2)::ordered)
	    }
	  }
      
      loopedOrder(freqs, List())._2
    };System.out.println("""makeOrderedLeafList: (freqs: List[(Char, Int)])List[patmat.Huffman.Leaf]""");$skip(71); 
  
  val testo = string2Chars("domaniandiamoalmarechecisonotanteonde");System.out.println("""testo  : List[Char] = """ + $show(testo ));$skip(29); 
  val counter = times(testo);System.out.println("""counter  : List[(Char, Int)] = """ + $show(counter ));$skip(47); 
  
  val foglie = makeOrderedLeafList(counter);System.out.println("""foglie  : List[patmat.Huffman.Leaf] = """ + $show(foglie ));$skip(43); 
  
  val alberetto = createCodeTree(testo);System.out.println("""alberetto  : patmat.Huffman.CodeTree = """ + $show(alberetto ));$skip(33); 
 
  val tab = convert(alberetto);System.out.println("""tab  : patmat.Huffman.CodeTable = """ + $show(tab ));$skip(49); 
  
  val provo = combine(List(new Leaf('c', 5)));System.out.println("""provo  : List[patmat.Huffman.CodeTree] = """ + $show(provo ));$skip(33); 
  val provetto = combine(List());System.out.println("""provetto  : List[patmat.Huffman.CodeTree] = """ + $show(provetto ))}
  
  
//  val coded = quickEncode(alberetto)(string2Chars("occoloco"))
  
//  val word = decode(alberetto, coded)
  
  
}
