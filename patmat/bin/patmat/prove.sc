package patmat
import Huffman._

object prove {
	def string2Chars(str: String): List[Char] = str.toList
                                                  //> string2Chars: (str: String)List[Char]
	
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
  }                                               //> times: (chars: List[Char])List[(Char, Int)]
  
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
    }                                             //> makeOrderedLeafList: (freqs: List[(Char, Int)])List[patmat.Huffman.Leaf]
  
  val testo = string2Chars("domaniandiamoalmarechecisonotanteonde")
                                                  //> testo  : List[Char] = List(d, o, m, a, n, i, a, n, d, i, a, m, o, a, l, m, 
                                                  //| a, r, e, c, h, e, c, i, s, o, n, o, t, a, n, t, e, o, n, d, e)
  val counter = times(testo)                      //> counter  : List[(Char, Int)] = List((d,3), (o,5), (m,3), (a,6), (n,5), (i,3
                                                  //| ), (l,1), (r,1), (e,4), (c,2), (h,1), (s,1), (t,2))
  
  val foglie = makeOrderedLeafList(counter)       //> foglie  : List[patmat.Huffman.Leaf] = List(Leaf(s,1), Leaf(h,1), Leaf(r,1),
                                                  //|  Leaf(l,1), Leaf(t,2), Leaf(c,2), Leaf(i,3), Leaf(m,3), Leaf(d,3), Leaf(e,4
                                                  //| ), Leaf(n,5), Leaf(o,5), Leaf(a,6))
  
  val alberetto = createCodeTree(testo)           //> alberetto  : patmat.Huffman.CodeTree = Fork(Fork(Fork(Leaf(d,3),Fork(Leaf(t
                                                  //| ,2),Leaf(c,2),List(t, c),4),List(d, t, c),7),Fork(Fork(Fork(Leaf(r,1),Leaf(
                                                  //| l,1),List(r, l),2),Fork(Leaf(s,1),Leaf(h,1),List(s, h),2),List(r, l, s, h),
                                                  //| 4),Leaf(e,4),List(r, l, s, h, e),8),List(d, t, c, r, l, s, h, e),15),Fork(F
                                                  //| ork(Leaf(n,5),Leaf(o,5),List(n, o),10),Fork(Fork(Leaf(i,3),Leaf(m,3),List(i
                                                  //| , m),6),Leaf(a,6),List(i, m, a),12),List(n, o, i, m, a),22),List(d, t, c, r
                                                  //| , l, s, h, e, n, o, i, m, a),37)
 
  val tab = convert(alberetto)                    //> tab  : patmat.Huffman.CodeTable = List((d,List(0, 0, 0)), (t,List(0, 0, 1, 
                                                  //| 0)), (c,List(0, 0, 1, 1)), (r,List(0, 1, 0, 0, 0)), (l,List(0, 1, 0, 0, 1))
                                                  //| , (s,List(0, 1, 0, 1, 0)), (h,List(0, 1, 0, 1, 1)), (e,List(0, 1, 1)), (n,L
                                                  //| ist(1, 0, 0)), (o,List(1, 0, 1)), (i,List(1, 1, 0, 0)), (m,List(1, 1, 0, 1)
                                                  //| ), (a,List(1, 1, 1)))
  
  val provo = combine(List(new Leaf('c', 5)))     //> provo  : List[patmat.Huffman.CodeTree] = List(Leaf(c,5))
  val provetto = combine(List())                  //> provetto  : List[patmat.Huffman.CodeTree] = List()
  
  
//  val coded = quickEncode(alberetto)(string2Chars("occoloco"))
  
//  val word = decode(alberetto, coded)
  
  
}