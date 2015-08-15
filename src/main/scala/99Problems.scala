import scala.util.Random._

object Problems extends App {
  //Problem #1
  def last(lst: List[Any]):Any = {
    lst match {
      case head :: Nil => head
      case head :: tail => last(tail)
    }
  }

  //Problem #2
  def penultimate(lst: List[Any]):Any = {
    lst match {
      case head :: tail :: Nil => head
      case head :: tail => penultimate(tail)
    }
  }

  //Problem #3
  def nth(n: Int, lst: List[Any]):Any = {
    lst match {
      case head :: tail => {
        if(n == 0) {
          head
        } else nth(n - 1, tail)
      }
      case head :: Nil => 
        println("Kth element does not exist, returning last element of list") 
        head
    }
  }

  //Problem #4
  def length(lst: List[Any]): Int = {
    def recurseCounter(in: List[Any], length: Int): Int = {
      in match {
        case Nil => length
        case head :: tail => recurseCounter(tail, length + 1)
      }
    }

    recurseCounter(lst, 0)
  }

  //Problem #5
  def reverse(lst: List[Any]):List[Any] = {
    def reverseRecursor(in: List[Any], ans: List[Any]):List[Any] = {
      in match {
        case Nil => ans
        case head :: tail => reverseRecursor(tail, head :: ans)
      }
    }
    
    reverseRecursor(lst, List[Any]())
  }

  //Problem #6 
  def isPalindrome(lst: List[Any]): Boolean = {
    val rev = reverse(lst)
    def palindromeCheck(in: List[Any], rev: List[Any]): Boolean = {
      in match {
        case head :: tail => 
          if(head == rev.head) {
            palindromeCheck(tail, rev.tail)
          } else {
            false
          }
        case Nil => true
      }
    }

    palindromeCheck(lst, rev) 
  }

  //Problem #7
  def flatten(lst: Any): List[Any] = {
    def flattenRecursor(in: Any, ans: List[Any]): List[Any] = {
      in match {
        case head :: tail => {
          head match {
            case chkList: List[Any] => flattenRecursor(tail, flattenRecursor(chkList, List[Any]()) ::: ans) 
            case _ => flattenRecursor(tail, head :: ans)
          }
        }
        case Nil => ans
      }
    }

    reverse(flattenRecursor(lst, List[Any]()))
  }

  //Problem #8
  def compress(lst: List[Any]): List[Any] = {
    def compressRecursor(in: List[Any], ans: List[Any], tmp: Any): List[Any] = {
      in match {
        case head :: tail => {
          if(head != tmp) {
            compressRecursor(tail, ans :+ head, head) 
          } else compressRecursor(tail, ans, tmp)
        }
        case Nil => ans
      }
    }

    compressRecursor(lst, List[Any](), null)
  }

  //Problem #9
  def pack(lst: List[Any]): List[List[Any]] = {
    def listMaker(in: List[Any], ans: (List[Any], List[Any]), tmp: Any): (List[Any], List[Any]) = {
      in match {
        case head :: tail => {
          if(head == tmp) {
            listMaker(tail, (tail, ans._2 :+ head), tmp)
          } else ans
        }
        case Nil => ans
      }
    }

    def packRecursor(in: List[Any], ans: List[List[Any]], tmp: Any): List[List[Any]] = {
      in match {
        case head :: tail => {
          if(head != tmp) {
            val (tmp1,tmp2) = listMaker(in, (List[Any](),List[Any]()), head)

            packRecursor(tmp1, ans :+ tmp2, head)
          }
          else ans 
        }
        case Nil => ans
      }
    }

    packRecursor(lst, List[List[Any]](), null)
  }


  //Problem #10
  def encode(lst: List[Any]): List[(Int,Any)] = {
    def encodeRecursor(in: List[List[Any]], ans: List[(Int, Any)]): List[(Int, Any)] = {
      in match {
        case head :: tail => {
          head match {
            case x :: y => encodeRecursor(tail, ans :+ (length(head), x))
            case Nil => encodeRecursor(tail, ans)
          }
        }
        case Nil => ans
      }
    }

    encodeRecursor(pack(lst), List[(Int,Any)]())
  }

  //Problem #11
   def encodeModified(lst: List[Any]): List[Any] = {
    def encodeRecursor(in: List[List[Any]], ans: List[Any]): List[Any] = {
      in match {
        case head :: tail => {
          head match {
            case x :: y => {
              if(length(head) == 1) encodeRecursor(tail, ans :+ x)
              else encodeRecursor(tail, ans :+ (length(head), x))
            }
            case Nil => encodeRecursor(tail, ans)
          }
        }
        case Nil => ans
      }
    }

    encodeRecursor(pack(lst), List[(Int,Any)]())
  }

  //Problem #12
  def decode(lst: List[(Int, Any)]): List[Any] = {
    def decodeRecursor(curr: Int, in: List[(Int,Any)], ans: List[Any]): List[Any] = {
      in match {
        case head :: tail => {
          if(curr > 0) decodeRecursor(curr - 1, in, ans :+ head._2)
          else if(curr == -1) decodeRecursor(head._1, in, ans)
          else decodeRecursor(-1, tail, ans)
        }
        case Nil => ans
      }
    }

    lst match {
      case head :: tail => decodeRecursor(head._1, lst, List[Any]())
      case Nil => List[Any]()
    }
  }

  //Problem #13
  def encodeDirect(lst: List[Any]): List[Any] = {
    def encodeDirectRecursor(cnt: Int, in: List[Any], ans: List[Any]): List[Any] = {
       in match {
        case head :: tail :: rest => {
          if(head == tail) encodeDirectRecursor(cnt + 1, tail::rest, ans)
          else encodeDirectRecursor(1, tail::rest, ans :+ (cnt, head))
        }
        case head :: Nil => ans :+ (cnt, head)
        case Nil => ans
       }
    }

    encodeDirectRecursor(1, lst, List[(Int,Any)]())
  }

  //Problem #14
  def duplicate(lst: List[Any]): List[Any] = {
    def duplicateRecursor(in: List[Any], ans: List[Any]): List[Any] = {
      in match {
        case head :: tail => duplicateRecursor(tail, ans :+ head :+ head)
        case Nil => ans
      }
    }

    duplicateRecursor(lst, List[Any]())
  }

  //Problem #15
   def duplicateN(n: Int, lst: List[Any]): List[Any] = {
    def nListBuilder(num: Int, elem: Any, ans: List[Any]): List[Any] = {
      if(num > 0) {
        nListBuilder(num - 1, elem, elem :: ans)
      } else ans
    }
    def duplicateRecursor(in: List[Any], ans: List[Any]): List[Any] = {
      in match {
        case head :: tail => {
          val tempList = nListBuilder(n, head, List[Any]())
          duplicateRecursor(tail, tempList ::: ans)
        }
        case Nil => ans
      }
    }

    reverse(duplicateRecursor(lst, List[Any]()))
  }

  //Problem #16
  def drop(n: Int, lst: List[Any]): List[Any] = {
    def dropRecursor(ctr: Int, in: List[Any], ans: List[Any]): List[Any] = {
      in match {
        case head :: tail => {
          if(ctr == 1) {
            dropRecursor(n, tail, ans)
          } else dropRecursor(ctr - 1, tail, ans :+ head)
        }
        case Nil => ans
      }
    }

    dropRecursor(n, lst, List[Any]())
  }

  //Problem #17
  def split(n: Int, lst: List[Any]):(List[Any],List[Any]) = {
    def nListBuilder(num: Int, inLst: List[Any], ans: List[Any]): List[Any] = {
      inLst match {
        case head :: tail => {
          if(num > 0) {
          nListBuilder(num - 1, tail, head :: ans)
          } else ans
        } 
        case Nil => ans
      }
    }

    def listTwo(num: Int, inLst: List[Any]): List[Any] = {
      inLst match {
        case head :: tail => {
          if(num > 0) {
            listTwo(num - 1, tail)
          } else inLst
        }
        case Nil => inLst
      }
    }

    (reverse(nListBuilder(n, lst, List[Any]())), listTwo(n, lst)) 
  }

  //Problem #18
  def slice(beg: Int, end: Int, lst: List[Any]):List[Any] = {
    def sliceRecursor(curr: Int, beg: Int, end: Int, in: List[Any], ans: List[Any]):List[Any] = {
      in match {
        case head :: tail => {
          if(curr < beg) {
            sliceRecursor(curr + 1, beg, end, tail, ans)
          } else if(curr >= beg && curr < end) {
            sliceRecursor(curr + 1, beg, end, tail, ans :+ head)
          } else ans
        }
        case Nil => ans
      }
    }

    sliceRecursor(0, beg, end, lst, List[Any]())
  }

  //Problem #19
  def rotate(n: Int, lst: List[Any]): List[Any] = {
    def rotateRecursor(curr: Int, in: List[Any], ans: List[Any]): List[Any] = {
      in match {
        case head :: tail => {
          if(n > 0) {
            if(curr < n) rotateRecursor(curr + 1, tail, ans :+ head)
            else in ::: ans
          } else reverse(rotate(-n, reverse(lst)))
        }
        case Nil => in ::: ans
      }
    }

    rotateRecursor(0, lst, List[Any]())
  }

  //Problem #20
  def removeAt(n: Int, lst: List[Any]): (List[Any],Any) = {
    def removeAtRecursor(curr: Int, n: Int, in: List[Any], ans: List[Any], tmp: Any): (List[Any],Any) = {
      in match {
        case head :: tail => {
          if(curr != n) {
            removeAtRecursor(curr + 1, n, tail, ans :+ head, tmp)
          } else removeAtRecursor(curr + 1, n, tail, ans, head)
        }
        case Nil => (ans, tmp)
      }
    }

    removeAtRecursor(0, n, lst, List[Any](), null)
  }

  //Problem #21 
  def insertAt(elem: Any, n: Int, lst: List[Any]): List[Any] = {
    def instertAtRecursor(elem: Any, loc: Int, in: List[Any], ans: List[Any]): List[Any] = {
      in match {
        case head :: tail => {
          if(loc > 0) {
            instertAtRecursor(elem, loc - 1, tail, ans :+ head)
          } else (ans :+ elem :+ head) ::: tail
        }
        case Nil => ans
      }
    }

    instertAtRecursor(elem, n, lst, List[Any]())
  }

  //Problem #22
  def range(low: Int, high: Int): List[Any] = {
    def rangeRecursor(curr: Int, max: Int, ans: List[Any]):List[Any] = {
      if(curr > max) ans
      else rangeRecursor(curr + 1, max, ans :+ curr)
    }

    rangeRecursor(low, high, List[Any]())
  }

  //Problem #23
  def randomSelect(n: Int, lst: List[Any]): List[Any] = {
    def randomRecursor(n: Int, in: List[Any], ans: List[Any]): List[Any] = {
      in match {
        case head :: tail => {
          if(n > 0) {
            val (tmp1,tmp2) = removeAt(nextInt(length(in)), in)
            randomRecursor(n - 1, tmp1, ans :+ tmp2)
          } else ans
        }
        case Nil => ans
      }
    }

    randomRecursor(n, lst, List[Any]())
  }

  //Problem #24
  def lotto(n: Int, end: Int): List[Any] = {
    val rng = range(1, end)
    randomSelect(n, rng)
  }

  //Problem #25
  def randomPermute(lst: List[Any]): List[Any] = {
    randomSelect(length(lst), lst)
  }

  //Test Values
  val lst = List[Any](1,1,2,3,5,8)
  println(last(lst))
  println(penultimate(lst))
  println(nth(2, lst))
  println(length(lst))
  println(reverse(lst))
  println(isPalindrome(List[Any](1,2,1)))
  println(isPalindrome(lst))
  println(flatten(List(List(1,1), 2, List(3, List(5, 8)))))
  println(compress(lst))
  println(pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
  println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  println(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  println(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
  println(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  println(duplicate(List('a', 'b', 'c', 'c', 'd')))
  println(duplicateN(3, List('a', 'b', 'c', 'c', 'd')))
  println(drop(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  println(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println(removeAt(1, List('a, 'b, 'c, 'd)))
  println(insertAt('new, 1, List('a, 'b, 'c, 'd)))
  println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
  println(range(4, 9))
  println(lotto(6,49))
  println(randomPermute(List('a, 'b, 'c, 'd)))
}
