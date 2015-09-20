/**
 * This is me going through the book "The Little Schemer", and working through it in Scala
 */
object TheLittleScalaSchemer {

  // self defined helpers
  def isAtom(a: Any): Boolean = {
    return a.isInstanceOf[List[Any]] == false
  }
  def isZero(n: Int) = n == 0
  def addOne(n: Int) = n + 1
  def subOne(n: Int) = n - 1
  // Pg. 77
  def isNumber(a: Any) = a.isInstanceOf[Int]
  def isEq(a1: Any, a2: Any) = a1 == a2

   /**
   * Chapter 2
   */

  // Pg 16
  def isLat(l: List[Any]): Boolean = {
    l match {
      case Nil => true
      case _ if isAtom(l.head) => isLat(l.tail)
      case _ => false
    }
  }


  // Pg 22
  def isMember(a: Any, l: List[Any]): Boolean = {
    l match {
      case Nil => false
      case _ => a.equals(l.head) || isMember(a, l.tail)
    }
  }

  /**
   * The First Commandment:
   * Always ask null? as the first question in expressing any function
   */

   /**
   * Chapter 3
   */

  // Pg 34

  def rember(a: Any, l: List[Any]): List[Any] = {
    l match {
      case Nil => l
      case _ if a.equals(l.head) => l.tail
      case _ => l.head :: rember(a, l.tail)
    }
  }

  /**
   * The Second Commandment:
   * Use cons to build lists
   */

   // Pg. 44
  def firsts(l: List[List[Any]]): List[Any] = {
    l match {
      case Nil => Nil
      case _ => l.head.head :: firsts(l.tail)
    }
  }

  /**
   * The Third Commandment:
   * When building a list, describe the first typical element, and then cons it onto the natural recursion
   */

  // Pg. 50
  def insertR(neww: Any, old: Any, lat: List[Any]): List[Any] = {
    lat match {
      case Nil => Nil
      case _ if lat.head.equals(old) => old :: neww :: lat.tail
      case _ => lat.head :: insertR(neww, old, lat.tail)
    }
  }


  // Pg. 51
  def subst(neww: Any, old: Any, lat: List[Any]): List[Any] = {
    lat match {
      case Nil => Nil
      case _ if lat.head.equals(old) => neww :: lat.tail
      case _ => lat.head :: subst(neww, old, lat.tail)
    }
  }

  // Pg. 52
  def subst2(neww: Any, old1: Any, old2: Any, lat: List[Any]): List[Any] = {
    lat match {
      case Nil => Nil
      case _ if lat.head.equals(old1) || lat.head.equals(old2) => neww :: lat.tail
      case _ => lat.head :: subst2(neww, old1, old2, lat.tail)
    }
  }


  // Pg. 53
  def multirember(a: Any, l: List[Any]): List[Any] = {
    l match {
      case Nil => l
      case _ if a.equals(l.head) => multirember(a, l.tail)
      case _ => l.head :: multirember(a, l.tail)
    }
  }

  /**
   * The Fourth Commandment:
   * Always change at least one argument while recurring. It must be changed to be closer to termination. The changing argument must be tested in the termination condition: when using cdr, test termination with null?.
   */

  /**
  * Chapter 4
  */

   // Pg. 60

   def plus(n: Int, m: Int): Int = {
      m match {
        case _ if isZero(m) => n
        case _ => plus(addOne(n), subOne(m))
      }

   }


  def minus(n: Int, m: Int): Int = {
    m match {
      case _ if isZero(m) => n
      case _ => minus(subOne(n), subOne(m))
    }
  }


  /**
   * The First Commandment: (first revision)
   * When recurring on a list of atoms, lat, ask two questions about it: (null? lat) and else. When recurring on a number, n, ask two questions about it: (zero? n) and else
   */

  // Pg. 64

  def addtup(tup: List[Int]): Int = {
    tup match {
      case _ if (tup.isEmpty) => 0
      case _ => tup.head + addtup(tup.tail)
    }
  }

  // Pg. 65
  def x(n: Int, m: Int): Int = {
    if (isZero(n)) return 0
    else return plus(m, x(subOne(n), m))
  }

  /**
   * The Fifth Commandment:
   * When building a value with +, always use 0 for the value of the terminating line, for adding 0 does not change the value of an addition.
   * When building a value with x, always use 1 for the value of the terminating line, for multiplying by 1 does not change the value of a multiplication.
   * When building a value with cons, always consider () for the value of the terminating line.
   */

  // Pg. 69
  def tupPlus(tup1: List[Int], tup2: List[Int]): List[Int] = {
    if (tup1.isEmpty) tup2
    else if (tup2.isEmpty) tup1
    else return plus(tup1.head, tup2.head) :: tupPlus(tup1.tail, tup2.tail)
  }

  // Pg. 72
  def greaterThan(m: Int, n: Int): Boolean = {
    if (isZero(m)) return false
    else if (isZero(n)) return true
    return greaterThan(subOne(m), subOne(n))
  }

  def lessThan(m: Int, n: Int): Boolean = {
    if (isZero(m)) return true
    else if (isZero(n)) return false
    return lessThan(subOne(m), subOne(n))
  }

  def equalsTo(m: Int, n: Int): Boolean = {
    if (isZero(m)) return isZero(n)
    return equalsTo(subOne(m), subOne(n))
  }

  def toThePowerOf(m: Int, n: Int): Int = {
    if (isZero(n)) return 1
    return x(m, toThePowerOf(m, subOne(n)))
  }

  def dividedBy(m: Int, n: Int): Int = {
    if (lessThan(m, n)) return 0
    return addOne(dividedBy(minus(m, n), n))
  }

  def lengthOf(lat: List[Any]): Int = {
    if (lat.isEmpty) return 0
    return addOne(lengthOf(lat.tail))
  }

  def pick(n: Int, lat: List[Any]): Any = {
    if (isZero(subOne(n))) return lat.head
    return pick(subOne(n), lat.tail)
  }

  // Pg. 77
  def rempick(n: Int, lat: List[Any]): List[Any] = {
    if (isZero(subOne(n))) return lat.tail
    return lat.head :: rempick(subOne(n), lat.tail)
  }

  def noNums(lat: List[Any]): List[Any] = {
    if (lat == Nil) return Nil
    else if (isNumber(lat.head)) return noNums(lat.tail)
    else return lat.head :: noNums(lat.tail)

  }

  def allNums(lat: List[Any]): List[Any] = {
    if (lat == Nil) return Nil
    else if (!isNumber(lat.head)) return allNums(lat.tail)
    else return lat.head :: allNums(lat.tail)

  }

  def isEqan(a1: Any, a2: Any): Boolean = {
    if (isNumber(a1) && isNumber(a2)) return equalsTo(a1.asInstanceOf[Int], a2.asInstanceOf[Int])
    else if (isNumber(a1) || isNumber(a2)) return false
    else return isEq(a1, a2)

  }

  def occur(a: Any, lat: List[Any]): Int = {
    if (lat == Nil) return 0
    else if (isEqan(a, lat.head)) return addOne(occur(a, lat.tail))
    else return occur(a, lat.tail)
  }

  // Pg. 79
  def isOne(n: Int): Boolean = {
    return isEq(n, 1)
  }

  def rempick2(n: Int, lat: List[Any]): List[Any] = {
    if (isOne(n)) return lat.tail
    return lat.head :: rempick(subOne(n), lat.tail)
  }

  /**
   * Chapter 5
   */

  def remberStar(a: Any, lat: List[Any]): List[Any] = {
    if (lat == Nil) return Nil
    else if (isAtom(lat.head)) {
      if (isEq(lat.head, a)) return remberStar(a, lat.tail)
      else
        return lat.head :: remberStar(a, lat.tail)
    }
    else
      return remberStar(a, lat.head.asInstanceOf[List[Any]]) :: remberStar(a, lat.tail)
  }

  def insertRStar(neww: Any, old: Any, lat: List[Any]): List[Any] = {
    if (lat == Nil) return Nil
    else if (isAtom(lat.head)) {
      if (isEq(lat.head, old)) return old :: neww :: insertRStar(neww, old, lat.tail)
      else
        return lat.head :: insertRStar(neww, old, lat.tail)
    }
    else
      return insertRStar(neww, old, lat.head.asInstanceOf[List[Any]]) :: insertRStar(neww, old, lat.tail)
  }

}
