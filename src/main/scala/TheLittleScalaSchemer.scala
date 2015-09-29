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

  def and(b1: Boolean, b2: Boolean) = b1 && b2
  def or(b1: Boolean, b2: Boolean) = b1 || b2

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
      case _ => or(a.equals(l.head), isMember(a, l.tail))
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
      case _ if (or(lat.head.equals(old1), lat.head.equals(old2))) => neww :: lat.tail
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
      case _ => plus(tup.head, addtup(tup.tail))
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
    if (and(isNumber(a1), isNumber(a2))) return equalsTo(a1.asInstanceOf[Int], a2.asInstanceOf[Int])
    else if (or(isNumber(a1), isNumber(a2))) return false
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

  // Pg. 82
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

  /**
   * The First Commandment (final version)
   * When recurring on a list of atoms, lat, ask two questions about it: (null? lat) and else.
   * When recurring on a number, n, ask two questions about it: (zero? n) and else.
   * When recurring on a list of S-expressions, l, ask three questions about it: (null? l), (atom? (car l)), and else.
   */

  /**
   * The Fourth Commandment (final version)
   * Always change at least one argument while recurring.
   * When recurring on a list of atoms, lat, use (cdr lat). When recurring on a number, n, use (sub1 n). And when recurring on a list of S-expressions, l, use (car l) and (cdr l) if neither (null? l) nor (atom? (car l)) are true.
   * It must be changed to be closer to termination. The changing argument must be tested in the termination condition:
   * when using cdr, test termination with null? and
   * when using sub1, test termination with zero?.
   */

  def occurStar(a: Any, lat: List[Any]): Int = {
    if (lat == Nil) return 0
    else if (isAtom(lat.head)) {
      if (isEq(a, lat.head))
        return addOne(occurStar(a, lat.tail))
      else
        return occurStar(a, lat.tail)
    }
    else
      return plus(
          occurStar(a, lat.head.asInstanceOf[List[Any]]),
          occurStar(a, lat.tail)
        )
  }

  def subStar(neww: Any, old: Any, lat: List[Any]): List[Any] = {
    if (lat == Nil) return Nil
    else if (isAtom(lat.head)) {
      if (isEq(old, lat.head))
        return neww :: subStar(neww, old, lat.tail)
      else
        return lat.head :: subStar(neww, old, lat.tail)
    }
    else
      return subStar(neww, old, lat.head.asInstanceOf[List[Any]]) :: subStar(neww, old, lat.tail)
  }

  // Pg. 86
  def insertLStar(neww: Any, old: Any, lat: List[Any]): List[Any] = {
    if (lat == Nil) return Nil
    else if (isAtom(lat.head)) {
      if (isEq(lat.head, old)) return neww :: old :: insertLStar(neww, old, lat.tail)
      else
        return lat.head :: insertLStar(neww, old, lat.tail)
    }
    else
      return insertLStar(neww, old, lat.head.asInstanceOf[List[Any]]) :: insertLStar(neww, old, lat.tail)
  }

  // Pg. 87
  def memberStar(a: Any, lat: List[Any]): Boolean = {
    if (lat == Nil) return false
    else if (isAtom(lat.head)) {
        or((isEq(lat.head, a)), memberStar(a, lat.tail))
    }
    else
      return or(memberStar(a, lat.head.asInstanceOf[List[Any]]), memberStar(a, lat.tail))
  }

  def leftmost(lat: List[Any]): Any = {
    if (lat == Nil) return null
    else if (isAtom(lat.head)) {
      return lat.head
    }
    else
      return leftmost(lat.head.asInstanceOf[List[Any]])
  }

  def isEqList(l1: List[Any], l2: List[Any]): Boolean = {
    if (and(l1 == Nil, l2 == Nil)) return true
    else if (and(l1 == Nil, isAtom(l2.head))) return false
    else if (l1 == Nil) return false
    else if (and(isAtom(l1.head), l2 == null)) return false

    else if (and(isAtom(l1.head), isAtom(l2.head)))
      return and(isEqan(l1.head, l2.head), isEqList(l1.tail, l2.tail))
    else if (isAtom(l1.head)) return false
    else if (l2 == Nil) return false
    else if (isAtom(l2.head)) return false
    else return and(isEqList(l1.head.asInstanceOf[List[Any]], l2.head.asInstanceOf[List[Any]]), isEqList(l1.tail, l2.tail))

  }

  def isEqListV2(l1: List[Any], l2: List[Any]): Boolean = {
    if (and(l1 == Nil, l2 == Nil)) return true
    else if (or(l1 == Nil, l2 == Nil)) return false
    else if (and (isAtom(l1.head), isAtom(l2.head)))
      return and(isEqan(l1.head, l2.head), isEqListV2(l1.tail, l2.tail))
    else if (or(isAtom(l1.head), isAtom(l2.head))) return false
    else return and(isEqListV2(l1.head.asInstanceOf[List[Any]], l2.head.asInstanceOf[List[Any]]), isEqListV2(l1.tail, l2.tail))

  }

  // Pg. 92
  def isEqual(s1: List[Any], s2: List[Any]): Boolean = {
    if (and(isAtom(s1.head), isAtom(s2.head)))
      return (isEqan(s1, s2))
    else if (isAtom(s1))
      return false
    else if (isAtom(s2))
      return false
    else return isEqListV2(s1, s2)

  }

  def isEqualV2(s1: Any, s2: Any): Boolean = {
    if (and (isAtom(s1), isAtom(s2)))
      return (isEqan(s1, s2))
    else if (or(isAtom(s1), isAtom(s2)))
      return false
    else return isEqListV2(s1.asInstanceOf[List[Any]], s2.asInstanceOf[List[Any]])

  }

  def isEqListV3(l1: List[Any], l2: List[Any]): Boolean = {
    if (and(l1 == Nil, l2 == Nil)) return true
    else if (or(l1 == Nil, l2 == Nil)) return false
    else return and(isEqualV2(l1.head.asInstanceOf[List[Any]], l2.head.asInstanceOf[List[Any]]), isEqListV3(l1.tail, l2.tail))
  }

  /**
   * The Sixth Commandment
   * Simplify only after the function is correct
   */

  /**
   * Chapter 6
   */
  def isNumbered(aexp: Any): Boolean = {
    if (isAtom(aexp)) return isNumber(aexp)
    else if (isEq(aexp.asInstanceOf[List[Any]].tail.head, '+))
      return and(isNumbered(aexp.asInstanceOf[List[Any]].head), isNumbered(aexp.asInstanceOf[List[Any]].tail.tail.head))
    else if (isEq(aexp.asInstanceOf[List[Any]].tail.head, '*))
      return and(isNumbered(aexp.asInstanceOf[List[Any]].head), isNumbered(aexp.asInstanceOf[List[Any]].tail.tail.head))
    else if (isEq(aexp.asInstanceOf[List[Any]].tail.head, '^))
      return and(isNumbered(aexp.asInstanceOf[List[Any]].head), isNumbered(aexp.asInstanceOf[List[Any]].tail.tail.head))
    else
      throw new Exception()
  }

  def isNumberedV2(aexp: Any): Boolean = {
    if (isAtom(aexp)) return isNumber(aexp)
    else
      return and(isNumberedV2(aexp.asInstanceOf[List[Any]].head), isNumberedV2(aexp.asInstanceOf[List[Any]].tail.tail.head))
  }



}
