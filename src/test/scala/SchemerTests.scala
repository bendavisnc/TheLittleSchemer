import TheLittleScalaSchemer._

class SchemerTests extends org.specs2.mutable.Specification {
  "Chapter 2:" >> {

    "isLat" >> {
      isLat(List("bacon", "and", "eggs")) must_== true
    }

    "isMember" >> {
      isMember("meat", List("mashed", "potatos", "and", "meat", "gravy")) must_== true
    }
  }

  "Chapter 3:" >> {

    "rember" >> {
      rember("bacon", List("bacon", "lettuce", "and", "tomato")) must_== List("lettuce", "and", "tomato")
    }

    "firsts" >> {
      firsts(List(List("a", "b"), List("c", "d"), List("e", "f"))) must_== List("a", "c", "e")
    }

    "insertR" >> {
      insertR("topping", "fudge", List("ice", "cream", "with", "fudge", "for", "dessert")) must_== List("ice", "cream", "with", "fudge", "topping", "for", "dessert")
    }

    "subst" >> {
      subst("topping", "fudge", List("ice", "cream", "with", "fudge", "for", "dessert")) must_== List("ice", "cream", "with", "topping", "for", "dessert")
    }

    "subst2" >> {
      subst2("topping", "fudge", "cream", List("ice", "cream", "with", "fudge", "for", "dessert")) must_== List("ice", "topping", "with", "fudge", "for", "dessert")
    }

    "multirember" >> {
      multirember("bacon", List("bacon", "lettuce", "and", "tomato", "bacon")) must_== List("lettuce", "and", "tomato")
    }
  }

  "Chapter 4:" >> {
    "plus" >> {
      plus(3, 4) must_== 7
    }
    "minus" >> {
      minus(7, 4) must_== 3
    }

    "addtup" >> {
      addtup(List(3, 5, 2, 8)) must_== 18
    }

    "x" >> {
      x(3, 6) must_== 18
    }

    "tupPlus" >> {
      tupPlus(List(1,2,3), List(4,5,6)) must_== List(5, 7, 9)
      tupPlus(List(1,2), List(4,5,6)) must_== List(5, 7, 6)
      tupPlus(List(1,2,3), List(4,5)) must_== List(5, 7, 3)
    }

    "greaterThan" >> {
      TheLittleScalaSchemer.greaterThan(3, 6) must_== false
      TheLittleScalaSchemer.greaterThan(7, 6) must_== true
      TheLittleScalaSchemer.greaterThan(3, 3) must_== false
    }

    "lessThan" >> {
      TheLittleScalaSchemer.lessThan(3, 6) must_== true
      TheLittleScalaSchemer.lessThan(7, 6) must_== false
      TheLittleScalaSchemer.lessThan(3, 3) must_== true
    }

    "equalsTo" >> {
      equalsTo(3, 6) must_== false
      equalsTo(7, 6) must_== false
      equalsTo(3, 3) must_== true
    }

    "toThePowerOf" >> {
      toThePowerOf(1, 1) must_== 1
      toThePowerOf(2, 3) must_== 8
      toThePowerOf(5, 3) must_== 125
    }

    "dividedBy" >> {
//      dividedBy(1, 1) must_== 1
//      dividedBy(2, 3) must_== 0
      dividedBy(15, 4) must_== 3
//      dividedBy(8, 3) must_== 2
    }

    "lengthOf" >> {
      lengthOf(1 :: 2 :: 3 :: 8 :: Nil) must_== 4
    }

    "pick" >> {
      pick(4, "lasagna" :: "spegheti" :: "ravioli" :: "macaroni" :: "meatball" :: Nil) must_== "macaroni"
    }

    "rempick" >> {
      rempick(3, "hotdogs" :: "with" :: "hot" :: "mustard" :: Nil) must_== "hotdogs" :: "with" :: "mustard" :: Nil
    }

    "noNums" >> {
      noNums(5 :: "pears" :: 6 :: "prunes" :: 9 :: "dates" :: Nil) must_== "pears" :: "prunes" :: "dates" :: Nil
    }

    "allNums" >> {
      allNums(5 :: "pears" :: 6 :: "prunes" :: 9 :: "dates" :: Nil) must_== 5 :: 6 :: 9 :: Nil
    }

    "isEqan" >> {
      isEqan(5, 3) must_== false
      isEqan(5, 5) must_== true
      isEqan("apples", "bananas") must_== false
      isEqan("apples", "apples") must_== true
    }

    "occur" >> {
      occur(5, 5 :: "pears" :: 6 :: "prunes" :: 9 :: "dates" :: 5 :: Nil) must_== 2
      occur("pears", 5 :: "pears" :: 6 :: "prunes" :: 9 :: "dates" :: 5 :: Nil) must_== 1
    }

    "rempick2" >> {
      rempick2(3, "lemon" :: "merinque" :: "salty" :: "pie" :: Nil) must_== "lemon" :: "merinque" :: "pie" :: Nil
    }
  }

  "Chapter 5:" >> {

    "remberStar" >> {
      val p2 = (("coffee" :: Nil) :: "cup" :: (("tea" :: Nil) :: "cup" :: Nil) ::
        ("and" :: ("hick" :: Nil) :: Nil) :: "cup":: Nil)
      val expectedResult = (("coffee" :: Nil)  :: (("tea" :: Nil) :: Nil) ::
        ("and" :: ("hick" :: Nil) :: Nil) :: Nil)
      remberStar("cup", p2) must_== expectedResult
    }

    "insertRStar" >> {
      val p3 =
        List(
          List("how", "much", List("wood")),
          "could",
          List(List("a", List("wood"), "chuck")),
          List(List(List("chuck"))),
          List("if", List("a"), List(List("wood", "chuck"))),
          "could", "chuck", "wood"
        )

      val expectedResult =
        List(
          List("how", "much", List("wood")),
          "could",
          List(List("a", List("wood"), "chuck", "roast")),
          List(List(List("chuck", "roast"))),
          List("if", List("a"), List(List("wood", "chuck", "roast"))),
          "could", "chuck", "roast", "wood"
        )
      insertRStar("roast", "chuck", p3) must_== expectedResult

    }
  }
}

