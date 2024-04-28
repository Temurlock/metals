package tests.pc

import tests.BaseCompletionSuite

class CompletionArgTypeSuite extends BaseCompletionSuite {

  check(
    "arg",
    s"""|object Main {
        |
        |  val a = 12
        |  val b = "string"
        |  def foo(banana: String, a: Int) = ???
        |  foo(@@)
        |}
        |""".stripMargin,
    """b: String
      |banana = b : String
      |a: Int
      |a = a : Int
      |banana = foo : (banana: String, a: Int): Nothing
      |banana = : String
       |""".stripMargin,
    topLines = Option(6)
  )

  check(
    "arg1",
    s"""|object Main {
        |
        |  val a = "string"
        |  val b = "string"
        |  def foo(banana: String, a: Int) = ???
        |  foo(b@@)
        |}
        |""".stripMargin,
    """b: String
      |banana = b : String
      |banana = a : String
      |Autofill with default values(banana: String, a: Int): Nothing
       |""".stripMargin,
    topLines = Option(4)
  )

  check(
    "arg3",
    s"""|object Main {
        |
        |  val a = "string"
        |  val b = 12
        |  def foo(banana: String, a: Int) = ???
        |  foo(a, @@)
        |}
        |""".stripMargin,
    """|b: Int
       |a = b : Int
       |a: String
       |foo(banana: String, a: Int): Nothing
       |a = foo : (banana: String, a: Int): Nothing
       |a = : Int
       |""".stripMargin,
    topLines = Option(6)
  )

  check(
    "arg4",
    s"""|object Main {
        |
        |  val aa = 10
        |  val bb = 12
        |  def foo(bb: Int) = ???
        |  foo(b@@)
        |}
        |""".stripMargin,
    """|bb: Int
       |bb = bb : Int
       |bb = aa : Int
       |bb = foo : (bb: Int): Nothing
       |bb = : Int
       |""".stripMargin,
    topLines = Option(5)
  )

  check(
    "arg5-method",
    s"""|object Main {
        |
        |def foo(ba: Int, voookuu: String): Unit = ???
        |def baInt(v: String): Int = ???
        |
        |val baaaa = "asdasd"
        |val bawww = 3
        |
        |foo(ba@@)
        |}
        |""".stripMargin,
    """|bawww: Int
       |ba = bawww : Int
       |baaaa: String
       |baInt(v: String): Int
       |ba = baInt : (v: String): Int
       |ba = : Int
      |""".stripMargin,
    topLines = Option(6)
  )


  check(
    "arg6-method",
    s"""|object Main {
        |
        |def foo(ba: Int, voookuu: String): Unit = ???
        |val baInt = (v: String) => 13
        |
        |val baaaa = "asdasd"
        |val bawww = 3
        |
        |foo(ba@@)
        |}
        |""".stripMargin,
    """
      |baInt: String => Int
      |bawww: Int
      |ba = baInt : String => Int
      |ba = bawww : Int
      |baaaa: String
      |ba = : Int
      |""".stripMargin,
    topLines = Option(6)
  )

}
