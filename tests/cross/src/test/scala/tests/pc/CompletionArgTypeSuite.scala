package tests.pc

import tests.BaseCompletionSuite

class CompletionArgTypeSuite extends BaseCompletionSuite {

  // Нужно понять как сортировать.
  // Было бы норм по указателю понять аргумент какой позиции заполняется
  // Искать по его типу
  // Если что оставить другие возможные типы
  // Если несколько с таким типом, то сортировать по схожести имен аргументов

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
    """|b: String
       |banana = b : String
       |banana = : String
       |a: Int
       |""".stripMargin,
    topLines = Option(4)
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
    """|b: String
       |banana = a : String
       |banana = b : String
       |banana = : String
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
       |a = : Int
       |a: String
       |""".stripMargin,
    topLines = Option(4)
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
       |bb = aa : Int
       |bb = bb : Int
       |bb = : Int
       |""".stripMargin,
    topLines = Option(4)
  )

  check(
    "arg5",
    s"""|object Main {
        |
        |  def bb(): Int = 123
        |
        |  val aa = 10
        |  def foo(bb: Int) = ???
        |  foo(b@@)
        |}
        |""".stripMargin,
    """|bb(): Int
       |bb = aa : Int
       |bb = bb : Int
       |bb = : Int
       |""".stripMargin,
    topLines = Option(4)
  )
}
