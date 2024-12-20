package tests.pc

import tests.BaseCompletionSuite

import scala.language.ScalaSource

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
    topLines = Option(8)
  )

  /*
  (notGetter, notCaseAccessor,arg.Main.banana,class scala.reflect.internal.Symbols$TermSymbol)
  (notLocalByBlock, notCaseAccessor,arg.Main.b,class scala.reflect.internal.Symbols$TermSymbol)

  Но почему так?

  (value value banana=b,notDefinedInFile, notGetter, notCaseAccessor, synthetic)
  (value b,notLocalByBlock, notCaseAccessor)
   */

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
    topLines = Option(8)
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
    topLines = Option(8)
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
    topLines = Option(8)
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
    """bawww: Int
      |baInt(v: String): Int
      |ba = Integer2int : Int
      |ba = baInt : Int
      |ba = bawww : Int
      |ba = hashCode : Int
      |ba = : Int
      |baaaa: String
      |""".stripMargin,
    topLines = Option(8)
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
      |ba = Integer2int : Int
      |ba = baInt : Int
      |ba = bawww : Int
      |ba = hashCode : Int
      |ba = : Int
      |baaaa: String
      |""".stripMargin,
    topLines = Option(8)
  )

}
