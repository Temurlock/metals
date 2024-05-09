package tests.pc

import tests.BaseCompletionSuite

import coursierapi._

class CompletionContextSuite extends BaseCompletionSuite {

  override val referenceCountProvider = {
    case (_, "java/time/Clock#") => 1
    case (_, "scala/concurrent/Future.") => 1
    case _ => 0
  }

  // ++3.1.3 cross/testOnly tests.pc.CompletionContextSuite

  override def extraDependencies(scalaVersion: String): Seq[Dependency] =
    Seq(
      if (scalaVersion.startsWith("2.13"))
        Dependency.of(Module.of("org.typelevel", s"cats-effect_2.13"), "3.5.3")
      else if (isScala3Version(scalaVersion))
        Dependency.of(Module.of("org.typelevel", s"cats-effect_3"), "3.5.3")
      else
        Dependency.of(Module.of("org.typelevel", s"cats-effect_2.12"), "3.5.3")
    )

  // java.time.Clock should be ranked higher than cats.effect.kernel.Clock
  val clockCompletionResult =
    List("Clock - java.time", "Clock - cats.effect.kernel")

  checkItems(
    "context",
    """package context
      |object A {
      |  Cloc@@
      |}""".stripMargin,
    _.map(_.getLabel)
      .filter(clockCompletionResult.contains)
      .toList == clockCompletionResult
  )

  // scala.concurrent.Future should be ranked higher than java.util.concurrent.Future
  val futureCompletionResult =
    List("Future - scala.concurrent", "Future - java.util.concurrent")

  checkItems(
    "context",
    """package fut
      |object A {
      |  Futur@@
      |}""".stripMargin,
    _.map(_.getLabel)
      .filter(futureCompletionResult.contains)
      .toList == futureCompletionResult
  )
}
