package tests.pc

import tests.BaseCompletionSuite

import coursierapi._

class CompletionContextSuite extends BaseCompletionSuite {

  override val referenceCountProvider = {
    case "java.time.Clock" => 1
    case "scala.concurrent.Future" => 1
    case _ => 0
  }

  override def extraDependencies(scalaVersion: String): Seq[Dependency] =
    Seq(
      Dependency.of(
        Module.of("org.typelevel", s"cats-effect_2.13"),
        "3.5.3"
      )
    )

  // Идея смотреть на соседние зависимости и предлагать их в приоритете

  private val contextCompletions =
    """Clock - java.time
      |Clock - cats.effect.kernel
      |ClockOps - cats.effect.kernel.syntax
      |ClockSyntax - cats.effect.kernel.syntax
      |ClockPlatform - cats.effect.kernel
      |ResourceClock - cats.effect.kernel""".stripMargin

  checkItems(
    "context".tag(IgnoreScala3),
    """package context
      |object A {
      |  Cloc@@
      |}
      |
      |object B {
      |  import java.time.Clock
      |
      |  Clock.systemUTC().getZone().toString()
      |}""".stripMargin,
    seq => {
      assertNoDiff(seq.map(_.getLabel).mkString("\n"), contextCompletions)
      true
    }
  )

  val futureCompletions =
    """Future - scala.concurrent
      |Future - java.util.concurrent
      |FutureOps - scala.jdk.FutureConverters
      |FutureOps - scala.jdk.FutureConverters
      |FutureTask - java.util.concurrent
      |FutureMonoid - cats.instances
      |FutureMonoid - cats.kernel
      |RunnableFuture - java.util.concurrent
      |FutureCoflatMap - cats.instances
      |FutureInstances - cats.instances""".stripMargin

  checkItems(
    "context".tag(IgnoreScala3),
    """package fut
      |object A {
      |  Futur@@
      |
      |  object kek {
      |    import scala.concurrent.Future
      |  }
      |""".stripMargin,
    seq => {
      assertNoDiff(seq.map(_.getLabel).mkString("\n"), futureCompletions)
      true
    }
  )
}
