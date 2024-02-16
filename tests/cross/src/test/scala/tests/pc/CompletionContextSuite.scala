package tests.pc

import tests.BaseCompletionSuite

import coursierapi._

class CompletionContextSuite extends BaseCompletionSuite {

  override def extraDependencies(scalaVersion: String): Seq[Dependency] =
    Seq(
      Dependency.of(
        Module.of("org.typelevel", s"cats-effect_2.13"), "3.5.3" // TODO: возмонжо добавить обработку других версий
      )
    )

  // Идея смотреть на соседние зависимости и предлагать их в приоритете

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
      println(seq.map(_.getLabel).mkString("\n"))
      true
    }
  )

  /* Хотелось бы java.time видеть выше
  Clock - cats.effect.kernel
  Clock - java.time
  ClockOps - cats.effect.kernel.syntax
  ClockSyntax - cats.effect.kernel.syntax
  ClockPlatform - cats.effect.kernel
  ResourceClock - cats.effect.kernel
   */


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
      println(seq.map(_.getLabel).mkString("\n"))
      true
    }
  )

  /* Котовьи тут лишние, нужно будет унести в отдельный кейс
  Future - java.util.concurrent
  Future - scala.concurrent
  FutureOps - scala.jdk.FutureConverters
  FutureOps - scala.jdk.FutureConverters
  FutureTask - java.util.concurrent
  FutureMonoid - cats.instances
  FutureMonoid - cats.kernel
  RunnableFuture - java.util.concurrent
  FutureCoflatMap - cats.instances
  FutureInstances - cats.instances
   */
}
