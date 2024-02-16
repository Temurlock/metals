package scala.meta.internal.metals

import java.nio.file.Path
import scala.collection.mutable
import scala.meta.internal.mtags.AtomicTrieMap
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.io.AbsolutePath
import scala.meta.internal.semanticdb.Scala._
import scala.meta.pc.ReferenceCountProvider

final class WorkspaceToplevelReferenceCountProvider(
) extends SemanticdbFeatureProvider
    with ReferenceCountProvider {

  // 1ый параметр - файл. 2ой - канутер ссылок

  private val toplevelFileCounter = AtomicTrieMap.empty[Path, Map[String, Int]]

  private val toplevelCounter = AtomicTrieMap.empty[String, Integer]

  override def onChange(docs: TextDocuments, path: AbsolutePath): Unit = {
    val map = mutable.Map.empty[String, Int]
    docs.documents
      .foreach(doc =>
        doc.occurrences.foreach(occ =>
          if (
            occ.role.isReference && occ.symbol.isGlobal && occ.symbol.isType
          ) { // Возможно isGlobal лишний
            val key =
              occ.symbol.slice(0, occ.symbol.length - 1).replace("/", ".")
            map.updateWith(key) { intOpt =>
              Some(intOpt.fold(1)(_ + 1))
            }
          }
        )
      )

    val immutableMap = map.toMap
    toplevelFileCounter.updateWith(path.toNIO) {
      case Some(refCounter) => {
        // Можно будет улучшить эту часть, взяв диффы из текущих референсов и старых
        immutableMap.foreach { case (toplevel, count) =>
          toplevelCounter.updateWith(toplevel) {
            case Some(c) => Some(c + count)
            case None => Some(count)
          }
        }
        refCounter.foreach { case (toplevel, count) =>
          toplevelCounter.updateWith(toplevel) {
            case Some(c) =>
              Option.when(c - count > 0)(
                c - count
              ) // Может ли случиться что c - count < 0?
            case None => None // Такого не должно быть
          }
        }
        Some(immutableMap)
      }

      case None => {
        immutableMap.foreach { case (toplevel, count) =>
          toplevelCounter.updateWith(toplevel) {
            case Some(oldCount) => Some(oldCount + count)
            case None => Some(count)
          }
        }
        Some(immutableMap)
      }
    }
  }

  override def onDelete(path: AbsolutePath): Unit =
    toplevelFileCounter.updateWith(path.toNIO) {
      case None => None
      case Some(map) => {
        map.foreach { case (toplevel, count) =>
          toplevelCounter.updateWith(toplevel) {
            case None =>
              None // Такого не должно быть
            case Some(c) => Option.when(c - count > 0)(c - count)
          }
        }
        None
      }
    }

  // подумать как сделать обнуление toplevelCounter
  override def reset(): Unit = {
    toplevelCounter.clear()
    toplevelFileCounter.clear()
  }

  override def references(symbol: String): Integer = {
    toplevelCounter.get(symbol).getOrElse(0)
  }
}
