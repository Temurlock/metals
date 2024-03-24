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

    println(s"change happened ${path.toNIO}")

    println(s"old file counter: ${toplevelFileCounter}")

    // На первый вариант пришли к тому чтобы хранить текущее состояние для каждого файла
    // потом если надо будет оптимизируем
    /*
    // а точно это старый? Надо будет потестить
    semanticdbs.textDocument(path).documentIncludingStale match {
      case Some(oldDocument) => onChangeFile(docs, oldDocument)
      case None => onCreateFile(docs)
    }
     */

    // Проверить что файл из workspace?
    // Дак это же всегда должно быть так

    /*
     * Должно быть 2 уникальных валидных случаев:
     * 1. Новый файл. Значит в semanticdbs надеюсь не должно его быть
     * 2. Меняем старый файл, меняем референсы. Значит в semanticdbs должен найтись файл
     *
     * И особая обработка случая:
     * - Меняется toplevel объект (не понятно что будет в OnDemandSymbolIndex)
     * Возможно поможет mtags.topLevelSymbols
     *  */
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

    println(s"new file counter: ${toplevelFileCounter}")
  }

  override def onDelete(path: AbsolutePath): Unit =
    toplevelFileCounter.updateWith(path.toNIO) {
      case None => None
      case Some(map) => {
        map.foreach { case (toplevel, count) =>
          toplevelCounter.updateWith(toplevel) {
            case None =>
              None // Такого не должно быть (Кажется нужно кастомную структуру данных для этого делать)
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

  // Подумать что делать с разными скала диалектами
  override def references(symbol: String): Integer = {
    println(s"$symbol - ${toplevelCounter.get(symbol)}")
    toplevelCounter.get(symbol).getOrElse(0)
  }
}
