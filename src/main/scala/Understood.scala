// This file is part of Pika-Mirror.
// Copyright Rex Kerr, 2016.
// Distributed under the Apache 2 License.

package XJ.pika

import java.io._
import java.nio._
import java.nio.file._

import scala.util._
import scala.util.control.NonFatal

case class Attributed(exec: Boolean, time: attribute.FileTime) {}

class Understood(
  val path: Path,
  val attributes: Attributed,
  val content: Entity,
  val alternates: Array[(Path, Attributed)]
) {
  override def hashCode = content.hashCode
  override def toString = s"'$path' is $content"
}

object Understood {
  private val emptyAlternate = Array[(Path, Attributed)]()

  def apply(root0: File): Try[Array[Understood]] = {
    val root = Try{ root0.getCanonicalFile } match { case Failure(f) => return Failure(f); case Success(x) => x }
    val rp = Try{ root.toPath } match { case Failure(f) => return Failure(f); case Success(x) => x }
    val tv = Treeverse(root) match { case Failure(f) => return Failure(f); case Success(x) => x }
    tv.foreach(tu => if (!tu.value) return Failure(new IOException(s"Did not fully process ${tu.me}")))
    val ub = Array.newBuilder[Understood]
    tv.foreach{ tu =>
      (tu.files.iterator ++ tu.zips.iterator).foreach{ p =>
        try {
          val a = Attributed(Files isExecutable p, Files getLastModifiedTime p)
          val c = Entity from (p.toFile, root) match { case Failure(f) => return Failure(f); case Success(x) => x }
          ub += new Understood(p, a, c, emptyAlternate)
        }
        catch {
          case NonFatal(t) => return Failure(t)
        }
      }
    }
    Success(ub.result)
  }

  def apply(root: String): Try[Array[Understood]] = apply(new File(root))
}
