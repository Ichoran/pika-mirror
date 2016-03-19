// This file is part of Pika-Mirror.
// Copyright Rex Kerr, 2016.
// Distributed under the Apache 2 License.

package XJ.pika

import java.io._
import java.nio._
import java.nio.file._
import java.util.zip._

import scala.util._
import scala.util.control.NonFatal

case class Attributed(exec: Boolean, time: attribute.FileTime, zipped: Boolean, size: Long)
extends ToJs {
  def toJson: Js = JObj ~ ("exec", exec) ~ ("time", time) ~ ("zip" ~ zipped) ~ ("size", "%016d".format(size)) ~ JObj
}

case class Alternative(path: Path, internal: Option[String], attributes: Attributed)
extends ToJs {
  def toJson: Js = JObj ~ ("path" ~ path) ~? ("internal", internal) ~ ("attr", attributes) ~ JObj
}

class Understood(
  val path: Path,
  val internal: Option[String],
  val attributes: Attributed,
  val content: Entity,
  val alternates: Array[Alternative]
) extends ToJs {
  import Understood.pathHasAMiniJ
  override def hashCode = content.hashCode
  override def toString = if (internal.isEmpty) s"'$path' is $content" else s"'$path'//'${internal.get}' is $content"
  def toJson: Js = {
    val x = JObj ~ ("path", path)
    if (internal.nonEmpty) x ~ ("internal", internal.get)
    x ~ ("attr", attributed) ~ ("content", content) ~ ("alt", alternates) ~ JObj
  }
}

object Understood {
  implicit val pathHasAMiniJ: MiniJ[Path] = new MiniJ[Path] { def asJs(p: Path) = JStr(p.toString) }
  private val emptyAlternate = Array[(Path, Option[String], Attributed)]()

  def apply(root0: File): Try[Array[Understood]] = {
    val root = Try{ root0.getCanonicalFile } match { case Failure(f) => return Failure(f); case Success(x) => x }
    val rp = Try{ root.toPath } match { case Failure(f) => return Failure(f); case Success(x) => x }
    val tv = Treeverse(root) match { case Failure(f) => return Failure(f); case Success(x) => x }
    tv.foreach(tu => if (!tu.value) return Failure(new IOException(s"Did not fully process ${tu.me}")))
    val ub = Array.newBuilder[Understood]
    tv.foreach{ tu =>
      (tu.files.iterator ++ tu.zips.iterator).foreach{ p =>
        try {
          val relp = rp relativize p
          val a = Attributed(Files isExecutable p, Files getLastModifiedTime p, false, Files size p)
          val c = Entity from (p.toFile, root) match { case Failure(f) => return Failure(f); case Success(x) => x }
          ub += new Understood(relp, None, a, c, emptyAlternate)
          if (c.name endsWith ".zip") {
            val zf = new ZipFile(p.toFile)
            try {
              val zes = zf.entries
              while (zes.hasMoreElements) {
                val ze = zes.nextElement
                val za = Attributed(false, ze.getLastModifiedTime, true, ze.getSize)
                val zi = zf getInputStream ze
                val zc = Entity.from(zi, ze.getName, relp.toString) match { case Failure(f) => return Failure(f); case Success(x) => x }
                ub += new Understood(relp, Some(ze.getName), za, zc, emptyAlternate)
              }
            }
            finally { zf.close }
          }
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
