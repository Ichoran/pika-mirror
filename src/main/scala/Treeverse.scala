// This file is part of Pika-Mirror.
// Copyright Rex Kerr, 2016.
// Distributed under the Apache 2 License.

package XJ.pika

import java.io._
import java.nio.file._

import scala.util._
import scala.util.control.NonFatal

class Treeverse[A](
  val me: Path,
  val sub: Array[Treeverse[A]],
  val files: Array[Path],
  val zips: Array[Path],
  val links: Array[Path],
  val value: A
) {
  override def hashCode = me.hashCode
  override lazy val toString = 
    if (sub.isEmpty && files.isEmpty && zips.isEmpty && links.isEmpty) s"'$me'"
    else s"'$me' with ${files.length + zips.length} files${if (zips.length > 0) s" (${zips.length} zips)" else ""}, ${links.length} links, ${sub.length} subdirectories."
  override def equals(o: Any) = o match {
    case tv: Treeverse[_] => me == tv.me && value == tv.value
    case _ => false
  }
  def mapValues[B](f: Treeverse[A] => B): Treeverse[B] = new Treeverse[B](me, sub.map(_.mapValues(f)), files, zips, links, f(this))
  def foreach[U](f: Treeverse[A] => U) { f(this); sub.foreach(_.foreach(f)) }
  def foreachPath[U](f: Path => U) { f(me); files.foreach(f); zips.foreach(f); links.foreach(f); sub.foreach(_.foreachPath(f)) }
}

object Treeverse{
  private val emptySubs = Array[Treeverse[Boolean]]()
  private val emptyPaths = Array[Path]()

  def apply(root: File): Try[Treeverse[Boolean]] = {
    val result: Array[Treeverse[Boolean]] = Array(null)
    val canon = Try { root.getCanonicalFile } match { case Failure(f) => return Failure(f); case Success(x) => x }
    if (!canon.isDirectory) return Failure(new IOException("Treeverse must be rooted on a directory, not " + canon.getPath))

    var work: Array[(Path, Array[Treeverse[Boolean]], Int)] =
      Array((Try{ canon.toPath } match { case Failure(f) => return Failure(f); case Success(x) => x }, result, 0))

    val seen = scala.collection.mutable.AnyRefMap[Path, Unit](work(0)._1 -> ())

    while (work.nonEmpty) {
      val morework = Array.newBuilder[(Path, Array[Treeverse[Boolean]], Int)]
      for ((p, a, i) <- work) {
        try {
          val fs = p.toFile.listFiles
          val d, n, z, l = new scala.collection.mutable.ArrayBuffer[Path]
          for (f <- fs) {
            val q = f.toPath
            if (!(seen contains q)) {
              seen += (q, ())
              if (Files isSymbolicLink q) l += q
              else if (Files isDirectory q) d += q
              else if (f.getName endsWith ".zip") z += q
              else n += q
            }
          }
          val tv = new Treeverse(
            p,
            if (d.isEmpty) emptySubs else new Array[Treeverse[Boolean]](d.length),
            if (n.isEmpty) emptyPaths else n.toArray,
            if (z.isEmpty) emptyPaths else z.toArray,
            if (l.isEmpty) emptyPaths else l.toArray,
            true
          )
          if (d.nonEmpty) d.zipWithIndex.foreach{ case (q, i) => morework += ((q, tv.sub, i)) }
          a(i) = tv
        }
        catch {
          case t if NonFatal(t) =>
            a(i) = new Treeverse(p, emptySubs, emptyPaths, emptyPaths, emptyPaths, false)
        }
      }
      work = morework.result
    }
    Success(result(0))
  }

  def apply(root: String): Try[Treeverse[Boolean]] = apply(new File(root))
}
