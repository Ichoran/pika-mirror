// This file is part of Pika-Mirror.
// Copyright Rex Kerr, 2016.
// Distributed under the Apache 2 License.

package XJ.pika

import java.io._
import java.nio.file._

import scala.util._
import scala.util.control.NonFatal

import minij._

class Treeverse[A](
  val me: Path,
  val sub: Array[Treeverse[A]],
  val files: Array[Path],
  val zips: Array[Path],
  val links: Array[Path],
  val value: A
)(implicit ev: MiniJ[A]) extends ToJs {
  import Treeverse.pathHasAMiniJ
  override def hashCode = me.hashCode
  override lazy val toString = 
    if (sub.isEmpty && files.isEmpty && zips.isEmpty && links.isEmpty) s"'$me'"
    else s"'$me' with ${files.length + zips.length} files${if (zips.length > 0) s" (${zips.length} zips)" else ""}, ${links.length} links, ${sub.length} subdirectories."
  override def equals(o: Any) = o match {
    case tv: Treeverse[_] => me == tv.me && value == tv.value
    case _ => false
  }
  def mapValues[B: MiniJ](f: Treeverse[A] => B): Treeverse[B] = new Treeverse[B](me, sub.map(_.mapValues(f)), files, zips, links, f(this))
  def foreach[U](f: Treeverse[A] => U) { f(this); sub.foreach(_.foreach(f)) }
  def foreachPath[U](f: Path => U) { f(me); files.foreach(f); zips.foreach(f); links.foreach(f); sub.foreach(_.foreachPath(f)) }
  def foreachFile[U](f: Path => U) { files.foreach(f); sub.foreach(_.foreachFile(f)) }
  def foreachZip[U](f: Path => U) { zips.foreach(f); sub.foreach(_.foreachZip(f)) }
  def toJson =
    JObj ~ ("me", me) ~ ("sub", sub) ~ ("files", files) ~ ("zips", zips) ~ ("links", links) ~ ("value", value) ~ JObj
}

object Treeverse {
  implicit val pathHasAMiniJ: MiniJ[Path] = new MiniJ[Path] { def asJs(p: Path) = JStr(p.toString) }

  def parser[A](mkJ: FromJson[A])(implicit ev: MiniJ[A]): FromJson[Treeverse[A]] = new FromJson[Treeverse[A]] {
    def parse(js: Js): Treeverse[A] = js match {
      case j: JObj =>
        var mestr: String = null
        var subarr: Array[Treeverse[A]] = Array()
        var files: Array[Path] = emptyPaths
        var zips: Array[Path] = emptyPaths
        var links: Array[Path] = emptyPaths
        var vj: Js = null
        var i = 0
        while (i < j.kvs.length-1) {
          j.kvs(i).asInstanceOf[JStr].value match {
            case "me" => mestr = j.kvs(i+1).asInstanceOf[JStr].value
            case "sub" => subarr = j.kvs(i+1).asInstanceOf[JArr].values.map(x => parse(x))
            case "files" => files = j.kvs(i+1).asInstanceOf[JArr].values.map(x => (new File(x.asInstanceOf[JStr].value)).toPath)
            case "zips" => zips = j.kvs(i+1).asInstanceOf[JArr].values.map(x => (new File(x.asInstanceOf[JStr].value)).toPath)
            case "links" => links = j.kvs(i+1).asInstanceOf[JArr].values.map(x => (new File(x.asInstanceOf[JStr].value)).toPath)
            case "value" => vj = j.kvs(i+1)
          }
          i += 2
        }
        if (mestr == null || i > 12)
          throw new IllegalArgumentException("Missing or duplicate fields in Entity")
        new Treeverse((new File(mestr)).toPath, subarr, files, zips, links, mkJ parse vj)
      case _ => throw new IllegalArgumentException("Expected JSON corresponding to Treeverse, but not even an object")
    }
  }
  
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
