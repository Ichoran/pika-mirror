// This file is part of Pika-Mirror.
// Copyright Rex Kerr, 2016.
// Distributed under the Apache 2 License.


package XJ.pika

import java.io._
import java.nio.file._

import scala.util._
import scala.util.control.NonFatal

import kse.jsonal._
import JsonConverters._

class Treeverse[A](
  val me: Path,
  val sub: Array[Treeverse[A]],
  val files: Array[Path],
  val zips: Array[Path],
  val links: Array[Path],
  val value: A
)(implicit jser: Jsonize[A]) extends AsJson {
  import Treeverse.pathCanJsonize
  override def hashCode = me.hashCode
  override lazy val toString = 
    if (sub.isEmpty && files.isEmpty && zips.isEmpty && links.isEmpty) s"'$me'"
    else s"'$me' with ${files.length + zips.length} files${if (zips.length > 0) s" (${zips.length} zips)" else ""}, ${links.length} links, ${sub.length} subdirectories."
  override def equals(o: Any) = o match {
    case tv: Treeverse[_] => me == tv.me && value == tv.value
    case _ => false
  }
  def mapValues[B: Jsonize](f: Treeverse[A] => B): Treeverse[B] = new Treeverse[B](me, sub.map(_.mapValues(f)), files, zips, links, f(this))
  def foreach[U](f: Treeverse[A] => U) { f(this); sub.foreach(_.foreach(f)) }
  def foreachPath[U](f: Path => U) { f(me); files.foreach(f); zips.foreach(f); links.foreach(f); sub.foreach(_.foreachPath(f)) }
  def foreachFile[U](f: Path => U) { files.foreach(f); sub.foreach(_.foreachFile(f)) }
  def foreachZip[U](f: Path => U) { zips.foreach(f); sub.foreach(_.foreachZip(f)) }
  def json =
    Json ~ ("me", me) ~ ("sub", sub) ~ ("files", files) ~ ("zips", zips) ~ ("links", links) ~ ("value", value) ~ Json
}

object Treeverse {
  implicit val pathCanJsonize: Jsonize[Path] = new Jsonize[Path] { def jsonize(p: Path) = Json(p.toString) }

  val pathFromJson: FromJson[Path] = new FromJson[Path] {
    def parse(js: Json): Either[JastError, Path] = js match {
      case Json.Str(text) =>
        try Right((new File(text)).toPath)
        catch { case NonFatal(_) => Left(JastError("Not a path: "+text)) }
      case _ => Left(JastError("Path not represented as a string?"))
    }
  }

  def parser[A](mkA: FromJson[A])(implicit jser: Jsonize[A]): FromJson[Treeverse[A]] = new FromJson[Treeverse[A]] {
    def parse(js: Json): Either[JastError, Treeverse[A]] = js match {
      case j: Json.Obj =>
        val me = j("me") match { 
          case je: JastError => return Left(je)
          case js: Json => pathFromJson.parse(js) match {
            case Left(e) => return Left(e)
            case Right(p) => p
          }
        }
        val subarr: Array[Treeverse[A]] = j("sub") match {
          case ja: Json.Arr => parseArray(ja) match {
            case Left(e) => return Left(e)
            case Right(a) => a
          }
          case _ => return Left(JastError("No array for 'sub' field"))
        }
        val List(files, zips, links) = List("files", "zips", "links").map(name => j(name) match {
          case ja: Json.Arr => pathFromJson.parseArray(ja) match {
            case Left(e) => return Left(e)
            case Right(a) => a
          }
          case _ => return Left(JastError("No array for '" + name + "' field"))
        })
        val value: A = j("value") match { 
          case je: JastError => return Left(je)
          case js: Json => mkA.parse(js) match {
            case Left(e) => return Left(e)
            case Right(a) => a
          }
        }
        Right(new Treeverse(me , subarr, files, zips, links, value))
      case _ => Left(JastError("Expected JSON corresponding to Treeverse, but not even an object"))
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

