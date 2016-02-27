// This file is part of Pika-Mirror.
// Copyright Rex Kerr, 2016.
// Distributed under the Apache 2 License.

package XJ.pika

import java.io._
import java.nio._
import java.util.concurrent.atomic.AtomicReference

import scala.util._
import scala.util.control.NonFatal
import scala.util.hashing.{ MurmurHash3 => mh3 }

import kse.maths.hashing._

import minij._

case class Entity(m3: Int, xx: Int, xx2: Long, size: Long, name: String, where: Array[String])
extends ToJs {
  override def hashCode = m3 ^ xx ^ (xx2 & 0xFFFFFFFFL).toInt ^ (xx2 >>> 32).toInt
  override def toString = "%08x%08x%016x %d '%s' / '%s'".format(m3, xx, xx2, size, where.mkString("'/'"), name)
  def toJson = Entity.entityMiniJ.asJs(this)
}

object Entity extends FromJson[Entity] {
  implicit val entityMiniJ: MiniJ[Entity] = new MiniJ[Entity] {
    def asJs(e: Entity): JObj = 
      JObj ~ 
      ("hash", "%08x%08x%016x".format(e.m3, e.xx, e.xx2)) ~ 
      ("size", "%016x".format(e.size)) ~
      ("name", e.name) ~ 
      ("where", e.where) ~ 
      JObj
  }
  def parse(js: Js): Entity = js match {
    case j: JObj =>
      var hashstr: String = null
      var sizestr: String = null
      var namestr: String = null
      var wherearr: JArr = null
      var i = 0
      while (i < j.kvs.length-1) {
        j.kvs(i).asInstanceOf[JStr].value match {
          case "hash" => hashstr = j.kvs(i+1).asInstanceOf[JStr].value
          case "size" => sizestr = j.kvs(i+1).asInstanceOf[JStr].value
          case "name" => namestr = j.kvs(i+1).asInstanceOf[JStr].value
          case "where" => wherearr = j.kvs(i+1).asInstanceOf[JArr]
        }
        i += 2
      }
      if (hashstr == null || sizestr == null || namestr == null || wherearr == null || i != 8)
        throw new IllegalArgumentException("Missing or duplicate fields in Entity")
      val m3 = java.lang.Integer.parseInt(hashstr.substring(0, 8), 16)
      val xx = java.lang.Integer.parseInt(hashstr.substring(8, 16), 16)
      val x2 = java.lang.Integer.parseInt(hashstr.substring(16, 32), 16)
      val sz = java.lang.Integer.parseInt(sizestr, 16)
      val wh = wherearr.values.map(_.asInstanceOf[JStr].value)
      new Entity(m3, xx, x2, sz, namestr, wh) 
    case _ => throw new IllegalArgumentException("Not an Entity (not even a JSON object)")
  }
  private final def partialBytesHash(seed: Int, a: Array[Byte], i0: Int, iN: Int, totalSize: Option[Int] = None): Int = {
    var i = math.max(0, math.min(a.length, i0))
    val iM = math.min(a.length, math.max(iN, i))
    var h = seed
    while (i <= iM-4) {
      h = mh3 mix (h, (a(i)&0xFF) | ((a(i+1)&0xFF) << 8) | ((a(i+2)&0xFF) << 16) | (a(i+3) << 24))
      i += 4
    }
    if (totalSize.isDefined) {
      var g = 0
      if (i == iM-3) g |= (a(i+2)&0xFF) << 16
      if (i >= iM-2) g |= (a(i+1)&0xFF) << 8
      if (i < iM) {
        g |= a(i)&0xFF
        h = mh3 mixLast (h, g)
      }
      mh3 finalizeHash (h, totalSize.get)
    }
    else h
  }

  private final val BiggestBuffer = 1 << 29  // 512M

  private def newBufferSize(old: Int, desired: Long) =
    if (desired >= (BiggestBuffer >> 1) + (BiggestBuffer >> 2)) BiggestBuffer
    else math.min(math.max(old + (old >> 1), 32*((desired + 31)>>>5)), BiggestBuffer).toInt

  private val cachedBuffer = new AtomicReference[Array[Byte]](null)

  private def getAnArray(sz: Long): Array[Byte] = {
    val temp = cachedBuffer.getAndSet(null)
    if (temp == null || (temp.length < BiggestBuffer && temp.length < sz))
      new Array[Byte](newBufferSize(4096, sz))
    else temp 
  }

  private def maybeCacheArray(array: Array[Byte]): Boolean = {
    var replaced = false
    while(!{
      replaced = false
      // Try to place our buffer in cache if it's better than what's there (thread-safely)
      cachedBuffer.get match {
        case null                         => replaced = true; cachedBuffer.compareAndSet(null, array)
        case x if x.length < array.length => replaced = true; cachedBuffer.compareAndSet(x, array)
        case _                            => true
      }
    }) {}
    replaced
  }

  def from(src: InputStream, name: String, path: String): Try[Entity] = {
    val array = getAnArray(1024)
    val buffer = ByteBuffer.wrap(array)
    var hash: Int = 98132357
    val xx32  = new XxHash32(23897518);
    val xx64 = new XxHash64(81392579187516L);
    var iN = 0
    var total = 0L
    var atEnd = false
    try {
      while (!atEnd) {
        val i = src.read(array, iN, array.length - iN)
        if (i == -1) atEnd = true
        else if (i+i > array.length) {
          total += i
          iN += i
          buffer.position(0).limit(iN)
          xx64(buffer)
          val iM = buffer.position
          buffer.position(0).limit(iM)
          xx32(buffer)
          hash = partialBytesHash(hash, array, 0, iM)
          if (iM < iN) System.arraycopy(array, iM, array, 0, iN-iM)
          iN = (iN - iM)
        }
        else {
          total += i
          iN += i
        }
      }
      hash = partialBytesHash(hash, array, 0, iN, Some(total.toInt))
      buffer.position(0).limit(iN)
      val x32 = xx32.result(buffer)
      buffer.position(0).limit(iN)
      val x64 = xx64.result(buffer)
      Try { src.close } match { case Failure(f) => return Failure(f); case _ => }
      Success(new Entity(hash, x32, x64, total, name, Array(path)))
    }
    catch { case NonFatal(t) => Try{ src.close }; Failure(t) }
  }

  def from(src: File, root: File): Try[Entity] = {
    val (c, cr) = Try{ val s = src.getCanonicalFile; s -> root.toPath.relativize(s.toPath).toFile } match {
      case Failure(f) => return Failure(f)
      case Success(x) => x
    }
    val sz = Try { c.length } match { case Failure(f) => return Failure(f); case Success(x) => x }
    var hash: Int = 98132357;
    val xx32  = new XxHash32(23897518);
    val xx64 = new XxHash64(81392579187516L);
    val array = getAnArray(sz)
    val buffer = ByteBuffer.wrap(array)
    val fis = Try{ new FileInputStream(c) } match { case Failure(f) => return Failure(f); case Success(x) => x }
    val fc = Try{ fis.getChannel } match { case Failure(f) => Try{ fis.close }; return Failure(f); case Success(x) => x }
    var total = 0L
    val result = {
      var chances = 100
      while (buffer.capacity < sz - total) {
        val n = Try{ fc.read(buffer) } match {
          case Failure(f) => Try{ fc.close }; return Failure(f)
          case Success(x) => x
        }
        if (n > 0) {
          chances += 10
          total += n
          buffer.flip
          xx64(buffer)
          val m = buffer.position
          val l = buffer.limit
          buffer.position(0).limit(m)
          xx32(buffer)
          hash = partialBytesHash(hash, array, 0, m)
          if (m < l) System.arraycopy(array, m, array, 0, l-m)
          buffer.position(l-m).limit(array.length)
        }
        else if (n < 0 && total < sz) return Failure(new IOException(s"Unable to read expected $sz bytes from ${c.getPath}"))
        else if (chances <= 0) return Failure(new IOException("No progress made reading "+c.getPath))
        else chances -= 1
      }
      while ({
        Try{ fc.read(buffer) } match {
          case Failure(f) => Try{ fc.close }; return Failure(f)
          case Success(x) => x >= 0
        } 
      }) {}
      if (buffer.position < sz - total) {
        Try{ fc.close }
        return Failure(new IOException(s"Read only ${buffer.remaining+total} of $sz bytes from ${c.getPath}"))
      }
      Try{ fc.close } match { case Failure(f) => return Failure(f); case _ => }
      buffer.flip
      val n = buffer.remaining
      var i = 0
      hash = partialBytesHash(hash, array, 0, n, Some(sz.toInt))
      val x32 = xx32.result(buffer)
      buffer.rewind
      val x64 = xx64.result(buffer)
      new Entity(hash, x32, x64, sz, c.getName, Array(Try{ cr.getParentFile.getPath } match { case Success(x) => x; case _ => "" }))
    }
    maybeCacheArray(array)
    Success(result)
  }
  def from(src: String, root: File): Try[Entity] = from(new File(src), root)
  def from(src: String, root: String) : Try[Entity] =
    from(new File(src), Try{ (new File(root)).getCanonicalFile } match { case Failure(f) => return Failure(f); case Success(x) => x })
}
