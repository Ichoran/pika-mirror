package XJ.pika

import java.io._
import java.nio._
import java.util.concurrent.atomic.AtomicReference

import scala.util._
import scala.util.control.NonFatal
import scala.util.hashing.{ MurmurHash3 => mh3 }

import kse.maths.hashing._

case class Entity(m3: Int, xx: Int, xx2: Long, name: String, where: String, also: Vector[(String, String)]) {}
object Entity {
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
      Success(new Entity(hash, x32, x64, name, path, Vector.empty))
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
    val result =
      if (buffer.capacity >= sz) {
        while ({
          Try{ fc.read(buffer) } match {
            case Failure(f) => Try{ fc.close }; return Failure(f)
            case Success(x) => x >= 0
          } 
        }) {}
        if (buffer.position < sz) {
          Try{ fc.close }
          return Failure(new IOException(s"Read only ${buffer.remaining} of $sz bytes from ${c.getPath}"))
        }
        Try{ fc.close } match { case Failure(f) => return Failure(f); case _ => }
        buffer.flip
        val n = buffer.remaining
        var i = 0
        hash = partialBytesHash(hash, array, 0, n, Some(sz.toInt))
        val x32 = xx32.result(buffer)
        buffer.rewind
        val x64 = xx64.result(buffer)
        new Entity(hash, x32, x64, c.getName, Try{ cr.getParentFile.getPath } match { case Success(x) => x; case _ => "" }, Vector.empty)
      }
      else ???
    maybeCacheArray(array)
    Success(result)
  }
  def from(src: String, root: File): Try[Entity] = from(new File(src), root)
  def from(src: String, root: String) : Try[Entity] =
    from(new File(src), Try{ (new File(root)).getCanonicalFile } match { case Failure(f) => return Failure(f); case Success(x) => x })
}
