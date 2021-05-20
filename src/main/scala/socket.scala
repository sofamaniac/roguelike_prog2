package socket

import java.io.{ IOException, InputStreamReader, BufferedReader, PrintWriter, Closeable }
import java.io.{ BufferedOutputStream, BufferedReader }
import java.io.{ ByteArrayOutputStream, ByteArrayInputStream, ObjectOutputStream, ObjectInputStream }
import java.net.{ ServerSocket, SocketException, SocketTimeoutException, InetAddress, Socket => JSocket }
import scala.io.Codec
import scala.tools.nsc.io.Streamable

object Request
{
  val sep = "<"
  val end = "END_REQUEST"
}

import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

import scala.util.Try

object Gzip {

  def compress(input: Array[Byte]): Array[Byte] = {
    val bos = new ByteArrayOutputStream(input.length)
    val gzip = new GZIPOutputStream(bos)
    gzip.write(input)
    gzip.close()
    val compressed = bos.toByteArray
    bos.close()
    compressed
  }

  def decompress(compressed: Array[Byte]): Option[String] =
    Try {
      val inputStream = new GZIPInputStream(new ByteArrayInputStream(compressed))
      scala.io.Source.fromInputStream(inputStream).mkString
    }.toOption
}

object Socket {
  class Box[+T](f: () => T) {
    private def handlerFn[U](f: Throwable => U): PartialFunction[Throwable, U] = {
      case x @ (_: IOException | _: SecurityException)  => f(x)
    }
    private val optHandler = handlerFn[Option[T]](_ => None)
    private val eitherHandler = handlerFn[Either[Throwable, T]](x => Left(x))

    def either: Either[Throwable, T]    = try Right(f()) catch eitherHandler
    def opt: Option[T]                  = try Some(f()) catch optHandler
  }

  def localhost(port: Int)                = apply(InetAddress.getLocalHost(), port)
  def apply(host: InetAddress, port: Int) = new Box(() => new Socket(new JSocket(host, port)))
  def apply(host: String, port: Int)      = new Box(() => new Socket(new JSocket(host, port)))
}

class Socket(jsocket: JSocket) extends Streamable.Bytes with Closeable {
  def inputStream()  = jsocket.getInputStream()
  def outputStream() = jsocket.getOutputStream()
  def getPort()      = jsocket.getPort()
  def close()        = jsocket.close()

  def printWriter()                         = new PrintWriter(outputStream(), true)
  def bufferedReader(implicit codec: Codec) = new BufferedReader(new InputStreamReader(inputStream()))
  def bufferedOutput(size: Int)             = new BufferedOutputStream(outputStream(), size)

  /** Creates an InputStream and applies the closure, automatically closing it on completion.
   */
  def applyReaderAndWriter[T](f: (BufferedReader, PrintWriter) => T): T = {
    val out = printWriter()
    val in  = bufferedReader

    try f(in, out)
    finally {
      in.close()
      out.close()
    }
  }
}
