//
//  Copyright (C) 2013, Jon Gettler
//  http://www.mvpmc.org/
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
//

import java.lang.RuntimeException
import java.nio.ByteBuffer
import java.security.MessageDigest

import org.mvpmc.cmyth.java._

import util.control.Breaks._

object TestScala {
  def test_host(host : String) {
    val conn = new connection(host)
    printf("Protocol version: %d\n", conn.protocol_version())

    val ev = conn.get_event(1)
    printf("Event: \"%s\" (%d) \"%s\"\n", ev.name(), 0, ev.message())
    ev.release()

    var pl = conn.get_proglist()

    printf("Recording count: %d\n", pl.get_count());
    printf("Storage space total: %d  used: %d\n",
	   conn.storage_space_total(), conn.storage_space_used())

    for (i <- 0 to pl.get_count()-1) {
      val prog = pl.get_prog(i)
      printf("  %s - %s\n", prog.title(), prog.subtitle())
      printf("    %s %d%n", prog.pathname(), prog.length())
      printf("    %d - %d\n", prog.start(), prog.end())
      printf("    %s - %s\n", prog.start_str(), prog.end_str())
      printf("    %s %s %d\n", prog.channel_sign(), prog.channel_name(),
	     prog.channel_id())
      printf("    %s\n", prog.description())
      prog.release()
    }

    pl.release()

    pl = conn.get_proglist(progtype_t.PROGTYPE_PENDING)

    for (i <- 0 to pl.get_count()-1) {
      val prog = pl.get_prog(i)
      printf("  %s - %s\n", prog.title(), prog.subtitle())
      printf("    %s - %s\n", prog.start_str(), prog.end_str())
      printf("    %s %s %d\n", prog.channel_sign(), prog.channel_name(),
	     prog.channel_id())
      prog.release()
    }

    pl.release()

    pl = conn.get_proglist(progtype_t.PROGTYPE_SCHEDULED)

    for (i <- 0 to pl.get_count()-1) {
      val prog = pl.get_prog(i)
      printf("  %s - %s\n", prog.title(), prog.subtitle())
      prog.release()
    }

    pl.release()

    conn.release()
  }

  def test_file(host : String) {
    val conn = new connection(host)
    val pl = conn.get_proglist()
    val prog = pl.get_prog(0)
    val file = prog.open()
    val md = MessageDigest.getInstance("MD5");

    file.seek(0)

    for (i <- 0 to 4) {
      val bb = ByteBuffer.allocateDirect(cmythConstants.DEFAULT_BUFLEN)
      val len = file.read(bb)
      if (len > 0) {
	val b : Array[Byte] = new Array[Byte](len)
	bb.get(b, 0, len)
	md.update(b, 0, len)
      }
    }

    val mdbytes : Array[Byte] = md.digest();

    printf("MD5: ")
    for (i <- 0 to (mdbytes.length - 1)) {
      printf("%02x", mdbytes(i));
    }
    printf("\n");

    file.release()
    prog.release()
    pl.release()
    conn.release()
  }

  def test_thumbnail(host : String) {
    val conn = new connection(host)
    val pl = conn.get_proglist()
    val prog = pl.get_prog(0)
    val file = prog.open(filetype_t.FILETYPE_THUMBNAIL)
    val md = MessageDigest.getInstance("MD5");

    file.seek(0)

    var size = 0

    breakable {
      while (true) {
	val bb = ByteBuffer.allocateDirect(cmythConstants.DEFAULT_BUFLEN)
	val len = file.read(bb)
	if (len > 0) {
	  val b : Array[Byte] = new Array[Byte](len)
	  bb.get(b, 0, len)
	    md.update(b, 0, len)
	  size = size + len
	} else {
	  break
	}
      }
    }

    printf("Thumbnail image size: %d\n", size)

    val mdbytes : Array[Byte] = md.digest();

    printf("MD5: ")
    for (i <- 0 to (mdbytes.length - 1)) {
      printf("%02x", mdbytes(i));
    }
    printf("\n");

    file.release()
    prog.release()
    pl.release()
    conn.release()
  }

  def main(args: Array[String]) {
    val host = args(0)

    try {
      test_host("nosuchhost")
    } catch {
      case e: RuntimeException => printf("Excpetion: %s\n", e.getMessage())
    }

    test_host(host)
    test_file(host)
    test_thumbnail(host)

    val ref = new refmem()

    printf("Refs:  %d\n", ref.refs())
    printf("Bytes: %d\n", ref.bytes())
  }
}
