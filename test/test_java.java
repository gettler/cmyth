//
//  Copyright (C) 2012, Jon Gettler
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

import java.nio.ByteBuffer;
import java.security.MessageDigest;

import org.mvpmc.cmyth.java.refmem;

import org.mvpmc.cmyth.java.filetype_t;
import org.mvpmc.cmyth.java.cmythConstants;
import org.mvpmc.cmyth.java.connection;
import org.mvpmc.cmyth.java.proglist;
import org.mvpmc.cmyth.java.proginfo;
import org.mvpmc.cmyth.java.file;

public class test_java {
	public static void test_host(String host) {
		connection conn;
		proglist list;
		proginfo prog;
		int i;

		conn = new connection(host);

		System.out.format("Protocol version: %d%n",
				  conn.protocol_version());

		list = conn.get_proglist();

		System.out.format("Recording count: %d%n", list.get_count());

		for (i=0; i<list.get_count(); i++) {
			prog = list.get_prog(i);
			System.out.format("  %s - %s%n",
					  prog.title(), prog.subtitle());
			System.out.format("    %s %d%n",
					  prog.pathname(), prog.length());
			System.out.format("    %d - %d%n",
					  prog.start(), prog.end());
			System.out.format("    %s - %s%n",
					  prog.start_str(), prog.end_str());
			System.out.format("    %s %s %d%n",
					  prog.channel_sign(),
					  prog.channel_name(),
					  prog.channel_id());
			System.out.format("    %s%n", prog.description());
			prog.release();
		}

		conn.release();
		list.release();
	}

	public static void test_file(String host) {
		connection conn;
		proglist list;
		proginfo prog;
		file file;
		ByteBuffer bb;
		int len;
		int i;
		MessageDigest md;

		try {
			md = MessageDigest.getInstance("MD5");
		} catch (Exception e) {
			System.out.format("Exception: %s%n", e.getMessage());
			return;
		}

		conn = new connection(host);
		list = conn.get_proglist();
		prog = list.get_prog(0);
		file = prog.open();
		file.seek(0);
		for (i=0; i<5; i++) {
			bb = ByteBuffer.allocateDirect(cmythConstants.DEFAULT_BUFLEN);
			len = file.read(bb);
			if (len > 0) {
				byte b[] = new byte[len];
				bb.get(b, 0, len);
				md.update(b, 0, len);
			}
		}

		byte[] mdbytes = md.digest();

		StringBuffer sb = new StringBuffer();
		for (i = 0; i < mdbytes.length; i++) {
			sb.append(Integer.toString((mdbytes[i] & 0xff) + 0x100, 16).substring(1));
		}

		System.out.println("MD5: " + sb.toString());

		file.release();
		prog.release();
		conn.release();
		list.release();
	}

	public static void test_thumbnail(String host) {
		connection conn;
		proglist list;
		proginfo prog;
		file file;
		ByteBuffer bb;
		int len;
		int i, size;
		MessageDigest md;

		try {
			md = MessageDigest.getInstance("MD5");
		} catch (Exception e) {
			System.out.format("Exception: %s%n", e.getMessage());
			return;
		}

		conn = new connection(host);
		list = conn.get_proglist();
		prog = list.get_prog(0);
		file = prog.open(filetype_t.FILETYPE_THUMBNAIL);
		file.seek(0);
		size = 0;
		while (true) {
			bb = ByteBuffer.allocateDirect(cmythConstants.DEFAULT_BUFLEN);
			len = file.read(bb);
			if (len > 0) {
				byte b[] = new byte[len];
				bb.get(b, 0, len);
				md.update(b, 0, len);
				size += len;
			} else {
				break;
			}
		}

		System.out.format("Thumbnail image size: %d%n", size);

		byte[] mdbytes = md.digest();

		StringBuffer sb = new StringBuffer();
		for (i = 0; i < mdbytes.length; i++) {
			sb.append(Integer.toString((mdbytes[i] & 0xff) + 0x100, 16).substring(1));
		}

		System.out.println("MD5: " + sb.toString());

		file.release();
		prog.release();
		conn.release();
		list.release();
	}

	public static void main(String[] args) {
		String host;

		System.loadLibrary("cmyth_java");

		if (args.length > 0) {
			host = args[0];
		} else {
			host = "localhost";
		}

		try {
			test_host("nosuchhost");
		} catch (RuntimeException e) {
			System.out.format("Exception: %s%n", e.getMessage());
		}

		try {
			test_host(host);
		} catch (RuntimeException e) {
			System.out.format("Exception: %s%n", e.getMessage());
		}

		try {
			test_file(host);
		} catch (RuntimeException e) {
			System.out.format("Exception: %s%n", e.getMessage());
		}

		try {
			test_thumbnail(host);
		} catch (RuntimeException e) {
			System.out.format("Exception: %s%n", e.getMessage());
		}

		refmem ref = new refmem();

		System.out.format("Refs:  %d%n", ref.refs());
		System.out.format("Bytes: %d%n", ref.bytes());
	}
}
