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

import org.mvpmc.cmyth.java.refmem;

import org.mvpmc.cmyth.java.connection;
import org.mvpmc.cmyth.java.proglist;
import org.mvpmc.cmyth.java.proginfo;

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

	public static void main(String[] args) {
		System.loadLibrary("cmyth_java");

		try {
			test_host("nosuchhost");
		} catch (RuntimeException e) {
			System.out.format("Exception: %s%n", e.getMessage());
		}

		try {
			test_host("localhost");
		} catch (RuntimeException e) {
			System.out.format("Exception: %s%n", e.getMessage());
		}

		refmem ref = new refmem();

		System.out.format("Refs:  %d%n", ref.refs());
		System.out.format("Bytes: %d%n", ref.bytes());
	}
}
