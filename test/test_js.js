//
//  Copyright (C) 2014, Jon Gettler
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

var cmyth = require(process.env.CMYTH_MODULE)

function test_host(host) {
    var conn = new cmyth.connection(host)
    console.log('Protocol version: %d', conn.protocol_version())

    var recorded = conn.get_proglist()
    var pending = conn.get_proglist(cmyth.PROGTYPE_PENDING)
    var scheduled = conn.get_proglist(cmyth.PROGTYPE_SCHEDULED)

    var e = conn.get_event(0.1)
    if (e) {
        console.log('Event: "%s" (%d) "%s"', e.name(), e.type(), e.message())
	e.release()
    }

    console.log('Recording count: %d', recorded.get_count())

    var total = conn.storage_space_total()
    var used = conn.storage_space_used()
    console.log('Storage space total: %d  used: %d', total, used)

    for (i=0; i<recorded.get_count(); i++) {
        var prog = recorded.get_prog(i)
        console.log('  %s - %s', prog.title(), prog.subtitle())
        console.log('    %s %d', prog.pathname(), prog.length())
        console.log('    %d - %d', prog.start(), prog.end())
        console.log('    %s - %s', prog.start_str(), prog.end_str())
        console.log('    %s - %s - %d', prog.channel_sign(),
		    prog.channel_name(), prog.channel_id())
        console.log('    %s', prog.description())
	prog.release()
    }

    for (i=0; i<pending.get_count(); i++) {
        var prog = pending.get_prog(i)
        console.log('  %s - %s', prog.title(), prog.subtitle())
        console.log('    %s - %s', prog.start_str(), prog.end_str())
        console.log('    %s - %s - %d', prog.channel_sign(),
		    prog.channel_name(), prog.channel_id())
	prog.release()
    }

    console.log('Scheduled count: %d', scheduled.get_count())

    for (i=0; i<scheduled.get_count(); i++) {
        var prog = scheduled.get_prog(i)
        console.log('  %s', prog.title())
	prog.release()
    }

    if (conn.hung()) {
        console.log('Connection is hung!')
    }

    scheduled.release()
    pending.release()
    recorded.release()
    conn.release()
}

var host

if (process.argv.length > 2) {
    host = process.argv[2]
} else {
    host = 'localhost'
}

try {
    test_host('nosuchhost')
} catch (e) {
    console.log('Exception: %s', e)
}

try {
    test_host(host)
} catch (e) {
    console.log('Exception: %s', e)
}

var ref = new cmyth.refmem()

console.log('Refs:  %d', ref.refs())
console.log('Bytes: %d', ref.bytes())
