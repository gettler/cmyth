#!/usr/bin/ruby
#
#  Copyright (C) 2012, Jon Gettler
#  http://www.mvpmc.org/
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#

require 'cmyth'

def test_host(host)
  conn = Cmyth::Connection.new(host)
  ver = conn.protocol_version()

  puts "Protocol version: #{ver}"

  list = conn.get_proglist()
  count = list.get_count()

  puts "Recording count: #{count}"

  for i in 0..count-1
    prog = list.get_prog(i)
    title = prog.title()
    subtitle = prog.subtitle()
    pathname = prog.pathname()
    length = prog.length()
    channel_sign = prog.channel_sign()
    channel_name = prog.channel_name()
    channel_id = prog.channel_id()
    description = prog.description()
    puts("  #{title} - #{subtitle}")
    puts("    #{pathname} #{length}")
    puts("    #{channel_sign} #{channel_name} #{channel_id}")
    puts("    #{description}")
    prog.release()
  end

  conn.release()
  list.release()
end

ref = Cmyth::Refmem.new()

begin
  test_host("nosuchhost")
rescue Exception => e
  puts("Exception: #{e.message}")
end

begin
  test_host("localhost")
rescue Exception => e
  puts("Exception: #{e.message}")
end

refs = ref.refs()
bytes = ref.bytes()
puts("Refs: #{refs}")
puts("Bytes: #{bytes}")
