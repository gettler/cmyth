#!/usr/bin/ruby
#
#  Copyright (C) 2012-2013, Jon Gettler
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

require 'digest/md5'
require 'cmyth'

def test_host(host)
  conn = Cmyth::Connection.new(host)
  ver = conn.protocol_version()

  puts "Protocol version: #{ver}"

  ev = conn.get_event(0.1)

  if (ev)
    name = ev.name()
    type = ev.type()
    message = ev.message()

    puts "Event: \"#{name}\" (#{type}) \"#{message}\""

    ev.release()
  end

  list = conn.get_proglist()
  count = list.get_count()

  total = conn.storage_space_total()
  used = conn.storage_space_used()

  puts "Storage space total: #{total}  used: #{used}"
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
    puts("    #{prog.start()} - #{prog.end()}")
    puts("    #{prog.start_str()} - #{prog.end_str()}")
    puts("    #{channel_sign} #{channel_name} #{channel_id}")
    puts("    #{description}")
    prog.release()
  end

  if conn.hung()
    puts("Connection is hung!")
  end

  list.release()

  list = conn.get_proglist(Cmyth::PROGTYPE_PENDING)
  count = list.get_count()
  puts "Pending count: #{count}"

  for i in 0..count-1
    prog = list.get_prog(i)
    title = prog.title()
    subtitle = prog.subtitle()
    channel_sign = prog.channel_sign()
    channel_name = prog.channel_name()
    channel_id = prog.channel_id()
    puts("  #{title} - #{subtitle}")
    puts("    #{prog.start_str()} - #{prog.end_str()}")
    puts("    #{channel_sign} #{channel_name} #{channel_id}")
    prog.release()
  end

  list.release()

  list = conn.get_proglist(Cmyth::PROGTYPE_SCHEDULED)
  count = list.get_count()
  puts "Scheduled count: #{count}"

  for i in 0..count-1
    prog = list.get_prog(i)
    title = prog.title()
    puts("  #{title}")
    prog.release()
  end

  list.release()

  conn.release()
end

def test_file(host)
  conn = Cmyth::Connection.new(host)
  list = conn.get_proglist()
  prog = list.get_prog(0)
  file = prog.open()
  file.seek(0)
  digest = Digest::MD5.new
  for i in 1..5
    rc,buf = file.read()
    if rc < 0
      puts("Error: file read failed!")
      break
    end
    digest.update(buf)
  end
  puts("MD5: #{digest.hexdigest}")
  file.release()
  prog.release()
  list.release()
  conn.release()
end

def test_thumbnail(host)
  conn = Cmyth::Connection.new(host)
  list = conn.get_proglist()
  prog = list.get_prog(0)
  file = prog.open(Cmyth::FILETYPE_THUMBNAIL)
  file.seek(0)
  digest = Digest::MD5.new
  size = 0
  while true
    rc,buf = file.read()
    if rc < 0 or buf.length == 0
      break
    end
    digest.update(buf)
    size += buf.length
  end
  puts("Thumbnail image size: #{size}")
  puts("MD5: #{digest.hexdigest}")
  file.release()
  prog.release()
  list.release()
  conn.release()
end

if ARGV.length > 0
  host = ARGV[0]
else
  host = "localhost"
end

ref = Cmyth::Refmem.new()

begin
  test_host("nosuchhost")
rescue Cmyth::Exception => e
  puts("Exception: #{e.what}")
end

begin
  test_host(host)
rescue Cmyth::Exception => e
  puts("Exception: #{e.what}")
end

begin
  test_file(host)
rescue Cmyth::Exception => e
  puts("Exception: #{e.what}")
end

begin
  test_thumbnail(host)
rescue Cmyth::Exception => e
  puts("Exception: #{e.what}")
end

refs = ref.refs()
bytes = ref.bytes()
puts("Refs: #{refs}")
puts("Bytes: #{bytes}")
