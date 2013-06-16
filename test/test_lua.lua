--
--  Copyright (C) 2013, Jon Gettler
--  http://www.mvpmc.org/
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--

function test_host(host)
   conn = cmyth.connection(host)

   print(string.format('Protocol version: %d',
		       conn:protocol_version()))

   recorded = conn:get_proglist()
   pending = conn:get_proglist(cmyth.PROGTYPE_PENDING)
   scheduled = conn:get_proglist(cmyth.PROGTYPE_SCHEDULED)

   e = conn:get_event(0.1)
   if e then
      print(string.format('Event: "%s" (%d) "%s"',
			  e:name(), e:type(), e:message()))
      e:release()
   end

   print(string.format('Recording count: %d', recorded:get_count()))

   total = conn:storage_space_total()
   used = conn:storage_space_used()

   print(string.format('Storage space total: %d  used: %d',
		       total, used))

   for i = 0, recorded:get_count()-1, 1 do
      prog = recorded:get_prog(i)
      print(string.format('  %s - %s', prog:title(), prog:subtitle()))
      print(string.format('    %s %d', prog:pathname(), prog:length()))
      print(string.format('    %d - %d', prog:start_t(), prog:end_t()))
      print(string.format('    %s - %s', prog:start_str(), prog:end_str()))
      print(string.format('    %s - %s - %d', prog:channel_sign(),
			  prog:channel_name(), prog:channel_id()))
      print(string.format('    %s', prog:description()))
      prog:release()
   end

   print(string.format('Pending count: %d', pending:get_count()))

   for i = 0, pending:get_count()-1, 1 do
      prog = pending:get_prog(i)
      print(string.format('  %s - %s', prog:title(), prog:subtitle()))
      print(string.format('    %s - %s', prog:start_str(), prog:end_str()))
      print(string.format('    %s - %s - %d', prog:channel_sign(),
			  prog:channel_name(), prog:channel_id()))
      prog:release()
   end

   print(string.format('Scheduled count: %d', scheduled:get_count()))

   for i = 0, scheduled:get_count()-1, 1 do
      prog = scheduled:get_prog(i)
      print(string.format('  %s', prog:title()))
      prog:release()
   end

   if conn:hung() then
      print('Connection is hung!')
   end

   recorded:release()
   pending:release()
   scheduled:release()
   conn:release()
end

function test_file(host)
   conn = cmyth.connection(host)
   list = conn:get_proglist()
   prog = list:get_prog(0)
   file = prog:open()

   file:seek(0)

   -- Call out to C++ to generate the MD5 checksum
   md5 = cmyth.with_object(file_md5, file)

   print(string.format('MD5: %s', md5))

   file:release()
   prog:release()
   list:release()
   conn:release()
end

function test_thumbnail(host)
   conn = cmyth.connection(host)
   list = conn:get_proglist()
   prog = list:get_prog(0)
   file = prog:open(cmyth.FILETYPE_THUMBNAIL)

   file:seek(0)

   -- Call out to C++ to generate the MD5 checksum
   bytes,md5 = cmyth.with_object(thumbnail_md5, file)

   print(string.format('Thumbnail image size: %d', bytes))
   print(string.format('MD5: %s', md5))

   file:release()
   prog:release()
   list:release()
   conn:release()
end

function main(host)
   rc,err = pcall(test_host, 'nosuchhost')

   if rc == false then
      print(string.format('Exception: %s', err))
   end

   test_host(host)
   test_file(host)
   test_thumbnail(host)

   ref = cmyth.refmem()

   print(string.format('Refs:  %d', ref:refs()))
   print(string.format('Bytes: %d', ref:bytes()))

   return 0
end
