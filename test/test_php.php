<?php
/*
 *  Copyright (C) 2012-2013, Jon Gettler
 *  http://www.mvpmc.org/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

include("cmyth.php");

function test_host($host) {
	$conn = new connection($host);
	$ver = $conn->protocol_version();

	echo "Protocol version: " . $ver . "\n";

	$ev = $conn->get_event(0.1);

	if ($ev) {
		$name = $ev->name();
		$type = $ev->type();
		$message = $ev->message();

		echo "Event: \"" . $name . "\" (" . $type . ") \"" . $message . "\"\n";

		$ev->release();
	}

	$list = $conn->get_proglist();

	$total = $conn->storage_space_total();
	$used = $conn->storage_space_used();

	echo "Storage space total: " . $total . "  used: " . $used . "\n";
	echo "Recording count: " . $list->get_count() . "\n";

	for ($i=0; $i<$list->get_count(); $i++) {
		$prog = $list->get_prog($i);
		echo "  " . $prog->title() . " - " . $prog->subtitle() . "\n";
		$pathname = $prog->pathname();
		$length = $prog->length();
		echo "    " . $pathname . " " . $length . "\n";
		echo "    " . $prog->start() . " - " . $prog->end() . "\n";
		echo "    " . $prog->start_str() . " - " . $prog->end_str() . "\n";
		$sign = $prog->channel_sign();
		$name = $prog->channel_name();
		$id = $prog->channel_id();
		echo "    " . $sign . " " . $name . " " . $id . "\n";
		echo "    " . $prog->description() . "\n";
		$prog->release();
	}

	if ($conn->hung()) {
		echo "Connection is hung!\n";
	}

	$list->release();

	$list = $conn->get_proglist(PROGTYPE_PENDING);
	echo "Pending count: " . $list->get_count() . "\n";

	for ($i=0; $i<$list->get_count(); $i++) {
		$prog = $list->get_prog($i);
		echo "  " . $prog->title() . " - " . $prog->subtitle() . "\n";
		echo "    " . $prog->start_str() . " - " . $prog->end_str() . "\n";
		$prog->release();
	}

	$list->release();

	$list = $conn->get_proglist(PROGTYPE_SCHEDULED);
	echo "Scheduled count: " . $list->get_count() . "\n";

	for ($i=0; $i<$list->get_count(); $i++) {
		$prog = $list->get_prog($i);
		echo "  " . $prog->title() . "\n";
		$prog->release();
	}

	$list->release();

	$conn->release();
}

function test_file($host) {
	$conn = new connection($host);
	$list = $conn->get_proglist();
	$prog = $list->get_prog(0);
	$file = $prog->open();
	$file->seek(0);
	$ctx = hash_init('md5');
	$buf = array();
	for ($i=0; $i<5; $i++) {
		$len = $file->read($buf);
		hash_update($ctx, $buf[0]);
	}
	$md5 = hash_final($ctx);
	echo "MD5: " . $md5 . "\n";
	$file->release();
	$prog->release();
	$conn->release();
	$list->release();
}

function test_thumbnail($host) {
	$conn = new connection($host);
	$list = $conn->get_proglist();
	$prog = $list->get_prog(0);
	$file = $prog->open(FILETYPE_THUMBNAIL);
	$file->seek(0);
	$ctx = hash_init('md5');
	$buf = array();
	$size = 0;
	while (1) {
		$len = $file->read($buf);
		if ($len == 0) {
			break;
		}
		hash_update($ctx, $buf[0]);
		$size += $len;
	}
	echo "Thumbnail image size: " . $size . "\n";
	$md5 = hash_final($ctx);
	echo "MD5: " . $md5 . "\n";
	$file->release();
	$prog->release();
	$conn->release();
	$list->release();
}

function test_perf($host) {
	$conn = new connection($host);
	$list = $conn->get_proglist();
	$prog = $list->get_prog(0);
	$file = $prog->open();
	$file->seek(0);
	$buf = array();
	$offset = 0;

	$start = microtime(true);

	while ($offset < 67108864) {
		$len = $file->read($buf);
		if ($len <= 0) {
			break;
		}
		$offset += $len;
	}

	$end = microtime(true);

	$duration = $end - $start;
	$mbps = ($offset * 8) / $duration / 1000000;

	echo "Perf: read " . $offset;
	echo " bytes in " . number_format($duration, 2) . " seconds";
	echo " (" . number_format($mbps, 2) . " mb/s)";
	echo "\n";

	$file->release();
	$prog->release();
	$conn->release();
	$list->release();
}

if ($argc > 1) {
	$host = $argv[1];
} else {
	$host = "localhost";
}

$ref = new refmem();

try {
	test_host("nosuchhost");
} catch (Exception $e) {
	echo "Exception: " . $e->getMessage() . "\n";
}

try {
	test_host($host);
} catch (Exception $e) {
	echo "Exception: " . $e->getMessage() . "\n";
}

try {
	test_file($host);
} catch (Exception $e) {
	echo "Exception: " . $e->getMessage() . "\n";
}

try {
	test_thumbnail($host);
} catch (Exception $e) {
	echo "Exception: " . $e->getMessage() . "\n";
}

try {
	test_perf($host);
} catch (Exception $e) {
	echo "Exception: " . $e->getMessage() . "\n";
}

echo "Refs: " . $ref->refs() . "\n";
echo "Bytes: " . $ref->bytes() . "\n";

?>
