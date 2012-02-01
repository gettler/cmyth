<?php
/*
 *  Copyright (C) 2012, Jon Gettler
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

	$list = $conn->get_proglist();

	echo "Recording count: " . $list->get_count() . "\n";

	for ($i=0; $i<$list->get_count(); $i++) {
		$prog = $list->get_prog($i);
		echo "  " . $prog->title() . " - " . $prog->subtitle() . "\n";
		$pathname = $prog->pathname();
		$length = $prog->length();
		echo "    " . $pathname . " " . $length . "\n";
		$sign = $prog->channel_sign();
		$name = $prog->channel_name();
		$id = $prog->channel_id();
		echo "    " . $sign . " " . $name . " " . $id . "\n";
		echo "    " . $prog->description() . "\n";
		$prog->release();
	}

	$conn->release();
	$list->release();
}

$ref = new refmem();

try {
	test_host("nosuchhost");
} catch (Exception $e) {
	echo "Exception: " . $e->getMessage() . "\n";
}

try {
	test_host("localhost");
} catch (Exception $e) {
	echo "Exception: " . $e->getMessage() . "\n";
}

echo "Refs: " . $ref->refs() . "\n";
echo "Bytes: " . $ref->bytes() . "\n";

?>
