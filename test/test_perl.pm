#!/usr/bin/perl
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

use cmyth;
use Digest::MD5 qw(md5 md5_hex md5_base64);

sub test_host {
    my $host = $_[0];

    my $conn = new cmyth::connection($host);

    my $ver = $conn->protocol_version();
    print "Protocol version: $ver\n";

    my $list = $conn->get_proglist();
    my $count = $list->get_count();
    print "Recording count: $count\n";

    my $total = $conn->storage_space_total();
    my $used = $conn->storage_space_used();
    print "Storage space total: $total  used: $used\n";

    for ($i=0; $i<$count; $i++) {
	my $prog = $list->get_prog($i);
	my $title = $prog->title();
	my $subtitle = $prog->subtitle();
	my $pathname = $prog->pathname();
	my $length = $prog->length();
	my $start = $prog->start();
	my $end = $prog->end();
	my $start_str = $prog->start_str();
	my $end_str = $prog->end_str();
	my $sign = $prog->channel_sign();
	my $name = $prog->channel_name();
	my $id = $prog->channel_id();
	my $description = $prog->description();

	print "  $title - $subtitle\n";
	print "    $pathname $length\n";
	print "    $start - $end\n";
	print "    $start_str - $end_str\n";
	print "    $sign - $name - $id\n";
	print "    $description\n";

	$prog->release();
    }

    $list->release();
    $conn->release();
}

sub test_file {
    my $host = $_[0];
    my $conn = new cmyth::connection($host);
    my $list = $conn->get_proglist();
    my $prog = $list->get_prog(0);
    my $file = $prog->open();

    my $md5 = Digest::MD5->new;

    $file->seek(0);
    for ($i=0; $i<5; $i++) {
	my $data = $file->read();
	$md5->add($data);
    }

    my $digest = $md5->hexdigest;

    print "MD5: $digest\n";

    $file->release();
    $prog->release();
    $list->release();
    $conn->release();
}

sub test_thumbnail {
    my $host = $_[0];
    my $conn = new cmyth::connection($host);
    my $list = $conn->get_proglist();
    my $prog = $list->get_prog(0);
    my $file = $prog->open($cmyth::FILETYPE_THUMBNAIL);

    my $md5 = Digest::MD5->new;
    my $size = 0;

    while (True) {
	my $data = $file->read();
	if (length($data) <= 0) {
	    last;
	}
	$md5->add($data);
	$size = $size + length($data);
    };

    my $digest = $md5->hexdigest;

    print "Thumbnail image size: $size\n";
    print "MD5: $digest\n";

    $file->release();
    $prog->release();
    $list->release();
    $conn->release();
}

if ($#ARGV >= 0) {
    $host = $ARGV[0];
} else {
    $host = "localhost";
}

eval {
    test_host("nosuchhost");
};
if ($@) {
    $e = $@->what();
    print "Exception: $e\n";
};

eval {
    test_host($host);
};
if ($@) {
    $e = $@->what();
    print "Exception: $e\n";
};

eval {
    test_file($host);
};
if ($@) {
    $e = $@->what();
    print "Exception: $e\n";
};

eval {
    test_thumbnail($host);
};
if ($@) {
    $e = $@->what();
    print "Exception: $e\n";
};

$ref = new cmyth::refmem();
$refs = $ref->refs();
$bytes = $ref->bytes();

print "Refs:  $refs\n";
print "Bytes: $bytes\n";
