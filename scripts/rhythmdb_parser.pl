#!/usr/bin/perl

# reads rhythmdb to yoink ratings

use strict;
use warnings;

use XML::Simple;
use Data::Dumper;

my $file = "$ENV{HOME}/.gnome2/rhythmbox/rhythmdb.xml";
my $simple = XML::Simple->new();
my $data   = $simple->XMLin($file);
#print Dumper($data->{'$VAR1'}) . "\n";

my @base = @{$data->{entry}};

foreach (@base) {
    if ($_->{'rating'}) {
        my $loc = $_->{'location'};
        my $rating = $_->{'rating'};

        #$loc =~ s/%20/ /g;
        $loc =~ s'file:///home/sagotsky/Music''g;
        print "\"$loc\" $rating \n";
    }
}
