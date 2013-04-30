#!/usr/bin/perl

# parses firefox's places.sqlite for bookmarks
# selects one bookmark via dmenu

use strict;
use warnings;
use DBI;            #libdbd-sqlite3-perl
use IPC::Open2;

sub help {
  print STDERR <<"ENDHELP"

fmarks Usage:

ENDHELP

}

# keyword search should be a separate function i think.
sub main {
  # find sqlite file.  assumes only one profile.
  my $dbfile = `find $ENV{HOME}/.mozilla/firefox/ -maxdepth 2 -name places.sqlite`;
  $dbfile =~ s/^\s+//;
  $dbfile =~ s/\s+$//;

  my $dbh = DBI->connect("dbi:SQLite:dbname=$dbfile","","");
  local (*Reader, *Writer);
  my %bookmarks = ();

  my $query = "SELECT url,moz_bookmarks.title FROM moz_bookmarks LEFT JOIN moz_places ON moz_bookmarks.fk=moz_places.id WHERE parent!=222";
  my $result = $dbh->selectall_arrayref($query);

  # write into and read from a dmenu
  open2(\*Reader, \*Writer, ' dmenu -l 20 -i ');
  foreach my $row (@$result) {
    if (@$row[0] and @$row[1]) {
      print Writer @$row[1] . "\n";
      $bookmarks{@$row[1]} = @$row[0];
    }
  }

  close Writer;
  my $dmenu_selected = <Reader>;

  close Reader;
  $dmenu_selected =~ s/\s+$//; # dmenu includes a newline in results

  print $bookmarks{$dmenu_selected};
  $dbh->disconnect();
}

main();


######## places.sqlite notes
#
# moz_bookmarks
# fk = moz_places.id
# type: 1=url, 2=folder, 3=separater?
# parent: 222 = latest headlines, so ignore it
