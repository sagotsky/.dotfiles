#!/usr/bin/perl

# parses firefox's places.sqlite for bookmarks
# selects one bookmark via dmenu

use strict;
use warnings;
use DBI;            #libdbd-sqlite3-perl
use IPC::Open2;
use Getopt::Std;

sub help {
  print STDERR <<"ENDHELP"

fmarks Usage:

ENDHELP

}

# keyword search should be a separate function i think.
sub main {
  # read in cli opts
  my %opts=();
  getopts("ad:h", \%opts);

  if (defined($opts{'h'})) {
    print <<EOF;
fmarks.pl
Lets you select a bookmark from firefox bookmarks via dmenu

-h  help
-a  all history, not just bookmarks
-d  dmenu opts.  wrap in quotes.

EOF

    exit 0;
  }
  

  # find sqlite file.  assumes only one profile.
  my $dbfile = `find $ENV{HOME}/.mozilla/firefox/ -maxdepth 2 -name places.sqlite`;
  $dbfile =~ s/^\s+//;
  $dbfile =~ s/\s+$//;

  my $dbh = DBI->connect("dbi:SQLite:dbname=$dbfile","","");
  local (*Reader, *Writer);
  my %bookmarks = ();

  # moz_keywords joins ids to bookmarks: keyword_id

  # query for bookmakrs
  my $query = "SELECT url,moz_bookmarks.title FROM moz_bookmarks LEFT JOIN moz_places ON moz_bookmarks.fk=moz_places.id WHERE parent!=222 ORDER BY visit_count DESC";
  if (defined($opts{'a'})) {
    #query all urls
    $query = "SELECT url,title FROM moz_places ORDER BY visit_count DESC";
  }

  my $result = $dbh->selectall_arrayref($query);

  # -d dmenu opts to override default
  my $dmenu_opts = (defined($opts{'d'})) ? $opts{'d'} : '-l 20 -i -b -fn -*-terminus-bold-r-*-*-16 -sb "#4a525a" -sf "#fe4" -nb "#3a424a" -nf "#fff"';

  # write into and read from a dmenu
  open2(\*Reader, \*Writer, ' dmenu ' . $dmenu_opts);
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
