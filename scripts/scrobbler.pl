#!/usr/bin/perl

##
#  scrobbler.pl
#
#  Scrobbles a single file specified with -f.
# 
#  Adapated from https://github.com/dck/scrobbler    (c) Denis Usanov 2011 :>
##

use utf8;
use strict;
use warnings;
use Audio::Scrobbler;
use MP3::Info;
use Cwd qw(abs_path cwd);
use Getopt::Long;
use XML::Feed;

binmode STDOUT, ":utf8";

my $login;
my $password;
my $help        = 0;
my $time        = 60;

sub show_help
{
  print <<HELP;
  Usage ./$0 -u <user> -p <password> [-t time] [-help]
  List of files provided by stdin will be sent to last.fm
  -u <user>           - your login on last.fm
  -p <password>       - your password on last.fm
  -t <delay>          - Delay between requests.  Default: 60 seconds.
  -h --help           - this text :>
  (c) Denis Usanov 2011
HELP
  die("\n");
}

GetOptions(
  'user=s'        => \$login,
  'password=s'    => \$password,
  'help'          => \$help,
  't=i'           => \$time,
) or show_help;

if ($help || !defined $login || !defined $password )
{
  show_help();
}

my $connect = new Audio::Scrobbler(
  cfg => {
    progname	=> 'tst',
    progver		=> '1.4',
    username	=> $login,
    password	=> $password,
    verbose		=> 0,
  }
);

my $ua = $connect->get_ua();
my $hs = $connect->handshake() or next and print "Error while connecting to last.fm: ".$connect->err;
my $workdir = cwd();
chdir $workdir;

foreach (<>) {
  chomp;
  my $tags = get_mp3tag("$_") or die;
  my $info = get_mp3info("$_") or die;
  my ($title, $artist, $album, $length) = ($tags->{TITLE}, $tags->{ARTIST}, $tags->{ALBUM}, $info->{MM}*60 + $info->{SS});

  my $submit = $connect->submit(
    {
      title	=> $title,
      artist	=> $artist,
      album	=> $album,
      length	=> $length,
    }
  ) or last and print "Error while submitting: ".$connect->err;

  print "'$artist - $title' has been scrobbled\n";

  sleep($time);
}
