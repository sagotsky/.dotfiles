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

binmode STDOUT, ":utf8";

my $login;
my $password;
my $file;
my $help        = 0;

sub show_help
{
print <<HELP;
Usage ./$0 -u <user> -p <password> -d <directory_or_file> [-t timeout] [-help]
The script scrobbles random file from the directory (it may use a single file)
with a specified interval.
-u <user>           - your login on las.fm
-p <password>       - your password on last.fm
-f <file>           - path to mp3 file
-h --help           - this text :>
(c) Denis Usanov 2011
HELP
    die("\n");
}


GetOptions(
    'user=s'        => \$login,
    'password=s'    => \$password,
    'file=s'         => \$file,
    'help'          => \$help,
) or show_help;

if ($help || !defined $login || !defined $password || !defined $file)
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
#my $mp3file = random_file($dir);
chdir $workdir;
my $tags = get_mp3tag($file) or die;
my $info = get_mp3info($file) or die;
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
