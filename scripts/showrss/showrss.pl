#!/opt/bin/perl
use strict;
use warnings;

use lib "lib";
use XML::FeedPP;
use XML::TreePP;
use Getopt::Std;

use vars qw/ %opt $output_dir $feed $log /;

# returns array of .torrent files from feed
sub parseFeed {
    my $source = shift;
    my $feed = XML::FeedPP->new( $source );
    my @torrents = (); 

    foreach my $item ( $feed->get_item() ) { 
        my $url = $item->link();
        push(@torrents, $url); 
    }   
    return @torrents;
}

# read in log of already downloaded torrents.  create it if needed.  return array of torrents we already have
sub getLog {
    my $log = shift;
    
    #log exists?
    if (not -e $log) {
        open LOGFILE, ">$log" or die $!;
        print LOGFILE "";
        close LOGFILE;
    }

    #read/write log?
    if (not -r $log or not -w $log) {
        print "Check permissions on log file";
        exit;
    }

    open LOGFILE, "<$log" or die $!;
    my @loggedTorrents = <LOGFILE>;
    close LOGFILE;

    return @loggedTorrents;
}

# usage message
sub usage {
    print "Usage:\n\t./showrss -o /path/to/output/dir -f http://url.of/rss.feed -l /path/to/log.file\n\n";
    exit;
}

# read options, make program go
sub init {
    getopts( "ho:f:l:", \%opt ) or usage();
    usage() if $opt{h};

    $output_dir = $opt{o} or $output_dir = "$ENV{PWD}";
    $feed = $opt{f} or $feed = "http://showrss.info/rss.php?user_id=2341&hd=0&proper=0&magnets=false";
    #$feed = $opt{f} or $feed = "http://showrss.karmorra.info/rss.php?user_id=2341\&hd=0\&proper=null\&namespaces=true";
    $log = $opt{l} or $log = "$ENV{HOME}/.showrsspl.log";

    my @loggedTorrents = getLog($log); 
    my @torrents = parseFeed($feed);

    foreach(@torrents) {
        my $torrent = $_;
        my $filename = $_;
        $filename =~ s/.*\/(.*)/$1/; # where to store the file later

        if ( grep(/$torrent/,  @loggedTorrents) ) {  # skip torrents we already have
            next;
        }

        print `curl --globoff --location --silent "$torrent" -o "$output_dir/$filename.torrent"`;
        if ($? > 0) {
            print "Error downloading $torrent\n";
        } else {
        open LOGFILE, ">>$log" or die $!;
        print LOGFILE "$_\n";
        close LOGFILE;
    }
    }

}

init()
