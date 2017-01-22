#!/usr/bin/perl
use strict;
use warnings;

use lib "lib";
use XML::FeedPP;
use XML::TreePP;
use Config::IniFiles;
use Getopt::Std;

use vars qw/ %opt $conffile $output_dir /;

sub parseFeed {
    my $source = shift;
    my $feed = XML::FeedPP->new( $source );
    #print "Title: ", $feed->title();
    my @episodes = ();

    foreach my $item ( $feed->get_item() ) {
        my $url = $item->link();
        my $pubDate = $item->pubDate();
        my $episode = $item->description();
        my $season  = $item->description();

        $episode =~ s/.*Episode: (\d+).*/$1/i if ($episode  =~ m/Episode/i);
        $season  =~ s/.*Season: (\d+).*/$1/i  if ($season   =~ m/Season/i);

        #my %hash = ();
        #$hash{key} = 'value';
        #push(@episodes, \%hash);

        push(@episodes, {
                url     => $url,
                pubDate => $pubDate,
                episode => $episode,
                season  => $season
            });
    }
    #print "Size: " .  @episodes . "\n";
    return @episodes;
}

# checks if show is new (according to last_sea and last_epi).  if so, download it and pass back the new last_sea and ep (which can't update yet...)
sub downloadNewShows {
    my %episode = %{(shift)}; # perl only takes scalars as args.  hash must be passed as a reference.  this line dereferences it back to a hash.
    my $last_sea = shift;
    my $last_epi = shift;
    my $dest = $output_dir;

    if (($episode{season} >= $last_sea && $episode{episode} > $last_epi) || ($episode{season} > $last_sea)) {
        my $filename = $episode{url};
        $filename =~ s/.*\/(.*)/$1/;
        my $file = $dest . '/' . $filename;
        print `curl --globoff --silent \"$episode{url}\" -o \"$file\"`;
        return ($episode{season}, $episode{episode});
    }
    return 0;
}

sub getShow {
    my $name = shift;
    my $last_sea = shift;
    my $last_epi = shift; 

    my $feed = "http://ezrss.it/search/index.php?show_name=".$name."&show_name_exact=true&date=&quality=&release_group=&mode=rss";
    #my $last_epi = 20;
    #my $last_sea = 3;
    my @episodes = parseFeed($feed);

    # store newest downloads.  add them to config later.
    my $new_epi = $last_epi;
    my $new_sea = $last_sea;

    while ($#episodes >= 0) {
        my @dled = downloadNewShows( pop(@episodes), $last_sea, $last_epi );
        if ($dled[0] > $new_sea) {
            $new_sea = $dled[0];
            $new_epi = $dled[1];
        }
        if ($dled[0] == $new_sea && $dled[1] > $new_epi) {
            $new_epi = $dled[1];
        }

    }
    return ($new_sea, $new_epi);
}


sub usage() {
    print "Usage:\n\t./plbroadcatch -c config_file -o /path/to/output/dir/\nDefault config files is ~/.plbroadcatch.ini\nDefault output path is current directory.\n\n";
    exit;
}

sub init() {
    getopts( "hc:o:", \%opt ) or usage();
    usage() if $opt{h};

# read the config, download shows for each section.
    $output_dir = $opt{o} or $output_dir = "$ENV{PWD}";
    $conffile = $opt{c} or $conffile = "$ENV{HOME}/.plbroadcatch.ini";

    my $cfg = Config::IniFiles->new( -file => $conffile );
    foreach ( $cfg->Sections ) {
        my $name = $_;
        $name =~ s/ /\+/g;
        my $season = $cfg->val($_, 'Season') || 0;
        my $episode = $cfg->val($_, 'Episode') || 0;

        my @new_recent = getShow($name, $season, $episode);

        $cfg->setval($_, 'Season', $new_recent[0]) || $cfg->newval($_, 'Season', $new_recent[0]);
        $cfg->setval($_, 'Episode', $new_recent[1]) ||  $cfg->newval($_, 'Episode', $new_recent[1]);
    }

    # needs options for specifying path to conf file and torrent location
    # maybe some more OO for controlling what shows get got.
    $cfg->RewriteConfig();
}

init();

