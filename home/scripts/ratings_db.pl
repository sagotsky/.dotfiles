#!/usr/bin/perl

# sqlite DB for storing song ratings from mpd

use strict;
use warnings;
use DBI;
use Switch;


# create a new table in db
sub initDB {
    my ($dbh) = @_;
    $dbh->do(  "CREATE TABLE 'song_ratings' (id INTEGER PRIMARY KEY, 'songfile' varchar(256) , 'rating' tinyint)"  );
}

# adds or changes a rating for a given song
sub set {
    my ($dbh, $songfile, $rating) = @_;
    $songfile = $dbh->quote($songfile);
    my $result = $dbh->selectall_arrayref(  "SELECT id FROM song_ratings WHERE songfile=$songfile");
    my $updated = 0; # if extra rows, delete them.  also insert new row if needed.

    foreach my $row (@$result) {
        my ($id) = @$row;
        if ($updated) {
            # already been updated.  delete extra row.
            $dbh->do( "DELETE FROM song_ratings WHERE id='$id'" );    
        } else {
            # update table for this song.  flag future updates for deletion.
            $dbh->do( "UPDATE song_ratings SET rating=$rating WHERE id='$id'" );
            $updated = 1;
        }
    }

    if (!$updated) {
        # create new row for this song.
        $dbh->do(  "INSERT INTO song_ratings (songfile, rating) VALUES (  $songfile, $rating )  "  );
    }


}

# gets rating of songfile
sub get {
    my ($dbh, $songfile) = @_;
    my $result = $dbh->selectall_arrayref(  "SELECT rating FROM song_ratings WHERE songfile='$songfile' LIMIT 1");


    if ( scalar @$result) {
        foreach my $row(@$result) {
            my ($rating) = @$row;
            print "$rating\n";
        }
    } else {
        print 0;
    }
    
}

# gets all songs with rating in range
sub getSongs {
    my ($dbh, $min, $max) = @_;

    my $result = $dbh->selectall_arrayref(  "SELECT songfile FROM song_ratings WHERE rating>='$min' AND rating<='$max'");

    foreach my $row(@$result) {
        my ($songfile) = @$row;
        print "$songfile\n";
    }
}

sub help {
    print STDERR <<"ENDHELP"

rating_db Usage:
rating_db.pl cmd

Where command is one of the following:
init                # Create new database overwriting current db.   Default location is ~/.ratings.db
get SONG            # Retrieve rating of SONG
set SONG RATING     # Set SONG's rating to RATING
getSongs MIN [MAX]  # Retrives list of songs rated MIN or with a rating between MIN and MAX inclusive
help|-h|-?          # This help page

ENDHELP

}

sub main {
    my $dbfile = "$ENV{HOME}/.ratings.db";
    my $mpcpath = "/usr/bin/mpc";
    #TODO add support for checking mpc for current song

    my $dbh = DBI->connect("dbi:SQLite:dbname=$dbfile","","");

    my $CMD = $ARGV[0];
    switch ($CMD) {
        case ""       {help();}
        case "help"   {help();}
        case "-h"     {help();}
        case "-?"     {help();}

        case "init"   {
                        initDB( $dbh );    
        }

        case "get"    {
            my $song = $ARGV[1] or die('get command requires a song file');
            get( $dbh, $song );
        }

        case "set" {
            my $song = $ARGV[1] or die('set command requires a song file');
            my $rating = $ARGV[2] or die('set command requires a numerical rating');
            set( $dbh, $song, $rating );
        }

        case "gets" {
            my $max;
            my $min = $ARGV[1] or die('gets command requires a numerical rating');
            $max = $ARGV[2] or $max = $min;
            getSongs( $dbh, $min, $max );

        }


    }

    $dbh->disconnect();
}

main();
