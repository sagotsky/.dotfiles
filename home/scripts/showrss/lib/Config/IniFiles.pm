package Config::IniFiles;

use vars qw($VERSION);

$VERSION = "2.58";

require 5.004;
use strict;
use Carp;
use Symbol 'gensym','qualify_to_ref';   # For the 'any data type' hack

@Config::IniFiles::errors = ( );

#	$Header: /home/shlomi/progs/perl/cpan/Config/IniFiles/config-inifiles-cvsbackup/config-inifiles/IniFiles.pm,v 2.41 2003-12-08 10:50:56 domq Exp $

=head1 NAME

Config::IniFiles - A module for reading .ini-style configuration files.

=head1 SYNOPSIS

  use Config::IniFiles;
  my $cfg = Config::IniFiles->new( -file => "/path/configfile.ini" );
  print "The value is " . $cfg->val( 'Section', 'Parameter' ) . "."
  	if $cfg->val( 'Section', 'Parameter' );

=head1 DESCRIPTION

Config::IniFiles provides a way to have readable configuration files outside
your Perl script. Configurations can be imported (inherited, stacked,...), 
sections can be grouped, and settings can be accessed from a tied hash.

=head1 FILE FORMAT

INI files consist of a number of sections, each preceded with the
section name in square brackets, followed by parameter names and
their values.

  [a section]
  Parameter=Value

  [section 2]
  AnotherParameter=Some value
  Setting=Something else
  Parameter=Different scope than the one in the first section

The first non-blank character of the line indicating a section must 
be a left bracket and the last non-blank character of a line indicating 
a section must be a right bracket. The characters making up the section 
name can be any symbols at all. However section names must be unique.

Parameters are specified in each section as Name=Value.  Any spaces
around the equals sign will be ignored, and the value extends to the
end of the line (including any whitespace at the end of the line. 
Parameter names are localized to the namespace of the section, but must 
be unique within a section.

Both the hash mark (#) and the semicolon (;) are comment characters.
by default (this can be changed by configuration). Lines that begin with 
either of these characters will be ignored. Any amount of whitespace may 
precede the comment character.

Multi-line or multi-valued parameters may also be defined ala UNIX 
"here document" syntax:

  Parameter=<<EOT
  value/line 1
  value/line 2
  EOT

You may use any string you want in place of "EOT". Note that whatever
follows the "<<" and what appears at the end of the text MUST match
exactly, including any trailing whitespace.

Alternately, as a configuration option (default is off), continuation 
lines can be allowed:

  [Section]
  Parameter=this parameter \
    spreads across \
    a few lines


=head1 USAGE -- Object Interface

Get a new Config::IniFiles object with the I<new> method:

  $cfg = Config::IniFiles->new( -file => "/path/config_file.ini" );
  $cfg = new Config::IniFiles -file => "/path/config_file.ini";

Optional named parameters may be specified after the configuration
file name. See the I<new> in the B<METHODS> section, below.

Values from the config file are fetched with the val method:

  $value = $cfg->val('Section', 'Parameter');

If you want a multi-line/value field returned as an array, just
specify an array as the receiver:

  @values = $cfg->val('Section', 'Parameter');

=head1 METHODS

=head2 new ( [-option=>value ...] )

Returns a new configuration object (or "undef" if the configuration
file has an error, in which case check the global C<@Config::IniFiles::errors> 
array for reasons why). One Config::IniFiles object is required per configuration
file. The following named parameters are available:

=over 10


=item I<-file>  filename

Specifies a file to load the parameters from. This 'file' may actually be 
any of the following things:

  1) the pathname of a file

    $cfg = Config::IniFiles->new( -file => "/path/to/config_file.ini" );

  2) a simple filehandle

    $cfg = Config::IniFiles->new( -file => STDIN );

  3) a filehandle glob

    open( CONFIG, "/path/to/config_file.ini" );
    $cfg = Config::IniFiles->new( -file => *CONFIG );

  4) a reference to a glob

    open( CONFIG, "/path/to/config_file.ini" );
    $cfg = Config::IniFiles->new( -file => \*CONFIG );

  5) an IO::File object

    $io = IO::File->new( "/path/to/config_file.ini" );
    $cfg = Config::IniFiles->new( -file => $io );

  or

    open my $fh, '<', "/path/to/config_file.ini" or die $!;
    $cfg = Config::IniFiles->new( -file => $fh );

  6) A reference to a scalar (requires newer versions of IO::Scalar)

    $ini_file_contents = <<EOT
    [section name]
    Parameter=A value
    Setting=Another value
    EOT
    
    $cfg = Config::IniFiles->new( -file => $ini_file_contents );


If this option is not specified, (i.e. you are creating a config file from scratch) 
you must specify a target file using L<SetFileName> in order to save the parameters.


=item I<-default> section

Specifies a section to be used for default values. For example, in the 
following configuration file, if you look up the "permissions" parameter 
in the "joe" section, there is none.

   [all]
   permissions=Nothing
        
   [jane]
   name=Jane
   permissions=Open files

   [joe]
   name=Joseph

If you create your Config::IniFiles object with a default section of "all" like this:

   $cfg = Config::IniFiles->new( -file => "file.ini", -default => "all" );
   
Then requsting a value for a "permissions" in the [joe] section will 
check for a value from [all] before returning undef.

   $permissions = $cfg->val( "joe", "permissions");   // returns "Nothing"


=item I<-nocase> 0|1

Set -nocase => 1 to handle the config file in a case-insensitive
manner (case in values is preserved, however).  By default, config
files are case-sensitive (i.e., a section named 'Test' is not the same
as a section named 'test').  Note that there is an added overhead for
turning off case sensitivity.


=item I<-import> object

This allows you to import or inherit existing setting from another 
Config::IniFiles object. When importing settings from another object, 
sections with the same name will be merged and parameters that are 
defined in both the imported object and the I<-file> will take the 
value of given in the I<-file>. 

If a I<-default> section is also given on this call, and it does not 
coincide with the default of the imported object, the new default 
section will be used instead. If no I<-default> section is given, 
then the default of the imported object will be used.


=item I<-allowcontinue> 0|1

Set -allowcontinue => 1 to enable continuation lines in the config file.
i.e. if a line ends with a backslash C<\>, then the following line is
appended to the parameter value, dropping the backslash and the newline
character(s).

Default behavior is to keep a trailing backslash C<\> as a parameter
value. Note that continuation cannot be mixed with the "here" value
syntax.


=item I<-allowempty> 0|1

If set to 1, then empty files are allowed at L</ReadConfig|ReadConfig()> 
time. If set to 0 (the default), an empty configuration file is considered 
an error.


=item I<-negativedeltas> 0|1

If set to 1 (the default if importing this object from another one),
parses and honors lines of the following form in the configuration
file:

  ; [somesection] is deleted

or

  [inthissection]
  ; thisparameter is deleted

If set to 0 (the default if not importing), these comments are treated
like ordinary ones.

The L</WriteConfig|WriteConfig(-delta=>1)> form will output such
comments to indicate deleted sections or parameters. This way,
reloading a delta file using the same imported object produces the
same results in memory again. See L<IMPORT / DELTA FEATURES> for more
details.

=item I<-commentchar> 'char'

The default comment character is C<#>. You may change this by specifying
this option to another character. This can be any character except 
alphanumeric characters, square brackets or the "equal" sign.


=item I<-allowedcommentchars> 'chars'

Allowed default comment characters are C<#> and C<;>. By specifying this
option you may change the range of characters that are used to denote a
comment line to include any set of characters

Note: that the character specified by B<-commentchar> (see above) is 
I<always> part of the allowed comment characters. 

Note 2: The given string is evaluated as a regular expression character 
class, so '\' must be escaped if you wish to use it.


=item I<-reloadwarn> 0|1

Set -reloadwarn => 1 to enable a warning message (output to STDERR)
whenever the config file is reloaded.  The reload message is of the
form:

  PID <PID> reloading config file <file> at YYYY.MM.DD HH:MM:SS

Default behavior is to not warn (i.e. -reloadwarn => 0).

This is generally only useful when using Config::IniFiles in a server 
or daemon application. The application is still responsible for determining
when the object is to be reloaded.


=back

=cut

sub new {
  my $class = shift;
  my %parms = @_;

  my $errs = 0;
  my @groups = ( );

  my $self = bless {
	default => '',
	imported =>undef,
	v =>{},
	cf => undef,
	firstload => 1,
  }, $class;

  if( ref($parms{-import}) && ($parms{-import}->isa('Config::IniFiles')) ) {
    $self->{imported}=$parms{-import}; # ReadConfig will load the data
    $self->{negativedeltas}=1;
  } elsif( defined $parms{-import} ) {
    carp "Invalid -import value \"$parms{-import}\" was ignored.";
  } # end if
  delete $parms{-import};

  # Copy the original parameters so we 
  # can use them when we build new sections 
  %{$self->{startup_settings}} = %parms;

  # Parse options
  my($k, $v);
  local $_;
  $self->{nocase} = 0;

  # Handle known parameters first in this order, 
  # because each() could return parameters in any order
  if (defined ($v = delete $parms{'-file'})) {
    # Should we be pedantic and check that the file exists?
    # .. no, because now it could be a handle, IO:: object or something else
    $self->{cf} = $v;
  }
  if (defined ($v = delete $parms{'-nocase'})) {
    $self->{nocase} = $v ? 1 : 0;
  }  
  if (defined ($v = delete $parms{'-default'})) {
    $self->{default} = $self->{nocase} ? lc($v) : $v;
  }
  if (defined ($v = delete $parms{'-reloadwarn'})) {
    $self->{reloadwarn} = $v ? 1 : 0;
  }
  if (defined ($v = delete $parms{'-allowcontinue'})) {
    $self->{allowcontinue} = $v ? 1 : 0;
  }
  if (defined ($v = delete $parms{'-allowempty'})) {
	  $self->{allowempty} = $v ? 1 : 0;
  }
  if (defined ($v = delete $parms{'-negativedeltas'})) {
	  $self->{negativedeltas} = $v ? 1 : 0;
  }
  if (defined ($v = delete $parms{'-commentchar'})) {
    if(!defined $v || length($v) != 1) {
      carp "Comment character must be unique.";
      $errs++;
    }
    elsif($v =~ /[\[\]=\w]/) {
      # must not be square bracket, equal sign or alphanumeric
      carp "Illegal comment character.";
      $errs++;
    } 
    else {
      $self->{comment_char} = $v;
    }
  }
  if (defined ($v = delete $parms{'-allowedcommentchars'})) {
    # must not be square bracket, equal sign or alphanumeric
    if(!defined $v || $v =~ /[\[\]=\w]/) {
      carp "Illegal value for -allowedcommentchars.";
      $errs++;
    }
    else {
      $self->{allowed_comment_char} = $v;
    }
  }
  $self->{comment_char} = '#' unless exists $self->{comment_char};
  $self->{allowed_comment_char} = ';' unless exists $self->{allowed_comment_char};
  # make sure that comment character is always allowed
  $self->{allowed_comment_char} .= $self->{comment_char};

  $self->{_comments_at_end_of_file} = [];

  # Any other parameters are unkown
  while (($k, $v) = each %parms) {
    carp "Unknown named parameter $k=>$v";
    $errs++;
  }

  return undef if $errs;
  
  if ($self->ReadConfig) {
    return $self;
  } else {
    return undef;
  }
}


=head2 val ($section, $parameter [, $default] )

Returns the value of the specified parameter (C<$parameter>) in section 
C<$section>, returns undef (or C<$default> if specified) if no section or 
no parameter for the given section exists.


If you want a multi-line/value field returned as an array, just
specify an array as the receiver:

  @values = $cfg->val('Section', 'Parameter');

A multi-line/value field that is returned in a scalar context will be
joined using $/ (input record separator, default is \n) if defined,
otherwise the values will be joined using \n.

=cut

sub val {
  my ($self, $sect, $parm, $def) = @_;

  # Always return undef on bad parameters
  return if not defined $sect;
  return if not defined $parm;
  
  if ($self->{nocase}) {
    $sect = lc($sect);
    $parm = lc($parm);
  }
  
  my $val = defined($self->{v}{$sect}{$parm}) ?
    $self->{v}{$sect}{$parm} :
    $self->{v}{$self->{default}}{$parm};
  
  # If the value is undef, make it $def instead (which could just be undef)
  $val = $def unless defined $val;
  
  # Return the value in the desired context
  if (wantarray) {
    if (ref($val) eq "ARRAY") {
      return @$val;
    } elsif (defined($val)) {
      return $val;
    } else {
      return;
    }
  } elsif (ref($val) eq "ARRAY") {
  	if (defined ($/)) {
	    return join "$/", @$val;
	} else {
		return join "\n", @$val;
	}
  } else {
    return $val;
  }
}



=head2 exists($section, $parameter)

True if and only if there exists a section C<$section>, with 
a parameter C<$parameter> inside, not counting default values.

=cut

sub exists {
	my ($self, $sect, $parm)=@_;

    if ($self->{nocase}) {
        $sect = lc($sect);
        $parm = lc($parm);
    }
    
	return (exists $self->{v}{$sect}{$parm});
}



=head2 push ($section, $parameter, $value, [ $value2, ...])

Pushes new values at the end of existing value(s) of parameter
C<$parameter> in section C<$section>.  See below for methods to write
the new configuration back out to a file.

You may not set a parameter that didn't exist in the original
configuration file.  B<push> will return I<undef> if this is
attempted. See B<newval> below to do this. Otherwise, it returns 1.

=cut

sub push {
  my ($self, $sect, $parm, @vals)=@_;

  return undef if not defined $sect;
  return undef if not defined $parm;

  if ($self->{nocase}) {
    $sect = lc($sect);
    $parm = lc($parm);
  }

  return undef if (! defined($self->{v}{$sect}{$parm}));

  return 1 if (! @vals);

  $self->_touch_parameter($sect, $parm);

  $self->{EOT}{$sect}{$parm} = 'EOT' if
	(!defined $self->{EOT}{$sect}{$parm});

  $self->{v}{$sect}{$parm} = [$self->{v}{$sect}{$parm}] unless
     (ref($self->{v}{$sect}{$parm}) eq "ARRAY");

  CORE::push @{$self->{v}{$sect}{$parm}}, @vals;
  return 1;
}

=head2 setval ($section, $parameter, $value, [ $value2, ... ])

Sets the value of parameter C<$parameter> in section C<$section> to 
C<$value> (or to a set of values).  See below for methods to write 
the new configuration back out to a file.

You may not set a parameter that didn't exist in the original
configuration file.  B<setval> will return I<undef> if this is
attempted. See B<newval> below to do this. Otherwise, it returns 1.

=cut

sub setval {
  my $self = shift;
  my $sect = shift;
  my $parm = shift;
  my @val  = @_;

  return undef if not defined $sect;
  return undef if not defined $parm;

  if ($self->{nocase}) {
    $sect = lc($sect);
    $parm = lc($parm);
  }

  if (defined($self->{v}{$sect}{$parm})) {
	$self->_touch_parameter($sect, $parm);
    if (@val > 1) {
      $self->{v}{$sect}{$parm} = \@val;
	  $self->{EOT}{$sect}{$parm} = 'EOT';
    } else {
      $self->{v}{$sect}{$parm} = shift @val;
    }
    return 1;
  } else {
    return undef;
  }
}

=head2 newval($section, $parameter, $value [, $value2, ...])

Assignes a new value, C<$value> (or set of values) to the 
parameter C<$parameter> in section C<$section> in the configuration 
file.

=cut

sub newval {
  my $self = shift;
  my $sect = shift;
  my $parm = shift;
  my @val  = @_;
  
  return undef if not defined $sect;
  return undef if not defined $parm;

  if ($self->{nocase}) {
    $sect = lc($sect);
    $parm = lc($parm);
  }

  $self->AddSection($sect);

  CORE::push(@{$self->{parms}{$sect}}, $parm) 
      unless (grep {/^\Q$parm\E$/} @{$self->{parms}{$sect}} );

  $self->_touch_parameter($sect, $parm);
  if (@val > 1) {
    $self->{v}{$sect}{$parm} = \@val;
    $self->{EOT}{$sect}{$parm} = 'EOT' unless defined $self->{EOT}{$sect}{$parm};
  } else {
    $self->{v}{$sect}{$parm} = shift @val;
  }
  return 1
}

=head2 delval($section, $parameter)

Deletes the specified parameter from the configuration file

=cut

sub delval {
  my $self = shift;
  my $sect = shift;
  my $parm = shift;
  
  return undef if not defined $sect;
  return undef if not defined $parm;

  if ($self->{nocase}) {
    $sect = lc($sect);
    $parm = lc($parm);
  }

  @{$self->{parms}{$sect}} = grep !/^\Q$parm\E$/, @{$self->{parms}{$sect}};
  $self->_touch_parameter($sect, $parm);
  delete $self->{v}{$sect}{$parm};
  return 1
}

=head2 ReadConfig

Forces the configuration file to be re-read. Returns undef if the 
file can not be opened, no filename was defined (with the C<-file>
option) when the object was constructed, or an error occurred while 
reading.

If an error occurs while parsing the INI file the @Config::IniFiles::errors
array will contain messages that might help you figure out where the 
problem is in the file.

=cut

# Auxillary function to make deep (aliasing-free) copies of data
# structures.  Ignores blessed objects in tree (could be taught not
# to, if needed)
sub _deepcopy {
  my $ref=shift;

  if (! ref($ref)) { return $ref; }

  local $_;

  if (UNIVERSAL::isa($ref, "ARRAY")) {
          return [map {_deepcopy($_)} @$ref];
  }

  if (UNIVERSAL::isa($ref, "HASH")) {
          my $return={};
          foreach my $k (keys %$ref) {
                  $return->{$k}=_deepcopy($ref->{$k});
          }
          return $return;
  }

  carp "Unhandled data structure in $ref, cannot _deepcopy()";
}

# Internal method, gets the next line, taking proper care of line endings.
sub _nextline {
	my ($self, $fh)=@_;
	local $_;
	if (!exists $self->{line_ends}) {
		# no $self->{line_ends} is a hint set by caller that we are at
		# the first line (kludge kludge).
		{
			local $/=\1;
			my $nextchar;
			do {
				$nextchar=<$fh>;
				return undef if (!defined $nextchar);
				$_ .= $nextchar;
			} until (m/((\015|\012|\025|\n)$)/s);
			$self->{line_ends}=$1;
			if ($nextchar eq "\x0d") {
				# peek at the next char
				$nextchar = <$fh>;
				if ($nextchar eq "\x0a") {
					$self->{line_ends} .= "\x0a";
				} else {
					seek $fh, -1, 1;
				}
			}
		}

		# If there's a UTF BOM (Byte-Order-Mark) in the first
		# character of the first line then remove it before processing
		# (http://www.unicode.org/unicode/faq/utf_bom.html#22)
		s/^﻿//;

		return $_;
	} else {
		local $/=$self->{line_ends};
		return scalar <$fh>;
	}
}

# Internal method, closes or resets the file handle. To be called
# whenever ReadConfig() returns.
sub _rollback {
	my ($self, $fh)=@_;
  # Only close if this is a filename, if it's
  # an open handle, then just roll back to the start
  if( !ref($self->{cf}) ) {
    close($fh);
  } else {
    # Attempt to rollback to beginning, no problem if this fails (e.g. STDIN)
    seek( $fh, 0, 0 );
  } # end if
}


sub ReadConfig {
  my $self = shift;

  my($lineno, $sect);
  my($group, $groupmem);
  my($parm, $val);
  my @cmts;

  @Config::IniFiles::errors = ( );

  # Initialize (and clear out) storage hashes
  $self->{sects}  = [];
  $self->{parms}  = {};
  $self->{group}  = {};
  $self->{v}      = {};
  $self->{sCMT}   = {};
  $self->{pCMT}   = {};
  $self->{EOT}    = {};
  $self->{mysects} = []; # A pair of hashes to remember which params are loaded
  $self->{myparms} = {}; # or set using the API vs. imported - useful for
  # import shadowing, see below, and WriteConfig(-delta=>1)

  if( defined $self->{imported} ) {
      # Run up the import tree to the top, then reload coming
      # back down, maintaining the imported file names and our 
      # file name.
      # This is only needed on a re-load though
	  $self->{imported}->ReadConfig() unless ($self->{firstload});

	  foreach my $field (qw(sects parms group v sCMT pCMT EOT)) {
		  $self->{$field} = _deepcopy($self->{imported}->{$field});
	  }
  } # end if
  
  return 1 if (
    (not exists $self->{cf}) or
    (not defined $self->{cf}) or
    ($self->{cf} eq '')
  );
  
  my $nocase = $self->{nocase};

  # If this is a reload and we want warnings then send one to the STDERR log
  unless( $self->{firstload} || !$self->{reloadwarn} ) {
    my ($ss, $mm, $hh, $DD, $MM, $YY) = (localtime(time))[0..5];
    printf STDERR
      "PID %d reloading config file %s at %d.%02d.%02d %02d:%02d:%02d\n",
      $$, $self->{cf}, $YY+1900, $MM+1, $DD, $hh, $mm, $ss;
  }
  
  # Turn off. Future loads are reloads
  $self->{firstload} = 0;

  # Get a filehandle, allowing almost any type of 'file' parameter
  my $fh = $self->_make_filehandle( $self->{cf} );
  if (!$fh) {
    carp "Failed to open $self->{cf}: $!";
    return undef;
  }
  
  # Get mod time of file so we can retain it (if not from STDIN)
  # also check if it's a real file (could have been a filehandle made from a scalar).
  if (ref($fh) ne "IO::Scalar" && -e $fh)
  {
    my @stats = stat $fh;
    $self->{file_mode} = sprintf("%04o", $stats[2]) if defined $stats[2];
  }
  
  
  # The first lines of the file must be blank, comments or start with [
  my $first = '';
  my $allCmt = $self->{allowed_comment_char};
  
  local $_;
  delete $self->{line_ends}; # Marks start of parsing for _nextline()
  while ( defined($_ = $self->_nextline($fh)) ) {
    s/(\015\012?|\012|\025|\n)$//;				# remove line ending char(s)
    $lineno++;
    if (/^\s*$/) {				# ignore blank lines
      next;
    }
    elsif (/^\s*[$allCmt]/) {			# collect comments
		if ($self->{negativedeltas} &&
			m/^$self->{comment_char} (.*) is deleted$/) {
			my $todelete=$1;
			if ($todelete =~ m/^\[(.*)\]$/) {
				$self->DeleteSection($1);
			} else {
				$self->delval($sect, $todelete);
			}
		} else {
			CORE::push(@cmts, $_);
		}
		next;
    }
    elsif (/^\s*\[\s*(\S|\S.*\S)\s*\]\s*$/) {		# New Section
      $sect = $1;
      if ($self->{nocase}) {
        $sect = lc($sect);
      }
      $self->AddSection($sect);
      $self->SetSectionComment($sect, @cmts);
      @cmts = ();
    }
    elsif (($parm, $val) = /^\s*([^=]*?[^=\s])\s*=\s*(.*)$/) {	# new parameter
		if (!defined $sect) {
			CORE::push(@Config::IniFiles::errors, sprintf('%d: %s', $lineno,
				qq#parameter found outside a section#));
			$self->_rollback($fh);
			return undef;
		}

      $parm = lc($parm) if $nocase;
      my @val = ( );
      my $eotmark;
      if ($val =~ /^<<(.*)$/) {			# "here" value
	   $eotmark  = $1;
	my $foundeot = 0;
	my $startline = $lineno;
	while ( defined($_=$self->_nextline($fh)) ) {
	  s/(\015\012?|\012|\025|\n)$//;				# remove line ending char(s)
	  $lineno++;
	  if ($_ eq $eotmark) {
	    $foundeot = 1;
	    last;
	  } else {
		# Untaint
		/(.*)/ms; 
        CORE::push(@val, $1);
	  }
	}
	if (! $foundeot) {
	  CORE::push(@Config::IniFiles::errors, sprintf('%d: %s', $startline,
			      qq#no end marker ("$eotmark") found#));
      $self->_rollback();
	  return undef;
    }
      } else { # no here value

        # process continuation lines, if any
        while($self->{allowcontinue} && $val =~ s/\\$//) {
          $_ = $self->_nextline($fh);
	  s/(\015\012?|\012|\025|\n)$//; # remove line ending char(s)
	  $lineno++;
          $val .= $_;
        }
		@val = $val;
      }
		# Now load value
		if (exists $self->{v}{$sect}{$parm} && 
			exists $self->{myparms}{$sect} && 
			grep( /^\Q$parm\E$/, @{$self->{myparms}{$sect}}) ) {
			$self->push($sect, $parm, @val);
		} else {
	        # Loaded parameters shadow imported ones, instead of appending
			# to them
			$self->newval($sect, $parm, @val);
		}
		$self->SetParameterComment($sect, $parm, @cmts);
		@cmts = ( );
		$self->SetParameterEOT($sect,$parm,$eotmark) if (defined $eotmark);

    } else {
      CORE::push(@Config::IniFiles::errors, sprintf("Line \%d in file " . $self->{cf} . " is mal-formed:\n\t\%s", $lineno, $_));
    }
  } # End main parsing loop

  # Special case: return undef if file is empty. (suppress this line to
  # restore the more intuitive behaviour of accepting empty files)
  if (! keys %{$self->{v}} && ! $self->{allowempty}) {
	  CORE::push @Config::IniFiles::errors, "Empty file treated as error";
	  $self->_rollback($fh);
	  return undef;
  }

  if( defined (my $defaultsect=$self->{startup_settings}->{-default}) ) {
	  $self->AddSection($defaultsect);
  } # end if

  $self->_SetEndComments(@cmts);

  $self->_rollback($fh);
  @Config::IniFiles::errors ? undef : 1;
}


=head2 Sections

Returns an array containing section names in the configuration file.
If the I<nocase> option was turned on when the config object was
created, the section names will be returned in lowercase.

=cut

sub Sections {
  my $self = shift;
  return @{$self->{sects}} if ref $self->{sects} eq 'ARRAY';
  return ();
}

=head2 SectionExists ( $sect_name )

Returns 1 if the specified section exists in the INI file, 0 otherwise (undefined if section_name is not defined).

=cut

sub SectionExists {
	my $self = shift;
	my $sect = shift;
	
	return undef if not defined $sect;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
	}
	
	return undef() if not defined $sect;
	return 1 if (grep {/^\Q$sect\E$/} @{$self->{sects}});
	return 0;
}

=head2 AddSection ( $sect_name )

Ensures that the named section exists in the INI file. If the section already
exists, nothing is done. In this case, the "new" section will possibly contain
data already.

If you really need to have a new section with no parameters in it, check that
the name that you're adding isn't in the list of sections already.

=cut

sub AddSection {
	my $self = shift;
	my $sect = shift;
	
	return undef if not defined $sect;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
	}
	
	return if $self->SectionExists($sect);
	CORE::push @{$self->{sects}}, $sect unless
	  grep /^\Q$sect\E$/, @{$self->{sects}};
	$self->_touch_section($sect);

	$self->SetGroupMember($sect);
	
	# Set up the parameter names and values lists
    $self->{parms}{$sect} = [] unless ref $self->{parms}{$sect} eq 'ARRAY';
	if (!defined($self->{v}{$sect})) {
		$self->{sCMT}{$sect} = [];
		$self->{pCMT}{$sect} = {};		# Comments above parameters
		$self->{parms}{$sect} = [];
		$self->{v}{$sect} = {};
	}
}

# Marks a section as modified by us (this includes deleted by us).
sub _touch_section {
	my ($self, $sect)=@_;

	$self->{mysects} ||= [];
	CORE::push @{$self->{mysects}}, $sect unless
	  grep /^\Q$sect\E$/, @{$self->{mysects}};
}

# Marks a parameter as modified by us (this includes deleted by us).
sub _touch_parameter {
	my ($self, $sect, $parm)=@_;

	$self->_touch_section($sect);
	return if (!exists $self->{v}{$sect});
	$self->{myparms}{$sect} ||= [];
	CORE::push @{$self->{myparms}{$sect}}, $parm unless
	  grep /^\Q$parm\E$/, @{$self->{myparms}{$sect}};
}


=head2 DeleteSection ( $sect_name )

Completely removes the entire section from the configuration.

=cut

sub DeleteSection {
	my $self = shift;
	my $sect = shift;
	
	return undef if not defined $sect;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
	}

	# This is done the fast way, change if data structure changes!!
	delete $self->{v}{$sect};
	delete $self->{sCMT}{$sect};
	delete $self->{pCMT}{$sect};
	delete $self->{EOT}{$sect};
	delete $self->{parms}{$sect};
	delete $self->{myparms}{$sect};

	@{$self->{sects}} = grep !/^\Q$sect\E$/, @{$self->{sects}};
	$self->_touch_section($sect);

	if( $sect =~ /^(\S+)\s+\S+/ ) {
		my $group = $1;
		if( defined($self->{group}{$group}) ) {
			@{$self->{group}{$group}} = grep !/^\Q$sect\E$/, @{$self->{group}{$group}};
		} # end if
	} # end if

	return 1;
} # end DeleteSection

=head2 Parameters ($sect_name)

Returns an array containing the parameters contained in the specified
section.

=cut

sub Parameters {
  my $self = shift;
  my $sect = shift;
  
  return undef if not defined $sect;
  
  if ($self->{nocase}) {
    $sect = lc($sect);
  }
  
  return @{$self->{parms}{$sect}} if ref $self->{parms}{$sect} eq 'ARRAY';
  return ();
}

=head2 Groups

Returns an array containing the names of available groups.
  
Groups are specified in the config file as new sections of the form

  [GroupName MemberName]

This is useful for building up lists.  Note that parameters within a
"member" section are referenced normally (i.e., the section name is
still "Groupname Membername", including the space) - the concept of
Groups is to aid people building more complex configuration files.

=cut

sub Groups	{
  my $self = shift;
  return keys %{$self->{group}} if ref $self->{group} eq 'HASH';
  return ();
}

=head2 SetGroupMember ( $sect )

Makes sure that the specified section is a member of the appropriate group.

Only intended for use in newval.

=cut

sub SetGroupMember {
	my $self = shift;
	my $sect = shift;
	
	return undef if not defined $sect;
	
	return(1) unless $sect =~ /^(\S+)\s+\S+/;
	
	my $group = $1;
	if (not exists($self->{group}{$group})) {
		$self->{group}{$group} = [];
	}
	if (not grep {/^\Q$sect\E$/} @{$self->{group}{$group}}) {
		CORE::push @{$self->{group}{$group}}, $sect;
	}
}

=head2 RemoveGroupMember ( $sect )

Makes sure that the specified section is no longer a member of the
appropriate group. Only intended for use in DeleteSection.

=cut

sub RemoveGroupMember {
	my $self = shift;
	my $sect = shift;
	
	return undef if not defined $sect;
	
	return(1) unless $sect =~ /^(\S+)\s+\S+/;
	
	my $group = $1;
	return unless exists $self->{group}{$group};
	@{$self->{group}{$group}} = grep {!/^\Q$sect\E$/} @{$self->{group}{$group}};
}

=head2 GroupMembers ($group)

Returns an array containing the members of specified $group. Each element
of the array is a section name. For example, given the sections

  [Group Element 1]
  ...

  [Group Element 2]
  ...

GroupMembers would return ("Group Element 1", "Group Element 2").

=cut

sub GroupMembers {
  my $self  = shift;
  my $group = shift;
  
  return undef if not defined $group;
  
  if ($self->{nocase}) {
  	$group = lc($group);
  }
  
  return @{$self->{group}{$group}} if ref $self->{group}{$group} eq 'ARRAY';
  return ();
}

=head2 SetWriteMode ($mode)

Sets the mode (permissions) to use when writing the INI file.

$mode must be a string representation of the octal mode.

=cut

sub SetWriteMode
{
	my $self = shift;
	my $mode = shift;
	return undef if not defined ($mode);
	return undef if not ($mode =~ m/[0-7]{3,3}/);
	$self->{file_mode} = $mode;
	return $mode;
}

=head2 GetWriteMode ($mode)

Gets the current mode (permissions) to use when writing the INI file.

$mode is a string representation of the octal mode.

=cut

sub GetWriteMode
{
	my $self = shift;
	return undef if not exists $self->{file_mode};
	return $self->{file_mode};
}

=head2 WriteConfig ($filename [, %options])

Writes out a new copy of the configuration file.  A temporary file
(ending in '-new') is written out and then renamed to the specified
filename.  Also see B<BUGS> below.

If C<-delta> is set to a true value in %options, and this object was
imported from another (see L</new>), only the differences between this
object and the imported one will be recorded. Negative deltas will be
encoded into comments, so that a subsequent invocation of I<new()>
with the same imported object produces the same results (see the
I<-negativedeltas> option in L</new>).

C<%options> is not required.

Returns true on success, C<undef> on failure.

=cut

sub WriteConfig {
  my ($self, $file, %parms)=@_;
  %parms = () unless %parms;
  
  return undef unless defined $file;
  
  
  # If we are using a filename, then do mode checks and write to a 
  # temporary file to avoid a race condition
  if( !ref($file) ) {
    if (-e $file) {
          if (not (-w $file))
          {
                  #carp "File $file is not writable.  Refusing to write config";
                  return undef;
          }
          my $mode = (stat $file)[2];
          $self->{file_mode} = sprintf "%04o", ($mode & 0777);
          #carp "Using mode $self->{file_mode} for file $file";
    } elsif (defined($self->{file_mode}) and not (oct($self->{file_mode}) & 0222)) {
          #carp "Store mode $self->{file_mode} prohibits writing config";
    }
  
    my $new_file = $file . "-new";
    local(*F);
    open(F, "> $new_file") || do {
      carp "Unable to write temp config file $new_file: $!";
      return undef;
    };
    my $oldfh = select(F);
    $self->OutputConfig($parms{-delta});
    close(F);
    select($oldfh);
    rename( $new_file, $file ) || do {
      carp "Unable to rename temp config file ($new_file) to $file: $!";
      return undef;
    };
    if (exists $self->{file_mode}) {
      chmod oct($self->{file_mode}), $file;
    }
  
  } # Otherwise, reset to the start of the file and write, unless we are using STDIN
  else {
    # Get a filehandle, allowing almost any type of 'file' parameter
    ## NB: If this were a filename, this would fail because _make_file 
    ##     opens a read-only handle, but we have already checked that case
    ##     so re-using the logic is ok [JW/WADG]
    my $fh = $self->_make_filehandle( $file );
    if (!$fh) {
      carp "Could not find a filehandle for the input stream ($file): $!";
      return undef;
    }
    
    
    # Only roll back if it's not STDIN (if it is, Carp)
    if( $fh == \*STDIN ) {
      carp "Cannot write configuration file to STDIN.";
    } else {
      seek( $fh, 0, 0 );
      my $oldfh = select($fh);
      $self->OutputConfig($parms{-delta});
      seek( $fh, 0, 0 );
      select($oldfh);
    } # end if

  } # end if (filehandle/name)
  
  return 1;
  
}

=head2 RewriteConfig

Same as WriteConfig, but specifies that the original configuration
file should be rewritten.

=cut

sub RewriteConfig {
  my $self = shift;
  
  return undef if (
    (not exists $self->{cf}) or
    (not defined $self->{cf}) or
    ($self->{cf} eq '')
  );
  
  # Return whatever WriteConfig returns :)
  $self->WriteConfig($self->{cf});
}

=head2 GetFileName

Returns the filename associated with this INI file.

If no filename has been specified, returns undef.

=cut

sub GetFileName
{
	my $self = shift;
	my $filename;
	if (exists $self->{cf}) {
		$filename = $self->{cf};
	} else {
		undef $filename;
	}
	return $filename;
}

=head2 SetFileName ($filename)

If you created the Config::IniFiles object without initialising from
a file, or if you just want to change the name of the file to use for
ReadConfig/RewriteConfig from now on, use this method.

Returns $filename if that was a valid name, undef otherwise.

=cut

sub SetFileName {
  my $self = shift;
  my $newfile = shift;
  
  return undef if not defined $newfile;
  
  if ($newfile ne "") {
    $self->{cf} = $newfile;
    return $self->{cf};
  }
  return undef;
}

=head2 $ini->OutputConfig($delta)

Writes OutputConfig to STDOUT. Use select() to redirect STDOUT to
the output target before calling this function. Optional argument
should be set to 1 if writing only delta.

=cut

sub OutputConfig {
    my ($self, $delta) = @_;

    my($sect, $parm, @cmts);
    my $ors = $self->{line_ends} || $\ || "\n"; # $\ is normally unset, but use input by default
    my $notfirst = 0;
    local $_;
    SECT:
    foreach $sect (@{$self->{$delta ? "mysects" : "sects"}}) {
        if (!defined $self->{v}{$sect}) {
            if ($delta) {
                print "$self->{comment_char} [$sect] is deleted$ors";
            } else {
                warn "Weird unknown section $sect" if $^W;
            }
            next SECT;
        }
        next unless defined $self->{v}{$sect};
        print $ors if $notfirst;
        $notfirst = 1;
        if ((ref($self->{sCMT}{$sect}) eq 'ARRAY') &&
            (@cmts = @{$self->{sCMT}{$sect}})) {
            foreach (@cmts) {
                print "$_$ors";
            }
        }
        print "[$sect]$ors";
        next unless ref $self->{v}{$sect} eq 'HASH';

        PARM:
        foreach $parm (@{$self->{$delta ? "myparms" : "parms"}{$sect}}) {
            if (!defined $self->{v}{$sect}{$parm}) {
                if ($delta) {
                    print "$self->{comment_char} $parm is deleted$ors";
                } else {
                    warn "Weird unknown parameter $parm" if $^W;
                }
                next PARM;
            }
            if ((ref($self->{pCMT}{$sect}{$parm}) eq 'ARRAY') &&
                (@cmts = @{$self->{pCMT}{$sect}{$parm}})) {
                foreach (@cmts) {
                    print "$_$ors";
                }
            }

            my $val = $self->{v}{$sect}{$parm};
            next if ! defined ($val); # No parameter exists !!
            if (ref($val) eq 'ARRAY') {
                my $eotmark = $self->{EOT}{$sect}{$parm} || 'EOT';
                print "$parm= <<$eotmark$ors";
                foreach (@{$val}) {
                    print "$_$ors";
                }
                print "$eotmark$ors";
            } elsif( $val =~ /[$ors]/ ) {
                # The FETCH of a tied hash is never called in 
                # an array context, so generate a EOT multiline
                # entry if the entry looks to be multiline
                my @val = split /[$ors]/, $val, -1;
                if( @val > 1 ) {
                    my $eotmark = $self->{EOT}{$sect}{$parm} || 'EOT';

                    # Make sure the $eotmark does not occur inside the string.
                    my @letters = ('A' .. 'Z');
                    while (index($val, $eotmark) >= 0)
                    {
                        $eotmark .= $letters[rand(@letters)];
                    }

                    print "$parm= <<$eotmark$ors";
                    print map "$_$ors", @val;
                    print "$eotmark$ors";
                } else {
                    print "$parm=$val[0]$ors";
                } # end if
            } else {
                print "$parm=$val$ors";
            }
        }
    }
    foreach my $comment ($self->_GetEndComments()) {
        print "$comment$ors";
    }
    return 1;
}

=head2 SetSectionComment($section, @comment)

Sets the comment for section $section to the lines contained in @comment.

Each comment line will be prepended with the comment charcter (default
is C<#>) if it doesn't already have a comment character (ie: if the
line does not start with whitespace followed by an allowed comment
character, default is C<#> and C<;>).

To clear a section comment, use DeleteSectionComment ($section)

=cut

sub SetSectionComment
{
	my $self = shift;
	my $sect = shift;
	my @comment = @_;

	return undef if not defined $sect;
	return undef unless @comment;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
	}
	
	$self->_touch_section($sect);
	$self->{sCMT}{$sect} = [];
	# At this point it's possible to have a comment for a section that
	# doesn't exist. This comment will not get written to the INI file.
	
	CORE::push @{$self->{sCMT}{$sect}}, $self->_markup_comments(@comment);
	return scalar @comment;
}



# this helper makes sure that each line is preceded with the correct comment
# character
sub _markup_comments 
{
  my $self = shift;
  my @comment = @_;

  my $allCmt = $self->{allowed_comment_char};
  my $cmtChr = $self->{comment_char};
  foreach (@comment) {
    m/^\s*[$allCmt]/ or ($_ = "$cmtChr $_");
  }
  @comment;
}



=head2 GetSectionComment ($section)

Returns a list of lines, being the comment attached to section $section. In 
scalar context, returns a string containing the lines of the comment separated
by newlines.

The lines are presented as-is, with whatever comment character was originally
used on that line.

=cut

sub GetSectionComment
{
	my $self = shift;
	my $sect = shift;

	return undef if not defined $sect;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
	}
	
	if (exists $self->{sCMT}{$sect}) {
		my @ret = @{$self->{sCMT}{$sect}};
        if (wantarray()) {
            return @ret;
        }
        else {
            if (defined ($/)) {
                return join "$/", @ret;
            } else {
                return join "\n", @ret;
            }
        }
	} else {
		return undef;
	}
}

=head2 DeleteSectionComment ($section)

Removes the comment for the specified section.

=cut

sub DeleteSectionComment
{
	my $self = shift;
	my $sect = shift;
	
	return undef if not defined $sect;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
	}
	$self->_touch_section($sect);

	delete $self->{sCMT}{$sect};
}

=head2 SetParameterComment ($section, $parameter, @comment)

Sets the comment attached to a particular parameter.

Any line of @comment that does not have a comment character will be
prepended with one. See L</SetSectionComment($section, @comment)> above

=cut

sub SetParameterComment
{
	my $self = shift;
	my $sect = shift;
	my $parm = shift;
	my @comment = @_;

	defined($sect) || return undef;
	defined($parm) || return undef;
	@comment || return undef;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
		$parm = lc($parm);
	}
	
	$self->_touch_parameter($sect, $parm);
	if (not exists $self->{pCMT}{$sect}) {
		$self->{pCMT}{$sect} = {};
	}
	
	$self->{pCMT}{$sect}{$parm} = [];
	# Note that at this point, it's possible to have a comment for a parameter,
	# without that parameter actually existing in the INI file.
	CORE::push @{$self->{pCMT}{$sect}{$parm}}, $self->_markup_comments(@comment);
	return scalar @comment;
}

sub _SetEndComments
{
    my $self = shift;
    my @comments = @_;

    $self->{_comments_at_end_of_file} = \@comments;

    return 1;
}

sub _GetEndComments {
    my $self = shift;

    return @{$self->{_comments_at_end_of_file}};
}

=head2 GetParameterComment ($section, $parameter)

Gets the comment attached to a parameter. In list context returns all
comments - in scalar context returns them joined by newlines.

=cut

sub GetParameterComment
{
	my $self = shift;
	my $sect = shift;
	my $parm = shift;
	
	defined($sect) || return undef;
	defined($parm) || return undef;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
		$parm = lc($parm);
	};
	
	exists($self->{pCMT}{$sect}) || return undef;
	exists($self->{pCMT}{$sect}{$parm}) || return undef;
	
	my @comment = @{$self->{pCMT}{$sect}{$parm}};
	return wantarray() ? @comment : join((defined $/ ? $/ : "\n"), @comment);
}

=head2 DeleteParameterComment ($section, $parmeter)

Deletes the comment attached to a parameter.

=cut

sub DeleteParameterComment
{
	my $self = shift;
	my $sect = shift;
	my $parm = shift;
	
	defined($sect) || return undef;
	defined($parm) || return undef;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
		$parm = lc($parm);
	};
	
	# If the parameter doesn't exist, our goal has already been achieved
	exists($self->{pCMT}{$sect}) || return 1;
	exists($self->{pCMT}{$sect}{$parm}) || return 1;

	$self->_touch_parameter($sect, $parm);
	delete $self->{pCMT}{$sect}{$parm};
	return 1;
}

=head2 GetParameterEOT ($section, $parameter)

Accessor method for the EOT text (in fact, style) of the specified parameter. If any text is used as an EOT mark, this will be returned. If the parameter was not recorded using HERE style multiple lines, GetParameterEOT returns undef.

=cut

sub GetParameterEOT
{
	my $self = shift;
	my $sect = shift;
	my $parm = shift;

	defined($sect) || return undef;
	defined($parm) || return undef;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
		$parm = lc($parm);
	};

	if (not exists $self->{EOT}{$sect}) {
		$self->{EOT}{$sect} = {};
	}

	if (not exists $self->{EOT}{$sect}{$parm}) {
		return undef;
	}
	return $self->{EOT}{$sect}{$parm};
}

=head2 $cfg->SetParameterEOT ($section, $parameter, $EOT)

Accessor method for the EOT text for the specified parameter. Sets the HERE style marker text to the value $EOT. Once the EOT text is set, that parameter will be saved in HERE style.

To un-set the EOT text, use DeleteParameterEOT ($section, $parameter).

=cut

sub SetParameterEOT
{
	my $self = shift;
	my $sect = shift;
	my $parm = shift;
	my $EOT = shift;

	defined($sect) || return undef;
	defined($parm) || return undef;
	defined($EOT) || return undef;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
		$parm = lc($parm);
	};

	$self->_touch_parameter($sect, $parm);
    if (not exists $self->{EOT}{$sect}) {
        $self->{EOT}{$sect} = {};
    }

    $self->{EOT}{$sect}{$parm} = $EOT;
}

=head2 DeleteParameterEOT ($section, $parmeter)

Removes the EOT marker for the given section and parameter.
When writing a configuration file, if no EOT marker is defined 
then "EOT" is used.

=cut

sub DeleteParameterEOT
{
	my $self = shift;
	my $sect = shift;
	my $parm = shift;
	
	defined($sect) || return undef;
	defined($parm) || return undef;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
		$parm = lc($parm);
	}

	$self->_touch_parameter($sect, $parm);
	delete $self->{EOT}{$sect}{$parm};
}


=head2 Delete

Deletes the entire configuration file in memory.

=cut

sub Delete {
	my $self = shift;

	foreach my $section ($self->Sections()) {
		$self->DeleteSection($section);
	}

	return 1;
} # end Delete



=head1 USAGE -- Tied Hash

=head2 tie %ini, 'Config::IniFiles', (-file=>$filename, [-option=>value ...] )

Using C<tie>, you can tie a hash to a B<Config::IniFiles> object. This creates a new
object which you can access through your hash, so you use this instead of the 
B<new> method. This actually creates a hash of hashes to access the values in 
the INI file. The options you provide through C<tie> are the same as given for 
the B<new> method, above.

Here's an example:

  use Config::IniFiles;
  
  my %ini
  tie %ini, 'Config::IniFiles', ( -file => "/path/configfile.ini" );

  print "We have $ini{Section}{Parameter}." if $ini{Section}{Parameter};

Accessing and using the hash works just like accessing a regular hash and 
many of the object methods are made available through the hash interface.

For those methods that do not coincide with the hash paradigm, you can use 
the Perl C<tied> function to get at the underlying object tied to the hash 
and call methods on that object. For example, to write the hash out to a new
ini file, you would do something like this:

  tied( %ini )->WriteConfig( "/newpath/newconfig.ini" ) ||
    die "Could not write settings to new file.";

=head2 $val = $ini{$section}{$parameter}

Returns the value of $parameter in $section. 

Multiline values accessed through a hash will be returned 
as a list in list context and a concatenated value in scalar
context.

=head2 $ini{$section}{$parameter} = $value;

Sets the value of C<$parameter> in C<$section> to C<$value>. 

To set a multiline or multiv-alue parameter just assign an 
array reference to the hash entry, like this:

 $ini{$section}{$parameter} = [$value1, $value2, ...];

If the parameter did not exist in the original file, it will 
be created. However, Perl does not seem to extend autovivification 
to tied hashes. That means that if you try to say

  $ini{new_section}{new_paramters} = $val;

and the section 'new_section' does not exist, then Perl won't 
properly create it. In order to work around this you will need 
to create a hash reference in that section and then assign the
parameter value. Something like this should do nicely:

  $ini{new_section} = {};
  $ini{new_section}{new_paramters} = $val;

=head2 %hash = %{$ini{$section}}

Using the tie interface, you can copy whole sections of the 
ini file into another hash. Note that this makes a copy of 
the entire section. The new hash in no longer tied to the 
ini file, In particular, this means -default and -nocase 
settings will not apply to C<%hash>.


=head2 $ini{$section} = {}; %{$ini{$section}} = %parameters;

Through the hash interface, you have the ability to replace 
the entire section with a new set of parameters. This call
will fail, however, if the argument passed in NOT a hash 
reference. You must use both lines, as shown above so that 
Perl recognizes the section as a hash reference context 
before COPYing over the values from your C<%parameters> hash.

=head2 delete $ini{$section}{$parameter}

When tied to a hash, you can use the Perl C<delete> function
to completely remove a parameter from a section.

=head2 delete $ini{$section}

The tied interface also allows you to delete an entire 
section from the ini file using the Perl C<delete> function.

=head2 %ini = ();

If you really want to delete B<all> the items in the ini file, this 
will do it. Of course, the changes won't be written to the actual
file unless you call B<RewriteConfig> on the object tied to the hash.

=head2 Parameter names

=over 4

=item my @keys = keys %{$ini{$section}}

=item while (($k, $v) = each %{$ini{$section}}) {...}

=item if( exists %{$ini{$section}}, $parameter ) {...}

=back

When tied to a hash, you use the Perl C<keys> and C<each> 
functions to iteratively list the parameters (C<keys>) or 
parameters and their values (C<each>) in a given section.

You can also use the Perl C<exists> function to see if a 
parameter is defined in a given section.

Note that none of these will return parameter names that 
are part of the default section (if set), although accessing
an unknown parameter in the specified section will return a 
value from the default section if there is one.


=head2 Section names

=over 4

=item foreach( keys %ini ) {...}

=item while (($k, $v) = each %ini) {...}

=item if( exists %ini, $section ) {...}

=back

When tied to a hash, you use the Perl C<keys> and C<each> 
functions to iteratively list the sections in the ini file.

You can also use the Perl C<exists> function to see if a 
section is defined in the file.

=cut

############################################################
#
# TIEHASH Methods
#
# Description:
# These methods allow you to tie a hash to the 
# Config::IniFiles object. Note that, when tied, the 
# user wants to look at thinks like $ini{sec}{parm}, but the 
# TIEHASH only provides one level of hash interace, so the 
# root object gets asked for a $ini{sec}, which this 
# implements. To further tie the {parm} hash, the internal 
# class Config::IniFiles::_section, is provided, below.
#
############################################################
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# ----------------------------------------------------------
sub TIEHASH {
  my $class = shift;
  my %parms = @_;

  # Get a new object
  my $self = $class->new( %parms );

  return $self;
} # end TIEHASH


# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# ----------------------------------------------------------
sub FETCH {
  my $self = shift;
  my( $key ) = @_;

  $key = lc($key) if( $self->{nocase} );
  return if (! $self->{v}{$key});

  my %retval;
  tie %retval, 'Config::IniFiles::_section', $self, $key;
  return \%retval;

} # end FETCH

# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000Jun14 Fixed bug where wrong ref was saved           JW
# 2000Oct09 Fixed possible but in %parms with defaults    JW
# 2001Apr04 Fixed -nocase problem in storing              JW
# ----------------------------------------------------------
sub STORE {
  my $self = shift;
  my( $key, $ref ) = @_;

  return undef unless ref($ref) eq 'HASH';

  $key = lc($key) if( $self->{nocase} );

  $self->AddSection($key);
  $self->{v}{$key} = {%$ref};
  $self->{parms}{$key} = [keys %$ref];
  $self->{myparms}{$key} = [keys %$ref];
  1;
} # end STORE


# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# 2000Dec17 Now removes comments, groups and EOTs too     JW
# 2001Arp04 Fixed -nocase problem                         JW
# ----------------------------------------------------------
sub DELETE {
  my $self = shift;
  my( $key ) = @_;

  my $retval=$self->FETCH($key);
  $self->DeleteSection($key);
  return $retval;
} # end DELETE


# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# ----------------------------------------------------------
sub CLEAR {
  my $self = shift;

  return $self->Delete();
} # end CLEAR

# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# ----------------------------------------------------------
sub FIRSTKEY {
  my $self = shift;

  $self->{tied_enumerator}=0;
  return $self->NEXTKEY();
} # end FIRSTKEY


# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# ----------------------------------------------------------
sub NEXTKEY {
  my $self = shift;
  my( $last ) = @_;

  my $i=$self->{tied_enumerator}++;
  my $key=$self->{sects}[$i]; 
  return if (! defined $key);
  return wantarray ? ($key, $self->FETCH($key)) : $key;
} # end NEXTKEY


# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# 2001Apr04 Fixed -nocase bug and false true bug          JW
# ----------------------------------------------------------
sub EXISTS {
  my $self = shift;
  my( $key ) = @_;
  return $self->SectionExists($key);
} # end EXISTS


# ----------------------------------------------------------
# DESTROY is used by TIEHASH and the Perl garbage collector,
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# ----------------------------------------------------------
sub DESTROY {
  # my $self = shift;
} # end if


# ----------------------------------------------------------
# Sub: _make_filehandle
#
# Args: $thing
#	$thing	An input source
#
# Description: Takes an input source of a filehandle, 
# filehandle glob, reference to a filehandle glob, IO::File
# object or scalar filename and returns a file handle to 
# read from it with.
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 06Dec2001 Added to support input from any source        JW
# ----------------------------------------------------------
sub _make_filehandle {
  my $self = shift;

  #
  # This code is 'borrowed' from Lincoln D. Stein's GD.pm module
  # with modification for this module. Thanks Lincoln!
  #
  
  no strict 'refs';
  my $thing = shift;

  if (ref($thing) eq "SCALAR") {
	  if (eval { require IO::Scalar; $IO::Scalar::VERSION >= 2.109; }) {
		  return IO::Scalar->new($thing);
	  } else {
		  warn "SCALAR reference as file descriptor requires IO::stringy ".
			"v2.109 or later" if ($^W);
		  return;
	  }
  }

  return $thing if defined(fileno $thing);
#  return $thing if defined($thing) && ref($thing) && defined(fileno $thing);
  
  # otherwise try qualifying it into caller's package
  my $fh = qualify_to_ref($thing,caller(1));
  return $fh if defined(fileno $fh);
#  return $fh if defined($thing) && ref($thing) && defined(fileno $fh);
  
  # otherwise treat it as a file to open
  $fh = gensym;
  open($fh,$thing) || return;
  
  return $fh;
} # end _make_filehandle

############################################################
#
# INTERNAL PACKAGE: Config::IniFiles::_section
#
# Description:
# This package is used to provide a single-level TIEHASH
# interface to the sections in the IniFile. When tied, the 
# user wants to look at thinks like $ini{sec}{parm}, but the 
# TIEHASH only provides one level of hash interace, so the 
# root object gets asked for a $ini{sec} and must return a 
# has reference that accurately covers the '{parm}' part.
#
# This package is only used when tied and is inter-woven 
# between the sections and their parameters when the TIEHASH
# method is called by Perl. It's a very simple implementation
# of a tied hash object that simply maps onto the object API.
#
############################################################
# Date        Modification                            Author
# ----------------------------------------------------------
# 2000.May.09 Created to excapsulate TIEHASH interface    JW
############################################################
package Config::IniFiles::_section;

use strict;
use Carp;
use vars qw( $VERSION );

$Config::IniFiles::_section::VERSION = 2.16;

# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::TIEHASH
#
# Args: $class, $config, $section
#	$class	  The class that this is being tied to.
#   $config   The parent Config::IniFiles object
#   $section  The section this tied object refers to
#
# Description: Builds the object that implements accesses to
# the tied hash.
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# ----------------------------------------------------------
sub TIEHASH {
  my $proto = shift;
  my $class = ref($proto) || $proto;
  my ($config, $section)=@_;

  # Make a new object
  return bless {config=>$config, section=>$section}, $class;
} # end TIEHASH


# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::FETCH
#
# Args: $key
#	$key	The name of the key whose value to get
#
# Description: Returns the value associated with $key. If
# the value is a list, returns a list reference.
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000Jun15 Fixed bugs in -default handler                JW
# 2000Dec07 Fixed another bug in -deault handler          JW
# 2002Jul04 Returning scalar values (Bug:447532)          AS
# ----------------------------------------------------------
sub FETCH {
	my ($self, $key)=@_;
	my @retval=$self->{config}->val($self->{section}, $key);
	return (@retval <= 1) ? $retval[0] : \@retval;
} # end FETCH


# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::STORE
#
# Args: $key, @val
#	$key	The key under which to store the value
#	@val	The value to store, either an array or a scalar
#
# Description: Sets the value for the specified $key
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2001Apr04 Fixed -nocase bug                             JW
# ----------------------------------------------------------
sub STORE {
	my ($self, $key, @val)=@_;
	return $self->{config}->newval($self->{section}, $key, @val);
} # end STORE


# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::DELETE
#
# Args: $key
#	$key	The key to remove from the hash
#
# Description: Removes the specified key from the hash and
# returns its former value.
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2001Apr04 Fixed -nocase bug                              JW
# ----------------------------------------------------------
sub DELETE {
	my ($self, $key)=@_;
	my $retval=$self->{config}->val($self->{section}, $key);
	$self->{config}->delval($self->{section}, $key);
	return $retval;
} # end DELETE

# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::CLEAR
#
# Args: (None)
#
# Description: Empties the entire hash
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# ----------------------------------------------------------
sub CLEAR    {
  my ($self) = @_;
  return $self->{config}->DeleteSection($self->{section});
} # end CLEAR

# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::EXISTS
#
# Args: $key
#	$key	The key to look for
#
# Description: Returns whether the key exists
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2001Apr04 Fixed -nocase bug                             JW
# ----------------------------------------------------------
sub EXISTS   {
  my ($self, $key)=@_;
  return $self->{config}->exists($self->{section},$key);
} # end EXISTS

# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::FIRSTKEY
#
# Args: (None)
#
# Description: Returns the first key in the hash
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# ----------------------------------------------------------
sub FIRSTKEY {
  my $self = shift;

  $self->{tied_enumerator}=0;
  return $self->NEXTKEY();
} # end FIRSTKEY

# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::NEXTKEY
#
# Args: $last
#	$last	The last key accessed by the interator
#
# Description: Returns the next key in line
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# ----------------------------------------------------------
sub NEXTKEY  {
  my $self = shift;
  my( $last ) = @_;

  my $i=$self->{tied_enumerator}++;
  my @keys = $self->{config}->Parameters($self->{section});
  my $key=$keys[$i];
  return if (! defined $key);
  return wantarray ? ($key, $self->FETCH($key)) : $key;
} # end NEXTKEY


# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::DESTROY
#
# Args: (None)
#
# Description: Called on cleanup
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# ----------------------------------------------------------
sub DESTROY  {
  # my $self = shift
} # end DESTROY

1;

=head1 IMPORT / DELTA FEATURES

The I<-import> option to L</new> allows one to stack one
I<Config::IniFiles> object on top of another (which might be itself
stacked in turn and so on recursively, but this is beyond the
point). The effect, as briefly explained in L</new>, is that the
fields appearing in the composite object will be a superposition of
those coming from the ``original'' one and the lines coming from the
file, the latter taking precedence. For example, let's say that
C<$master> and C<overlay> were created like this:

   my $master  = Config::IniFiles->new(-file => "master.ini");
   my $overlay = Config::IniFiles->new(-file => "overlay.ini",
			-import => $master);

If the contents of C<master.ini> and C<overlay.ini> are respectively

   ; master.ini
   [section1]
   arg0=unchanged from master.ini
   arg1=val1

   [section2]
   arg2=val2

and

   ; overlay.ini
   [section1]
   arg1=overriden

Then C<< $overlay->val("section1", "arg1") >> is "overriden", while
C<< $overlay->val("section1", "arg0") >> is "unchanged from
master.ini".

This feature may be used to ship a ``global defaults'' configuration
file for a Perl application, that can be overridden piecewise by a
much shorter, per-site configuration file. Assuming UNIX-style path
names, this would be done like this:

   my $defaultconfig = Config::IniFiles->new
       (-file => "/usr/share/myapp/myapp.ini.default");
   my $config = Config::IniFiles->new
       (-file => "/etc/myapp.ini", -import => $defaultconfig);
   # Now use $config and forget about $defaultconfig in the rest of
   # the program

Starting with version 2.39, I<Config::IniFiles> also provides features
to keep the importing / per-site configuration file small, by only
saving those options that were modified by the running program. That
is, if one calls

   $overlay->setval("section1", "arg1", "anotherval");
   $overlay->newval("section3", "arg3", "val3");
   $overlay->WriteConfig(-delta=>1);

C<overlay.ini> would now contain

   ; overlay.ini
   [section1]
   arg1=anotherval

   [section3]
   arg3=val3

This is called a I<delta file> (see L</WriteConfig>). The untouched
[section2] and arg0 do not appear, and the config file is therefore
shorter; while of course, reloading the configuration into C<$master>
and C<$overlay>, either through C<< $overlay->ReadConfig() >> or through
the same code as above (e.g. when application restarts), would yield
exactly the same result had the overlay object been saved in whole to
the file system.

The only problem with this delta technique is one cannot delete the
default values in the overlay configuration file, only change
them. This is solved by a file format extension, enabled by the
I<-negativedeltas> option to L</new>: if, say, one would delete
parameters like this,

   $overlay->DeleteSection("section2");
   $overlay->delval("section1", "arg0");
   $overlay->WriteConfig(-delta=>1);

The I<overlay.ini> file would now read:

   ; overlay.ini
   [section1]
   ; arg0 is deleted
   arg1=anotherval

   ; [section2] is deleted

   [section3]
   arg3=val3

Assuming C<$overlay> was later re-read with C<< -negativedeltas => 1 >>,
the parser would interpret the deletion comments to yield the correct
result, that is, [section2] and arg0 would cease to exist in the
C<$overlay> object.

=cut


=head1 DIAGNOSTICS

=head2 @Config::IniFiles::errors

Contains a list of errors encountered while parsing the configuration
file.  If the I<new> method returns B<undef>, check the value of this
to find out what's wrong.  This value is reset each time a config file
is read.

=head1 BUGS

=over 3

=item *

The output from [Re]WriteConfig/OutputConfig might not be as pretty as
it can be.  Comments are tied to whatever was immediately below them.
And case is not preserved for Section and Parameter names if the -nocase
option was used.

=item *

No locking is done by [Re]WriteConfig.  When writing servers, take
care that only the parent ever calls this, and consider making your
own backup.

=back

=head1 Data Structure

Note that this is only a reference for the package maintainers - one of the
upcoming revisions to this package will include a total clean up of the
data structure.

  $iniconf->{cf} = "config_file_name"
          ->{startup_settings} = \%orginal_object_parameters
          ->{firstload} = 0 OR 1
          ->{imported} = $object WHERE $object->isa("Config::IniFiles")
          ->{nocase} = 0
          ->{reloadwarn} = 0
          ->{sects} = \@sections
          ->{mysects} = \@sections
          ->{sCMT}{$sect} = \@comment_lines
          ->{group}{$group} = \@group_members
          ->{parms}{$sect} = \@section_parms
          ->{myparms}{$sect} = \@section_parms
          ->{EOT}{$sect}{$parm} = "end of text string"
          ->{pCMT}{$sect}{$parm} = \@comment_lines
          ->{v}{$sect}{$parm} = $value   OR  \@values

=head1 AUTHOR and ACKNOWLEDGEMENTS

The original code was written by Scott Hutton.
Then handled for a time by Rich Bowen (thanks!),
It is now managed by Jeremy Wadsack,
with many contributions from various other people.

In particular, special thanks go to (in roughly chronological order):

Bernie Cosell, Alan Young, Alex Satrapa, Mike Blazer, Wilbert van de Pieterman,
Steve Campbell, Robert Konigsberg, Scott Dellinger, R. Bernstein,
Daniel Winkelmann, Pires Claudio, Adrian Phillips, 
Marek Rouchal, Luc St Louis, Adam Fischler, Kay R�pke, Matt Wilson, 
Raviraj Murdeshwar and Slaven Rezic, Florian Pfaff

Geez, that's a lot of people. And apologies to the folks who were missed.

If you want someone to bug about this, that would be:

	Jeremy Wadsack <dgsupport at wadsack-allen dot com>

If you want more information, or want to participate, go to:

	http://sourceforge.net/projects/config-inifiles/

Please send bug reports to config-inifiles-bugs@lists.sourceforge.net

Development discussion occurs on the mailing list
config-inifiles-dev@lists.sourceforge.net, which you can subscribe
to by going to the project web site (link above).

This program is free software; you can redistribute it and/or 
modify it under the same terms as Perl itself.

=cut

1;

# Please keep the following within the last four lines of the file
#[JW for editor]:mode=perl:tabSize=8:indentSize=2:noTabs=true:indentOnEnter=true:

