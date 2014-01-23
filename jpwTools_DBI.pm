#!/usr/bin/perl
#
# Copyright (C) 2011 by John P. Weiss
#
# This package is free software; you can redistribute it and/or modify
# it under the terms of the Artistic License, included as the file
# "LICENSE" in the source code archive.
#
# This package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the file "LICENSE", containing
# the License John Weiss originally placed this program under.
#
# $Id$
############


############
#
# Std. Package Boilerplate
#
############


package jpwTools_DBI;
require 5;
use strict;

BEGIN {
    use Exporter ();
    our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

    # if using RCS/CVS, this may be preferred
    $VERSION = do { my @r = (q$Revision: 1401 $ =~ /\d+/g); sprintf "%d."."%02d" x $#r, @r }; # must be all one line, for MakeMaker

    @ISA         = qw(Exporter);

    # Default exports.
    @EXPORT = qw(xx ut);
    # Permissable exports.
    # your exported package globals go here,
    # as well as any optionally exported functions
    @EXPORT_OK = qw($_AbortOnError $_Verbose $_UnitTest);

    # Tagged groups of exports; 'perldoc Exporter' for details.
    %EXPORT_TAGS = (all => [@EXPORT, @EXPORT_OK]);
}
our @EXPORT_OK;

# Other Imported Packages/requirements.
use Carp;
use jpwTools;
use DBI;


############
#
# Global Variables
#
############


our $_Verbose;  $_Verbose = 0;
our $_UnitTest; $_UnitTest = 0;
our $_AbortOnError;  $_AbortOnError = 1;


############
#
# Internal Variables
#
############


my @_RecursiveRegexpGrouper_Stack = ();
my %_dbiDrivers = ();
$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Indent = 1;
# None


############
#
# Internal Functions
#
############


INIT {
    my @dbidrv = DBI->available_drivers();
    my @dbidrv_lc = map(lc, @dbidrv);
    @_dbiDrivers{@dbidrv_lc} = @dbidrv;
}


sub set_handle_defaults($) {
    my $handle=shift();

    $handle->{'RaiseError'} = 1;
    $handle->{'PrintError'} = 0;
    $handle->{'ChopBlanks'} = 1;
    $handle->{'FetchHashKeyName'} = 'NAME_lc';
}


sub extract_db_args(\$\$\$\$\$%) {
    my $ref_dbName=shift();
    my $ref_host=shift();
    my $ref_port=shift();
    my $ref_username=shift();
    my $ref_passwd=shift();
    my %dbArgs = @_;

    lc_keys(%dbArgs);

    $$ref_dbName = (exists($dbArgs{'db'})
                    ? $dbArgs{'db'}
                    : (exists($dbArgs{'database'})
                       ? $dbArgs{'database'}
                       : undef));
    $$ref_host = (exists($dbArgs{'host'}) ? $dbArgs{'host'} : undef);
    $$ref_port =  (exists($dbArgs{'port'}) ? $dbArgs{'port'} : undef);

    # The username and password have a default value of '' rather than undef.
    $$ref_username =  (exists($dbArgs{'user'})
                       ? $dbArgs{'user'}
                       : (exists($dbArgs{'username'})
                          ? $dbArgs{'username'}
                          : ''));
    $$ref_passwd =  (exists($dbArgs{'passwd'})
                     ? $dbArgs{'passwd'}
                     : (exists($dbArgs{'password'})
                        ? $dbArgs{'password'}
                        : ''));
}


sub disassemble_dsn($\$\$\$\$) {
    my $dsn=shift();
    my $ref_driver=shift();
    my $ref_dbName=shift();
    my $ref_username=shift();
    my $ref_passwd=shift();

    my ($dummy, $driverArgs, $host, $port);

    ($dummy, $$ref_driver, $dummy, $dummy, $driverArgs)
        = DBI->parse_dsn($dsn)
            or return 0;

    my %driverArgs = map({ split(/=/); } split(/;/, $driverArgs));
    extract_db_args($$ref_dbName, $host, $port, $$ref_username, $$ref_passwd,
                    %driverArgs);
    return 1;
}


sub assemble_dsn(\%$$\$\$) {
    my $ref_dbArgs=shift();
    my $driver=shift();
    my $dbName=shift();
    my $ref_username=shift();
    my $ref_passwd=shift();

    my ($dummy, $driverArgs, $host, $port, $sqliteFile);
    extract_db_args($dummy, $host, $port, $$ref_username, $$ref_passwd,
                    %$ref_dbArgs);

    my $dsn = 'dbi:'.$driver;
    if (lc($driver) eq 'sqlite') {
        $dsn .= ':database=';
        $dsn .= $dbName;
    } else {
        $dsn .= ':database=';
        $dsn .= $dbName;
        if (defined($host)) {
            $dsn .= ';host=';
            $dsn .= $host;
        }
        if (defined($port)) {
            $dsn .= ';port=';
            $dsn .= $port;
        }
    }

    return $dsn;
}


sub yy {
}


############
#
# Exported Functions
#
############


sub check_dbicmd_status {
    # Save the value of "$@" before doing anything else.
    my $errMsgCaught="$@";
    my $ref_flags = ((scalar(@_) && (ref($_[0]) eq 'HASH'))
                     ? shift() : {});

    # As in 'check_syscmd_status()', Return %Carp::CarpInternal and
    # $Carp::Verbose to their original states if there's no error.
    local %Carp::CarpInternal;
    local $Carp::Verbose;
    ++$Carp::CarpInternal{jpwTools_DB};
    $Carp::Verbose = (($_Verbose > 1) || $_UnitTest);

    my $abortOnError = ($_AbortOnError ? $_AbortOnError : 0);
    my $noStacktrace = 0;
    if (exists($ref_flags->{"no_stacktrace"})) {
        $noStacktrace = $ref_flags->{"no_stacktrace"};
    }
    # Warn/Abort flags (mutually-exclusive)
    if (exists($ref_flags->{"warn"})) {
        if ($ref_flags->{"warn"}) {
            $abortOnError = 0;
        }
    } elsif (exists($ref_flags->{"abort"}) && $ref_flags->{"abort"}) {
        $abortOnError = $ref_flags->{"abort"};
    }

    my $where =  (exists($ref_flags->{"where"})
                  ? 'In '.$ref_flags->{"where"}.'():  '
                  : '');

    # The Error Checking:  First see if there was a caught error.  Those
    # should override any other error processing.
    my $errMsg = undef;
    if ($errMsgCaught) {
        $errMsg = $where;
        $errMsg .= '"';
        $errMsg .= $errMsgCaught;
        $errMsg .= '"';
    } elsif (exists($ref_flags->{"missingArg"})) {
        $errMsg = $where;
        $errMsg .= 'Missing required argument:  "';
        $errMsg .= $ref_flags->{"missingArg"};
        $errMsg .= '"';
    } elsif (exists($ref_flags->{"unknownDriver"})) {
        $errMsg = $where;
        $errMsg .= 'Driver "';
        $errMsg .= $ref_flags->{"unknownDriver"};
        $errMsg .= '" is not supported/installed.';
    } elsif (exists($ref_flags->{"bad_dsn"})) {
        $errMsg = $where;
        $errMsg .= 'Invalid DSN, "';
        $errMsg .= $ref_flags->{"bad_dsn"};
        $errMsg .= '"';
    } elsif (exists($ref_flags->{"connect"})) {
        unless (defined($ref_flags->{"connect"})) {
            $errMsg = $where;
            $errMsg .= 'Connection ';
            if (exists($ref_flags->{"dsn"})) {
                $errMsg .= "to '";
                $errMsg .= $ref_flags->{"dsn"};
                $errMsg .= "'";
            } else {
                $errMsg .= 'attempt';
            }
            $errMsg .= ' failed.\n\tReason: "';
            $errMsg .= $DBI::errstr;
            $errMsg .= '"';
        }
    }

    #
    # Handle the Error
    #

    if (defined($errMsg)) {
        if ($noStacktrace) {
            print STDERR ($errMsg);
            if ($abortOnError) {
                print "\nAborting...\n";
                exit $abortOnError;
            }
        } else {
            croak($errMsg) if ($abortOnError);
            # else:
            carp($errMsg);
        }
    }
}


sub set_ReadOnly($;$) {
    my $handle=shift();
    my $is_ro=(scalar(@_) ? shift() : 1);
}


sub db_connect($$;$%) {
    my $db_driver=shift();
    my $db_name=shift();
    my $ref_ourOpts=(scalar(@_) ? shift() : {});
    my %connOpts=@_;

    unless (ref($ref_ourOpts) eq 'HASH') {
        local %Carp::CarpInternal;
        ++$Carp::CarpInternal{jpwTools_DB};
        my $wrongType = "scalar value";
        if (ref($ref_ourOpts)) {
            $wrongType = lc(ref($ref_ourOpts));
            $wrongType .= " reference";
        }
        confess('Type of arg 3 to jpwTools_DBI::db_connect must be '.
                'an anonymous hash reference (not '.$wrongType.')');
    }

    # Parse the args. splitting them up if needed.
    my ($username, $passwd);
    my $dbi_dsn = $db_name;
    if ($dbi_dsn =~ m/^dbi:/i) {
        my $stat = disassemble_dsn($dbi_dsn, $db_driver, $db_name,
                                   $username, $passwd);
        unless ($stat) {
            # Fail:  dsn is wrong.
            check_dbicmd_status({"where" => "db_connect",
                                 "bad_dsn" => $dbi_dsn});
        }
    } else {
        # No db name is an error.
        unless ($db_name) {
            # Fail:  missing $db_name
            check_dbicmd_status({"where" => "db_connect",
                                 "missingArg" => "db_name"});
        }

        # Check that the driver is supported
        if (exists($_dbiDrivers{lc($db_driver)})) {
            # Look up the driver based on its lowercased form.
            $db_driver = $_dbiDrivers{lc($db_driver)};
        } else {
            # Fail:  Unsupported driver
            check_dbicmd_status({"where" => "db_connect",
                                 "unknownDriver" => $db_driver});
        }

        $dbi_dsn = assemble_dsn(%$ref_ourOpts, $db_driver, $db_name,
                                $username, $passwd);
    }

    if ($_UnitTest || $_Verbose) {
        print ("Connecting to:  \"", $dbi_dsn, "\"\n");
        if ($username) {
            print ("\t(Using username:  \"", $username, "\")\n");
        }
    }

    # Force these so that connection errors "throw and exception".
    $connOpts{'RaiseError'} = 1;
    $connOpts{'PrintError'} = 0;

    my $dbh;
    eval {
        $dbh = DBI->connect($dbi_dsn, $username, $passwd, \%connOpts);
    };
    check_dbicmd_status({"where" => "db_connect", "dsn" => $dbi_dsn,
                         "connect" => $dbh});

    set_handle_defaults($dbh);
    return $dbh;
}


sub xx {
    #print "usage: $_MyName [args...]\n";
    exit 1;
}


# Unit-testing stub function.  Used for testing internal member fns.
sub ut {
    $jpwTools::_UnitTest=1;
    $_UnitTest=1;

    dbgprint(0, ['_dbiDrivers', \%_dbiDrivers]);
    exit 0;
}


1;  # don't forget to return a true value from the file
## POD STARTS HERE ##
__END__

=head1 NAME

jpwTools_DBI - Package containing John's DBI Perl Tools.

=head1 SYNOPSIS

=over 1

=item check_dbicmd_status([I<ctrlRef>])

=item set_ReadOnly(I<handle> [, I<roBool>])

=item db_connect(I<driver>, I<db> [, I<flags>, I<connectFlags>])

=back

=head1 DESCRIPTION

=over 2

=item *

check_dbicmd_status([I<ctrlRef>])

Central function for checking the status of DBI commands and printing errors.
It can be used to handle any "exceptions" thrown by a DBI command, like so:

    eval {
        DBI->some_db_command(I<...args...>);
    };
    check_dbicmd_status({"where" => I<callingFnName>});

If you call the DBI functions through the functions in this package, you won't
need to call C<check_dbicmd_status> or use a code pattern like the one shown
above.

If I<ctrlRef> is a hash reference, its elements affect the behavior of
C<check_dbicmd_status>.  The valid keys and what they do are as follows:

=over 1

=item C<no_stacktrace>

Setting this flag to C<1> causes C<check_dbicmd_status> to omit the
stacktrace that it normally creates.

=item C<warn>

If the value of this key evaluates to true, C<check_dbicmd_status> won't abort
if there's an error.  Mutually-exclusive with C<abort>.

Overrides the default behavior specified by C<$jpwTools_DBI::_AbortOnError>.

=item C<abort>

The value of this flag directly controls whether or not C<check_dbicmd_status>
aborts on error.  Mutually-exclusive with C<warn>.

Used instead of the C<$jpwTools_DBI::_AbortOnError> package variable.

=item C<where>

This should be a string containing the name of the function that called
C<check_dbicmd_status> (and, therefore, called the DBI function).  If present,
it will be prepended to the error message in a standard format (including
appending the string, "():  ", to the function name).

=item C<dsn>

Special value used by the C<connect> key (see below).  Contains the
DSN of the attempted connection.  Ignored otherwise.

=item C<missingArg>|C<unknownDriver>|C<bad_dsn>|C<connect> (and C<$@>)

The following set of keys control what error-message is generated.  They are
all mutually exclusive, and checked in the order shown below.  Additionally if
the global C<$@> variable is set, it overrides all of these flags.

=over 2

=item C<$@>

Before checking I<ctrlRef> for any of the error-message flags,
C<check_dbicmd_status> first looks at the value of the C<$@> variable.  If
it's set, C<check_dbicmd_status> generates a standard error message (including
the C<where> key) ending with C<$@> in quotes.

=item C<missingArg>

Used internally to generate errors for missing function args.  The value
contains the missing arg's name.

Not intended for general use.

=item C<unknownDriver>

Used internally to generate errors for an unknown/unsupported driver.  The
value contains the driver.

Not intended for general use.

=item C<bad_dns>

Used by C<db_connect> when passed a bad DSN string.  The value is the DSN
string.

Not intended for general use.

=item C<connect>

The value is a database handle, as returned by C<DBI->connect()>.  If the
handle is undefined, C<check_dbicmd_status> generates an appropriate
connection error message.  The message will include the value of the C<dsn>
key, if it was present in the C<ctrlRef> hashref.

=back

The package variable, C<$jpwTools_DBI::_AbortOnError>, controls the default
behavior of C<check_dbicmd_status> when neither the C<warn> nor the C<abort>
keys are in the C<ctrlRef>.  When C<$jpwTools_DBI::_AbortOnError> is true (the
default), C<check_dbicmd_status> aborts on error.  Otherwise,
C<check_dbicmd_status> will print out the error message and return.

=item *

set_ReadOnly(I<handle> [, I<roBool>])

With only one arg, sets the specified DBI I<handle> read-only.  I<roBool> is a
boolean value.  If passed, I<handle> is set read-only if I<roBool> is
'true'. Read-only mode is disabled otherwise.

=item *

db_connect(I<driver>, I<db>|I<dsn> [, I<%flags>, I<%connectFlags>])

Invokes C<DBI->connect> in a standardized fashion, with error handling.
Returns the opened database handle (just as C<DBI->connect> does).

The first arg, I<driver>, is a string containing the name of the driver.  It's
case-insensitive.  (C<db_connect> will look up the correct form.)

The second arg can be one of two things.  Ordinarily, it will be I<db>, the
name of the database you're accessing (B<not> the hostname running the
server):

    my $dbh = db_connect('mysql', 'myDrupal', I<...>);

If you prefer the ordinary call syntax of C<DBI->connect>, you can pass a DSN
string as the second arg.  It B<must> begin with the string 'dbi:', or the
second arg will be interpreted as a database name.  If the second arg is a
DSN, C<db_connect> will validate it as well as check that the driver part is a
supported driver.

The optional third arg, I<%flags>, is a hash containing various connection
parameters.  C<db_connect> will use them to build a DSN string.  Only those
present are used.

The parameters present in I<%flags> are:

=over 4

=item C<host>

The host to connect to (i.e. the host running the database server).

=item C<port>

The port to connect to (i.e the database server's port).

=item C<sqlite>
=item C<sqlite_file>

The full path of the C<SQLite> database file to connect to.

=item C<db>
=item C<database>

The name of the database (B<not> the server host).  If present, overrides
I<db>.

=item C<user>
=item C<username>

The username required to connect to the database server.

=item C<passwd>
=item C<password>

The password required to connect to the database server.

=back

Any other keys in I<%flags> are ignored.

The fourth arg, I<%connectFlags>, if present, is passed to C<DBI->connect> as
the last argument.  If you want to pass I<%connectFlags> but not I<%flags>,
use '{}' as the placeholder.

The handle returned will have the following properties set:

    $handle->{'RaiseError'} = 1;
    $handle->{'PrintError'} = 0;
    $handle->{'ChopBlanks'} = 1;
    $handle->{'FetchHashKeyName'} = 'NAME_lc';

Refer to the DBI manual for details.

=back

=cut

##################
# Local Variables:
# coding: utf-8-unix
# End:
