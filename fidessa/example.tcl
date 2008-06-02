#!/bin/sh
#
# Copyright (C) 2006 by John P. Weiss
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
# RCS $Id: script.tcl 1503 2006-01-30 21:46:37Z candide $
############
# \
cd `dirname $0`
# \
exec tclsh "`basename $0`" "$@"


############
#
# Packages, Global Variables, and Defaults
#
############


##package require Foo 1.0

set UNIT_TEST 0
set VERBOSE 0


############
#
# Package Namespace
#
############


namespace eval Foo {
    # At the top of the file, in place of the interpreter-invocation lines,
    # you'd put:
    #    
    # package provide Foo 1.0


    set someLocalVar 1
    set anotherLocalVar 2
    set publicesqueLocalVar 3

    namespace export publicesqueLocalVar

    namespace export qualifiedProc
    # The latter would be declared below as:
    #     proc Foo::qualifiedProc { ... } ...


    # Note:
    #   Tcl won't auto-load when a package variable is accessed, only on calls
    #   to package proc's.  The 3 lines below are a workaround:
    #   
    ##  Kludge for forcing loading of the Foo package here, if it
    ##  hasn't been loaded already.  This is a TCL flaw.
    #   eval $::auto_index(::Foo::anyExportedProcNameHere)

}


############
#
# Functions
#
############


proc foo { out_nm inval } {
    upvar $out_nm out__u
}


#
# Option/Config processing.
#


proc getOpts { optarray_nm posnParams_nm argv args } {
    upvar $optarray_nm options__u \
          $posnParams_nm posnParameters__u

    if { [llength $args] != 0 } {
        upvar [lindex $args 0] opt_regexps
    }

    set key _PARAMETERS_
    set opt_names {}
    array set options__u {}

    foreach arg $argv {
        switch -regexp -- $arg {
            ^--$ {
                set key _NO_MORE_OPTIONS_
            }
            ^-.+ {
                switch -- $key {
                    _NO_MORE_OPTIONS_ {
                        lappend posnParameters__u $arg
                    }
                    default {
                        regexp -- {^-+(.*)$} "$arg" dummy key
                        set options__u($key) 1
                        # Save the 'raw' option names for use later on.
                        lappend opt_names $key
                    }
                }
            }
            default {
                switch -- $key {
                    _NO_MORE_OPTIONS_ -
                    _PARAMETERS_      {
                        lappend posnParameters__u $arg
                    }
                    default {
                        set options__u($key) $arg
                        set key _PARAMETERS_
                    }
                }
            }
        } ;# end switch
    } ;# end foreach argv

    # Reduce degenerate parameter names down to the cannonical one.
    if { ! [array exists opt_regexps]} { 
        return 
    }

    # Only works in TCL > v8.1:
    foreach { cannonical_name name_re } [array get opt_regexps] {
        set all_names [lsearch -regexp -all -inline $opt_names "^($name_re)$"]
        if {[llength $all_names] == 0} {
            continue
        }
        set last_val_specified $options__u([lindex $all_names end])
        foreach nm $all_names {
            array unset options__u $nm
        }
        set options__u($cannonical_name) $last_val_specified
    }
    ## This works for TCL v8.0.*:
#     foreach { cannonical_name name_re } [array get opt_regexps] {
#         set all_names_idx [lsearch -regexp $opt_names "^($name_re)$"]
#         if {[lindex $all_names_idx 0] == -1} {
#             continue
#         }
#         set last_val_specified $options__u([lindex $opt_names \
#                                                 [lindex $all_names_idx end]])
#         foreach i $all_names_idx {
#             set nm [lindex $opt_names $i]
#             unset options__u($nm)
#         }
#         set options__u($cannonical_name) $last_val_specified
#     }
}


proc parseOptions { optarray_nm argv } {
    upvar $optarray_nm options__u

    set posnParams {}

    # Map of (option-name, alt-form) pairs.  The key is the "official" name
    # for the option as known in the code.  The value is a regexp matching the
    # actual options one can specify on the command line for this, but with
    # any leading '-' or '--' removed.  The regexp will have a '^' and '$'
    # added to its beginning and end, respectively.
    # (Note that the "official" name can also be used as a command line option
    #  along with any of those that match the corresponding regexp.)

    array set optNames {
        help       {h}
        verbose    {v}
        unit_test  {unit[-_]?[Tt]est}

        axaxaxaxas {a|ml..?}
        beth       b
    }

    # Simple, standalone separation of flags from positional-parameters.
    getOpts options__u posnParams $argv optNames

    # 
    # Option Validation and Processing
    # 

    if [info exists options__u(help)] {
        usage
    }
    if [info exists options__u(verbose)] {
        set ::VERBOSE 1
    }
    if [info exists options__u(unit_test)] {
        set ::UNIT_TEST 1
    }

    # :
    # :
    # Any other option processing goes here.  (You can remove this comment.) 
    # :
    # :

    return $posnParams
}


proc usage { } {
    global argv0
    puts "Usage: $argv0 \[<Options>\] \[--\] \[<Parameters>\]"
    puts "\n<Options>:\n"

    puts "-h"
    puts "--help"
    puts "\tThis message."

    puts "-v"
    puts "--verbose"
    puts "\tEnable verbose output."

    puts "--foo"
    puts "\t-< describe here >-"

    puts "\n<Parameters> (in order):\n"
    puts "\t-< describe here >-"

    exit 1
}


##############################################################################
#
# Main Function & Entry Point
#
##############################################################################


proc Main { argc argv } {
    array set options {}
    set params [ parseOptions options $argv ]

    if {$::VERBOSE && $::UNIT_TEST} {
        parray options 
        puts $params
    }

    # A note about TCL arrays:
    # 
    # While they are mostly equivalent to perl hashes, you cannot make an
    # "array-of-array" in Tcl.  It doesn't work.
    #
    # Now, you *could* write something like 'set myarray($x)($y) $val', but
    # what you've really done is set the array to '$val' using the index
    # string "$x)($y".  The ')(' causes no end of syntax headaches... in
    # *some* places.  Not all.
    #
    # Tcl arrays are always 1-D.  You can create a "pseudo-multidimensional"
    # array using an index-construction convention.  That's the best you can
    # hope for.
    #
    # Now, you could, I suppose, use the convention that each direction's
    # index is separated by the string ")(".  You could, but that creates
    # dereferencing nightmares like:
    #    ... $myarray([join [list $x $y $z] ")("]) ...
    # or worse:
    #    ... $myarray($x\)\($y\)\($z) ...
    #    
    # However, you're better off using an index-separator that Tcl won't parse:
    #    set myarray($x,$y) $val
    #    ... $myarray($x,$y) ...
    # The latter works just fine.

    # Valid for-syntax, example 1:
    for { set i 0 } \
        { $i < [SomeLimit] } \
        { incr i } {

        FirstStatement

        SecondStatement
    }

    # Valid for-syntax, example 2:
    for { set i 0 } \
        { $i < [SomeLimit] && \
          $anotherVal != "second condition" } \
        { incr i } {

        FirstStatement

        SecondStatement
    }

    # Valid for-syntax, example 3:
    for { 
        set i 0 
      } { 
         $i < [SomeLimit] 
      } { 
        incr i 
      } {

        FirstStatement

        SecondStatement

        if { ( ( $clause == 1 ) && ( $clause == 2 ) \
               || \
               ( $clause3 == "something" ) ) } {
            DoStuff
        }

        # Alternate style for the same:
        if { ( ( $clause == 1 ) && ( $clause == 2 ) \
               || \
               ( $clause3 == "something" ) \
             ) } {
            DoStuff
        }

        set q { this "is" \
                an "array" \
                definition "list" \
                }

        puts [format "%s %s %s %s %s %s" \
                     "here is" \
                     "a function" \
                     "with a" \
                     "long" \
                     "arg list." \
                     [othercommand "Dolor ipse lorem sit." \
                                   "second arg"] \
                     ]

        puts "Dolor ipse lorem sit.  Dolor ipse lorem sit.  Dolor ipse\
              lorem sit.  Dolor ipse lorem sit.  Dolor ipse lorem sit. \
              Dolor ipse lorem sit.  Dolor ipse lorem sit."

        SampleCommand [with "a" 42 "terribly"] \
                      "long list" \
                      $of \
                      [command $options] \
                      [in [array get it]]

        SampleCommand "with a" $long \
                      [list $of [command $options]] \
                      [in [array get it]] \
                      [nil]

        # This is a comment
    }

    return 0
}


# Run Main in such a way that we can catch locally-thrown errors while
# rethrowing other errors.
catch {
    exit [Main $argc $argv]
} errMesg

if {$errMesg == ""} {
    exit 0
} ;# else: Error

switch -- [lindex $::errorCode 0] {
    LOCAL_KNOWN {
        puts stderr $errMesg
        exit 1
    }
} ; # else: Unhandled error.  Rethrow below (to keep the stack-trace clean)
error $errMesg $::errorInfo $::errorCode


#################
#
#  End
