#!D:/prog/Perl/bin/perl

use strict;
use CGI;

my $q = new CGI;
print $q->header( "text/plain" );

print "These are the parameters I received:\n\n";

my( $name, $value );

foreach $name ( $q->param ) {
    print "$name:\n";
    foreach $value ( $q->param( $name ) ) {
        print "  $value\n";
    }
}
