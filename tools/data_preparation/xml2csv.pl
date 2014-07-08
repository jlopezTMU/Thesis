#This script reads in XML data from StackOverflow and returns it in text format
#Output "title \t body", one line per post
#At this stage we ignore replies to the posts, outputting only the question

#Usage example: perl xml2csv.pl Posts.xml Posts.xml.csv

use warnings;
use strict;

use XML::Simple;    # use XML::Twig for big files
use HTML::Strip;

use utf8;
use Text::Unidecode;    #translates unicode into ascii

if ( scalar(@ARGV) != 2 ) {
	print "usage: xml2csv.pl in_file_name out_file_name\n";
	exit;
}

#read XML file into memory
my $ref = XMLin( $ARGV[0] );

open( my $fid, ">" . $ARGV[1] )
  or die("Cannot open $ARGV[1] : $!\n");    #export to CSV

my $hs = HTML::Strip->new();
print $fid "title\tbody\n";                 #header
foreach ( @{ $ref->{row} } ) {
	if ( $_->{PostTypeId} == "1" ) {        #if the post is the original question
		my $title = unidecode( $_->{Title} );
		my $body  = unidecode( $hs->parse( $_->{Body} ) );    #strip HTML tags
		$body =~ s/\n|\t/ /g;                                 #replace new line or tab with space
		print $fid $title . "\t" . $body . "\n";
	}
}
close $fid;
$hs->eof;