#!/bin/perl -w

# Keep a subset of xml rows associated with a partcular tag

# Algorithm:
# if post type == q
# 	if tag == db2
# 		print
# 		add id to hash

# if post type == a
# 	if parent id in hash
# 		print

use strict;
use XML::Twig;


if ( scalar(@ARGV) != 2 ) {
	print "usage: xml_filter_by_tag.pl in_file_name my_tag > out_file_name\n";
	exit;
}

my $file_in = $ARGV[0];
my $tag_of_interest = $ARGV[1];

my $quesion_ids = {};

my $twig= new XML::Twig( twig_handlers => { row => \&row_parser } ); 
$twig->parsefile( $file_in );                 # parse the twig
$twig->flush;

sub row_parser{ 
	my( $twig, $row)= @_;    
	if ( $row->{'att'}->{PostTypeId} eq "1"){ #question
		my $tags_string = $row->{'att'}->{Tags};
		$tags_string =~ s/></\t/g;
		$tags_string =~ s/^<//g;
		$tags_string =~ s/>$//g;
		my @tags_string = split "\t", $tags_string;
		foreach(@tags_string){
			if($_ eq $tag_of_interest){ #save id and print to stodut
				#save id to hash
				my $postId = $row->{'att'}->{Id};
				$quesion_ids->{$postId} = 0;				
				$twig->flush;	
				return;			
			}
		}
	}elsif($row->{'att'}->{PostTypeId} eq "2"){ #answer
		#print Dumper $quesion_ids;
		if( exists $quesion_ids->{ $row->{'att'}->{ParentId} } ){ #if we kept the related question
			$twig->flush;
			return;
		}
	}
	
	$row->delete(); #if row is not interesting -- delete
	
}

