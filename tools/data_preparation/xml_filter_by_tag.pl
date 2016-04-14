#!/bin/perl -w

# Keep a subset of xml rows associated with a partcular tag

# to stream from an archive, to save space, do something like
# gunzip -c posts.xml.gz | perl xml_filter_by_tag.pl db2 > out_file_name
# or 
# 7z x posts.xml.gz -so | perl xml_filter_by_tag.pl db2 > out_file_name

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
use IO::Handle;


if ( scalar(@ARGV) != 1 ) {
	print "usage: xml_filter_by_tag.pl my_tag < in_file_name > out_file_name\n";
	exit;
}

my $tag_of_interest = $ARGV[0];

my $quesion_ids = {};

my $twig= new XML::Twig( twig_handlers => { row => \&row_parser } ); 

my $io = IO::Handle->new();
$twig->parse( $io->fdopen(fileno(STDIN),"r") );                 # parse the twig
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

