#Converts continuous white spaces to a single tab
#Usage: spaces2tab.pl < in_file > out_file
use warnings;
use strict;

while(<STDIN>){
	chomp;
	$_ =~ s/\h+/\t/g;
	print "$_\n";
}
