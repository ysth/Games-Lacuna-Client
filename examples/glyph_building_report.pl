#!/usr/bin/perl
#

use strict;
use warnings;

use FindBin;
use Getopt::Long;

use lib "$FindBin::Bin/../lib";
use Games::Lacuna::Client;

my %opts;
GetOptions(\%opts,
    'h|help',
    'config=s',
    'planet=s@',
);

usage() if $opts{h};

my %do_planets;
if ($opts{planet}) {
    %do_planets = map { normalize_planet($_) => 1 } @{$opts{planet}};
}

my $glc = Games::Lacuna::Client->new(
    cfg_file => $opts{config} || "$FindBin::Bin/../lacuna.yml",
);

my $empire = $glc->empire->get_status->{empire};

# reverse hash, to key by name instead of id
my %planets = map { $empire->{planets}{$_}, $_ } keys %{$empire->{planets}};

my @types = qw(Food Ore Water Energy Storage);
my %glyph_buildings = (
    'Malcud Field'          => ['Food','malcud'],
    'Algae Pond'            => ['Food','algae'],
    'Beeldeban Nest'        => ['Food','beeldeban'],
    'Lapis Forest'          => ['Food','lapis'],
    'Volcano'               => ['Ore','volcano'],
    'Natural Spring'        => ['Water','spring'],
    'Geo Thermal Vent'      => ['Energy','vent'],
    'Interdimensional Rift' => ['Storage','rift'],
    'Ravine'                => ['Storage','ravine'],
);

printf "%-20s", "Colony";
for my $type (@types) {
    printf "| %-10s", $type;
}
print "\n";
print "--------------------",("+-----------")x5,"\n";

# Scan each planet
for my $planet_name (sort keys %planets) {
    if (keys %do_planets) {
        next unless $do_planets{normalize_planet($planet_name)};
    }

    my %planet_buildings;

    # Load planet data
    my $planet    = $glc->body(id => $planets{$planet_name});
    my $result    = $planet->get_buildings;
    my $buildings = $result->{buildings};

    for (keys %$buildings) {
        my $name = $buildings->{$_}{name};
        my $level = $buildings->{$_}{level};
        if (my $data = $glyph_buildings{$name}) {
            push @{$planet_buildings{$data->[0]}}, [$level, $data->[1]];
        }
    }

    printf "%-20s", $planet_name;
    for my $type (@types) {
        printf "| %-10s", join(",", map { $_->[0] } @{$planet_buildings{$type} || []});
    }
    print "\n";
}

sub normalize_planet {
    my ($planet_name) = @_;

    $planet_name =~ s/\W//g;
    $planet_name = lc($planet_name);
    return $planet_name;
}


sub usage {
    print STDERR <<END;
Usage: $0 [--config <file>] [--planet <name>]

Displays the resource producing and storage glyph buildings on your colones.
You can specify --planet to only inspect a subset of your planets.
END

    exit 1;
}
