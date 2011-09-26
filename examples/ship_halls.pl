#!/usr/bin/perl
#
# Ship Halls of Vrbansk recipes to another colony
#

use strict;
use warnings;

use FindBin;
use Getopt::Long;
use Data::Dumper;
use List::Util qw(first min max sum);

use lib "$FindBin::Bin/../lib";
use Games::Lacuna::Client;

my %opts;
GetOptions(\%opts,
    'h|help',
    'v|verbose',
    'q|quiet',
    'config=s',
    'from=s',
    'to=s',
    'max=i',
    'use-last',
    'dry-run',
    'type=s@',
);

usage() if $opts{h};
usage() unless $opts{to} and $opts{from};

my $from = normalize_planet($opts{from});
my $to = normalize_planet($opts{to});

my $glc = Games::Lacuna::Client->new(
    cfg_file => $opts{config} || "$FindBin::Bin/../lacuna.yml",
    rpc_sleep => 1,
);

my $empire = $glc->empire->get_status->{empire};

# reverse hash, to key by name instead of id
my %planets = map { $empire->{planets}{$_}, $_ } keys %{$empire->{planets}};

my @recipes = (
    [qw/ goethite  halite      gypsum        trona     /],
    [qw/ gold      anthracite  uraninite     bauxite   /],
    [qw/ kerogen   methane     sulfur        zircon    /],
    [qw/ monazite  fluorite    beryl         magnetite /],
    [qw/ rutile    chromite    chalcopyrite  galena    /],
);

my %build_types;
my %normalized_types = (
    (map { $_ => $_ - 1 } 1..5),
    a => 0,
    b => 1,
    c => 2,
    d => 3,
    e => 4,
);
if ($opts{type}) {
    # normalize type
    for (@{$opts{type}}) {
        $build_types{lc($normalized_types{$_})} = 1
            if defined lc($normalized_types{$_});
    }
}

# Scan each planet
my (%glyphs, $glyph_size, $trade_min, $from_planet_id, $to_planet_id);
for my $planet_name (sort keys %planets) {
    if ($to eq normalize_planet($planet_name)) {
        $to_planet_id = $planets{$planet_name};
    }
    next unless $from eq normalize_planet($planet_name);
    $from_planet_id = $planets{$planet_name};

    my %planet_buildings;

    # Load planet data
    my $planet    = $glc->body(id => $planets{$planet_name});
    my $result    = $planet->get_buildings;
    my $buildings = $result->{buildings};

    # Find the TradeMin
    my $trade_min_id = first {
        $buildings->{$_}->{name} eq 'Trade Ministry'
    } keys %$buildings;

    die "No trade min on $from?" unless $trade_min_id;

    $trade_min = $glc->building(id => $trade_min_id, type => 'Trade');
    my $glyphs_result = $trade_min->get_glyphs;
    $glyph_size = $glyphs_result->{cargo_space_used_each};
    my $glyphs = $glyphs_result->{glyphs};

    for my $glyph (@$glyphs) {
        push @{$glyphs{$planet_name}{$glyph->{type}}}, $glyph->{id};
    }
}

die "Couldn't find planet $from?" unless $from_planet_id;

my (%possible_builds, $all_possible);
for my $i (0..$#recipes) {
    next if $opts{type} and not $build_types{$i};
    my $type = $i + 1;

    # Determine how many of each we're able to build
    PLANET:
    for my $planet (keys %glyphs) {
        my $can_build_here = min(
            map { $glyphs{$planet}{$_} ? scalar @{$glyphs{$planet}{$_}} : 0 } @{$recipes[$i]}
        );

        output("$planet can build $can_build_here Halls #$type\n")
            if $can_build_here;

        for my $j (0 .. $can_build_here - 1) {
            push @{$possible_builds{$type}}, {
                planet => $planet,
                type   => $type,
                glyphs => [
                    map { $glyphs{$planet}{$_}[$j] } @{$recipes[$i]}
                ],
            };
            $all_possible++;
        }
    }
}

# Drop one from each type unless we're allowed to use all glyphs
unless ($opts{'use-last'}) {
    verbose("Not using last, dropping one of each type\n");
    pop @{$possible_builds{$_}} for keys %possible_builds;
}

# Do builds
my $total = sum(map { scalar @{$possible_builds{$_}} } keys %possible_builds) || 0;
my $need = $opts{max} ? min($opts{max}, $total) : $total;
verbose("Planning to ship $need Halls\n");

# First grab approximately the right percentage from each set
my @builds;
for my $type (keys %possible_builds) {
    my $have = @{$possible_builds{$type}};
    my $grab = $total ? int(($have / $total) * $need) : 0;
    verbose("Grabbing $grab of type $type\n");
    for (1..$grab) {
        push @builds, pop @{$possible_builds{$type}};
    }
}

while (@builds < $need) {
    my ($type) = sort { @{$possible_builds{$b}} <=> @{$possible_builds{$a}}} keys %possible_builds;
    verbose("Not enough (" . scalar(@builds) . " of $need), taking another $type\n");
    push @builds, pop @{$possible_builds{$type}};
}

my @ship_glyphs;
for my $build (sort { $a->{type} cmp $b->{type} } @builds) {
    if ($opts{'dry-run'}) {
        output("Would have shipped a Halls #$build->{type} from $build->{planet}\n");
    } else {
        output("Shipping a Halls #$build->{type} from $build->{planet}\n");
        push @ship_glyphs, @{$build->{glyphs}};
    }
}

my $total_cargo = $glyph_size * @ship_glyphs;

verbose(scalar(@ship_glyphs) . " glyphs = $total_cargo cargo\n");

my $ships = $trade_min->get_trade_ships->{ships};
next unless $ships and @$ships;

# Find the fastest ship we have that holds the entire shipment
my ($ship) = sort { $b->{speed} <=> $a->{speed} }
    grep { $_->{hold_size} >= $total_cargo }
    @$ships;

# Didn't have one big enough, find the biggest available one
unless ($ship) {
    ($ship) = sort { $b->{hold_size} <=> $a->{hold_size} } @$ships;
    verbose("Didn't find a ship with $total_cargo capacity, picking one with $ship->{hold_size}\n");
}

my $cargo = 0;
my @items;
while ($cargo < $ship->{hold_size}) {
    if (@ship_glyphs) {
        last if $cargo + $glyph_size > $ship->{hold_size};

        my $glyph = shift @ship_glyphs;
        push @items, {
            type     => 'glyph',
            glyph_id => $glyph,
        };
        $cargo += $glyph_size;
    } else {
        last;
    }
}

unless (@items) {
    verbose("Couldn't fit anything into the largest available ship, skipping planet\n");
    next;
}

my $return = $trade_min->push_items(
    $to_planet_id,
    \@items,
    { ship_id => $ship->{id} },
);

output(sprintf "Pushed %d glyphs from %s, arriving %s\n",
    scalar(grep { $_->{type} eq 'glyph' } @items),
    $from, $return->{ship}{date_arrives}
);

output("$glc->{total_calls} api calls made.\n");
output("You have made $glc->{rpc_count} calls today\n");

sub normalize_planet {
    my ($planet_name) = @_;

    $planet_name =~ s/\W//g;
    $planet_name = lc($planet_name);
    return $planet_name;
}

sub verbose {
    return unless $opts{v};
    print @_;
}

sub output {
    return if $opts{q};
    print @_;
}

sub usage {
    print STDERR <<END;
Usage: $0 [options]

This will assemble Halls of Vrbansk recipes wherever there are enough
glyphs in the same location to do so.  By default, it will not use
the last of any particular type of glyph.

Options:

  --verbose       - Print more output
  --quiet         - Only output errors
  --config <file> - GLC config, defaults to lacuna.yml
  --max <n>       - Build at most <n> Halls
  --planet <name> - Build only on the specified planet(s)
  --use-last      - Use the last of any glyph if necessary
  --dry-run       - Print what would have been built, but don't do it
  --type <type>   - Specify a particular recipe to build (1-5 or A-E)
END

    exit 1;
}
