#!/usr/bin/perl

use strict;
use warnings;

use FindBin;
use lib "$FindBin::Bin/../lib";

use List::Util qw(first);
use Games::Lacuna::Client;
use Getopt::Long;
use POSIX qw(floor);
use Data::Dumper;

my %opts;
GetOptions(\%opts,
    'help',
    'verbose',
    'quiet',
    'config=s',
    'planet=s@',
    'to=s',
) or usage();

usage() if $opts{help};
usage() unless $opts{to};

my $dest = normalize_planet($opts{to});

my %do_planets;
if ($opts{planet}) {
    %do_planets = map { normalize_planet($_) => 1 } @{$opts{planet}};
}

my $client = Games::Lacuna::Client->new(
    cfg_file => $opts{config} || 'lacuna.yml',
);

my $empire  = $client->empire->get_status->{empire};
my $planets = $empire->{planets};

# reverse hash, to key by name instead of id
my %planets_by_name = map { normalize_planet($planets->{$_}) => $_ } keys %$planets;

# Load planet data
for my $planet (keys %planets_by_name) {
    next if $planet eq $dest or (keys %do_planets and !$do_planets{$planet});

    verbose("Checking $planet\n");

    my $body      = $client->body( id => $planets_by_name{$planet} );
    my $buildings = $body->get_buildings->{buildings};

    # Find the TradeMin
    my $trade_min_id = first {
        $buildings->{$_}->{name} eq 'Trade Ministry'
    } keys %$buildings;

    next unless $trade_min_id;

    my $trade_min = $client->building( id => $trade_min_id, type => 'Trade' );

    my $glyphs_result = $trade_min->get_glyphs;
    my @glyphs = @{$glyphs_result->{glyphs}};

    my $plans_result = $trade_min->get_plans;
    my @plans =
        sort { $b->{extra_build_level} <=> $a->{extra_build_level} or $b->{level} <=> $a->{level} }
        grep { interesting_plan($_) }
        @{$plans_result->{plans}};

    next unless @glyphs or @plans;

    my $total_cargo = $glyphs_result->{cargo_space_used_each} * @glyphs
        + $plans_result->{cargo_space_used_each} * @plans;

    verbose(scalar(@glyphs) . " glyphs and " . scalar(@plans) . " plans = $total_cargo cargo\n");

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
        if (@glyphs) {
            last if $cargo + $glyphs_result->{cargo_space_used_each} > $ship->{hold_size};

            my $glyph = shift @glyphs;
            push @items, {
                type     => 'glyph',
                glyph_id => $glyph->{id},
            };
            $cargo += $glyphs_result->{cargo_space_used_each};
        } elsif (@plans) {
            last if $cargo + $plans_result->{cargo_space_used_each} > $ship->{hold_size};

            my $plan = shift @plans;
            push @items, {
                type    => 'plan',
                plan_id => $plan->{id},
            };
            $cargo += $plans_result->{cargo_space_used_each};
        } else {
            last;
        }
    }

    unless (@items) {
        verbose("Couldn't fit anything into the largest available ship, skipping planet\n");
        next;
    }

    my $return = $trade_min->push_items(
        $planets_by_name{$dest},
        \@items,
        { ship_id => $ship->{id} }
    );

    output(sprintf "Pushed %d glyphs and %d plans from %s, arriving %s\n",
        scalar(grep { $_->{type} eq 'glyph' } @items),
        scalar(grep { $_->{type} eq 'plan'  } @items),
        $planet, $return->{ship}{date_arrives}
    );
}

exit;

{
    my %types;
    BEGIN {
        %types = map { $_ => 1 } (
            'Algae Pond',
            'Beeldeban Nest',
            'Citadel of Knope',
            'Crashed Ship Site',
            'Geo Thermal Vent',
            'Interdimensional Rift',
            'Kalavian Ruins',
            'Lapis Forest',
            'Malcud Field',
            'Natural Spring',
            'Oracle of Anid',
            'Pantheon of Hagness',
            'Ravine',
            'Temple of the Drajilites',
            'Volcano',
        );
    }
    sub interesting_plan {
        my $plan = shift;
        return ($types{$plan->{name}} and ($plan->{level} > 1 or $plan->{extra_build_level}));
    }
}

sub usage {
  die <<END;
Usage: $0 --to <destination> [other options]

Options:
  --verbose              - Output extra information.
  --quiet                - Print no output except for errors.
  --config <file>        - Specify a GLC config file, normally lacuna.yml.
  --to <planet>          - Name of colony to ship all goodies to
  --planet <name>        - Name of colony to send from, can be specified multiple times
END

}

sub output {
    return if $opts{quiet};
    print @_;
}

sub verbose {
    return unless $opts{verbose};
    print @_;
}

sub diag {
    print STDERR @_;
}

sub normalize_planet {
    my ($planet_name) = @_;

    $planet_name =~ s/\W//g;
    $planet_name = lc($planet_name);
    return $planet_name;
}
