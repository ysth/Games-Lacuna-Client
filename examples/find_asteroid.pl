#!/usr/bin/perl
#
# Find an asteroid that fits your needs
#

use strict;
use warnings;

use FindBin;
use List::Util qw(first sum min);
use Getopt::Long;
use Data::Dumper;

use lib "$FindBin::Bin/../lib";
use Games::Lacuna::Client;

my %opts;
GetOptions(\%opts,
    # General options
    'h|help',
    'q|quiet',
    'v|verbose',
    'config=s',
    'planet=s@',
    'ore=s@',
    'skip-full',
) or usage();

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

# Scan each planet
my (@target_planets, @asteroids, $port);
for my $planet_name (keys %planets) {

    verbose("Inspecting $planet_name\n");

    # Load planet data
    my $planet    = $glc->body(id => $planets{$planet_name});
    my $result    = $planet->get_buildings;
    my $buildings = $result->{buildings};
    my $x = $result->{status}{body}{x};
    my $y = $result->{status}{body}{y};

    # Look at all possible observatories but only relay distance to
    # specified planets, if any
    if (!keys %do_planets or $do_planets{normalize_planet($planet_name)}) {
        push @target_planets, {
            name => $planet_name,
            x    => $x,
            y    => $y,
        };
    }

    # we need at least one spaceport
    $port ||= find_spaceport($buildings);

    my $observatory = find_observatory($buildings);
    if ($observatory) {
        verbose("Found an observatory on $planet_name\n");

        # Get bodies
        my $page = 1;
        my $done;
        while (!$done) {
            $done = 1;

            verbose("Getting page $page of stars...\n");
            my $stars = $observatory->get_probed_stars($page);
            for my $star (@{$stars->{stars}}) {
                if ($star->{bodies} and @{$star->{bodies}}) {
                    for my $body (@{$star->{bodies}}) {
                        next unless $body->{type} eq 'asteroid';

                        if ($opts{'skip-full'}) {
                            # Skip fully populated asteroids
                            my $size = $body->{size};
                            my $ships = $port->get_ships_for($planets{$planet_name}, {
                                x => $body->{x},
                                y => $body->{y},
                            });
                            my $used = @{$ships->{mining_platforms} || []};
                            if ($used >= $size) {
                                verbose("$body->{star_name} $body->{orbit} is full, skipping\n");
                                next;
                            }
                        }

                        if ($opts{ore}) {
                            $body->{pref_ore} = sum(map { $body->{ore}{lc $_} } @{$opts{ore}});

                            # If all 1s, skip it
                            next if $body->{pref_ore} <= @{$opts{ore}};
                        } else {
                            $body->{pref_ore} = 0;
                        }

                        push @asteroids, $body;
                    }
                }
            }

            if ($stars->{star_count} > $page * 25) {
                $done = 0;
                $page++;
            }
        }
    } else {
        verbose("No observatory on $planet_name\n");
    }
}

# Populate distance from each of @target_planets for each possible asteroid
for my $roid (@asteroids) {
    for my $planet (@target_planets) {
        $roid->{distances}{$planet->{name}} =
            compute_distance($roid->{x}, $roid->{y}, $planet->{x}, $planet->{y});
    }
}

sub compute_distance {
    my ($x1, $y1, $x2, $y2) = @_;

    my $delta_x = abs($x2 - $x1);
    my $delta_y = abs($y2 - $y1);
    return int(sqrt($delta_x * $delta_x + $delta_y * $delta_y));
}

sub sort_asteroids {
    our ($a, $b);

    my $closest_a = min(map { $a->{distances}{$_} } keys %{$a->{distances}});
    my $closest_b = min(map { $b->{distances}{$_} } keys %{$b->{distances}});

    return (($b->{pref_ore} <=> $a->{pref_ore}) || ($closest_a <=> $closest_b));
}

for my $asteroid (sort sort_asteroids @asteroids) {
    print "Found $asteroid->{star_name} $asteroid->{orbit} ($asteroid->{x}, $asteroid->{y})\n";
    if ($opts{ore}) {
        print "    ", join(', ', map { "$_: $asteroid->{ore}{lc $_}" } @{$opts{ore}}), "\n";
    }
    if ($asteroid->{distances}) {
        for my $planet (sort { $asteroid->{distances}{$a} <=> $asteroid->{distances}{$b} }
                keys %{$asteroid->{distances}}) {
            print "    Distance from $planet: $asteroid->{distances}{$planet}\n";
        }
    }
}

output("$glc->{total_calls} api calls made.\n");
output("You have made $glc->{rpc_count} calls today\n");

# Destroy client object prior to global destruction to avoid GLC bug
undef $glc;

exit 0;


sub normalize_planet {
    my ($planet_name) = @_;

    $planet_name =~ s/\W//g;
    $planet_name = lc($planet_name);
    return $planet_name;
}
## Buildings ##

sub find_observatory {
    my ($buildings) = @_;

    # Find the Observatory
    my $obs_id = first {
        $buildings->{$_}->{name} eq 'Observatory'
    }
    grep { $buildings->{$_}->{level} > 0 and $buildings->{$_}->{efficiency} == 100 }
    keys %$buildings;

    return if not $obs_id;

    my $building  = $glc->building(
        id   => $obs_id,
        type => 'Observatory',
    );

    return $building;
}

sub find_spaceport {
    my ($buildings) = @_;

    # Find a Spaceport
    my $port_id = first {
        $buildings->{$_}->{name} eq 'Space Port'
    }
    grep { $buildings->{$_}->{level} > 0 and $buildings->{$_}->{efficiency} == 100 }
    keys %$buildings;

    return if not $port_id;

    my $building  = $glc->building(
        id   => $port_id,
        type => 'Spaceport',
    );

    return $building;
}

sub usage {
    diag(<<END);
Usage: $0 [options]

Options:
  --verbose              - Output extra information.
  --quiet                - Print no output except for errors.
  --config <file>        - Specify a GLC config file, normally lacuna.yml.
  --db <file>            - Specify a star database, normally stars.db.
  --planet <name>        - Specify a planet to process.  This option can be
                           passed multiple times to indicate several planets.
                           If this is not specified, all relevant colonies will
                           be inspected.
  --ore <type>           - Find asteroids with the specified ore(s)
  --skip-full            - Don't show asteroids that have no open spots.  This
                           is not done by default because it incurs an extra API
                           call for every inspected asteroid!   This can get
                           very expensive!
END
    exit 1;
}

sub verbose {
    return unless $opts{v};
    print @_;
}

sub output {
    return if $opts{q};
    print @_;
}

sub diag {
    my ($msg) = @_;
    print STDERR $msg;
}
