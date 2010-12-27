#!/usr/bin/perl
#
# =================
#   Glyphinator
# =================
#
# Digs:
#   *) Collect list of current glyphs
#   *) On each ready planet, search in order of:
#       1. What we have the fewest glyphs of
#       2. What we have the most ore of
#       3. Random
#   *) Dig!
#
# Excavators:
#   *) Get list of ready excavators
#   *) Get closest ready body for each excavator
#   *) Launch!
#
# Spit out interesting times
#   *) When digs will be done
#   *) When excavators will arrive
#   *) When excavators will be finished building

use strict;
use warnings;

use feature ':5.10';

use DBI;
use FindBin;
use List::Util qw(first);
use Date::Parse qw(str2time);
use Math::Round qw(round);
use Getopt::Long;
use Data::Dumper;
use Exception::Class;

use lib "$FindBin::Bin/../lib";
use Games::Lacuna::Client;

my %opts;
GetOptions(\%opts,
    'h|help',
    'q|quiet',
    'v|verbose',
    'db=s',
    'config=s',
    'planet=s@',
    'do-digs',
    'send-excavators',
);

usage() if $opts{h};

my %do_planets;
if ($opts{planet}) {
    %do_planets = map { normalize_planet($_) => 1 } @{$opts{planet}};
}

my $glc = Games::Lacuna::Client->new(
    cfg_file => $opts{config} || "$FindBin::Bin/../lacuna.yml",
);

no warnings 'once';
my $db_file = $opts{db} || "$FindBin::Bin/../stars.db";
my $star_db;
if (-f $db_file) {
    $star_db = DBI->connect("dbi:SQLite:$db_file")
        or die "Can't open star database $db_file: $DBI::errstr\n";
} else {
    warn "No star database found.  Specify it with --db.\n";
    if ($opts{'send-excavators'}) {
        warn "Can't send excavators without star database!\n";
    }
}

my $status;
get_status();
do_digs() if $opts{'do-digs'};
send_excavators() if $opts{'send-excavators'} and $star_db;
report_status();
output("$glc->{total_calls} api calls made.\n");

# Destroy client object prior to global destruction to avoid GLC bug
undef $glc;

exit 0;

sub get_status {
    my $empire = $glc->empire->get_status->{empire};

    # reverse hash, to key by name instead of id
    my %planets = map { $empire->{planets}{$_}, $_ } keys %{$empire->{planets}};
    $status->{planets} = \%planets;

    # Scan each planet
    for my $planet_name (keys %planets) {
        if (keys %do_planets) {
            next unless $do_planets{normalize_planet($planet_name)};
        }

        verbose("Inspecting $planet_name\n");

        # Load planet data
        my $planet    = $glc->body(id => $planets{$planet_name});
        my $result    = $planet->get_buildings;
        my $buildings = $result->{buildings};
        $status->{planet_location}{$planet_name}{x} = $result->{status}{body}{x};
        $status->{planet_location}{$planet_name}{y} = $result->{status}{body}{y};

        my $arch = find_arch_min($buildings);
        if ($arch) {
            verbose("Found an archaeology ministry on $planet_name\n");
            $status->{archmin}{$planet_name} = $arch;
            my $now = time();
            my $arch_detail = $arch->view;
            if ($arch_detail->{building}{work}) {
                my $time_left = $arch_detail->{building}{work}{seconds_remaining};
                push @{$status->{digs}}, {
                    planet   => $planet_name,
                    finished => $now + $time_left,
                };
            } else {
                $status->{idle}{$planet_name} = 1;
                $status->{available_ore}{$planet_name} =
                    $arch->get_ores_available_for_processing->{ore};
            }

            my $glyphs = $arch->get_glyphs->{glyphs};
            for my $glyph (@$glyphs) {
                $status->{glyphs}{$glyph->{type}}++;
            }
        } else {
            verbose("No archaeology ministry on $planet_name\n");
        }

        my $spaceport = find_spaceport($buildings);
        if ($spaceport) {
            verbose("Found a spaceport on $planet_name\n");
            $status->{spaceports}{$planet_name} = $spaceport;

            # How many in flight?  When arrives?
            my $traveling = $spaceport->view_ships_travelling;
            push @{$status->{flying}},
                map {
                    {
                        planet      => $planet_name,
                        destination => $_->{to}{name},
                        arrives     => str2time(
                            map { s!^(\d+)\s+(\d+)\s+!$2/$1/!; $_ }
                            $_->{date_arrives}
                        ),
                    }
                }
                grep { $_->{type} eq 'excavator' }
                @{$traveling->{ships_travelling}};

            # How many ready now?
            my $sp_status = $spaceport->view;
            $status->{ready}{$planet_name} += $sp_status->{'docked_ships'}{'excavator'} || 0;

            # How many open spots?
            $status->{open_docks}{$planet_name} = $sp_status->{docks_available} || 0;
        } else {
            verbose("No spaceport on $planet_name\n");
        }

        my @shipyards = find_shipyards($buildings);
        verbose("No shipyards on $planet_name\n") unless @shipyards;
        for my $yard (@shipyards) {
            verbose("Found a shipyard on $planet_name\n");
            # Make sure this yard is capable of building excavators
            my $buildable = $yard->get_buildable->{buildable};

            unless($buildable->{excavator}->{can}) {
                verbose("$planet_name is not able to build excavators yet.\n");
                next;
            }

            # Keep a record of any planet that could be building excavators, but isn't
            $status->{not_building}{$planet_name} = 1
                unless exists $status->{not_building}{$planet_name};

            # How long to build one in this shipyard?
            my $build_time = $buildable->{excavator}->{cost}->{seconds};
            verbose("Excavators in this shipyard take " . format_time_delta_full($build_time) . "\n");

            # How many building?
            my $work_queue = $yard->view_build_queue;
            my @building =
                map {
                    {
                        planet   => $planet_name,
                        finished => str2time(
                            map { s!^(\d+)\s+(\d+)\s+!$2/$1/!; $_ }
                            $_->{date_completed}
                        ),
                    }
                }
                grep { $_->{type} eq 'excavator' }
                @{$work_queue->{ships_building}};

            if (@building) {
                push @{$status->{building}}, @building;
                $status->{not_building}{$planet_name} = 0;
            }

            # What's the queue time and time to build another?
            $status->{queue_end}{$planet_name}{$yard->{building_id}} =
                $work_queue->{building}{work}{seconds_remaining};
            $status->{additional_excavator_end}{$planet_name}{$yard->{building_id}} =
                $work_queue->{building}{work}{seconds_remaining} + $build_time;
        }
    }
}

sub report_status {
    my @events;

    if (keys %{$status->{glyphs} || {}}) {
        output("Current glyphs:\n");
        my $cnt;
        for my $glyph (sort keys %{$status->{glyphs}}) {
            output(sprintf '%13s: %3s', $glyph, $status->{glyphs}->{$glyph});
            output("\n") unless ++$cnt % 4
        }
        output("\n") if $cnt % 4;
        output("\n");
    }

    # Ready to go now?
    if (my @planets = grep { $status->{ready}{$_} } keys %{$status->{ready}}) {
        output(<<END);
**** Notice! ****
You have excavators ready to send.  Specify --send-excavators if you want to
send them to the closest available destinations.
*****************
END
        for my $planet (@planets) {
            output("$planet has " . pluralize($status->{ready}{$planet}, 'excavator') . " ready to launch!\n");
        }
        output("\n");
    }

    # Any idle archmins?
    if (keys %{$status->{idle}}) {
        output(<<END);
**** Notice! ****
You have idle archaeology minstries.  Specify --do-digs if you want to
start the recommended digs automatically.
*****************
END
        for my $planet (keys %{$status->{idle}}) {
            output("Archaeology Ministry on $planet is idle!\n");
        }
        output("\n");
    }

    # Any planets not building excavators?
    if (my @planets = grep { $status->{not_building}{$_} } keys %{$status->{not_building}}) {
        for my $planet (@planets) {
            output("$planet is not currently building any excavators!  It has " . pluralize($status->{open_docks}{$planet}, 'spot') . " currently available.\n");
        }
        output("\n");
    }

    for my $dig (@{$status->{digs}}) {
        push @events, {
            epoch  => $dig->{finished},
            detail => "Dig finishing on $dig->{planet}",
        };
    }

    for my $ship (@{$status->{flying}}) {
        push @events, {
            epoch  => $ship->{arrives},
            detail => "Excavator arriving at $ship->{destination}",
        };
    }

    for my $ship (@{$status->{building}}) {
        push @events, {
            epoch  => $ship->{finished},
            detail => "Excavator finishes building on $ship->{planet}",
        };
    }

    @events =
        sort { $a->{epoch} <=> $b->{epoch} }
        map  { $_->{when} = format_time($_->{epoch}); $_ }
        @events;

    if (@events) {
        output("Upcoming events:\n");
        for my $event (@events) {
            display_event($event);
        }
    }

    output("\n");
}

sub normalize_planet {
    my ($planet_name) = @_;

    $planet_name =~ s/\W//g;
    $planet_name = lc($planet_name);
    return $planet_name;
}

sub format_time_delta {
    my ($delta) = @_;

    given ($delta) {
        when ($_ < 0) {
            return "just finished";
        }
        when ($_ < 90) {
            return pluralize($_, 'second');
        }
        when ($_ < 5400) {
            my $min = round($_ / 60);
            return pluralize($min, 'minute');
        }
        when ($_ < 86400) {
            my $hrs = round($_ / 3600);
            return pluralize($hrs, 'hour');
        }
        default {
            my $days = round($_ / 86400);
            return pluralize($days, 'day');
        }
    }
}

sub format_time_delta_full {
    my ($delta) = @_;

    return "just finished" if $delta <= 0;

    my @formatted;
    my $sec = $delta % 60;
    if ($sec) {
        unshift @formatted, format_time_delta($sec);
        $delta -= $sec;
    }
    my $min = $delta % 3600;
    if ($min) {
        unshift @formatted, format_time_delta($min);
        $delta -= $min;
    }
    my $hrs = $delta % 86400;
    if ($hrs) {
        unshift @formatted, format_time_delta($hrs);
        $delta -= $hrs;
    }
    my $days = $delta;
    if ($days) {
        unshift @formatted, format_time_delta($days);
    }

    return join(', ', @formatted);
}

sub format_time {
    my $time = shift;
    my $delta = $time - time();
    return format_time_delta($delta);
}

sub pluralize {
    my ($num, $word) = @_;

    if ($num == 1) {
        return "$num $word";
    } else {
        return "$num ${word}s";
    }
}

sub display_event {
    my ($event) = @_;

    output(sprintf "%15s: %s\n", $event->{when}, $event->{detail});
}

## Buildings ##

sub find_arch_min {
    my ($buildings) = @_;

    # Find the Archaeology Ministry
    my $arch_id = first {
            $buildings->{$_}->{name} eq 'Archaeology Ministry'
    } keys %$buildings;

    return if not $arch_id;
    return $glc->building(id => $arch_id, type => 'Archaeology');
}

sub find_shipyards {
    my ($buildings) = @_;

    # Find the Shipyards
    my @yard_ids = grep {
            $buildings->{$_}->{name} eq 'Shipyard'
    } keys %$buildings;

    return if not @yard_ids;
    return map { $glc->building(id => $_, type => 'Shipyard') } @yard_ids;
}

sub find_spaceport {
    my ($buildings) = @_;

    # Find a Spaceport
    my $port_id = first {
            $buildings->{$_}->{name} eq 'Space Port'
    } keys %$buildings;

    return if not $port_id;
    return $glc->building(id => $port_id, type => 'Spaceport');
}

## Arch digs ##

sub do_digs {

    # Try to avoid digging for the same ore on every planet, even if it's
    # determined somehow to be the "best" option.  We don't have access to
    # whatever digs are currently in progress so we'll base this just on what
    # we've started during this run.  This will be computed simply by adding
    # each current dig to glyphs, as if it were going to be successful.
    my $digging = {};

    for my $planet (keys %{$status->{idle}}) {
        my $ore = determine_ore($status->{available_ore}{$planet}, $status->{glyphs}, $digging);
        if ($ore) {
            output("Starting a dig for $ore on $planet...\n");
            $status->{archmin}{$planet}->search_for_glyph($ore);
            push @{$status->{digs}}, {
                planet   => $planet,
                finished => time() + (6 * 60 * 60),
            };
            delete $status->{idle}{$planet};
        } else {
            output("Not starting a dig on $planet; not enough of any type of ore.\n");
        }
    }
}

sub determine_ore {
    my ($ore, $glyphs, $digging) = @_;

    my ($which) =
        sort {
            ($glyphs->{$a} || 0) + ($digging->{$a} || 0) <=> ($glyphs->{$b} || 0) + ($digging->{$b} || 0) or
            $ore->{$b} <=> $ore->{$a} or
            int(rand(3)) - 1
        }
        keys %$ore;

    if ($which) {
        $digging->{$which}++;
    }

    return $which;
}


## Excavators ##

sub send_excavators {
    PLANET:
    for my $planet (grep { $status->{ready}{$_} } keys %{$status->{ready}}) {
        verbose("Prepping excavators on $planet\n");
        my $port = $status->{spaceports}{$planet};

        for (1 .. $status->{ready}{$planet}) {
            my ($ships, $dest_name, $x, $y);
            while (! defined $dest_name) {
                ($dest_name, $x, $y) = pick_destination($planet);
                my $ok = eval {
                    $ships = $port->get_ships_for($status->{planets}{$planet}, {x => $x, y => $y});
                    return 1;
                };
                unless ($ok) {
                    if (my $e = Exception::Class->caught('LacunaRPCException')) {
                        if ($e->code eq '1002') {
                            # Empty orbit, update db and try again
                            output("$dest_name is an empty orbit, trying again...\n");
                            mark_orbit_empty($x, $y);
                            $dest_name = undef;
                            next;
                        }
                    }
                    else {
                        my $e = Exception::Class->caught();
                        ref $e ? $e->rethrow : die $e;
                    }
                }

                unless (grep { $_->{type} eq 'excavator' } @{$ships->{available}}) {
                    if (grep { $_->{reason}[0] eq '1010' } @{$ships->{unavailable}}) {
                        # This will set the "last_excavated" time to now, which is not
                        # the case, but it's as good as we have.  It means that some bodies
                        # might take longer to get re-dug but whatever, there are others
                        output("$dest_name was unavailable due to recent search, trying again...\n");
                        update_last_sent($x, $y);
                    } else {
                        diag("Unknown error sending excavator from $planet to $dest_name!\n");
                        next PLANET;
                    }
                    $dest_name = undef;
                }
            }

            my $ex = first {
                $_->{type} eq 'excavator'
            } @{$ships->{available}};

            output("Sending excavator from $planet to $dest_name...\n");
            my $launch_status = $port->send_ship($ex->{id}, {x => $x, y => $y});

            if ($launch_status->{ship}->{date_arrives}) {
                push @{$status->{flying}},
                    {
                        planet      => $planet,
                        destination => $launch_status->{ship}{to}{name},
                        arrives     => str2time(
                            map { s!^(\d+)\s+(\d+)\s+!$2/$1/!; $_ }
                            $launch_status->{ship}{date_arrives}
                        ),
                    };

                    update_last_sent($x, $y);
            } else {
                diag("Error sending excavator to $dest_name!\n");
                warn Dumper $launch_status;
            }
        }

        delete $status->{ready}{$planet};
    }
}

sub pick_destination {
    my ($planet) = @_;

    # Pick something closest and ready
    my $x = $status->{planet_location}{$planet}{x};
    my $y = $status->{planet_location}{$planet}{y};
    my $find_dest = $star_db->prepare(<<SQL);
select   s.name, o.orbit, o.x, o.y, (o.x - ?) * (o.x - ?) + (o.y - ?) * (o.y - ?) as dist
from     orbitals o
join     stars s on o.star_id = s.id
where    (type in ('planet', 'asteroid') or type is null)
and      (last_excavated is null or date(last_excavated) < date('now', '-30 days'))
and      o.x between ? and ?
and      o.y between ? and ?
order by (o.x - ?) * (o.x - ?) + (o.y - ?) * (o.y - ?) asc
limit    1
SQL

    my $max_dist = 0;
    my ($dest_x, $dest_y, $dest_name);
    while (!defined $dest_x) {
        $max_dist += 100;
        verbose("Increasing box size to " . ($max_dist * 2) . "\n");
        $find_dest->execute(
            $x, $x, $y, $y,
            $x - $max_dist,
            $x + $max_dist,
            $y - $max_dist,
            $y + $max_dist,
            $x, $x, $y, $y
        );
        my $row = $find_dest->fetchrow_hashref;
        if ($row) {
            $dest_x = $row->{x};
            $dest_y = $row->{y};
            $dest_name = "$row->{name} $row->{orbit}";

            my $dist = int(sqrt($row->{dist}));
            verbose("Selected destination $dest_name, which is $dist units away\n");
        }
    }

    return ($dest_name, $dest_x, $dest_y);
}

sub update_last_sent {
    my ($x, $y) = @_;

    my $r = $star_db->do(q{update orbitals set last_excavated = datetime(?,'unixepoch') where x = ? and y = ?}, {}, time(), $x, $y);
    unless ($r > 0) {
        diag("Warning: could not update orbitals table for body at $x, $y!\n");
    }
}

sub mark_orbit_empty {
    my ($x, $y) = @_;

    my $r = $star_db->do(q{update orbitals set type = 'empty' where x = ? and y = ?}, {}, $x, $y);
    unless ($r > 0) {
        diag("Warning: could not update orbitals table for body at $x, $y!\n");
    }
}

sub usage {
    diag(<<END);
Usage: $0 [options]

This program will manage your glyph hunting worries with minimal manual
intervention required.  It will notice archeology digs, ready-to-launch
excavators, and idle shipyards and notify you of them.  It can start digs
for the most needed glyphs, and send excavators to the nearest available
bodies.

This is suitable for automation with cron(8) or at(1), but you should
know that it tends to use a substantial number of API calls, often 50-100
per run.  With the daily limit of 5000, including all web UI usage, you
will want to keep these at a relatively infrequent interval, such as every
60 minutes at most.

Options:
  --verbose          - Output extra information.
  --quiet            - Print no output except for errors.
  --config <file>    - Specify a GLC config file, normally lacuna.yml.
  --db <file>        - Specify a star database, normally stars.db.
  --planet <name>    - Specify a planet to process.  This option can be passed
                       multiple times to indicate several planets.  If this is
                       not specified, all relevant colonies will be inspected.
  --do-digs          - Begin archaeology digs on any planets which are idle.
  --send-excavators  - Launch ready excavators at their nearest destinations.
                       The information for these is selected from the star
                       database, and the database is updated to reflect your
                       new searches.
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
