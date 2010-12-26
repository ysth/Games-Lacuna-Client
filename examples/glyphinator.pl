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

use Data::Dumper;
use Getopt::Long;
use DBI;
use Date::Parse qw(str2time);
use List::Util qw(first);
use FindBin;
use lib "$FindBin::Bin/../lib";
use Games::Lacuna::Client;

my %opts;
GetOptions(\%opts,
    'config=s',
    'db=s',
    'do-digs',
    'send-excavators',
);

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
print "$glc->{total_calls} api calls made.\n";

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
        # Load planet data
        my $planet    = $glc->body(id => $planets{$planet_name});
        my $result    = $planet->get_buildings;
        my $buildings = $result->{buildings};
        $status->{planet_location}{$planet_name}{x} = $result->{status}{body}{x};
        $status->{planet_location}{$planet_name}{y} = $result->{status}{body}{y};

        my $arch = find_arch_min($buildings);
        if ($arch) {
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
        }

        my $spaceport = find_spaceport($buildings);
        if ($spaceport) {
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
        }

        my @shipyards = find_shipyards($buildings);
        for my $yard (@shipyards) {
            # Make sure this yard is capable of building excavators
            my $buildable = $yard->get_buildable->{buildable};
            next unless $buildable->{excavator}->{can};

            # Keep a record of any planet that could be building excavators, but isn't
            $status->{not_building}{$planet_name} = 1
                unless exists $status->{not_building}{$planet_name};

            # How long to build one in this shipyard?
            my $build_time = $buildable->{excavator}->{cost}->{seconds};

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
        print "Current glyphs:\n";
        my $cnt;
        for my $glyph (sort keys %{$status->{glyphs}}) {
            printf '%13s: %3s', $glyph, $status->{glyphs}->{$glyph};
            print "\n" unless ++$cnt % 4
        }
        print "\n" if $cnt % 4;
        print "\n";
    }

    # Ready to go now?
    if (my @planets = grep { $status->{ready}{$_} } keys %{$status->{ready}}) {
        print <<END;
**** Notice! ****
You have excavators ready to send.  Specify --send-excavators if you want to
send them to the closest available destinations.
*****************
END
        for my $planet (@planets) {
            print "$planet has " . pluralize($status->{ready}{$planet}, 'excavator') . " ready to launch!\n";
        }
        print "\n";
    }

    # Any idle archmins?
    if (keys %{$status->{idle}}) {
        print <<END;
**** Notice! ****
You have idle archaeology minstries.  Specify --do-digs if you want to
start the recommended digs automatically.
*****************
END
        for my $planet (keys %{$status->{idle}}) {
            print "Archaeology Ministry on $planet is idle!\n";
        }
        print "\n";
    }

    # Any planets not building excavators?
    if (my @planets = grep { $status->{not_building}{$_} } keys %{$status->{not_building}}) {
        for my $planet (@planets) {
            print "$planet is not currently building any excavators!  It has " . pluralize($status->{open_docks}{$planet}, 'spot') . " currently available.\n";
        }
        print "\n";
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
        print "Upcoming events:\n";
        for my $event (@events) {
            display_event($event);
        }
    }

    print "\n";
}

sub format_time {
    my $time = shift;

    my $now = time();
    my $diff = $time - $now;

    given ($diff) {
        when ($_ < 0) {
            return "just finished";
        }
        when ($_ < 90) {
            return pluralize($_, 'second');
        }
        when ($_ < 5400) {
            my $min = int($_ / 60);
            return pluralize($min, 'minute');
        }
        when ($_ < 86400) {
            my $hrs = int($_ / 3600);
            return pluralize($hrs, 'hour');
        }
        default {
            my $days = int($_ / 86400);
            return pluralize($days, 'day');
        }
    }
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

    printf "%15s: %s\n", $event->{when}, $event->{detail};
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
    for my $planet (keys %{$status->{idle}}) {
        my $ore = determine_ore($status->{available_ore}{$planet}, $status->{glyphs});
        if ($ore) {
            print "Starting a dig for $ore on $planet...\n";
            $status->{archmin}{$planet}->search_for_glyph($ore);
            push @{$status->{digs}}, {
                planet   => $planet,
                finished => time() + (6 * 60 * 60),
            };
            delete $status->{idle}{$planet};
        } else {
            print "Not starting a dig on $planet; not enough of any type of ore.\n";
        }
    }
}

sub determine_ore {
    my ($ore, $glyphs) = @_;

    my ($which) =
        sort {
            ($glyphs->{$a} || 0) <=> ($glyphs->{$b} || 0) ||
            $ore->{$b} <=> $ore->{$a} ||
            int(rand(3)) - 1
        }
        keys %$ore;

    return $which;
}


## Excavators ##

sub send_excavators {
    PLANET:
    for my $planet (grep { $status->{ready}{$_} } keys %{$status->{ready}}) {
        my $port = $status->{spaceports}{$planet};

        for (1 .. $status->{ready}{$planet}) {
            my ($ships, $dest_name, $x, $y);
            while (! defined $dest_name) {
                ($dest_name, $x, $y) = pick_destination($planet);
                eval {
                    $ships = $port->get_ships_for($status->{planets}{$planet}, {x => $x, y => $y});
                };
                if ($@) {
                    my $e = $@;
                    if ($e->code eq '1002') {
                        # Empty orbit, update db and try again
                        print "$dest_name is an empty orbit, trying again...\n";
                        mark_orbit_empty($x, $y);
                        $dest_name = undef;
                        next;
                    } else {
                        die $e;
                    }
                }

                unless (grep { $_->{type} eq 'excavator' } @{$ships->{available}}) {
                    if (grep { $_->{reason}[0] eq '1010' } @{$ships->{unavailable}}) {
                        # This will set the "last_excavated" time to now, which is not
                        # the case, but it's as good as we have.  It means that some bodies
                        # might take longer to get re-dug but whatever, there are others
                        print "$dest_name was unavailable due to recent search, trying again...\n";
                        update_last_sent($x, $y);
                    } else {
                        print "Unknown error sending excavator from $planet to $dest_name!\n";
                        next PLANET;
                    }
                    $dest_name = undef;
                }
            }

            my $ex = first {
                $_->{type} eq 'excavator'
            } @{$ships->{available}};

            print "Sending excavator from $planet to $dest_name...\n";
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
                print "Error sending excavator to $dest_name!\n";
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
select   s.name, o.orbit, o.x, o.y
from     orbitals o
join     stars s on o.star_id = s.id
where    (type in ('planet', 'asteroid') or type is null)
and      (last_excavated is null or date(last_excavated) < date('now', '-30 days'))
and      o.x between ? and ?
and      o.y between ? and ?
order by (((o.x - ?) * (o.x - ?)) + ((o.y - ?) * (o.y - ?))) asc
limit    1
SQL

    my $max_dist = 0;
    my ($dest_x, $dest_y, $dest_name);
    while (!defined $dest_x) {
        $max_dist += 100;
        $find_dest->execute(
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
        }
    }

    return ($dest_name, $dest_x, $dest_y);
}

sub update_last_sent {
    my ($x, $y) = @_;

    my $r = $star_db->do(q{update orbitals set last_excavated = datetime(?,'unixepoch') where x = ? and y = ?}, {}, time(), $x, $y);
    unless ($r > 0) {
        print STDERR "Warning: could not update orbitals table for body at $x, $y!\n";
    }
}

sub mark_orbit_empty {
    my ($x, $y) = @_;

    my $r = $star_db->do(q{update orbitals set type = 'empty' where x = ? and y = ?}, {}, $x, $y);
    unless ($r > 0) {
        print STDERR "Warning: could not update orbitals table for body at $x, $y!\n";
    }
}
