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
    'create-db',
    'config=s',
    'planet=s@',
    'do-digs',
    'min-ore=i',
    'min-arch=i',
    'preferred-ore=s@',
    'send-excavators',
    'max-excavators=i',
    'min-dist=i',
    'max-dist=i',
    'dry-run',
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
    if ($opts{'create-db'}) {
        $star_db = DBI->connect("dbi:SQLite:$db_file")
            or die "Can't create star database $db_file: $DBI::errstr\n";
        for my $sql (create_star_db_sql()) {
            $star_db->do($sql);
        }
        output("$db_file initialized\n");
    } else {
        warn "No star database found.  Specify it with --db or use --create-db to create it.\n";
        if ($opts{'send-excavators'}) {
            warn "Can't send excavators without star database!\n";
        }
    }
}
if ($star_db) {
    # Check that db is populated
    my ($cnt) = $star_db->selectrow_array('select count(*) from orbitals');
    unless ($cnt) {
        diag("Star database is empty!\n");
        $star_db = undef;
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
            my $arch_detail = $arch->view;
            $status->{archlevel}{$planet_name} = $arch_detail->{building}{level};
            if ($arch_detail->{building}{work}) {
                my $time_left = $arch_detail->{building}{work}{seconds_remaining};
                push @{$status->{digs}}, {
                    planet   => $planet_name,
                    finished => time() + $time_left,
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

            if (!$buildable->{excavator}->{can} and $buildable->{excavator}->{reason}->[0] eq '1013') {
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
                        finished => str2time(
                            map { s!^(\d+)\s+(\d+)\s+!$2/$1/!; $_ }
                            $_->{date_completed}
                        ),
                    }
                }
                grep { $_->{type} eq 'excavator' }
                @{$work_queue->{ships_building}};

            if (@building) {
                push @{$status->{building}{$planet_name}}, @building;
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
    if (keys %{$status->{glyphs} || {}}) {
        my $total_glyphs = 0;
        output("Current glyphs:\n");
        my $cnt;
        for my $glyph (sort keys %{$status->{glyphs}}) {
            $total_glyphs += $status->{glyphs}->{$glyph};
            output(sprintf '%13s: %3s', $glyph, $status->{glyphs}->{$glyph});
            output("\n") unless ++$cnt % 4
        }
        output("\n") if $cnt % 4;
        output("\n");
        output("Current stock: $total_glyphs glyphs\n\n");
    }

    # Ready to go now?
    if (my @planets = grep { $status->{ready}{$_} } keys %{$status->{ready}}) {
        output(<<END);
**** Notice! ****
You have excavators ready to send.  Specify --send-excavators if you want to
send them to the closest available destinations.
*****************
END
        for my $planet (sort @planets) {
            output("$planet has ", pluralize($status->{ready}{$planet}, 'excavator')
                , " ready to launch!\n");
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


    # Fix this to be something like the following:
    #   Planet Foo is buildng N excavators, first done in [when], last done in [when]
    if (grep { @{$status->{building}{$_}} } keys %{$status->{building}}
        or grep { $status->{not_building}{$_} } keys %{$status->{not_building}}) {

        output("Excavators building:\n");
        for my $planet (sort keys %{$status->{planets}}) {
            if ($status->{building}{$planet} and @{$status->{building}{$planet}}) {
                my @sorted = sort { $a->{finished} <=> $b->{finished} }
                    @{$status->{building}{$planet}};

                my $first = $sorted[0];
                my $last = $sorted[$#sorted];

                output("    ",scalar(@sorted), " excavators building on $planet, ",
                    "first done in ", format_time($first->{finished}),
                    ", last done in ", format_time($last->{finished}), "\n");

            } elsif ($status->{not_building}{$planet}) {
                output("$planet is not currently building any excavators!  It has "
                    . pluralize($status->{open_docks}{$planet}, 'spot') . " currently available.\n");
            }
        }
        output("\n");
    }

    my @events;
    for my $dig (@{$status->{digs}}) {
        push @events, {
            epoch  => $dig->{finished},
            detail => "Dig finishing on $dig->{planet}",
        };
    }

    for my $ship (@{$status->{flying}}) {
        push @events, {
            epoch  => $ship->{arrives},
            detail => "Excavator from $ship->{planet} arriving at $ship->{destination}",
        };
    }
    @events =
        sort { $a->{epoch} <=> $b->{epoch} }
        map  { $_->{when} = format_time($_->{epoch}); $_ }
        @events;

    if (@events) {
        output("Searches completing:\n");
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

    output(sprintf "    %11s: %s\n", $event->{when}, $event->{detail});
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
        if ($opts{'min-arch'} and $status->{archlevel}{$planet} < $opts{'min-arch'}) {
            output("$planet is not above specified Archaeology Ministry level ($opts{'min-arch'}), skipping dig.\n");
            next;
        }
        my $ore = determine_ore(
            $opts{'min-ore'} || 10_000,
            $opts{'preferred-ore'} || [],
            $status->{available_ore}{$planet},
            $status->{glyphs},
            $digging
        );
        if ($ore) {
            if ($opts{'dry-run'}) {
                output("Would have started a dig for $ore on $planet.\n");
            } else {
                output("Starting a dig for $ore on $planet...\n");
                $status->{archmin}{$planet}->search_for_glyph($ore);
                push @{$status->{digs}}, {
                    planet   => $planet,
                    finished => time() + (6 * 60 * 60),
                };
            }
            delete $status->{idle}{$planet};
        } else {
            output("Not starting a dig on $planet; not enough of any type of ore.\n");
        }
    }
}

sub determine_ore {
    my ($min, $preferred, $ore, $glyphs, $digging) = @_;

    my %is_preferred = map { $_ => 1 } @$preferred;

    my ($which) =
        sort {
            ($is_preferred{$b} || 0) <=> ($is_preferred{$a} || 0) or
            ($glyphs->{$a} || 0) + ($digging->{$a} || 0) <=> ($glyphs->{$b} || 0) + ($digging->{$b} || 0) or
            $ore->{$b} <=> $ore->{$a} or
            int(rand(3)) - 1
        }
        grep { $ore->{$_} >= $min }
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

        # During a dry-run, not actually updating the database results in
        # each excavator from each planet going to the same target.  Add
        # them to an exclude list to simulate them being actually used.
        my %skip;

        my $count = $opts{'max-excavators'} || $status->{ready}{$planet};

        my @dests = pick_destination($planet,
            count    => $count,
            min_dist => $opts{'min-dist'} || undef,
            max_dist => $opts{'max-dist'} || undef,
        );

        if (@dests < $count) {
            diag("Couldn't fetch $count destinations from $planet!\n");
        }

        my $all_done;
        while (!$all_done) {
            my $need_more = 0;

            for (@dests) {
                my ($dest_name, $x, $y, $distance) = @$_;

                my $ships;
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

                            $need_more++;
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

                    $need_more++;
                    next;
                }

                $skip{$dest_name}++;

                my $ex = first {
                    $_->{type} eq 'excavator'
                } @{$ships->{available}};

                if ($opts{'dry-run'}) {
                    output("Would have sent excavator from $planet to $dest_name ($distance units).\n");
                } else {
                    output("Sending excavator from $planet to $dest_name ($distance units)...\n");
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

                $status->{ready}{$planet}--;
            }

            # Defer looking up more until we've finished processing our
            # current queue, otherwise we end up re-fetching ones we haven't
            # actually tried yet and get duplicates
            if ($need_more) {
                @dests = pick_destination($planet,
                    count    => $need_more,
                    min_dist => $opts{'min-dist'} || undef,
                    max_dist => $opts{'max-dist'} || undef,
                    skip     => [keys %skip],
                );
            } else {
                $all_done = 1;
            }
        }

        delete $status->{ready}{$planet}
            if !$status->{ready}{$planet};
    }
}

sub pick_destination {
    my ($planet, %args) = @_;

    my $base_x = $status->{planet_location}{$planet}{x};
    my $base_y = $status->{planet_location}{$planet}{y};

    $args{max_dist} ||= 3000;
    my $real_min = $args{min_dist} ? int(sqrt($args{min_dist} * $args{min_dist} / 2)) : 0;
    my $real_max = $args{max_dist} ? int(sqrt($args{max_dist} * $args{max_dist} / 2)) : 0;

    my $count       = $args{count} || 1;
    my $current_max = $real_min;
    my $skip        = $args{skip} || [];

    my @results;
    while (@results < $count and (!$real_max or $current_max < $real_max)) {
        my $current_min = $current_max;
        $current_max += 100;
        $current_max = $real_max
            if $real_max and $current_max > $real_max;
        verbose("Increasing box size, max is $current_max, min is $current_min\n");

        my $skip_sql = '';
        if (@$skip) {
            $skip_sql = "and s.name || ' ' || o.orbit not in (" . join(',',map { '?' } 1..@$skip) . ")";
        }
        my $inner_box = $current_min > 0 ? 'and o.x not between ? and ? and o.y not between ? and ?' : '';
        my $find_dest = $star_db->prepare(<<SQL);
select   s.name, o.orbit, o.x, o.y, (o.x - ?) * (o.x - ?) + (o.y - ?) * (o.y - ?) as dist
from     orbitals o
join     stars s on o.star_id = s.id
where    (type in ('planet', 'asteroid') or type is null)
and      (last_excavated is null or date(last_excavated) < date('now', '-30 days'))
and      o.x between ? and ?
and      o.y between ? and ?
$inner_box
$skip_sql
order by dist asc
limit    $count
SQL

        # select columns,x/y betweens
        my @vals = (
            $base_x, $base_x, $base_y, $base_y,
            $base_x - $current_max,
            $base_x + $current_max,
            $base_y - $current_max,
            $base_y + $current_max,
        );
        if ($current_min > 0) {
            push @vals,
                $base_x - $current_min,
                $base_x + $current_min,
                $base_y - $current_min,
                $base_y + $current_min,
        }
        push @vals, @$skip;

        $find_dest->execute(@vals);
        while (my $row = $find_dest->fetchrow_hashref) {
            my $dest_name = "$row->{name} $row->{orbit}";
            my $dist = int(sqrt($row->{dist}));
            verbose("Selected destination $dest_name, which is $dist units away\n");

            push @results, [$dest_name, $row->{x}, $row->{y}, $dist];
            push @$skip, $dest_name;
        }
    }

    return @results;
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

sub create_star_db_sql {
    return
        <<SQL,
CREATE TABLE stars(
    id    int   primary key,
    name  text,
    x     int,
    y     int,
    color text
)
SQL
        <<SQL,
CREATE TABLE orbitals(
    body_id        int,
    star_id        int,
    orbit          int,
    x              int,
    y              int,
    type           text,
    last_excavated datetime,
    PRIMARY KEY(star_id, orbit),
    FOREIGN KEY(star_id) REFERENCES stars(id)
)
SQL
        <<SQL,
CREATE INDEX orbital_x_y on orbitals(x,y)
SQL
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
  --verbose              - Output extra information.
  --quiet                - Print no output except for errors.
  --config <file>        - Specify a GLC config file, normally lacuna.yml.
  --db <file>            - Specify a star database, normally stars.db.
  --create-db            - Create the star database and initialize the schema.
  --planet <name>        - Specify a planet to process.  This option can be
                           passed multiple times to indicate several planets.
                           If this is not specified, all relevant colonies will
                           be inspected.
  --do-digs              - Begin archaeology digs on any planets which are idle.
  --min-ore <amount>     - Do not begin digs with less ore in reserve than this
                           amount.  The default is 10,000.
  --min-arch <level>     - Do not begin digs on any archaeology ministry less
                           than this level.  The default is 1.
  --preferred-ore <type> - Dig using the specified ore whenever available.
  --send-excavators      - Launch ready excavators at their nearest destination.
                           The information for these is selected from the star
                           database, and the database is updated to reflect your
                           new searches.
  --max-excavators <n>   - Send at most this number of excavators from any colony
  --min-dist <n>         - Minimum distance to send excavators
  --max-dist <n>         - Maximum distance to send excavators
  --dry-run              - Don't actually take any action, just report status and
                           what actions would have taken place.
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
