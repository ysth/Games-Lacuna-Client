#!/usr/bin/perl
#

use strict;
use warnings;

use feature ':5.10';

use FindBin;
use List::Util qw(first);
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
    'percent=i',
) or usage();

usage() if $opts{h};

my %do_planets;
if ($opts{planet}) {
    %do_planets = map { normalize_planet($_) => 1 } @{$opts{planet}};
}

my $glc = Games::Lacuna::Client->new(
    cfg_file => $opts{config} || "$FindBin::Bin/../lacuna.yml",
);

# Do it
my $empire = $glc->empire->get_status->{empire};

# reverse hash, to key by name instead of id
my %planets = map { $empire->{planets}{$_}, $_ } keys %{$empire->{planets}};

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

    my $int = find_int_min($buildings);
    if ($int) {
        verbose("Found intelligence ministry on $planet_name\n");

        my (@spies, $page, $done);
        while (!$done) {
            my $spies = $int->view_spies(++$page);
            push @spies, @{$spies->{spies}};
            $done = 25 * $page >= $spies->{spy_count};
        }

        my @defenders = grep { $_->{assigned_to}{name} eq $planet_name } @spies;

        # Set any idle defending spies to Counter
        for (@defenders) {
            if ($_->{assignment} eq 'Idle') {
                output("Setting idle spy $_->{name} on $planet_name to Counter Espionage\n");
                $int->assign_spy($_->{id}, 'Counter Espionage');
                $_->{assignment} = 'Counter Espionage';
            }
        }

        # Sort spies by lowest intel experience
        my @ready =
            sort { $a->{intel} <=> $b->{intel} }
            grep { $_->{assignment} eq 'Counter Espionage' }
            @defenders;

        my @busy = grep { $_->{assignment} eq 'Security Sweep' } @defenders;
        my $total_candidates = @ready + @busy;

        my $pct = $opts{percent} ? $opts{percent} / 100 : 0.33;
        my $need = int($total_candidates * $pct);
        my $to_assign = $need - @busy;

        my $def_cnt = @defenders;
        my $sweep_cnt = grep { $_->{assignment} eq 'Security Sweep' } @defenders;

        for (1 .. $to_assign) {
            my $spy = $ready[$_ - 1];
            output("Assigning spy $spy->{name} on $planet_name to Security Sweep\n");
            $int->assign_spy($spy->{id}, 'Security Sweep');
            $sweep_cnt++;
        }

        output("$sweep_cnt of $def_cnt defenders currently sweeping on $planet_name\n");
    } else {
        verbose("No intelligence ministry on $planet_name\n");
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

sub find_int_min {
    my ($buildings) = @_;

    # Find the Intelligence Ministry
    my $int_id = first {
        $buildings->{$_}->{name} eq 'Intelligence Ministry'
    }
    grep { $buildings->{$_}->{level} > 0 }
    keys %$buildings;

    return if not $int_id;

    my $building  = $glc->building(
        id   => $int_id,
        type => 'Intelligence',
    );

    return $building;
}

sub usage {
    diag(<<END);
Usage: $0 [options]

This will rotate your weakest spies through Security detail to give them
experience.  By default a third of your idle defending spies will be assigned.

Options:
  --verbose              - Output extra information.
  --quiet                - Print no output except for errors.
  --config <file>        - Specify a GLC config file, normally lacuna.yml.
  --db <file>            - Specify a star database, normally stars.db.
  --planet <name>        - Specify a planet to process.  This option can be
                           passed multiple times to indicate several planets.
                           If this is not specified, all relevant colonies will
                           be inspected.
  --percent <n>          - Percentage of idle spies to assign, default is 33
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
