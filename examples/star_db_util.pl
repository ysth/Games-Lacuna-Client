#!/usr/bin/perl
#
# This program manages the star database used by glyphinator
# It performs the following functions:
#
#  * Fill star db from probe data
#  * Merge a second db into the main one

use strict;
use warnings;

use DBI;
use FindBin;
use List::Util qw(first);
use Getopt::Long;
use Data::Dumper;

use lib "$FindBin::Bin/../lib";
use Games::Lacuna::Client;

my %opts;
GetOptions(\%opts,
    'h|help',
    'config=s',
    'db=s',
    'create-db',
    'merge-db=s',
    'planet=s@',
    'no-fetch',
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
        die "No star database found.  Specify it with --db or use --create-db to create it.\n";
    }
}
$star_db->{AutoCommit} = 0;

if ($opts{'merge-db'}) {
    my $merge_db = DBI->connect("dbi:SQLite:$opts{'merge-db'}")
        or die "Can't open star database $opts{'merge-db'}: $DBI::errstr\n";

    # Copy stars
    my $get_stars = $merge_db->prepare('select * from stars');
    $get_stars->execute;
    while (my $star = $get_stars->fetchrow_hashref) {
        if (! star_exists($star->{x}, $star->{y})) {
            insert_star(@{$star}{qw/id name x y color/});
        }
    }

    # Copy orbitals
    my $get_orbitals = $merge_db->prepare('select * from orbitals');
    $get_orbitals->execute;
    while (my $orbital = $get_orbitals->fetchrow_hashref) {
        # Check if it exists in the star db, and if so what its type is
        if (my $row = orbital_exists($orbital->{x}, $orbital->{y})) {
            if (defined $orbital->{type} and not defined $row->{type}) {
                update_orbital_type($orbital->{x}, $orbital->{y}, $orbital->{type});
            }
        } else {
            insert_orbital(@{$orbital}{qw/body_id star_id orbit x y type/});
        }
    }
}

unless ($opts{'no-fetch'}) {
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

        my $obs = find_observatory($buildings);
        next unless $obs;

        my $page = 1;
        my $done;
        while (!$done) {
            $done = 1;

            my $stars = $obs->get_probed_stars($page);
            for my $star (@{$stars->{stars}}) {
                if (! star_exists($star->{x}, $star->{y})) {
                    insert_star(@{$star}{qw/id name x y color/});
                }

                if ($star->{bodies} and @{$star->{bodies}}) {
                    for my $body (@{$star->{bodies}}) {
                        if (my $db_entry = orbital_exists($body->{x}, $body->{y})) {
                            if (!$db_entry->{type} or $db_entry->{type} ne $body->{type}) {
                                update_orbital_type($body->{x}, $body->{y}, $body->{type});
                            }
                        } else {
                            insert_orbital(@{$body}{qw/id star_id orbit x y type/});
                        }
                    }
                }
            }

            if ($stars->{star_count} > $page * 25) {
                $done = 0;
                $page++;
            }
        }
    }
}

$star_db->commit;

undef $glc;
exit 0;

{
    my $check_star;
    sub star_exists {
        my ($x, $y) = @_;
        $check_star ||= $star_db->prepare('select 1 from stars where x = ? and y = ?');
        $check_star->execute($x, $y);
        my ($res) = $check_star->fetchrow_array;
        return $res;
    }
}

{
    my $insert_star;
    sub insert_star {
        my ($id, $name, $x, $y, $color) = @_;

        print "Inserting star $name at $x, $y\n";
        $insert_star ||= $star_db->prepare('insert into stars (id, name, x, y, color) values (?,?,?,?,?)');
        $insert_star->execute($id, $name, $x, $y, $color)
            or die "Can't insert star: " . $insert_star->errstr;
    }
}

{
    my $check_orbital;
    sub orbital_exists {
        my ($x, $y) = @_;

        $check_orbital ||= $star_db->prepare('select * from orbitals where x = ? and y = ?');
        $check_orbital->execute($x, $y);
        return $check_orbital->fetchrow_hashref;
    }
}

{
    my $insert_orbital;
    sub insert_orbital {
        my ($id, $star_id, $orbit, $x, $y, $type) = @_;

        print "Inserting orbital for star $star_id orbit $orbit at $x, $y\n";
        $insert_orbital ||= $star_db->prepare('insert into orbitals (body_id, star_id, orbit, x, y, type) values (?,?,?,?,?,?)');
        $insert_orbital->execute($id, $star_id, $orbit, $x, $y, $type)
            or die "Can't insert orbital: " . $insert_orbital->errstr;
    }
}

{
    my $update_orbital_type;
    sub update_orbital_type {
        my ($x, $y, $type) = @_;

        print "Updating type for orbital at $x, $y to $type\n";
        $update_orbital_type ||= $star_db->prepare('update orbitals set type = ? where x = ? and y = ?');
        $update_orbital_type->execute($type, $x, $y)
            or die "Can't update orbital: " . $update_orbital_type->errstr;
    }
}


sub normalize_planet {
    my ($planet_name) = @_;

    $planet_name =~ s/\W//g;
    $planet_name = lc($planet_name);
    return $planet_name;
}

sub find_observatory {
    my ($buildings) = @_;

    # Find an Observatory
    my $obs_id = first {
            $buildings->{$_}->{name} eq 'Observatory'
    } keys %$buildings;

    return if not $obs_id;
    return $glc->building(id => $obs_id, type => 'Observatory');
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

Update the stars.db SQLite database for use with glyphinator.pl

Options:
  --config <file>        - Specify a GLC config file, normally lacuna.yml.
  --db <file>            - Specify a star database, normally stars.db.
  --create-db            - Create the star database and initialize the schema.
  --merge-db <file>      - Copy missing data from another database file
  --no-fetch             - Don't fetch probe data, only merge databases
  --planet <name>        - Specify a planet to process.  This option can be
                           passed multiple times to indicate several planets.
                           If this is not specified, all relevant colonies will
                           be inspected.
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
