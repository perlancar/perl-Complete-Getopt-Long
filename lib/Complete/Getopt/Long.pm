package Complete::Getopt::Long;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(
                       complete_cli_arg
               );

our %SPEC;

$SPEC{complete_cli_arg} = {
    v => 1.1,
    summary => 'Complete command-line argument using '.
        'Getopt::Long specification',
    description => <<'_',

This routine can complete option names, where the option names are retrieved
from `Getopt::Long` specification. If you provide completion hints in `hints`,
you can also complete option _values_ and _arguments_.

_
    args => {
        getopt_spec => {
            summary => 'Getopt::Long specification',
            schema  => 'hash*',
            req     => 1,
        },
        completion => {
            summary     =>
                'Completion routines for complete option values/arguments',
            description => <<'_',

The keys are option spec, like in `getopt_spec`. To refer to arguments, use ''
(empty string). The values are either arrayrefs (to specify valid values) or a
coderefs to supply custom completion. Completion code will receive a hash of
arguments containing these keys: `word` (word to be completed) and is expected
to return a completion reply in the form of array. The various `complete_*`
function like those in `Complete::Util` or the other `Complete::*` modules are
suitable to use here. Example:

    require Complete::Unix;
    complete_cli_arg(
        getopt_spec => {
            'help|h'   => sub{...},
            'format=s' => \$fmt,
            'user=s'   => \$user,
        },
        completion  => {
            'format=s' => ['json', 'text', 'xml', 'yaml'],
            'user=s'   => \&Complete::Unix::complete_user,
        },
    );

_
            schema      => 'hash*',
        },
        words => {
            summary     => 'Command line, already broken into words',
            description => <<'_',

See function `parse_cmdline` in `Complete::Bash` on how to produce this (if
you're using bash).

_
            schema      => 'array*',
            req         => 1,
        },
        cword => {
            summary     =>
                "Index in words of the word we're trying to complete",
            description => <<'_',

See function `parse_cmdline` in `Complete::Bash` on how to produce this (if
you're using bash).

_
            schema      => 'int*',
            req         => 1,
        },
    },
    result_naked => 1,
    result => {
        schema => ['any*' => of => ['hash*', 'array*']],
        description => <<'_',

You can use `format_completion` function in `Complete::Bash` module to format
the result of this function for bash.

_
    },
    examples => [
        {
            args => {
                getopt_spec => {'help|h'=>sub{}, 'arg1|a=s'=>sub{},
                                'arg2|b=s'=>sub{}},
                words       => [qw//],
                cword       => 0,
            },
            result  => ['--arg1', '--arg2', '--help', '-a', '-b', '-h'],
        },
        # not yet implemented
        #{
        #    args => {
        #        getopt_spec => {'help|h'=>sub{}, 'arg1|a=s'=>sub{},
        #                        'arg2|b=s'=>sub{}},
        #        words       => [qw/--arg1 one --a/],
        #        cword       => 2,
        #    },
        #    result  => ['--arg2'],
        #    summary => '--arg1 by default is no longer completed, unless '.
        #        'it assigns to array or has a "+" specifier',
        #},
    ],
};
sub complete_cli_arg {
    require Complete::Util;
    require Getopt::Long::Util;
    require List::MoreUtils;

    my %args = @_;

    my $words  = $args{words} or die "Please specify words";
    defined(my $cword = $args{cword}) or die "Please specify cword";
    my $gospec = $args{getopt_spec} or die "Please specify getopt_spec";
    my $comps = $args{completion};

    # parse all options first
    my %opts;
    for my $ospec (keys %$gospec) {
        my $res = Getopt::Long::Util::parse_getopt_long_opt_spec($ospec)
            or die "Can't parse option spec '$ospec'";
        for my $o0 (@{ $res->{opts} }) {
            my @o = $res->{is_neg} ? ($o0, "no$o0", "no-$o0") : ($o0);
            for my $o (@o) {
                my $k = length($o) > 1 ? "--$o" : "-$o";
                $opts{$k} = {
                    ospec => $ospec,
                    min_vals => $res->{min_vals} //
                        ($res->{type} ? 1 : 0),
                    max_vals => $res->{max_vals} //
                        ($res->{type} || $res->{opttype} ? 1:0),
                };
            }
        }
    }
    my @optnames = sort keys %opts;

    my $opt;
    my $what = 'optname,arg';
    my $ohash;
    my $i = 0;
  OPT:
    while ($i < $cword) {
        my $word = $words->[$i] // '';
        if ($word eq '--') {
            $what = 'arg';
            last OPT;
        }
        if ($what =~ /optname/) {
            # should always be the case
            if ($opts{$word}) {
                $ohash = $opts{$word};
                $what = 'optval';
                $i += $ohash->{min_vals};
                last OPT if $i >= $cword;
                $what = 'optname,optval,arg';
                for my $j (($ohash->{min_vals}+1) .. ($ohash->{max_vals})) {
                    $i++;
                    last OPT if $i >= @$words;
                    if ($words->[$i] =~ /\A-/) {
                        $what = 'optname';
                        next OPT;
                    }
                }
                $what = +($words->[$i] // '') =~ /\A-/ ?
                    'optname' : 'optname,arg';
                $i++;
                next OPT;
            } else {
                # assume nonexisting option to be with no value
                $i++;
                next OPT;
            }
        }
    }

    my @res;
    my $word = $words->[$cword] // '';
    if ($what =~ /optname/) {
        push @res, @{ Complete::Util::complete_array_elem(
            array => \@optnames, word => $word) };
    }
    if ($what =~ /optval/ && $ohash && $comps) {
        my $comp = $comps->{$ohash->{ospec}};
        if (ref($comp) eq 'ARRAY') {
            push @res, @{ Complete::Util::complete_array_elem(
                array => \@$comp, word => $word) };
        } elsif (ref($comp) eq 'CODE') {
            my $compres = $comp->(word=>$word);
            if (ref($compres) eq 'ARRAY') {
                push @res, @$compres;
            } elsif (ref($compres) eq 'HASH') {
                return $compres unless @res;
                push @res, @{ $compres->{completion} // [] };
            }
        }
    }
    if ($what =~ /arg/ && $comps) {
        my $comp = $comps->{''};
        if (ref($comp) eq 'ARRAY') {
            push @res, @$comp;
        } elsif (ref($comp) eq 'CODE') {
            my $compres = $comp->(word=>$word);
            if (ref($compres) eq 'ARRAY') {
                push @res, @$compres;
            } elsif (ref($compres) eq 'HASH') {
                return $compres unless @res;
                push @res, @{ $compres->{completion} // [] };
            }
        }
    }

    [sort(List::MoreUtils::uniq(@res))];
}

1;
#ABSTRACT: Complete command-line argument using Getopt::Long specification

=head1 SYNOPSIS

See L<Getopt::Long::Complete> for an easy way to use this module.


=head1 DESCRIPTION


=head1 SEE ALSO

L<Getopt::Long::Complete>

L<Complete>

L<Complete::Bash>

Other modules related to bash shell tab completion: L<Bash::Completion>,
L<Getopt::Complete>.

L<Perinci::CmdLine> - an alternative way to easily create command-line
applications with completion feature.

=cut
