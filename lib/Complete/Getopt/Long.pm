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

# return the key/element if $opt expands unambiguously to exactly one
# key/element in $opts (which can be a hash or array), otherwise return undef.
# e.g. _expand1('--fo', [qw/--foo --bar --baz/]) is true, but _expand1('--ba',
# ...) or _expand1('--qux', ...) are undef.
sub _expand1 {
    my ($opt, $opts) = @_;
    my @candidates;
    my $is_hash = ref($opts) eq 'HASH';
    for ($is_hash ? (keys %$opts) : @$opts) {
        next unless index($_, $opt) == 0;
        push @candidates, $is_hash ? $opts->{$_} : $_;
    }
    return @candidates == 1 ? $candidates[0] : undef;
}

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
            summary     => 'Command line arguments, like @ARGV',
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
    #say "D:words=", join(", ", @$words);
    my $gospec = $args{getopt_spec} or die "Please specify getopt_spec";
    my $comps = $args{completion};

    # parse all options first
    my %opts;
    for my $ospec (keys %$gospec) {
        my $res = Getopt::Long::Util::parse_getopt_long_opt_spec($ospec)
            or die "Can't parse option spec '$ospec'";
        $res->{min_vals} //= $res->{type} ? 1 : 0;
        $res->{max_vals} //= $res->{type} || $res->{opttype} ? 1:0;
        for my $o0 (@{ $res->{opts} }) {
            my @o = $res->{is_neg} ? ($o0, "no$o0", "no-$o0") : ($o0);
            for my $o (@o) {
                my $k = length($o) > 1 ? "--$o" : "-$o";
                $opts{$k} = {
                    name => $k,
                    ospec => $ospec, # key to getopt specification
                    parsed => $res,
                };
            }
        }
    }
    my @optnames = sort keys %opts;

    my %seen_opts;
    my @expects; # what each word expect (hash of optname,optval,arg,separator)

    my $i = -1;

  WORD:
    while (1) {
        $i++;
        last WORD if $i >= @$words;
        my $word = $words->[$i];
        #say "D:i=$i, word=$word, ~~\@\$words=",~~@$words;

        if ($word eq '--' && $i != $cword) {
            $expects[$i] = {separator=>1};
            while (1) {
                $i++;
                last WORD if $i >= @$words;
                $expects[$i] = {arg=>1};
            }
        }

        if ($word =~ /\A-/) {
            my $opt = $word; $opt =~ s/=.*//;
            my $opthash = _expand1($opt, \%opts);
            if ($opthash) {
                # a known argument
                $opt = $opthash->{name};
                $expects[$i]{optname} = $opt;
                $seen_opts{ $opt }++;
                my $min_vals = $opthash->{parsed}{min_vals};
                my $max_vals = $opthash->{parsed}{max_vals};
                for (1 .. $min_vals) {
                    $i++;
                    last WORD if $i >= @$words;
                    $expects[$i]{optval} = $opt;
                }
                for (1 .. $max_vals-$min_vals) {
                    last if $i+$_ >= @$words;
                    last if $words->[$i+$_] =~ /\A-/; # a new option
                    $expects[$i+$_]{optval} = $opt; # but can also be optname
                }
                next WORD;
            } else {
                # an unknown option, assume it doesn't require argument
                $opt = undef;
                $expects[$i]{optname} = $opt;
            }
            # = after --opt
            if ($i+1 < @$words && $words->[$i+1] eq '=') {
                $i++;
                $expects[$i] = {separator => 1};
                if ($i+1 < @$words) {
                    $i++;
                    $expects[$i]{optval} = $opt;
                }
                next WORD;
            }
        }
    }

    #use DD; dd \@expects;

    my $word = $words->[$cword];
    my $exp = $expects[$cword];
    my @res;
    if (exists $exp->{optname}) {
        my $opt = $exp->{optname};
        # complete option names
        my @o;
        for (@optnames) {
            #say "D:$_";
            my $repeatable = 0;
            if ($seen_opts{$_}) {
                my $opthash = $opts{$_};
                my $ospecval = $gospec->{$opthash->{ospec}};
                my $parsed = $opthash->{parsed};
                if (ref($ospecval) eq 'ARRAY') {
                    $repeatable = 1;
                } elsif ($parsed->{desttype} || $parsed->{is_inc}) {
                    $repeatable = 1;
                }
            }
            # skip options that have been specified and not repeatable
            next if $seen_opts{$_} && !$repeatable && (!$opt || $opt eq $_);
            #use DD; dd {seen=>$seen_opts{$_}, repeatable=>$repeatable, opt=>$opt};
            push @o, $_;
        }
        #use DD; dd \@o;
        push @res, @{ Complete::Util::complete_array_elem(
            array => \@o, word => $word) };
    }
    if (exists($exp->{optval}) && $comps) {
        my $opt = $exp->{optval};
        my $opthash = $opts{$opt};
        my $comp = $comps->{$opthash->{ospec}};
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

    if (exists($exp->{arg}) && $comps) {
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
