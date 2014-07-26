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

sub _default_fallback_completion {
    my %args = @_;
    my $word = $args{word} // '';
    if ($word =~ /\A\$/) {
        return Complete::Util::complete_env(word=>$word, ci=>$args{ci});
    }
    if ($word =~ /\A~/) {
        require Complete::Unix;
        $word =~ s/\A~//;
        return [
            map {"~$_"}
                @{ Complete::Unix::complete_user(word=>$word, ci=>$args{ci}) }
        ];
    }
    require String::Wildcard::Bash;
    if (String::Wildcard::Bash::contains_wildcard($word)) {
        return {completion=>[glob($word)], path_sep=>'/'};
    }
    return {completion=>Complete::Util::complete_file(word=>$word), path_sep=>'/'};
};

# return the key/element if $opt matches exactly a key/element in $opts (which
# can be an array/hash) OR expands unambiguously to exactly one key/element in
# $opts, otherwise return undef. e.g. _expand1('--fo', [qw/--foo --bar --baz
# --fee --feet/]) and _expand('--fee') is true, but _expand1('--ba', ...) or
# _expand1('--qux', ...) are undef.
sub _expand1 {
    my ($opt, $opts) = @_;
    my @candidates;
    my $is_hash = ref($opts) eq 'HASH';
    for ($is_hash ? (sort {length($a)<=>length($b)} keys %$opts) : @$opts) {
        next unless index($_, $opt) == 0;
        push @candidates, $is_hash ? $opts->{$_} : $_;
        last if $opt eq $_;
    }
    return @candidates == 1 ? $candidates[0] : undef;
}

# mark an option (and all its aliases) as seen
sub _mark_seen {
    my ($seen_opts, $opt, $opts) = @_;
    my $opthash = $opts->{$opt};
    return unless $opthash;
    my $ospec = $opthash->{ospec};
    for (keys %$opts) {
        my $v = $opts->{$_};
        $seen_opts->{$_}++ if $v->{ospec} eq $ospec;
    }
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
arguments containing these keys:

* `type` (str, what is being completed, either `optname`, `optval`, or `arg`)
* `word` (str, word to be completed)
* `opt` (str, option name, e.g. `--str`; undef if we're completing argument)
* `ospec` (str, Getopt::Long option spec, e.g. `str|S=s`; undef when completing
  argument)
* `argpos` (int, argument position, zero-based; undef if completing option)
* `parent_args`
* `seen_opts` (hash, all the options seen in `words`)

and is expected to return a completion reply in the form of array. The various
`complete_*` function like those in `Complete::Util` or the other `Complete::*`
modules are suitable to use here. Example:

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
        fallback_completion => {
            description => <<'_',

If completion routine for a certain option or argument is not provided in
`completion`, this fallback routine is used. The default, if this option is not
specified, is to complete environment variables (`$FOO`) and files.

_
            schema => 'code*',
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
};
sub complete_cli_arg {
    require Complete::Util;
    require Getopt::Long::Util;
    require List::MoreUtils;

    my %args = @_;

    my $words  = $args{words} or die "Please specify words";
    defined(my $cword = $args{cword}) or die "Please specify cword";
    #say "D:words=", join(", ", @$words), ", cword=$cword";
    my $gospec = $args{getopt_spec} or die "Please specify getopt_spec";
    my $comps = $args{completion};
    my $fbcomp = $args{fallback_completion} // \&_default_fallback_completion;

    # parse all options first & supply default completion routine
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

    # for each word, we try to find out whether it's supposed to complete option
    # name, or option value, or argument, or separator (or more than one of
    # them). plus some other information.
    my @expects;

    my $i = -1;
    my $argpos = 0;

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
                $expects[$i] = {arg=>1, argpos=>$argpos++};
            }
        }

        if ($word =~ /\A-/) {
            my $opt = $word; $opt =~ s/=.*//;
            my $opthash = _expand1($opt, \%opts);

            if ($opthash) {
                # a known argument
                $opt = $opthash->{name};
                $expects[$i]{optname} = $opt;
                _mark_seen(\%seen_opts, $opt, \%opts);

                my $min_vals = $opthash->{parsed}{min_vals};
                my $max_vals = $opthash->{parsed}{max_vals};
                #say "D:min_vals=$min_vals, max_vals=$max_vals";

                # detect = after --opt
                if ($i+1 < @$words && $words->[$i+1] eq '=') {
                    $i++;
                    $expects[$i] = {separator=>1, optval=>$opt, word=>''};
                    # force a value due to =
                    if (!$max_vals) { $min_vals = $max_vals = 1 }
                }

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
            } else {
                # an unknown option, assume it doesn't require argument, unless
                # it's --opt= or --opt=foo
                $opt = undef;
                $expects[$i]{optname} = $opt;

                # detect = after --opt
                if ($i+1 < @$words && $words->[$i+1] eq '=') {
                    $i++;
                    $expects[$i] = {separator=>1, optval=>undef, word=>''};
                    if ($i+1 < @$words) {
                        $i++;
                        $expects[$i]{optval} = $opt;
                    }
                }
            }
        } else {
            $expects[$i]{arg} = 1;
            $expects[$i]{argpos} = $argpos++;
        }
    }

    #use DD; dd \@expects;
    #use DD; dd \%seen_opts;

    my $exp = $expects[$cword];
    my $word = $exp->{word} // $words->[$cword];
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
            #use DD; dd {'$_'=>$_, seen=>$seen_opts{$_}, repeatable=>$repeatable, opt=>$opt};
            next if $seen_opts{$_} && !$repeatable && (!$opt || $opt ne $_);
            push @o, $_;
        }
        #use DD; dd \@o;
        push @res, @{ Complete::Util::complete_array_elem(
            array => \@o, word => $word) };
    }
    if (exists($exp->{optval})) {
        my $opt = $exp->{optval};
        my $opthash = $opts{$opt} if $opt;
        my $comp = $comps->{$opthash->{ospec}} if $opthash;
        $comp //= $fbcomp;
        if (ref($comp) eq 'ARRAY') {
            push @res, @{ Complete::Util::complete_array_elem(
                array => \@$comp, word => $word) };
        } elsif (ref($comp) eq 'CODE') {
            my $compres = $comp->(
                type=>'optval', word=>$word, opt=>$opt,
                ospec=>$opthash->{ospec}, argpos=>undef,
                parent_args=>\%args, seen_opts=>\%seen_opts);
            if (ref($compres) eq 'ARRAY') {
                push @res, @$compres;
            } elsif (ref($compres) eq 'HASH') {
                return $compres unless @res;
                push @res, @{ $compres->{completion} // [] };
            }
        }
    }

    if (exists($exp->{arg}) && $comps) {
        my $comp = $comps->{''} // $fbcomp;
        if (ref($comp) eq 'ARRAY') {
            push @res, @$comp;
        } elsif (ref($comp) eq 'CODE') {
            my $compres = $comp->(
                type=>'arg', word=>$word, opt=>undef,
                ospec=>undef, argpos=>$exp->{argpos},
                parent_args=>\%args, seen_opts=>\%seen_opts);
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
