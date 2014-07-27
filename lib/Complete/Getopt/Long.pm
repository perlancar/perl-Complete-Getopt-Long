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

sub _default_completion {
    my %args = @_;
    my $word = $args{word} // '';
    if ($word =~ /\A\$/) {
        return {completion=>
                    Complete::Util::complete_env(word=>$word, ci=>$args{ci}),
                escmode=>'shellvar'};
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
from `Getopt::Long` specification. If you provide completion routine in
`completion`, you can also complete _option values_ and _arguments_.

_
    args => {
        getopt_spec => {
            summary => 'Getopt::Long specification',
            schema  => 'hash*',
            req     => 1,
        },
        completion => {
            summary     =>
                'Completion routine to complete option value/argument',
            schema      => 'code*',
            description => <<'_',

Completion code will receive a hash of arguments containing these keys:

* `type` (str, what is being completed, either `optname`, `optval`, or `arg`)
* `word` (str, word to be completed)
* `opt` (str, option name, e.g. `--str`; undef if we're completing argument)
* `ospec` (str, Getopt::Long option spec, e.g. `str|S=s`; undef when completing
  argument)
* `argpos` (int, argument position, zero-based; undef if completing option)
* `extras`
* `seen_opts` (hash, all the options seen in `words`)

and is expected to return a completion reply in the form of array. The various
`complete_*` function like those in `Complete::Util` or the other `Complete::*`
modules are suitable to use here.

Code can also return undef, in which the default completion routine is called.
It completes from environment variables (`$foo`), usernames (`~foo`), and files.

Example:

    use Complete::Unix qw(complete_user);
    use Complete::Util qw(complete_array_elem);
    complete_cli_arg(
        getopt_spec => {
            'help|h'   => sub{...},
            'format=s' => \$format,
            'user=s'   => \$user,
        },
        completion  => sub {
            my %args  = @_;
            my $word  = $args{word};
            my $ospec = $args{ospec};
            if ($ospec && $ospec eq 'format=s') {
                complete_array(array=>[qw/json text xml yaml/], word=>$word);
            } else {
                complete_user(word=>$word);
            }
        },
    );

_
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
        extras => {
            summary => 'To pass extra arguments to completion routines',
            schema  => 'hash',
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
    my $comp0 = $args{completion};
    my $comp = $comp0 // &_default_completion;
    my $extras = $args{extras};

    # before v0.06, completion is a hash, we'll support this for a while
    if (ref($comp) eq 'HASH') {
        $comp = sub {
            my %cargs = @_;
            my $type  = $cargs{type};
            my $ospec = $cargs{ospec} // '';
            my $word  = $cargs{word};
            for my $k (keys %$comp0) {
                my $v = $comp0->{$k};
                next unless $k eq '' ? $type eq 'arg' : $k eq $ospec;
                if (ref($v) eq 'ARRAY') {
                    return Complete::Util::complete_array_elem(
                        word=>$word, array=>$v);
                } else {
                    return $v->(%cargs);
                }
            }
        };
    }

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
                my $nth = $seen_opts{$opt} // 0;
                $expects[$i]{nth} = $nth;
                _mark_seen(\%seen_opts, $opt, \%opts);

                my $min_vals = $opthash->{parsed}{min_vals};
                my $max_vals = $opthash->{parsed}{max_vals};
                #say "D:min_vals=$min_vals, max_vals=$max_vals";

                # detect = after --opt
                if ($i+1 < @$words && $words->[$i+1] eq '=') {
                    $i++;
                    $expects[$i] = {separator=>1, optval=>$opt, word=>'', nth=>$nth};
                    # force a value due to =
                    if (!$max_vals) { $min_vals = $max_vals = 1 }
                }

                for (1 .. $min_vals) {
                    $i++;
                    last WORD if $i >= @$words;
                    $expects[$i]{optval} = $opt;
                    $expects[$i]{nth} = $nth;
                }
                for (1 .. $max_vals-$min_vals) {
                    last if $i+$_ >= @$words;
                    last if $words->[$i+$_] =~ /\A-/; # a new option
                    $expects[$i+$_]{optval} = $opt; # but can also be optname
                    $expects[$i]{nth} = $nth;
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
        my %compargs = (
            type=>'optval', word=>$word, opt=>$opt, ospec=>$opthash->{ospec},
            argpos=>undef, extras=>$extras, nth=>$exp->{nth},
            seen_opts=>\%seen_opts,
        );
        my $compres = $comp->(%compargs);
        if (!defined $compres) {
            $compres = _default_completion(%compargs);
        }
        if (ref($compres) eq 'ARRAY') {
            push @res, @$compres;
        } elsif (ref($compres) eq 'HASH') {
            return $compres unless @res;
            push @res, @{ $compres->{completion} // [] };
        }
    }

    if (exists($exp->{arg})) {
        my %compargs = (
            type=>'arg', word=>$word, opt=>undef, ospec=>undef,
            argpos=>$exp->{argpos}, extras=>$extras, seen_opts=>\%seen_opts,
        );
        my $compres = $comp->(%compargs);
        if (!defined $compres) {
            $compres = _default_completion(%compargs);
        }
        if (ref($compres) eq 'ARRAY') {
            push @res, @$compres;
        } elsif (ref($compres) eq 'HASH') {
            return $compres unless @res;
            push @res, @{ $compres->{completion} // [] };
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
