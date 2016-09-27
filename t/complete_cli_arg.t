#!perl

use 5.010;
use strict;
use warnings;

use Test::More 0.98;
use Test::Deep 0.107; # perl 5.10 fixes

subtest basics => sub {
    my %gospec = (
        'flag1|1'     => sub{},
        'flag2|f'     => sub{},
        'bool!'       => sub{},
        'int=i'       => sub{},
        'float|F=f'   => sub{},
        'str|S=s'     => sub{},
    );

    test_complete(
        name        => 'option name',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD ^',
        default     => 1,
        result      => [qw/--bool --flag1 --flag2 --float --int
                           --no-bool --nobool --str -1 -F -S -f/],
    );
    test_complete(
        name        => 'option name (2)',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD --f^',
        result      => {words=>
                            [qw/--flag1 --flag2 --float/],
                        esc_mode=>'option'},
    );
    test_complete(
        name        => 'option name (3)',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD --str^',
        result      => {words=>
                            [qw/--str/],
                        esc_mode=>'option'},

    );
    test_complete(
        name        => 'option name (single letter n! does not get --nox and --no-x)',
        args        => {getopt_spec=>{'n!'=>sub{}}, },
        comp_line0  => 'CMD ^',
        default     => 1,
        result      => [qw/-n/],
    );
    test_complete(
        name        => 'option name (bundling)',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD -f^',
        result      => {words=>
                            [qw/-f1 -fF -fS/],
                        esc_mode=>'option'},
    );
    test_complete(
        name        => 'option name (bundling), bundling=0',
        args        => {getopt_spec=>\%gospec, bundling=>0},
        comp_line0  => 'CMD -f^',
        result      => {words=>
                            [qw/-F -f/],
                        esc_mode=>'option'},
    );
    test_complete(
        name        => 'option name (bundling, stops after value expected)',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD -fS^',
        result      => [qw//],
    );
    test_complete(
        name        => 'option name (bundling 2)',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD -f -1^',
        result      => {words=>
                            [qw/-1F -1S/],
                        esc_mode=>'option'},
    );
    test_complete(
        name        => 'option name with mentioned non-repeatable option (alias)',
        args        => {getopt_spec=>\%gospec},
        comp_line0  => 'CMD --flag1 -^', # means -1 is also mentioned
        result      => {words=>
                            [qw/--bool --flag2 --float --int --no-bool
                                --nobool --str -F -f -S/],
                        esc_mode=>'option'},
    );
    test_complete(
        name        => 'option name with mentioned non-repeatable option (alias 2)',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD -^  --flag1', # ditto
        result      => {words=>
                            [qw/--bool --flag2 --float --int --no-bool
                                --nobool --str -F -f -S/],
                        esc_mode=>'option'},
    );
    test_complete(
        name        => 'option name with mentioned non-repeatable option (bundling)',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD -f1 -^',
        result      => {words=>
                            [qw/--bool --float --int --no-bool
                                --nobool --str -F -S/],
                        esc_mode=>'option'},
    );
    my @foo;
    test_complete(
        name        => 'repeatable option name 1 (assigned to arrayref)',
        args        => {getopt_spec=>{'foo=s'=>\@foo, 'bar=s'=>sub{}}, },
        comp_line0  => 'CMD --foo 1 --bar 2 --^',
        result      => {words=>
                            [qw/--foo/],
                        esc_mode=>'option'},
    );
    test_complete(
        name        => 'repeatable option name 2 (desttype @)',
        args        => {getopt_spec=>{'foo=s@'=>sub{}, 'bar=s'=>sub{}}, },
        comp_line0  => 'CMD --foo 1 --bar 2 --^',
        result      => {words=>
                            [qw/--foo/],
                        esc_mode=>'option'},
    );
    test_complete(
        name        => 'repeatable option name 3 (desttype %)',
        args        => {getopt_spec=>{'foo=s%'=>sub{}, 'bar=s'=>sub{}}, },
        comp_line0  => 'CMD --foo 1 --bar 2 --^',
        result      => {words=>
                            [qw/--foo/],
                        esc_mode=>'option'},
    );
    test_complete(
        name        => 'repeatable option name 4 (incremental)',
        args        => {getopt_spec=>{'foo+'=>sub{}, 'bar=s'=>sub{}}, },
        comp_line0  => 'CMD --foo --bar 2 --^',
        result      => {words=>
                            [qw/--foo/],
                        esc_mode=>'option'},
    );
    test_complete(
        name        => 'option name + arg completion',
        args        => {getopt_spec=>\%gospec,
                        completion=>sub { [qw/-x/] }},
        comp_line0  => 'CMD ^',
        result      => [qw/--bool --flag1 --flag2 --float --int --no-bool
                           --nobool --str -1 -F -S -f -x/],
    );
    # if the user types '-', she indicates that she wants option names only
    test_complete(
        name        => 'option name',
        args        => {getopt_spec=>\%gospec,
                        completion=>sub { [qw/-x/] }},
        comp_line0  => 'CMD -^',
        result      => {words=>
                            [qw/--bool --flag1 --flag2 --float --int --no-bool
                                --nobool --str -1 -F -f -S/],
                        esc_mode=>'option'},
    );

    test_complete(
        name        => 'option value without completion routine',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD --str ^',
        default     => 1,
        result      => [qw//],
    );
    test_complete(
        name         => 'option value without completion routine (2)',
        args         => {getopt_spec=>\%gospec, },
        comp_line0   => 'CMD --str=^',
        default      => 1,
        default_word => '', # Don't use '--str=' (words expanded into --str, =, '')
        result       => [qw//],
    );

    # XXX Unless pass_through, Getopt::Long would error on this
    test_complete(
        name         => 'option value for unknown option',
        args         => {getopt_spec=>\%gospec, },
        comp_line0   => 'CMD --foo=^',
        default      => 1,
        default_word => '', # Don't use '--str=' (words expanded into --str, =, '')
        result       => [qw//],
    );

    # XXX Unless pass_through, Getopt::Long would error on this
    test_complete(
        name        => 'option value for option that does not expect value',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD --flag1=^',
        default     => 1,
        default_word => '', # Don't use '--flag1=' (words expanded into --str, =, '')
        result      => [qw//],
    );
    test_complete(
        name        => 'option value with completion routine returning array',
        args        => {getopt_spec=>\%gospec,
                        completion=>sub { [qw/aa a b c/] }},
        comp_line0  => 'CMD --str ^',
        result      => [qw/aa a b c/],
    );

    test_complete(
        name        => 'extras argument',
        args        => {getopt_spec=>\%gospec,
                        extras=>{foo=>10, bar=>20},
                        completion=>sub {
                            my %args = @_;
                            [$args{bar}, $args{foo}];
                        }},
        comp_line0  => 'CMD --str ^',
        result      => [qw/20 10/],
    );

    # XXX test option value with completion routine returning hash

    # XXX test arg with code completion returning hash
    # XXX test option name + arg with code completion returning hash

    # XXX test optional value
};

subtest 'config bundling=0' => sub {
    my %gospec = (
        'flag1'     => sub{},
        'flag2'     => sub{},
        '-flag3'    => sub{},
    );
    test_complete(
        name        => 'basics',
        args        => {getopt_spec=>\%gospec, bundling=>0},
        comp_line0  => 'CMD -^',
        result      => {
            words   => [qw/--flag1 --flag2 -flag3/],
            esc_mode => 'option',
        },
    );

};

# XXX test default fallback completion: file
# XXX test default fallback completion: env

DONE_TESTING:
done_testing;

sub test_complete {
    my (%args) = @_;

    subtest +($args{name} // $args{comp_line0}) => sub {

        # $args{comp_line0} contains comp_line with '^' indicating where
        # comp_point should be, the caret will be stripped. this is more
        # convenient than counting comp_point manually.
        my $comp_line  = $args{comp_line0};
        defined ($comp_line) or die "BUG: comp_line0 not defined";
        my $comp_point = index($comp_line, '^');
        $comp_point >= 0 or
            die "BUG: comp_line0 should contain ^ to indicate where ".
                "comp_point is";
        $comp_line =~ s/\^//;

        require Complete::Bash;
        my ($words, $cword) = @{ Complete::Bash::parse_cmdline(
            $comp_line, $comp_point, {truncate_current_word=>1}) };
        ($words, $cword) = @{ Complete::Bash::join_wordbreak_words(
            $words, $cword) };
        shift @$words; $cword--; # strip command name

        require Complete::Getopt::Long;

        my $res = Complete::Getopt::Long::complete_cli_arg(
            words=>$words, cword=>$cword,
            %{$args{args}},
        );

        my $exp;
        if ($args{default}) {
            # Generate expected defaults if necessary
            # Presumably, the results of this routine have been tested
            #   in the Complete::{Env|File|Util} packages, for which
            #   this routine is a wrapper.
            my $default = Complete::Getopt::Long::_default_completion(
                word => $args{default_word} // $words->[$cword]
            ) if $args{default};

            $exp = Complete::Util::combine_answers($args{result}, $default);

            # combine_answers sorts the words
            $exp->{words} = bag(@{$exp->{words}});
        }
        else {
            $exp = $args{result};
        }

        #use DD; dd { words=>$words, cword=>$cword, %{$args{args}} };
        cmp_deeply($res, $exp, 'result') or diag explain($res);

        done_testing();
    };
}
