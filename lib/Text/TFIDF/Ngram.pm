package Text::TFIDF::Ngram;

# ABSTRACT: Compute the TF-IDF measure for ngram phrases

our $VERSION = '0.02';

use Moo;
use strictures 2;
use namespace::clean;

use Carp;
use Encode;
use Lingua::EN::Ngram;
use Lingua::StopWords qw( getStopWords );
use List::Util qw( sum0 );

=head1 NAME

Text::TFIDF::Ngram - Compute the TF-IDF measure for ngram phrases

=head1 SYNOPSIS

  use Text::TFIDF::Ngram;
  my @files = [qw( foo.txt bar.txt )];
  my $obj   = Text::TFIDF::Ngram->new(
    files     => \@files,
    size      => 3,
    stopwords => 1,
  );
  my $x = $obj->tf( 'foo.txt', 'foo' );
  $x = $obj->idf('foo');
  $x = $obj->tfidf( 'foo.txt', 'foo' );
  $x = $obj->tfidf_by_file;
  print Dumper $x;

=head1 DESCRIPTION

The TF-IDF ("term frequency-inverse document frequency") measure is used in
information retrieval and text mining.  It is a statistical measure used to see
how important a word is in a document or collection of documents.

=head1 ATTRIBUTES

=head2 files

ArrayRef of filenames.

=cut

has files => (
    is  => 'ro',
    isa => sub { croak 'Invalid files list' unless ref $_[0] eq 'ARRAY' },
);

=head2 size

Integer ngram phrase size.

=cut

has size => (
    is      => 'ro',
    isa     => sub { croak 'Invalid integer' unless $_[0] && $_[0] =~ /^\d+$/ && $_[0] > 0 },
    default => sub { 2 },
);

=head2 stopwords

Boolean indicating that phrases with stopwords will be ignored.  Default is 1.

=cut

has stopwords => (
    is      => 'ro',
    isa     => sub { croak 'Invalid Boolean' unless defined $_[0] },
    default => sub { 1 },
);

=head2 counts

HashRef of the ngram counts of each processed file.

=cut

has counts => (
    is       => 'ro',
    init_arg => undef,
);

=head1 METHODS

=head2 new

  $obj = Text::TFIDF::Ngram->new(
    files     => \@files,
    size      => $size,
    stopwords => $stopwords,
  );

Create a new C<Text::TFIDF::Ngram> object.  If the B<files> argument is passed
in, populates the object using those files.

The B<size> is the number of words in an ngram phrase and defaults to 2.

=head2 BUILD

Load the given file phrase counts.

=cut

sub BUILD {
    my ( $self, $args ) = @_;

    return unless $args->{files};

    $args->{size} ||= 1;

    for my $file ( @{ $args->{files} } ) {
        $self->_process_ngrams( $file, $args->{size} );
    }
}

sub _process_ngrams {
    my ( $self, $file, $size ) = @_;

    my $ngram  = Lingua::EN::Ngram->new( file => $file );
    my $phrase = $ngram->ngram($size);

    my $stop = getStopWords('en');

    my $hash;

    for my $p ( keys %$phrase ) {
        next if $self->stopwords
            && grep { $stop->{$_} } split /\s/, $p;  # Exclude stopwords

        $p =~ s/[\-?;:!,."\(\)]//g; # Remove unwanted punctuation

        # XXX Why are there are blanks in the returned phrases??
        my @p = grep { $_ } split /\s/, $p;
        next unless @p == $size;

        $hash->{$p} = $phrase->{$p};
    }

	$self->{counts}{$file} = $hash;
}

=head2 tf

  $tf = $obj->tf( $file, $phrase );

Returns the frequency of the given B<phrase> in the document B<file>.  This is
not the "raw count" of the phrase, but rather the percentage of times it is
seen.

=cut

sub tf {
    my ( $self, $file, $word ) = @_;
    return 0 unless exists $self->{counts}{$file} && exists $self->{counts}{$file}{$word};
    return $self->{counts}{$file}{$word} / sum0( values %{ $self->{counts}{$file} } );
}

=head2 idf

  $idf = $obj->idf($phrase);

Returns the inverse document frequency of a B<phrase>.

=cut

sub idf {
    my ( $self, $word ) = @_;

    my $count = 0;

    for my $file ( keys %{ $self->{counts} } ) {
        $count++ if exists $self->{counts}{$file}{$word};
    }

    unless ( $count ) {
        carp "'$word' is not present in any document";
        return undef;
    }

    return - log( $count / scalar( keys %{ $self->{counts} } ) ) / log(10) + 0;
}

=head2 tfidf

  $tfidf = $obj->tfidf( $file, $phrase );

Computes the TF-IDF weight for the given document and word.  If the file is not
in the corpus used to populate the module, undef is returned.

=cut

sub tfidf {
    my ( $self, $file, $word ) = @_;
    my $idf = $self->idf($word);
    return undef unless $idf;
    return $self->tf( $file, $word ) * $idf;
}

=head2 tfidf_by_file()

  $tfidf = $obj->tfidf_by_file;

Construct a HashRef of all files with all terms and their B<tfidf> values.

=cut

sub tfidf_by_file {
    my ($self) = @_;

    my %seen;

    for my $file ( keys %{ $self->{counts} } ) {
        for my $word ( keys %{ $self->{counts}{$file} } ) {
            my $tfidf = $self->tfidf( $file, $word );

            next if $seen{$word}++ || !defined $tfidf;

            $self->{tfidf_by_file}{$file}{$word} = $tfidf;
        }
    }

    return $self->{tfidf_by_file};
}

1;
__END__

=head1 SEE ALSO

L<Moo>

L<https://en.wikipedia.org/wiki/Tf%E2%80%93idf>

=cut
