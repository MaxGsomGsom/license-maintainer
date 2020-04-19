#!/usr/bin/perl

# Copyright 2013, 2015-2016 Nitor Creations Oy, Jonas Berlin
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

use strict;
use warnings;
use IPC::Open2;
use File::Temp qw(mktemp);
use File::Basename;

#######################
###### Functions ######
#######################

BEGIN {
    use Exporter   ();
    our ($VERSION, @ISA, @EXPORT);

    $VERSION     = 1.00;
    @ISA         = qw(Exporter);
    @EXPORT      = qw(isLackingProperLicense maintainLicense); # default export list
}

our $YEARS_CAPTURE_GROUP = 'year';
our $AUTHORS_CAPTURE_GROUP = 'author';

my %licenseTextCache; # filename => contents

sub _getLicenseText {
    my $license_text_file = $_[0];
    my $license = $licenseTextCache{$license_text_file};
    unless (defined($license)) {
		open F, '<', $license_text_file or die 'Could not read license file '.$license_text_file;
		my $sep = $/;
		undef $/;
		$license = <F>;
		$/ = $sep;
		close F;
		$licenseTextCache{$license_text_file} = $license;
    }
    return $license;
}

# transform license into a regexp that matches an existing license
# block ignoring whitespace and with "YEAR" changed to the appropriate
# regexp
# in: "# Copyright YEAR Company Ltd\n\n" out: "\s*# Copyright\s+(\d{4}(?:\s*-\s*\d{4})?)\s+Company\s+Ltd\s*"
sub regexpify_license {
    my ($license) = @_ or die;
    $license =~ s!^\s+!!mg; $license =~ s!\s+$!!mg; # remove heading & trailing whitespace on each line
    $license =~ s{^(?:\h*\v)+}{}s; $license =~ s{(?:\v\h*)+$}{}s; # remove heading & trailing empty lines
    my @parts = split(/(\s+|YEAR|AUTHORS)/, $license);
    push @parts, ''; # avoid having to handle final-iteration special cases in for loop
    my $regexp = '\s*'; # compensate for previously removed heading empty lines & whitespace
    for(my $i=0; $i<$#parts; $i+=2) {
		my $verbatim = $parts[$i]; # normal non-whitespace text that is supposed to exist as-is
		$regexp .= quotemeta($verbatim);

		my $special = $parts[$i+1]; # empty, whitespace or "YEAR" which are replaced with regexps
		if ($special eq 'YEAR') {
			# accept any sensibly formatted set of years and/or year ranges, ignoring whitespace
			my $year_or_year_range_regexp = '\d{4}(?:\s*-\s*\d{4})?';
			$special = '(?<'.$YEARS_CAPTURE_GROUP.'>'.$year_or_year_range_regexp.'(?:\s*,\s*'.$year_or_year_range_regexp.')*)';
		} elsif ($special eq 'AUTHORS') {
			# accept any sensibly formatted set of authors, ignoring whitespace
			my $author_regexp = '\w[^\r\n,]*[\w>]';
			$special = '(?<'.$AUTHORS_CAPTURE_GROUP.'>'.$author_regexp.'(?:\s*,\s*'.$author_regexp.')*)';
		} elsif(length($special)) {
			$special = '\s+'; # instead of exact sequence of whitespace characters accept any amount of whitespace
		}
		$regexp .= $special;
    }
    $regexp .= '\s*'; # compensate for previously removed trailing empty lines & whitespace
    return $regexp;
}

# in: "2005, 2007-2009, 2012" out: ( 2005=>1, 2007=>1, 2008=>1, 2009=>1, 2012=>1 )
sub unpack_ranges {
    my $years_str = $_[0];
    my @year_ranges = split(/\s*,\s*/,$years_str);
    my %years;
    for (my $i=0; $i<=$#year_ranges; ++$i) {
		my $year_range = $year_ranges[$i];
		my $low_year;
		my $high_year;
		if ($year_range =~ m!(\d{4})\s*-\s*(\d{4})!) {
			$low_year = $1;
			$high_year = $2;
		} else {
			$low_year = $year_range;
			$high_year = $year_range;
		}
		for (my $y=$low_year; $y<=$high_year; ++$y) {
			$years{$y} = 1;
		}
    }
    return %years;
}

# in: ( 2005=>1, 2007=>1, 2008=>1, 2009=>1, 2012=>1 ) out: "2005, 2007-2009, 2012"
sub pack_ranges {
    my %years = @_;
    my @years = sort (keys %years, 9999); # 9999 -> avoid having to handle final-iteration special case in for loop
    my @year_ranges = ();
    for (my $i=0; $i<$#years; ) {
		my $j;
		for ($j=1; $i+$j<$#years; ++$j) {
			last if($years[$i]+$j != $years[$i+$j]);
		}
		push @year_ranges, $j == 1 ? $years[$i] : $years[$i].'-'.($years[$i]+$j-1);
		$i += $j;
    }
    return join(", ", @year_ranges);
}

sub unpack_authors {
    return split(/\s*,\s*/, $_[0]);
}

sub pack_authors {
    return join(", ", grep { defined($_) && length($_) } @_);
}

sub _execute($$$$$$$) {
    my ($license_text_file, $source_file, $contents, $author, $add_author_only_if_no_authors_listed, $author_years, $dry_run) = @_;

    my $license = _getLicenseText($license_text_file);

	# check if file contains BOM
	my $bom = 0;
	if ($contents =~ s/^\xEF\xBB\xBF//) {
		$bom = 1;
    }

    # create regexp version of license for relaxed detection of existing license

    my $license_regexp = regexpify_license($license);

    # check for possibly existing license and remove it

    my $years_str;
    my $authors_str;
    if ($contents =~ s!^$license_regexp!!s) { # this removes the license as a side effect
		# license present, construct new $years_str based on currently mentioned years combined with provided list of years, and list of authors merged with provided author
		return 0 if($dry_run);

		my $old_years_str = $+{$YEARS_CAPTURE_GROUP};
		my $old_authors_str = $+{$AUTHORS_CAPTURE_GROUP};

		my %years = unpack_ranges($old_years_str);
		foreach my $author_year (keys %{$author_years}) {
			$years{$author_year} = 1; # add year to set if not yet there
		}
		$years_str = pack_ranges(%years);

		my @authors = unpack_authors($old_authors_str);
		my %authors = map { $_ => 1 } @authors;
		if (defined($authors{$author}) || ($#authors >= 0 && $add_author_only_if_no_authors_listed)) {
			$authors_str = $old_authors_str;
		} else {
			push @authors, $author;
			$authors_str = pack_authors(@authors);
		}
    } else {
		# full license not present - see if any single line of license is
		# present, in which case someone broke the header accidentally
		my @license_line_regexps = map { regexpify_license($_) } grep { m![a-zA-Z]! } split("\r\n", $license);
		foreach my $license_line_regexp (@license_line_regexps) {
			if ($contents =~ m!^$license_line_regexp$!m) {
				print STDERR "ERROR: License header broken in ",$source_file," - please fix manually\n";
				return 1;
			}
		}

		# no license - new list of years is just provided list of years, and list of authors is just provided author
		return 2 if($dry_run);
			$years_str = pack_ranges(%{$author_years});
			$authors_str = $author;
    }

    # format new license

    my $newlicense = $license;
    $newlicense =~ s!YEAR!$years_str!g;
    $newlicense =~ s!AUTHORS!$authors_str!g;

    # output

    return 0, $bom, $newlicense, $contents;
}

sub isLackingProperLicense($$$) {
    my ($license_text_file, $source_file, $contents) = @_;
    return _execute($license_text_file, $source_file, $contents, undef, 0, undef, 1);
}

sub maintainLicense($$$$$$) {
    my ($license_text_file, $source_file, $contents, $author, $add_author_only_if_no_authors_listed, $author_years) = @_;
    return _execute($license_text_file, $source_file, $contents, $author, $add_author_only_if_no_authors_listed, $author_years, 0);
}

sub extract_timestamp($) {
    my $gitdate = $_[0];
    if (defined($gitdate)) {
		$gitdate =~ m!(\d{9,})! and return $1;
    }
    return undef;
}

#######################
##### Main script #####
#######################

my $EMPTY_SHA = 'e69de29bb2d1d6434b8b29ae775ad8c2e48c5391';

my $INJECT_LICENSES = length(($ENV{"INJECT_LICENSES"} || "") =~ s!\s+!!r) > 0;

if ($INJECT_LICENSES) {
    print STDERR qq{NOTE: Adding/updating licenses for all maintained files, i.e.
 - adds license to files lacking an license
 - adds the author (configured in the "license.author" configuration option) to
   files lacking an author
 - adds missing years to existing licenses, for example if some user committed
   stuff without updating the license e.g. did not have the pre-commit hook
   installed

};
}

my $origLineSeparator = $/;
$/="\0";

# Fetch author string to use
my $author = `git config --get license.author`;
unless(length($author)) {
    print STDERR "ERROR: Author not configured.\n\nConfigure with:\n\n\tgit config license.author \"My Company Inc\"\n";
    exit(1);
}
$author =~ s!^\s+!!;
$author =~ s!\s+$!!;

# get list of files in stage
my @filesInStage;
open my $fileListFh, '-|', qw(git ls-files --stage -z) or die;
while(<$fileListFh>) {
    chomp;
    die $_ unless(m!^([0-7]{6}) ([0-9a-f]{40}) ([0-3])\t(.*)$!);
    push @filesInStage, { mode => $1, sha => $2, stage => $3, name => $4 };
}
close $fileListFh;

# get the license file to use for each staged file
my %licensefile;
my $tmpfile = mktemp(File::Spec->tmpdir.'/licXXXXX');
open my $checkAttrInFh, "|-", "git check-attr licensefile -z --stdin > '$tmpfile'" or die;
print $checkAttrInFh join("\0", map { $_->{name} } @filesInStage),"\0";
close $checkAttrInFh;
open my $changeAttrOutFh, "<", $tmpfile or die;
while(<$changeAttrOutFh>) {
    chomp;
    my $file = $_;
    <$changeAttrOutFh>; # attribute name
    my $license = <$changeAttrOutFh>;
    chomp $license;
    $licensefile{$file} = $license if(defined($license) && length($license) && $license ne "unset" && $license ne "unspecified");
}
close $changeAttrOutFh;
#unlink($tmpfile);

# get list of files that will be changed by this commit
my %changed;
open my $changeListFh, "-|", qw(git diff --cached --raw -z --name-only --no-renames --diff-filter=AM) or die;
while(<$changeListFh>) {
    chomp;
    $changed{$_} = 1;
}
close $changeListFh;

$/=$origLineSeparator;

# For all files, check license. For files with changes add/update the license as needed.
my @filesInCheckoutThatCouldNotBePatchedWithLicenseUpdate;
my $numFilesWithLicenseBrokenOrMissing = 0;
foreach my $staged (@filesInStage) {
    next if ($staged->{stage} != 0); # conflict exists for file, skip

    my $licenseFile = $licensefile{$staged->{name}};

    next unless(defined($licenseFile)); # no license formatting for file, skip

    my $isFileChangedByCommit = defined($changed{$staged->{name}});

    # transform file and store transformed in git
    # transformedsha=`git cat-file blob <origsha> | perl license.pl LICENSE-<format> <file> | git hash-object -w --path <filepath> --stdin`

	# file changed, add license or update license to contain current year
    if ($isFileChangedByCommit || $INJECT_LICENSES) {
		# Run "git cat-file blob <origsha>" to get the staged files' contents on stdout e.g. readable through *origFileContentFh
		local *origFileContentFh;
		open(\*origFileContentFh, '-|', 'git', 'cat-file', 'blob', $staged->{sha}) or die 'git cat-file '.$staged->{sha};

		undef $/;
		my $origFileContent = <origFileContentFh>;
		$/ = $origLineSeparator;
		close origFileContentFh;

		my $license_text_file = $licenseFile;

		my %author_years;
		if ($isFileChangedByCommit) {
			# use author timestamp of current commit author
			my $author_date = extract_timestamp($ENV{'GIT_AUTHOR_DATE'}) || time();
			my @author_date_fields = localtime($author_date);
			my $author_year = $author_date_fields[5] + 1900;
			$author_years{$author_year} = 1;
		} else {
			# use author timestamp of commit when file was last changed
			open TSTAMPS, '-|', 'git', 'log', '--format=format:%ai', $staged->{name} or die "Internal error: Unable to find any commits for $staged->{name}";
			while (<TSTAMPS>) {
				my $author_year = substr($_,0,4);
				$author_years{$author_year} = 1;
			}
			close TSTAMPS;
			scalar(keys %author_years) >= 0 or die "Internal error: Unable to find any commits for $staged->{name}";
		}
		my ($ret, $bom, @transformedContent) = maintainLicense($license_text_file, $staged->{name}, $origFileContent, $author, !$isFileChangedByCommit, \%author_years);
		if ($ret == 1) {
			print STDERR "Please correct the problems before re-attempting commit\n";
			exit(1);
		} elsif ($ret != 0) {
			die "Unhandled maintainLicense() return code $ret";
		}

		# Run "git hash-object -w --path <file> --stdin". stdin: transformed contents from *transformedFileContentFh, stdout: sha of contents through *transformedShaFh
		local *transformedShaFh;
		local *transformedFileContentFh;
		my $gitHashObjectPid = open2(\*transformedShaFh, \*transformedFileContentFh, 'git', 'hash-object', '-w', '--path', $staged->{name}, '--stdin') or die 'git hash-object';
		
		# replace line endings
        my $transformedContentCrlf = join('', @transformedContent);
		$transformedContentCrlf =~ s/\r?\n/\r\n/g;
		
		# add BOM if required
		if ($bom) {
			print transformedFileContentFh chr(65279);
		}
		
		print transformedFileContentFh $transformedContentCrlf;
		close transformedFileContentFh;

		# read sha of transformed file
		my $transformedSha = <transformedShaFh>;
		die 'read git hash-object sha' unless(defined($transformedSha));
		close transformedShaFh;
		chomp $transformedSha;

		# check exit codes of commands run
		waitpid $gitHashObjectPid, 0;
		die "git hash-object failed ".($? >> 8) if ($? >> 8);

		# sanity check
		die "Refusing to result in empty file" if($transformedSha eq $EMPTY_SHA);

		# if transformed version is different from original, update stage and checkout with transformed version
		unless ($staged->{sha} eq $transformedSha) {
			# Update stage by using the sha of the transformed contents for the filename in question

			# Run "git update-index --cacheinfo <mode> <sha> <file>" to update stage with transformed version
			system('git', 'update-index', '--cacheinfo', $staged->{mode}, $transformedSha, $staged->{name}) and die 'git update-index '.$transformedSha.' '.$staged->{name}.' returned '.($? >> 8);

			# The file in the checkout may be different than the file
			# in the index, so we cannot overwrite it directly.
			# Instead we try to apply the differencies applied by the
			# license transformer, and in case that fails, let user
			# resolve it.

			# Run "git diff <origsha> <transformedsha>". stdin: none, stdout: effective diff of license transformer operation
			local *licenseDiffFh;
			open(\*licenseDiffFh, '-|', 'git', 'diff', $staged->{sha}, $transformedSha) or die 'git diff';

			undef $/;
			my $licenseDiff = <licenseDiffFh>;
			$/ = $origLineSeparator;
			close licenseDiffFh;

			# replace line endings
			$licenseDiff =~ s/\r?\n/\r\n/g;
			
			# Run "patch --no-backup-if-mismatch <filepath>". stdin: license diff, stdout: passed on to our stdout
			local *licenseDiffCrlfFh;
			my $checkoutPatchPid = open2('>&STDOUT', \*licenseDiffCrlfFh, 'patch', '--no-backup-if-mismatch', '--binary', $staged->{name}) or die 'patch';

			print licenseDiffCrlfFh $licenseDiff;
			close licenseDiffCrlfFh;

			# check exit codes of commands run
			waitpid $checkoutPatchPid, 0;
			if ($? >> 8) {
				push @filesInCheckoutThatCouldNotBePatchedWithLicenseUpdate, $staged->{name};
			}
		}
    }
}

if ($#filesInCheckoutThatCouldNotBePatchedWithLicenseUpdate >= 0) {
    print STDERR ("\n",
		"WARNING: Unable to apply license update in the checkout to the following files:\n",
		"  ",join("\n  ", @filesInCheckoutThatCouldNotBePatchedWithLicenseUpdate),"\n",
		"\n",
		"Please apply the license updates manually for these files. Possibly see the associated .rej files for what changes are needed.\n",
		"\n");
}
if ($numFilesWithLicenseBrokenOrMissing) {
    print STDERR qq{
NOTE: Some files had license problems, kindly fix them in a separate commit by running:

  INJECT_LICENSES=1 git commit --allow-empty -m 'Add/update licenses for all maintained files' --edit

This adds/updates licenses for all maintained files, i.e.
 - adds license to files lacking an license
 - adds the author (configured in the "license.author" configuration option) to
   files lacking an author
 - adds missing years to existing licenses, for example if some user committed
   stuff without updating the license e.g. did not have the pre-commit hook
   installed

};
}