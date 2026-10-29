#!/usr/bin/env bash
# naming-gate — a Plexus backend is a *service*, never a *hub* (owner,
# 2026-10-29: "we don't call them hubs anymore. we call them services";
# ~/AGENTS.md "Naming"). This gate fails on any remnant of *hub* in the
# repository: the word (`hub`, `hubs`, `HUB`) and the syllable inside an
# identifier (`DynamicHub`, `build_hub`, `hub-codegen`, `HUB_PORT`).
#
#   scripts/naming-gate.sh          check every file git tracks or would track
#   scripts/naming-gate.sh -c       print only the count of offending lines
#
# Exit 0: none. Exit 1: each offending line printed as path:line:text.
#
# What may stay is a foreign name we do not own, and only that:
#   - GitHub / github / GITHUB_* — the forge's own name, everywhere.
#   - the per-file entries in ALLOW below, each with its reason.
#   - a line marked naming-alias: an old public name kept as a deprecated
#     binding until NAMING-18 removes it (NAMING-1's alias rule).
# Taken from plexus-starter (NAMING-4); NAMING-9 copied it here.
# This file itself is not scanned: it has to spell the word to find it.
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
self="scripts/naming-gate.sh"
count_only=0
[ "${1:-}" = "-c" ] && count_only=1

# path-regex <TAB> line-regex (Perl, case-sensitive) <TAB> why.
# A line matching both has the line-regex's match taken out before the
# check, so anything else on that line is still caught.
ALLOW=$(cat <<'ALLOWED'
# The IR golden of SYNAPSE-CC-8 is byte-identical by contract (SYNAPSE-CC-10's
# generators read it). One description in it is plexus-core's doc comment on
# PlexusStreamItem, as menger served it on 2026-10-29; it changes when
# plexus-core renames the comment (NAMING-5) and the golden is captured again.
^test/fixtures/ir-coverage/menger-2026-10-29\.ir-2\.1\.json$	\(DynamicHub routing layer\)	plexus-core's doc comment, served by menger, in a byte-identical golden — removed by NAMING-5 + a recapture (NAMING-18)
ALLOWED
)

cd "$root"
git ls-files -z --cached --others --exclude-standard \
  | ALLOW="$ALLOW" SELF="$self" COUNT_ONLY="$count_only" perl -0 -ne '
    BEGIN {
      @allow = map { [split /\t/] } grep { /\S/ && !/^\s*#/ } split /\n/, $ENV{ALLOW};
      $n = 0;
    }
    chomp; my $f = $_;
    next if $f eq $ENV{SELF} || ! -f $f || -l $f;
    open my $fh, "<", $f or next;
    binmode $fh;
    my $head; read $fh, $head, 8000; next if defined $head && $head =~ /\0/;   # binary
    seek $fh, 0, 0;
    local $/ = "\n";
    while (my $l = <$fh>) {
      next unless $l =~ /hub/i;
      next if $l =~ /naming-alias/;            # a deprecated old name, until NAMING-18
      my $t = $l;
      $t =~ s/github//gi;                       # the forge: GitHub, github.com, GITHUB_TOKEN
      for my $a (@allow) { my ($p, $r) = @$a; $t =~ s/$r//g if $f =~ /$p/; }
      next unless $t =~ /hub/i;
      $n++;
      unless ($ENV{COUNT_ONLY}) { chomp $l; print "$f:$.:$l\n"; }
    }
    close $fh;
    END {
      if ($ENV{COUNT_ONLY}) { print "$n\n"; }
      elsif ($n) { print STDERR "naming-gate: $n line(s) still say hub — a Plexus backend is a service (~/AGENTS.md \"Naming\")\n"; }
      exit($n ? 1 : 0);
    }'
