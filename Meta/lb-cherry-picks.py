#!/usr/bin/env python3

# SerenityOS's browser forked as ladybird.org in Jun 2024, and
# Ladybird is since independently developed. It sees much more
# development than SerenityOS's browser. Given the common heritage,
# many of Ladybird's commits can be cherry-picked into SerenityOS,
# to make its browser more featureful.

# Ladybird has some philosophical differences, which means not all
# of its commits can be cherry-picked:
# * Ladybird embraces using third-party libraries, while SerenityOS
#   wants to write everything from scratch.
# * Ladybird is planning to adopt Swift, which is something we don't
#   want in SerenityOS. (We'll use Jakt instead.)
# Ladybird commits that add dependencies on third-party libraries or use
# swift can't be cherry-picked.

# This script looks through all commits in Ladybird, checks which of
# those have already been cherry-picked, and prints a report which
# Ladybird PRs haven't been cherry-picked yet.
# Those can either be cherry-picked, using e.g.
# https://github.com/circl-lastname/LBSync or a similar script),
# or can be added to the never_merge_prs or never_merge_commits dicts
# below.

# When in doubt, do cherry-pick, to make later cherry-picks have fewer
# conflicts.
# Some PRs need additional downstream changes in Serenity too.

# Assumes:
# * you have a remote 'ladybird' pointing to
#   https://github.com/LadybirdBrowser/ladybird.git
# * both origin/master and ladybird/master are up-to-date
# * you have git notes set up for the ladybird remote:
#   git config --add remote.ladybird.fetch '+refs/notes/*:refs/notes/*'
#   and then `git fetch ladybird` again.
# * Cherry-picks are done using `git cherry-pick -x`, since this script
#   uses the "(cherry picked from commit" lines to discover what has
#   been cherry-picked.

# https://github.com/circl-lastname/LBSync might be useful.


import subprocess
import re

print_fully_merged_prs = False
print_dont_want_commits = False


# Get the last common commit between origin/master and ladybird/master
merge_base_commit = subprocess.check_output(
    ['git', 'merge-base', 'origin/master', 'ladybird/master']
).decode().strip()

# Get all Ladybird commits since the fork, grouped by pull request
ladybird_commits = {}
log_output = subprocess.check_output(
    ['git', 'log', f'{merge_base_commit}..ladybird/master', '--notes']
).decode()

commit_title = {}
current_pr = None
current_commit = None
line_iterator = iter(log_output.splitlines())
for line in line_iterator:
    if line.startswith('commit '):
        # The previous commit had no associated pull request.
        if current_commit and not current_pr:
            ladybird_commits.setdefault(None, []).append(current_commit)
        current_pr = None
        current_commit = line.split()[1]
        next(line_iterator)  # Author:
        next(line_iterator)  # Date:
        next(line_iterator)  # newline
        title = next(line_iterator).strip()
        commit_title[current_commit] = title
    elif line.strip().startswith('Pull-request:'):
        current_pr = line.strip().split(': ')[1]
        ladybird_commits.setdefault(current_pr, []).append(current_commit)

# Get all cherry-picked commits in SerenityOS since the fork
serenity_cherry_picks = set()
log_output = subprocess.check_output(
    ['git', 'log', f'{merge_base_commit}..origin/master', '--grep=cherry picked from commit']
).decode()

for line in log_output.splitlines():
    if '(cherry picked from commit' in line:
        cherry_picked_commit = line.split('(cherry picked from commit ')[1].split(';')[0]
        m = re.search('[^0-9a-f]', cherry_picked_commit)
        if m:
            cherry_picked_commit = cherry_picked_commit[:m.start()]
        serenity_cherry_picks.add(cherry_picked_commit)

# Some Ladybird commits landed in SerenityOS without `git cherry-pick -x`.
# This section here amends serenity_cherry_picks with these Ladybird
# commit hashes that we can't get from `git log` output.

# https://github.com/LadybirdBrowser/ladybird/pull/22 landed here:
# https://github.com/SerenityOS/serenity/pull/24479
serenity_cherry_picks.add('8f0d03514586eade16c90234c9978af6ad3e568f')
serenity_cherry_picks.add('215937729650e7b6fe97cbbf732c2466279bbc44')
serenity_cherry_picks.add('7ce35b75aa52a9c215bd1c59e0c66b51bf23f4eb')

# https://github.com/LadybirdBrowser/ladybird/pull/23 landed here:
# https://github.com/SerenityOS/serenity/pull/24500
serenity_cherry_picks.add('53d7aa53a2410a56e00cae0a6adf297e6229ed0c')
serenity_cherry_picks.add('469cbe78de33791f3c4e826e177cff73161b42d0')
serenity_cherry_picks.add('f82c7270716d99f180679057a037666fe0a7eea4')
serenity_cherry_picks.add('1aa58b6d8c07c9bcd40d974e10b1faa592981523')
serenity_cherry_picks.add('64eadab181da9c0fadc15a6c9833d47bd347a9fb')
serenity_cherry_picks.add('8f2cb6755bac103bc022b958b3d7ad8c9a43e985')
serenity_cherry_picks.add('8542a8b458a4efe56a150074db37f7a8086d5d09')
serenity_cherry_picks.add('5a40a00d9ed799e73643b4737e129ecce1b060dd')
serenity_cherry_picks.add('ab6b687d4cf4dcd35b86510e82d6eabd9dbda371')

# https://github.com/LadybirdBrowser/ladybird/pull/24 landed here:
# https://github.com/SerenityOS/serenity/pull/24491
serenity_cherry_picks.add('2fde20adf99fec7d7e6aa74687aead8f0bc1a037')
serenity_cherry_picks.add('956106c6d8a40fd1678f442dbb168e2deddb63f5')
serenity_cherry_picks.add('8315ad67595e98bacc81a0100bf9e239e9cef7cf')
serenity_cherry_picks.add('21cf2c29813e297a26e08b522ddd95d1fe7d4fed')
serenity_cherry_picks.add('d1f884533e322dcb38ac1e9de3944f298ebeaba8')
serenity_cherry_picks.add('735859bc108257cd8e40168e89d47f547cef8ab1')
serenity_cherry_picks.add('d7d60268ec558cc9bc13b443cafe858042bc2151')
serenity_cherry_picks.add('2ffda003471db31923bcc763ee5ec32e83649370')
serenity_cherry_picks.add('8e062a52a3605c8c787679304d78ea7f6f2a5021')

# https://github.com/LadybirdBrowser/ladybird/pull/33 landed here:
# https://github.com/SerenityOS/serenity/pull/24537
serenity_cherry_picks.add('a419dcbe8bd0f4ae484fa8f45a5244d86d6cae45')

# https://github.com/LadybirdBrowser/ladybird/pull/82 landed here:
# https://github.com/SerenityOS/serenity/pull/24273
serenity_cherry_picks.add('89092e98a4688ec61daf807248392e1db703a68f')
serenity_cherry_picks.add('405ce6e5f542b91b7aac81c3ed76bdd2c7ff329c')
serenity_cherry_picks.add('b5a60c0f9f50cd702a48aa836099b11b9b50732d')
serenity_cherry_picks.add('2770b7eecd5f73c857ec76e5492e18dc33a4c1f9')

# https://github.com/LadybirdBrowser/ladybird/pull/85 landed here:
# https://github.com/SerenityOS/serenity/pull/24540
serenity_cherry_picks.add('839dc01280573f8cc4db507053cccffc9948c70f')
serenity_cherry_picks.add('c6a6a7c4f0c9118dbcdc0af5ffe29071b8b6d65b')
serenity_cherry_picks.add('42105867303465326159f0bf4a4effc874326b81')
serenity_cherry_picks.add('e56e09b820bf81d39a7d31ec7d977719aa18ca4c')
serenity_cherry_picks.add('bbd82265e1b5ea6a96fe29a4f845ae0784af3913')

# https://github.com/LadybirdBrowser/ladybird/pull/99 landed here:
# https://github.com/SerenityOS/serenity/pull/24546 (and in #24540)
serenity_cherry_picks.add('1dda129fe1649607cf7db4c596fb4e5021c45765')
serenity_cherry_picks.add('fdb4e05d7f6e57dd77f840c8999faf98a1257728')
serenity_cherry_picks.add('0c683af57e29f7ecde254c09bd512da0d9d8b61b')

# https://github.com/LadybirdBrowser/ladybird/pull/301 landed here:
# https://github.com/SerenityOS/serenity/pull/24614
serenity_cherry_picks.add('450b9ffcfd37497a84cf86a37ee8808a44044dac')
serenity_cherry_picks.add('c4e935aa9711f9a07faf561f21f38042dcbc8ca7')

# https://github.com/LadybirdBrowser/ladybird/pull/772 landed here:
# https://github.com/SerenityOS/serenity/pull/24897
serenity_cherry_picks.add('18499c4eac301db7e8915284f33766ca96cdeef2')

# https://github.com/LadybirdBrowser/ladybird/pull/67/files was
# committed directly in
# https://github.com/SerenityOS/serenity/commit/6c4e0e8549be445d44a4440
serenity_cherry_picks.add('376b956214b87fbf6f5c71a8ebf6c4b1f1f10bf3')

# https://github.com/LadybirdBrowser/ladybird/pull/349 was independently
# rediscovered as part of https://github.com/SerenityOS/serenity/pull/24629
serenity_cherry_picks.add('5f06594bbd2d045ac026cbdba7c912249cdd5812')

# https://github.com/LadybirdBrowser/ladybird/pull/617/commits was a subset of
# https://github.com/SerenityOS/serenity/pull/24522/commits and
# https://github.com/SerenityOS/serenity/pull/24668/commits
serenity_cherry_picks.add('9ee334e9700d48a3471e7eab3e02d42e2c383bc0')
serenity_cherry_picks.add('873b03f6618f4718523cf370b39c75ec2f4020a8')
serenity_cherry_picks.add('9c583154b099da1b53fc9465e12458d35a76a051')
serenity_cherry_picks.add('7e9dc9c1fdfa7ecffb61c109d646080ed49c12e3')
serenity_cherry_picks.add('144e822de2639a77658537e364ae9a9e76759722')

# https://github.com/SerenityOS/serenity/pull/24549 cherry-picked
# https://github.com/LadybirdBrowser/ladybird/pull/97
# https://github.com/LadybirdBrowser/ladybird/pull/111
# https://github.com/LadybirdBrowser/ladybird/pull/41
# https://github.com/LadybirdBrowser/ladybird/pull/96
# but didn't use `git cherry-pick -x` so it has no "cherry-picked from"
# in the commit message.
serenity_cherry_picks.add('bd6ee060d20f2432fb6240593ed92bad7875b99a')
serenity_cherry_picks.add('64e27cb659eeb8d165e3be14cdc4802f4f03c26b')
serenity_cherry_picks.add('5e66512cbda66ce4a0f9aa38d52d902e9caefcf8')
serenity_cherry_picks.add('9235c3268f484636bf356e493d33e7c30612cada')
serenity_cherry_picks.add('5f66e31e56e1c5595868d9e623bcad6ff25fd382')
serenity_cherry_picks.add('7560b640f317c2eb5b46a4705ef96fa239b83647')
serenity_cherry_picks.add('c86e89665b1113f396e7ce83215f3615a959b450')

# https://github.com/LadybirdBrowser/ladybird/pull/322 landed here:
# https://github.com/SerenityOS/serenity/pull/24634
serenity_cherry_picks.add('a4eb46fcca88d16f193a2ae0d7ca890bb8447c88')

# https://github.com/LadybirdBrowser/ladybird/pull/397 landed here:
# https://github.com/SerenityOS/serenity/pull/24664
serenity_cherry_picks.add('176e3ba16a007dce11175c68167f6486a8a817c0')

# https://github.com/SerenityOS/serenity/pull/24563 cherry-picked
# https://github.com/LadybirdBrowser/ladybird/pull/134
# https://github.com/LadybirdBrowser/ladybird/pull/130
# https://github.com/LadybirdBrowser/ladybird/pull/149
# https://github.com/LadybirdBrowser/ladybird/pull/148
# https://github.com/LadybirdBrowser/ladybird/pull/147
# but somehow got the commit hashes wrong.
serenity_cherry_picks.add('e64ac8c17732bdb97689644b43d07f61d3f6cab2')
serenity_cherry_picks.add('3d7c8246072997038ff8ad41875cd3b4604bcfd2')
serenity_cherry_picks.add('a7b1a9ded759fdf27cace181128c9cd0a5528d44')
serenity_cherry_picks.add('67749300c301faec825e6f0b215f4eae89adc3b6')
serenity_cherry_picks.add('94c2b85959a7afa0168f1e1184bedb87827666ee')
serenity_cherry_picks.add('d20f1a99f87d0477ada1a112a1d55d6ed47eadf9')
serenity_cherry_picks.add('40fcb00c142457615a840a6b5c78d21b6ac7159b')
serenity_cherry_picks.add('cf7937e369c85472fef72bcacbc11c1ad172e206')
serenity_cherry_picks.add('b92bd12a8e3b372ba374bfbebd91972b3d5d7a00')
serenity_cherry_picks.add('991759f453349baf754ec3aa22934fae66377ab2')
serenity_cherry_picks.add('8217a7772878d407452858ecc915202941f3e01f')
serenity_cherry_picks.add('6e419db26c3750609fb4eabcb8157495ad8d3d16')

# https://github.com/SerenityOS/serenity/pull/24744 cherry-picked
# https://github.com/LadybirdBrowser/ladybird/pull/558
# https://github.com/LadybirdBrowser/ladybird/pull/559
# https://github.com/LadybirdBrowser/ladybird/pull/563
# https://github.com/LadybirdBrowser/ladybird/pull/582
# https://github.com/LadybirdBrowser/ladybird/pull/591
# https://github.com/LadybirdBrowser/ladybird/pull/603
# https://github.com/LadybirdBrowser/ladybird/pull/605
# https://github.com/LadybirdBrowser/ladybird/pull/594
# https://github.com/LadybirdBrowser/ladybird/pull/614
# https://github.com/LadybirdBrowser/ladybird/pull/615
# but somehow got the commit hashes wrong.
# (It also cherry-picked #635 and #646, but got those hashes right.)
serenity_cherry_picks.add('625fbc8085debf5f145ec1e9de24854fc9573834')
serenity_cherry_picks.add('d07cf26894bcaa5791ed3b7715b94e25227b2bda')
serenity_cherry_picks.add('420a626554ac595d6be68a590b8ad32b42db8ef8')
serenity_cherry_picks.add('da8633b2d0ab3b9d8f1cdad39a8ad85ca2accf03')
serenity_cherry_picks.add('524e09dda180a67e9504d12668e20ada0c278f04')
serenity_cherry_picks.add('3efb11f5d8316ee81924b123f008a21c52afcc83')
serenity_cherry_picks.add('13a8c2a79df59502da0d3f40e4844b10f3a8f041')
serenity_cherry_picks.add('4c7ef01b4479066016118306a67949480bb1ac44')
serenity_cherry_picks.add('3a0f80bbae4c291df1e1847b5ce2058221482d81')
serenity_cherry_picks.add('f0a306fe500c392a5207b3bdd88f781230c2cbfe')
serenity_cherry_picks.add('906fa0482236c050e8386f1e1969edf531e5e257')

# Define a list of pull request IDs that should never be merged
# and the reasons why they shouldn't be merged.
# We fairly likely don't want the PRs in here (but it isn't 100% set
# in stone for some of them). But if a PR is not in here, it doesn't
# necessarily mean that we want it: It might just not be triaged yet.
# If we want some commits of a PR but not all, the PR should not be
# in this list -- but some commits of the PR we don't want in
# never_merge_commits further down instead.
never_merge_prs = {
    "https://github.com/LadybirdBrowser/ladybird/pull/13": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/15": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/16": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/17": "Ladybird-specific",

    # Might want sommething like the CMakePresets.json commit in here, maybe.
    "https://github.com/LadybirdBrowser/ladybird/pull/34": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/47": "Ladybird-specific",  # More CMakePresets

    "https://github.com/LadybirdBrowser/ladybird/pull/26": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/27": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/30": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/35": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/37": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/45": "NIH violation: sqlite",
    "https://github.com/LadybirdBrowser/ladybird/pull/46": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/48": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/52": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/59": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/77": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/80": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/81": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/83": "Ladybird-specific",

    # See https://github.com/SerenityOS/serenity/pull/25060
    "https://github.com/LadybirdBrowser/ladybird/pull/86": "Ladybird-specific",

    "https://github.com/LadybirdBrowser/ladybird/pull/78": "NIH violation: woff2",
    "https://github.com/LadybirdBrowser/ladybird/pull/87": "Serenity does not use vcpkg",
    "https://github.com/LadybirdBrowser/ladybird/pull/89": "Serenity does not use vcpkg",
    "https://github.com/LadybirdBrowser/ladybird/pull/92": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/109": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/110": "NIH violation: icu",
    "https://github.com/LadybirdBrowser/ladybird/pull/115": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/117": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/125": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/126": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/145": "NIH violation: icu",
    "https://github.com/LadybirdBrowser/ladybird/pull/170": "NIH violation: icu",
    "https://github.com/LadybirdBrowser/ladybird/pull/183": "NIH violation: libjpeg-turbo",
    "https://github.com/LadybirdBrowser/ladybird/pull/184": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/190": "Serenity does not use vcpkg",
    "https://github.com/LadybirdBrowser/ladybird/pull/191": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/195": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/197": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/201": "NIH violation: icu",
    "https://github.com/LadybirdBrowser/ladybird/pull/204": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/212": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/214": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/218": "NIH violation: icu",
    "https://github.com/LadybirdBrowser/ladybird/pull/219": "NIH violation: icu",
    "https://github.com/LadybirdBrowser/ladybird/pull/223": "NIH violation: libpng+apng",
    "https://github.com/LadybirdBrowser/ladybird/pull/236": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/239": "NIH violation: icu",
    "https://github.com/LadybirdBrowser/ladybird/pull/249": "NIH violation: icu / vcpkg",
    "https://github.com/LadybirdBrowser/ladybird/pull/259": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/307": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/313": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/315": "Ladybird-specific",

    # wolfssl was merged upstream and quickly reverted again:
    "https://github.com/LadybirdBrowser/ladybird/pull/330": "NIH violation: wolfssl",
    "https://github.com/LadybirdBrowser/ladybird/pull/469": "NIH violation: wolfssl",

    "https://github.com/LadybirdBrowser/ladybird/pull/348": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/374": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/375": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/382": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/393": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/402": "No Ladybird/Android in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/409": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/410": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/427": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/446": "vcpkg (vulkan)",
    "https://github.com/LadybirdBrowser/ladybird/pull/454": "No Ladybird/Android in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/455": "NIH violation: libavif",
    "https://github.com/LadybirdBrowser/ladybird/pull/463": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/478": "No Ladybird/Android in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/481": "No Ladybird/Android in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/487": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/551": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/574": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/581": "No Ladybird/Android in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/592": "No Ladybird/Android in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/597": "No Ladybird/Android in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/598": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/599": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/636": "NIH violation: libwebp",
    "https://github.com/LadybirdBrowser/ladybird/pull/641": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/642": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/644": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/674": "NIH violation: simdutf",
    "https://github.com/LadybirdBrowser/ladybird/pull/689": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/715": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/726": "swift",
    "https://github.com/LadybirdBrowser/ladybird/pull/753": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/756": "NIH violation: libjxl",

    "https://github.com/LadybirdBrowser/ladybird/pull/787": "Got reverted",
    "https://github.com/LadybirdBrowser/ladybird/pull/810": "Reverted PR787",

    # Maybe want the first commit here?
    "https://github.com/LadybirdBrowser/ladybird/pull/821": "swift",

    "https://github.com/LadybirdBrowser/ladybird/pull/845": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/860": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/866": "Still used in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/1006": "No Ladybird/Android in Serenity",
    "https://github.com/LadybirdBrowser/ladybird/pull/1050": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/1071": "NIH violation: harfbuzz",
    "https://github.com/LadybirdBrowser/ladybird/pull/1110": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/1120": "NIH violation: libpng+apng",
    "https://github.com/LadybirdBrowser/ladybird/pull/1130": "swift",
    "https://github.com/LadybirdBrowser/ladybird/pull/1138": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/1220": "swift",
    "https://github.com/LadybirdBrowser/ladybird/pull/1221": "swift",
    "https://github.com/LadybirdBrowser/ladybird/pull/1263": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/1328": "NIH violation: skia",

    # Removing HeaderCheck might be fine, not sure if that's used. But the rest is.
    "https://github.com/LadybirdBrowser/ladybird/pull/1349": "Still used in Serenity",

    "https://github.com/LadybirdBrowser/ladybird/pull/1436": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/1449": "NIH violation: skia",
    "https://github.com/LadybirdBrowser/ladybird/pull/1451": "NIH violation: curl, sqlite3, simdutf",
    "https://github.com/LadybirdBrowser/ladybird/pull/1455": "NIH violation: libjxl",
    "https://github.com/LadybirdBrowser/ladybird/pull/1477": "swift",
    "https://github.com/LadybirdBrowser/ladybird/pull/1515": "Ladybird-specific",
    "https://github.com/LadybirdBrowser/ladybird/pull/1589": "swift",
}

# Quick consistency check:
# All PRs in never_merge_prs should have none of their commits merged.
for pr in never_merge_prs.keys():
    assert pr in ladybird_commits, f'Unknown PR {pr} in never_merge_prs'
    for commit in ladybird_commits[pr]:
        assert commit not in serenity_cherry_picks, \
            f'commit {commit} of never_merge_pr {pr} merged'


# For PRs where we do want (or did already merge) some of the commits, this is
# can be use to mark other commits in those PRs that we don't want.
# Also for unwanted commits that don't belong to a PR.
never_merge_commits = {
    "6a96920dbcae99608782e6ba4ddf9d63b9c44c38": "Still used in Serenity",
    "5e059c80cd49683cce47528a459eb3b78b0a9be9": "Still used in Serenity",
    "2f23912a55715304d8546f705e09dae6c6afaa36": "Still used in Serenity",
    "a1a59ec3abe87dd537c88b53a8260fb70192920c": "Still used in Serenity",
    "81001b37ceb708fec344e91073c10e02614d0355": "NIH violation: ffmpeg",
    "084cf68dd57531f5f08fb05b67e108310275e7c3": "NIH violation: ffmpeg",
    "c4c91f02b3d80b3beb1ba08268d94195ca8f783c": "Still used in Serenity",
    "345ae189298adb7351d3ea5b27ac1323c80ee424": "Still used in Serenity",
    "d3a8f9ba9a40b208e81b13a9c78791fca74c5997": "Still used in Serenity",
    "4cdc5d2ce96f5760a4b36209257b687376436200": "NIH violation: skia",
    "330d5996ed477f348f2ae9a5fd48cf215fcc7c60": "NIH violation: skia",
    "3f97f733164742ced090669187752e9a95c33017": "NIH violation: skia",
    "bce7b24cfb86ba1b8a253d999e5bcc9d330cba6b": "NIH violation: skia",
    "d9927d128c25c740fe5d396b1a3e1d532121fdb2": "Still used in Serenity",

    # We merged one commit of https://github.com/LadybirdBrowser/ladybird/pull/42
    # but don't want the rest: Serenity still uses bitmap fonts.
    "1b2d08ee7e32d3934a6add0e6438f782900fccc4": "Still used in Serenity",
    "10335a07ee2d72a97babfa66e7374ac72ae465c5": "Still used in Serenity",
    "d3e802cbeff58ed27dd8c8537e63bf336f2a770e": "Still used in Serenity",
    "49d546fe8bfce540c3d8b901cfa14958054015f9": "Still used in Serenity",
    "fac126bce99197aa4d3f231783fbeace29537ee8": "Still used in Serenity",
    "966d4421523fdab0806d7dbbf169821d55075746": "Still used in Serenity",
    "f78ed0600af354fbf383a3ac72175ac97efaf813": "Still used in Serenity",
    "30a92911fa5f962025854577c2477243ab259617": "Still used in Serenity",
    "04a6e2f83d4341b3b359bad655b953e56afb2706": "Still used in Serenity",
    "1a2a34fa435bd0dfde318706da66c4f232b004f8": "Still used in Serenity",
    "23bb449026d4f2d565ae29e14c5db6356e45e165": "Still used in Serenity",
    "eccfdede10c8f72dbb3e16736b4606325f809cfe": "Still used in Serenity",
    "d95f5692a38bfb50bc5e911728aaa31335cb2bd7": "Still used in Serenity",
    "d86f54da30fb9f72158b500d4ac955967bd4dd4d": "Still used in Serenity",
    "1670eda0955839e217ff491ba8016d1bd3e3b6a7": "Still used in Serenity",
    "a4a3703fb41300b24fafc23f31955dd882668f55": "Still used in Serenity",
    "e81bd345610aae7b77339d9abddd2957a7b51591": "Still used in Serenity",
    "4822d1da4e4199b248f94f7c2a55940404fa29b6": "Still used in Serenity",
    "cd84d23afad6e15feef630acfa58140f57cb6db7": "Still used in Serenity",

    # The 2nd commit of https://github.com/LadybirdBrowser/ladybird/pull/1125
    # was reverted in https://github.com/LadybirdBrowser/ladybird/pull/1195.
    "6c9adf3dbc641445a03da9cd1083f89c911504be": "Got reverted",
    "660e846e945b1a73982a150bb6be928f2ed9f4da": "Reverted 6c9adf3dbc64",

    # We merged two commits of https://github.com/LadybirdBrowser/ladybird/pull/257,
    # but I think we don't want the other three since we still need that code (?)
    "666979fb9039540261de9a9ecc1dc6569933a256": "Sill used in Serenity",
    "546f740772cd328f46400d9666d8f24dede4fe23": "Sill used in Serenity",
    "ebdb92eef6e0e718f459533ed75a833e9bcb52b2": "Sill used in Serenity",

    # Commits from the first 14 PRs didn't get Pull-request: footers:
    "6d3a54e4a8d149171105d1e6378cbd2e2d1bb7fc": "Ladybird-specific (PR2)",
    "f3a0e16ae9325739c1d62c42f99fec2d875455f3": "Ladybird-specific (PR3, commit 4)",
    "3e56835611f9eddc29a332099a5c046cd89fe9f3": "Ladybird-specific (PR3, commit 5)",
    "d147ed8549deccf8d42338e1aee1c5294b0d088e": "Still used in Serenity (PR4, commit 1)",
    "1a4fbfe49563433937fdda0d9029a79b97f15b92": "Still used in Serenity (PR4, commit 2)",
    "44c8d42157eab2f7c95efa0a736acea8e953751c": "Still used in Serenity (PR4, commit 3)",
    "cc435e7a78dbd88aff69d696e1c5defed3aafc0b": "Still used in Serenity (PR5)",
    "5863ef852d344ff8f1cdb262ed582f5a77424b4f": "Still used in Serenity (PR6)",
    "72cdd1892b3ca532218ad8ba4de65e5aaea20af7": "Ladybird-specific (PR8, commit 1)",
    "9b05fb98f3c742e66b70a661952f0c601e8437c6": "Still used in Serenity (PR11, commit 1)",
    "faeff81ce90a8123c81e02cae2093669d58e7e36": "Ladybird-specific (PR11, commit 2)",
    "9dd24991a85a58416533f0edfbc36acd3b68950e": "Ladybird-specific (PR11, commit 3)",

    # Direct commits without a PR:
    "0f7df8d3e03e5d0db5b57620318990b72f74753b": "Ladybird-specific",
    "baf7a2dee7ff3756ba455d2609322bb869e79750": "Ladybird-specific",
    "c096608dd9d3a8b0916bd7158ce1bd6b28477ad5": "Ladybird-specific",
    "23b5e47b7bcb9943ecd59327d74cfd44530fb2b3": "Ladybird-specific",
    "a8d0712c28a9576be621c6a14847fbbb1df622a4": "NIH violation: harfbuzz",
    "fdfbfcab37d679e0b59a0573706490c458d3fd55": "NIH violation: harfbuzz",  # Reverts previous commit.
    "2936a9a45ef41838861d16134f6fcb74f13a9b3b": "Ladybird-specific",  # Got reverted in PR16 again.
}

for commit in never_merge_commits:
    assert commit not in serenity_cherry_picks
    # FIXME: If all commits of a PR are in never_merge_commits, diag that
    # the PR should be in never_merge_prs instead.

print("Report of Ladybird commits and their cherry-pick status in SerenityOS:")
print("=====================================================================")

num_cherry_picked = 0
num_dont_want = 0
for pr, commits in reversed(ladybird_commits.items()):
    # Arbitrarily count PRs where we merged some commits and don't want the
    # others as cherry-picked instead of don't-want.
    all_cherry_picked = all(c in serenity_cherry_picks or
                            c in never_merge_commits for c in commits)
    if all_cherry_picked:
        num_cherry_picked += 1
        if not print_fully_merged_prs:
            continue

    if pr in never_merge_prs:
        num_dont_want += 1
        if not print_dont_want_commits:
            continue

    print(f"\nPull Request: {pr}")

    for commit in reversed(commits):
        if pr in never_merge_prs or commit in never_merge_commits:
            icon = '✋'
        elif commit in serenity_cherry_picks:
            icon = '✅'
        else:
            icon = '🚫'
        print(f"  {icon} {commit} {commit_title[commit]}")


print("\n=====================================================================")
print(f"{len(ladybird_commits)} PRs, {num_cherry_picked} cherry-picked PRs, "
      f"{num_dont_want} unwanted PRs")
print(f"{len(ladybird_commits) - num_cherry_picked - num_dont_want} to go")
print("=====================================================================")