# ("08c7e293bb",(num,"$TMPDIR/exprs/num/ea11b49459/0/result")) 2.0
# ("1337284c73",(num,"$TMPDIR/exprs/num/992d750479/0/result")) 3.5
#
# this switched to e77 with bop2fun, which is probably right:
# ("dc140a89af",(num,"$TMPDIR/exprs/multiply/08c7e293bb/1337284c73/0/result")) bad expr path! from bob?
# (PathDigest "e77c7a2b98",(num,Path "$TMPDIR/exprs/multiply/7ca850b5f6/0/result"))
#
# so what's still wrong about it? result path is correct so just a need issue?
# no, it fails to decode the path but DOES need it in shake. just never gets in the digestmap :/
#
# maybe it's because they are being added to the map but not passed on in RulesR?
# modify state each time and see if that fixes it
result = 2 * 3.5

# SOLVED :D (but not the first one ^ yet)
# it DOES get added in the long form multiply version
# but somehow that still fails with another bug? maybe just fn args aren't being needed in newrules.hs?
# that one (fa0) doesn't show up in any of these tests at all
# the issue isn't in aMath because that never runs yet; must be in the list compiler itself? or handoff between them
# ("08c7e293bb",(num,"$TMPDIR/exprs/num/ea11b49459/0/result")) 2.0
# ("1337284c73",(num,"$TMPDIR/exprs/num/992d750479/0/result")) 3.5
# ("7ca850b5f6",(num.list,"$TMPDIR/exprs/list/fa0b8b18ad/0/result")) [2.0, 3.5]
# ("e77c7a2b98",(num,"$TMPDIR/exprs/multiply/7ca850b5f6/0/result")) 7.0
# result = multiply [2, 3.5]

# hah fixed this one with bop2fun in exprPath
# missing before: list of lists [[2.0, 3.5], [7.0]]
# ("08c7e293bb",(num,"$TMPDIR/exprs/num/ea11b49459/0/result")) 2.0
# ("1337284c73",(num,"$TMPDIR/exprs/num/992d750479/0/result")) 3.5
# ("6aab43cbab",(num.list.list,"$TMPDIR/exprs/list/17d5a36deb/0/result"))
# ("7ca850b5f6",(num.list,"$TMPDIR/exprs/list/fa0b8b18ad/0/result")) [2.0, 3.5]
# ("dc140a89af",(num,"$TMPDIR/exprs/multiply/08c7e293bb/1337284c73/0/result"))
# ("eb602c25c6",(num.list,"$TMPDIR/exprs/list/757438a882/0/result"))
# result = [[2, 3.5], [2 * 3.5]]

# this one works, which implies that needing the [2, 3.5] list is the issue
# ("08c7e293bb",(num,"$TMPDIR/exprs/num/ea11b49459/0/result")) 2.0
# ("1337284c73",(num,"$TMPDIR/exprs/num/992d750479/0/result")) 3.5
# ("2e21883c4f",(num.list,"$TMPDIR/exprs/list/a0d8f28b1f/0/result")) [7.0]
# ("7ca850b5f6",(num.list,"$TMPDIR/exprs/list/fa0b8b18ad/0/result")) [2.0, 3.5]
# ("98d8110c5f",(num.list.list,"$TMPDIR/exprs/list/65afabf966/0/result")) [[2.0, 3.5], [7.0]]
# ("e77c7a2b98",(num,"$TMPDIR/exprs/multiply/7ca850b5f6/0/result")) 7.0
# result = [[2, 3.5], [multiply [2, 3.5]]]

# ok they all digested 2.0 and 3.5 the same
# only the last one got to 7.0
# they all got [2.0, 3.5], except the plain * one (is this the only issue?)
# only the last one got the list of lists (should also have been in the variant before that)
