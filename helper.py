A = [" | | | | | | | |",
" | | | | | | | |",
" | | | |x|x| | |",
" | | | |o|o| | |",
" | | | |o|o| | |",
" | | |o|o|x| | |",
" | |x|x|x|o| | |",
" |x|o|x|x|o| | |"]


b = []
A.reverse()
for r,rn in enumerate(A):
    rr = rn.split("|")
    rr.reverse()
    for c,cn in enumerate(rr):
        if not cn:
            continue
        if cn==" ":
            cn = "empty"
        b.append("{}-{}-{}".format(c, r+1, cn))


B = [1-1-empty, 2-1-empty, 3-1-o, 4-1-x, 5-1-x, 6-1-o, 7-1-x, 8-1-empty, 1-2-empty, 2-2-empty, 3-2-o, 4-2-x, 5-2-x, 6-2-x, 7-2-empty, 8-2-empty, 1-3-empty, 2-3-empty, 3-3-x, 4-3-o, 5-3-o, 6-3-empty, 7-3-empty, 8-3-empty, 1-4-empty, 2-4-empty, 3-4-o, 4-4-o, 5-4-empty, 6-4-empty, 7-4-empty, 8-4-empty, 1-5-empty, 2-5-empty, 3-5-o, 4-5-o, 5-5-empty, 6-5-empty, 7-5-empty, 8-5-empty, 1-6-empty, 2-6-empty, 3-6-x, 4-6-x, 5-6-empty, 6-6-empty, 7-6-empty, 8-6-empty, 1-7-empty, 2-7-empty, 3-7-empty, 4-7-empty, 5-7-empty, 6-7-empty, 7-7-empty, 8-7-empty, 1-8-empty, 2-8-empty, 3-8-empty, 4-8-empty, 5-8-empty, 6-8-empty, 7-8-empty, 8-8-empty]