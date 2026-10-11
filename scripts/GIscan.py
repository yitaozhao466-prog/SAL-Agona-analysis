#! /usr/bin/python3
import numpy as np, os, subprocess, tempfile, re
import click, pandas as pd, gzip
import tdiamondx


@click.command()
@click.option('-p', '--out', help='out file. default: based on prefix of the inputs', default=None)
@click.option('-i', '--ani', help='minimum ANI for clustering. default: 60', type=float, default=60)
@click.option('-c', '--afr', help='minimum proportion of reference in alignments. default: 50', type=float, default=50.)
@click.option('-C', '--afq', help='minimum proportion of query contig in alignments. default: 50', type=float, default=50.)
@click.option('-f', '--frag_ref', help='minimum proportion of reference in alignments as a fragment of GI. default: 10', type=float, default=10.)
@click.option('-g', '--qry_gap', help='maximum gap size between matches in query sequence. default: 20000', type=int, default=20000)
@click.option('-G', '--ref_gap', help='maximum gap size between matches in references. default: 500', type=int, default=500)
@click.option('-d', '--db', help = 'default: /titan/softwares/GIscan/SPIs.fasta', default='/titan/softwares/GIscan/SPIs.fasta')
@click.argument('query')
def main(query, out, db, ani, afr, afq, frag_ref, qry_gap, ref_gap) :
    if not out :
        out = os.path.basename(query).rsplit('.', 1)[0] + '.GIs'
    
    with tempfile.TemporaryDirectory(dir='.') as dname :
        if query.lower().endswith('.gz') :
            if query.lower().find('fastq') > 0 :
                with open(os.path.join(dname, 'query'), 'wt') as fout, gzip.open(query, 'rt') as fin :
                    for i, line in enumerate(fin) :
                        if i % 4 == 0 :
                            fout.write('>' + line[1:])
                        elif i % 4 == 1 :
                            fout.write(line)

            else :
                with open(os.path.join(dname, 'query'), 'wt') as fout, gzip.open(query, 'rt') as fin :
                    fout.write(fin.read())
            query = os.path.join(dname, 'query')
        else :
            if query.lower().find('fastq') > 0 :
                with open(os.path.join(dname, 'query'), 'wt') as fout, open(query, 'rt') as fin :
                    for i, line in enumerate(fin) :
                        if i % 4 == 0 :
                            fout.write('>' + line[1:])
                        elif i % 4 == 1 :
                            fout.write(line)
                query = os.path.join(dname, 'query')

        bsn = tdiamondx.tdiamondx(query, db)
        res = parseBsn(bsn, ani, afr, afq, frag_ref, qry_gap, ref_gap)
        with open(out, 'wt') as fout :
            for spi, cont, s, e, qry_p, ref_p, iden, ends, ref in res :
                ends = ('M', 'L', 'R', 'B')[ends]
                fout.write('{0}\t{1}\t{2}\t{3}\tcover={4:.2f}\tiden={5:.2f}\tends={6}\tref={7}\n'.format(spi, cont, s, e, ref_p, iden, ends, ref))
                print('{0}\t{1}\t{2}\t{3}\tcover={4:.2f}\tiden={5:.2f}\tends={6}\tref={7}'.format(spi, cont, s, e, ref_p, iden, ends, ref))


def eachBsn(bsn, af_len=500, gap_len=500) :
    if len(bsn) == 0 :
        return 0

    region = [[bsn[0, 0], bsn[0, 6], bsn[0, 7]]]
    region_ids = [0]
    for p in bsn[1:] :
        if p[0] == region[-1][0] and p[6] <= region[-1][2] + gap_len :
            if region[-1][2] < p[7] :
                region[-1][2] = p[7]
        else :
            region.append([p[0], p[6], p[7]])
        region_ids.append(len(region)-1)
    idx = [ r[2]-r[1]+1 >= af_len for r in region ]
    bsn = bsn[np.array([ idx[r] for r in region_ids ])]
    if len(bsn) == 0 :
        return 0, []

    bsn = bsn[np.argsort(np.min(bsn[:, 8:10], 1))]
    region = [[bsn[0,8], bsn[0,9]]] if bsn[0,8] < bsn[0,9] else [[bsn[0,9], bsn[0,8]]]
    for p in bsn[1:] :
        s, e = [p[8], p[9]] if p[8] < p[9] else [p[9], p[8]]
        if s > region[-1][1]+gap_len :
            region.append([s, e])
        elif region[-1][1] < e :
            region[-1][1] = e
    tot_region = sum([ r[1] - r[0] + 1 for r in region ])
    return 100.*tot_region/bsn[0, 13], bsn


def parseBsn(bsn, ani, af_r, af_q, frag_ref, qry_gap, ref_gap) :
    data = np.array(bsn, dtype=object)
    data = data[data.T[2] >= ani]
    idx = (data.T[7] < data.T[6])
    data[idx, 6:8] = data[idx, 7:5:-1]
    data[idx, 8:10] = data[idx, 9:7:-1]
    
    qry_sizes = {d[0]:d[1] for d in data[:, [0, 12]] }
    ref_sizes = {d[0]:d[1] for d in data[:, [1, 13]] }

    data = data[np.argsort(data.T[6])]
    data = data[np.argsort(data.T[0], kind='mergesort')]
    res = []
    while data.shape[0] :
        data = data[np.argsort(data.T[1], kind='mergesort')]
        aligns = {}
        ref_cover = {}
        starts = [0] + (np.where(data[:-1, 1] != data[1:, 1])[0] + 1).tolist() + [len(data)]
        for s, e in zip(starts[:-1], starts[1:]) :
            c = data[s, 1].rsplit('.', 1)[0]
            r, dat = eachBsn(data[s:e], gap_len=ref_gap)
            if r >= af_r :
                if c not in aligns :
                    aligns[c] = []
                aligns[c].append(dat)
                if r > ref_cover.get(c, 0) :
                    ref_cover[c] = r
        if not ref_cover :
            break
        top_spi, top_cover = max(ref_cover.items(), key=lambda x:x[1])
        dat = np.concatenate(aligns[top_spi])
        dat = dat[np.argsort(dat.T[6])]
        dat = dat[np.argsort(dat.T[0], kind='mergesort')]
        
        region = [[dat[0, 0], dat[0, 6], dat[0, 7], 0., 0., 0., 0, '']]
        reg_ids = [0]
        ref_reg = { r:[] for r in np.unique(dat.T[1]) }
        ref_reg[dat[0, 1]].append([0] + sorted(dat[0, 8:10]) + [dat[0, 2]*dat[0, 3], dat[0, 3]])
        for p in dat[1:] :
            if p[0] == region[-1][0] and p[6] <= region[-1][2] + qry_gap :
                if region[-1][2] < p[7] :
                    region[-1][2] = p[7]
            else :
                region.append([p[0], p[6], p[7], 0., 0., 0., 0, '']),
            reg_id = len(region)-1
            reg_ids.append(reg_id)
            ref_reg[p[1]].append([reg_id] + sorted(p[8:10])+ [p[2]*p[3], p[3]])
        for r in region :
            r[3] = 100*float(r[2]-r[1]+1)/qry_sizes[r[0]]

        for rep, reg in ref_reg.items() :
            reg = sorted(reg)
            r0 = [reg[0]]
            for rid, rs, rx, ai, al in reg[1:] :
                if rid != r0[-1][0] or rs > ref_gap + r0[-1][2] :
                    r0.append([rid, rs, rx, ai, al])
                else :
                    if rx > r0[-1][2] :
                        r0[-1][2] = rx
                    r0[-1][3] += ai
                    r0[-1][4] += al
            r_score = [[0, 0, 0, 0] for r in region]
            for rid, rs, rx, ai, al in r0 :
                r_score[rid][0] += rx - rs + 1
                r_score[rid][1] += ai
                r_score[rid][2] += al
                if rs-1 <= ref_gap :
                    r_score[rid][3] |= 1
                if ref_sizes[rep]-rx <= ref_gap :
                    r_score[rid][3] |= 2
            for rid, rs in enumerate(r_score) :
                if (region[rid][4], region[rid][6]) < (100.*rs[0]/ref_sizes[rep], rs[3]) :
                    region[rid][4:8] = [100.*rs[0]/ref_sizes[rep], rs[1]/rs[2], rs[3], rep]

        for r in region :
            fold = 1.
            if r[1]-1 <= 800 :
                fold *= 1.19
            if qry_sizes[r[0]] - r[2] <= 800 :
                fold *= 1.19
            if r[6] & 1 > 0 :
                fold *= 1.19
            if r[6] & 2 > 0 :
                fold *= 1.19
            if r[4]*fold >= af_r :
                res.append([top_spi] + r)
            elif (r[3]*fold >= af_q and r[4]*fold >= frag_ref) :
                res.append([top_spi] + r[:-1] + [r[-1] + '(Fragment)'])

        data = data[([d not in set(dat.T[1]) for d in data.T[1]], )]
        data = data[np.argsort(data.T[6])]
        data = data[np.argsort(data.T[0], kind='mergesort')]
        idx = np.ones(len(data), dtype=bool)
        i = 0
        for x, d in enumerate(data) :
            while i < len(region) and (d[0] > region[i][0] or (d[0] == region[i][0] and d[6] > region[i][2])) :
                i += 1
            if i >= len(region) :
                break
            ovl = min(region[i][2], d[7]) - max(region[i][1], d[6]) + 1
            if ovl >= 0.4 * (d[7] - d[6] +1) :
                idx[x] = False
        data = data[idx]
    return res



if __name__ == '__main__' :
    main()
