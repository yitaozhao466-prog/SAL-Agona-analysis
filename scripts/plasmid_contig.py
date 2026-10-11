import os
import re
import gzip
import pandas as pd
from Bio import SeqIO

def extract_sample_name(filename):
    m = re.match(r'(SAL_[A-Z0-9]+_[A-Z]+)', filename)
    if m:
        return m.group(1)
    parts = filename.split('_')
    if len(parts) >= 3:
        return '_'.join(parts[:3])
    else:
        return filename.split('.')[0]

def clean_contig_name(raw):
    raw = str(raw).strip()
    raw = re.sub(r'_ID_\d+', '', raw)  # 删除 _ID_ 后缀
    return raw

def guess_fasta(klety_file):
    base = re.sub(r'\.KleTy', '', klety_file)
    options = [
        base + ".gz",
        base.replace('.result.fasta', '.result.fna') + ".gz",
        base.replace('.fasta', '.fna') + ".gz",
        base.replace('.fna', '.fasta') + ".gz",
        base + ".fasta.gz",
        base + ".fna.gz",
        base + "_genomic.fna.gz",
        base + "_genomic.fasta.gz"
    ]
    for cand in options:
        if os.path.exists(cand):
            return cand
    search_prefix = base.split(".KleTy")[0]
    for f in os.listdir('.'):
        if f.startswith(search_prefix) and f.endswith(('.fasta.gz', '.fna.gz', '.fa.gz')):
            return f
    return None

for file in os.listdir('.'):
    if file.endswith('.KleTy'):
        sample = extract_sample_name(file)
        try:
            df = pd.read_csv(file, sep='\t')
        except Exception as e:
            print(f"读取文件出错: {file} - {e}")
            continue
        if set(df.columns) == {'INPUT'}:
            continue
        df = df[df['PLASTYPE'].astype(str).str.contains('PT_', na=False)]
        if df.empty:
            continue
        outdir = sample
        os.makedirs(outdir, exist_ok=True)
        fasta_file = guess_fasta(file)
        if not fasta_file:
            print(f"❌ 找不到匹配的fasta.gz或fna.gz: {file}")
            continue
        try:
            with gzip.open(fasta_file, 'rt') as handle:
                seq_records = SeqIO.to_dict(SeqIO.parse(handle, 'fasta'))
        except Exception as e:
            print(f"❌ 无法解析fasta文件 {fasta_file}: {e}")
            continue

        for _, row in df.iterrows():
            plastypes = [pt for pt in str(row['PLASTYPE']).split(',') if pt.startswith('PT_')]
            if pd.isna(row['CONTIGS']):
                continue
            contigs = str(row['CONTIGS']).split(',')
            for pt in plastypes:
                outpath = os.path.join(outdir, f"{sample}_{pt}.fasta")
                written = 0
                with open(outpath, 'w') as out_fasta:
                    for contig in contigs:
                        contig_main = clean_contig_name(contig)
                        candidates = [contig_main, contig_main.split('_ID_')[0]]
                        found = False
                        for seq_id in seq_records:
                            if any(cand in seq_id for cand in candidates):
                                SeqIO.write(seq_records[seq_id], out_fasta, 'fasta')
                                written += 1
                                found = True
                                break
                        if not found:
                            print(f"⚠️ 找不到匹配contig: {contig_main} → {candidates} in {fasta_file}")
                if not written:
                    print(f"⚠️  {outpath} 没有写入任何contig")
print("所有完成。")
