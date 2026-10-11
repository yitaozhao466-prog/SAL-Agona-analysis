#!/usr/bin/env bash
set -euo pipefail

# ---- 用户可改的默认参数（一般不需要改）----
MINID=90
MINCOV=60
DB_CARD="card"
DB_RESFINDER="resfinder"
# ------------------------------------------

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 /full/path/to/input.fasta [outdir(optional)]"
  exit 1
fi

FASTA="$1"
if [[ ! -f "$FASTA" ]]; then
  echo "ERROR: FASTA not found: $FASTA" >&2
  exit 1
fi

BASENAME="$(basename "$FASTA")"
PREFIX="${BASENAME%.*}"

OUTDIR="${2:-${PREFIX}_abricate_${DB_CARD}_${MINID}id_${MINCOV}cov}"
mkdir -p "$OUTDIR"

log() { echo "[$(date '+%F %T')] $*"; }

log "Input FASTA: $FASTA"
log "Output dir : $OUTDIR"
log "MINID=$MINID  MINCOV=$MINCOV"

CARD_TSV="$OUTDIR/card.tsv"
RESFINDER_TSV="$OUTDIR/resfinder.tsv"
MERGED_ALL_TSV="$OUTDIR/merged_all.tsv"
MERGED_UNIQUE_TSV="$OUTDIR/merged_unique_by_coord.tsv"
POSITIONS_TSV="$OUTDIR/positions.tsv"

# 1) CARD
log "Running CARD..."
abricate --db "$DB_CARD" --minid "$MINID" --mincov "$MINCOV" --outfmt tsv "$FASTA" > "$CARD_TSV"

# 2) ResFinder
log "Running ResFinder..."
abricate --db "$DB_RESFINDER" --minid "$MINID" --mincov "$MINCOV" --outfmt tsv "$FASTA" > "$RESFINDER_TSV"

# 3) 给每行加 SOURCE 列并合并（保留全部命中）
log "Merging CARD + ResFinder (all hits)..."
awk -F'\t' 'BEGIN{OFS="\t"} /^#/{print $0,"SOURCE"; next} {print $0,"card"}' "$CARD_TSV" > "$OUTDIR/card.tag.tsv"
awk -F'\t' 'BEGIN{OFS="\t"} /^#/{print $0,"SOURCE"; next} {print $0,"resfinder"}' "$RESFINDER_TSV" > "$OUTDIR/resfinder.tag.tsv"

{
  head -n 1 "$OUTDIR/card.tag.tsv"
  tail -n +2 "$OUTDIR/card.tag.tsv"
  tail -n +2 "$OUTDIR/resfinder.tag.tsv"
} > "$MERGED_ALL_TSV"

# 4) 去重但汇总 SOURCE/RESISTANCE（按：GENE+START+END+STRAND）
#    abricate 列号（tsv）为：
#    1 #FILE,2 SEQUENCE,3 START,4 END,5 STRAND,6 GENE, ... ,11 %IDENTITY, ... ,15 RESISTANCE,16 SOURCE
log "Deduplicating by (GENE,START,END,STRAND) with SOURCE/RESISTANCE aggregation..."
awk -F'\t' '
BEGIN{OFS="\t"}
function join_unique(arr,  newv,   i){
  # arr is comma-separated stored in arrstr (we do it outside)
  return newv;
}
/^#/{
  # header line; add SOURCES column
  print $0; next
}
{
  gene=$6; st=$3; en=$4; strand=$5;
  key = gene OFS st OFS en OFS strand;

  src=$16;
  res=$15;

  # store base columns 1..15 (i.e., everything except SOURCE) only once
  if(!(key in base)){
    base[key]="";
    for(i=1;i<=15;i++){
      base[key]=(i==1 ? $i : base[key] OFS $i);
    }
    sources[key]="";
    reslist[key]="";
  }

  # append SOURCE uniquely
  if(sources[key]==""){
    sources[key]=src;
  } else {
    # check if already present
    if(index("," sources[key] ",", "," src ",")==0){
      sources[key]=sources[key] "," src;
    }
  }

  # append RESISTANCE uniquely (有时不同数据库同坐标可能给出不同RESISTANCE描述)
  if(reslist[key]==""){
    reslist[key]=res;
  } else {
    if(index("," reslist[key] ",", "," res ",")==0){
      reslist[key]=reslist[key] "," res;
    }
  }
}
END{
  # 输出：base(1..15) + SOURCES替换原来的SOURCE列位置
  # 这里 base 里原本第15列是 RESISTANCE；我们用 aggregated reslist 覆盖它
  # 为了简单：重新拼接时把 RESISTANCE 换成 reslist[key]，并把 SOURCES放到最后。
  # base[key] 结构是：col1..col14, col15(原RESISTANCE)
  for(key in base){
    split(base[key], f, "\t");
    # f[15] is RESISTANCE; overwrite:
    f[15]=reslist[key];
    print f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],f[9],f[10],f[11],f[12],f[13],f[14],f[15],sources[key];
  }
}' "$MERGED_ALL_TSV" \
| awk -F'\t' 'BEGIN{OFS="\t"}
NR==1{print $0; next}
{print}' > "$MERGED_UNIQUE_TSV"

# 5) 坐标汇总（你关心的：GENE/START/END/STRAND/%IDENTITY/RESISTANCE/SOURCE(S)）
log "Exporting positions summary..."
# columns: 1 #FILE,2 SEQUENCE,3 START,4 END,5 STRAND,6 GENE,11 %IDENTITY,15 RESISTANCE,16 SOURCE
awk -F'\t' 'BEGIN{OFS="\t"}
/^#/{
  print "SEQUENCE","START","END","STRAND","GENE","%IDENTITY","RESISTANCE","SOURCE(S)";
  next
}
{
  print $2,$3,$4,$5,$6,$11,$15,$16
}' "$MERGED_UNIQUE_TSV" > "$POSITIONS_TSV"

log "DONE."
log "Outputs:"
log " - $CARD_TSV"
log " - $RESFINDER_TSV"
log " - $MERGED_ALL_TSV"
log " - $MERGED_UNIQUE_TSV"
log " - $POSITIONS_TSV"
