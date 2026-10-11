#!/bin/bash

output="merged_votes.csv"
first=1

echo 'filename,"","Gastro","Invasive","qcnum"' > "$output"

for f in *.votes.csv; do
    # 跳过不存在的文件情况
    [ -e "$f" ] || continue

    # 跳过表头（第一行），仅处理数据行
    # 假设每个文件都只有一行数据（跳过第一行）
    tail -n +2 "$f" | while read line; do
        echo "$f,$line" >> "$output"
    done
done
