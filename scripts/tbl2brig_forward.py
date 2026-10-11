#!/usr/bin/env python3
import sys, re

tbl_path = sys.argv[1]

def clean_label(s: str) -> str:
    s = s.strip()
    s = re.sub(r"\s+", "", s)
    return s

def pick_label(attrs):
    for k in ["gene", "Name"]:
        v = attrs.get(k)
        if v and v.strip():
            return clean_label(v)

    product = attrs.get("product", "")
    if product and "hypothetical" in product.lower():
        return clean_label(attrs.get("locus_tag", "hypothetical"))

    # 尝试从 product 中提取 IS 名称（便于你在 BRIG 上看转座子/IS）
    m = re.search(r"\bIS[A-Za-z0-9]+(?:[A-Za-z0-9]+)?\b", product)
    if m:
        return clean_label(m.group(0))

    # hin_1 / hin_2
    m2 = re.search(r"\bhin[_-]?\d+\b", product, re.IGNORECASE)
    if m2:
        return clean_label(m2.group(0))

    return clean_label(product)[:40] if product else clean_label(attrs.get("locus_tag", "CDS"))

with open(tbl_path, "r", encoding="utf-8") as f:
    lines = f.readlines()

features = []
i = 0
while i < len(lines):
    line = lines[i].rstrip("\n")
    m = re.match(r"^\s*(\d+)\s+(\d+)\s+CDS\s*$", line)
    if not m:
        i += 1
        continue

    s = int(m.group(1))
    e = int(m.group(2))

    attrs = {}
    i += 1
    while i < len(lines):
        l2 = lines[i].rstrip("\n")
        if re.match(r"^\s*\d+\s+\d+\s+CDS\s*$", l2):
            break
        if l2.strip() == "":
            i += 1
            continue
        parts = l2.strip().split(None, 1)
        if len(parts) == 2:
            key, val = parts
            attrs[key] = val
        i += 1

    # ---- 强制正向坐标 ----
    start_fwd = min(s, e)
    end_fwd = max(s, e)
    label = pick_label(attrs)

    features.append((start_fwd, end_fwd, label))

print("#start\tend\tlabel")
for start, end, label in sorted(features, key=lambda x: (x[0], x[1])):
    print(f"{start}\t{end}\t{label}")
