#!/usr/bin/env python3
import os
import glob
import pandas as pd

def merge_gis_files(output_file='merge.GIs.txt'):
    """
    汇总当前目录下所有 .GIs 文件
    """
    # 获取当前目录下所有 .GIs 文件
    gis_files = sorted(glob.glob('*.GIs'))
    
    if not gis_files:
        print("当前目录下没有找到 .GIs 文件！")
        return
    
    print(f"找到 {len(gis_files)} 个 .GIs 文件：")
    for f in gis_files:
        print(f"  - {f}")
    
    # 存储所有数据
    all_data = []
    
    # 逐个读取文件
    for gis_file in gis_files:
        print(f"\n正在处理: {gis_file}")
        with open(gis_file, 'r') as f:
            line_count = 0
            for line in f:
                line = line.strip()
                if line:  # 跳过空行
                    # 分割制表符
                    fields = line.split('\t')
                    # 在最前面插入文件名
                    row_data = [gis_file] + fields
                    all_data.append(row_data)
                    line_count += 1
        print(f"  读取了 {line_count} 行")
    
    # 定义列名
    column_names = ['FileName', 'SPI', 'Contig', 'Start', 'End', 'cover', 'iden', 'ends', 'ref']
    
    # 创建 DataFrame
    df = pd.DataFrame(all_data, columns=column_names)
    
    # 保存到文件
    df.to_csv(output_file, sep='\t', index=False, header=True)
    
    print(f"\n汇总完成！")
    print(f"总共处理 {len(gis_files)} 个文件")
    print(f"总共 {len(df)} 行数据")
    print(f"输出文件: {output_file}")
    
    # 显示前几行
    print("\n前 5 行预览：")
    print(df.head())
    
    # 显示统计信息
    print(f"\n统计信息：")
    print(f"唯一的 SPI 数量: {df['SPI'].nunique()}")
    print(f"唯一的 Contig 数量: {df['Contig'].nunique()}")

if __name__ == '__main__':
    merge_gis_files()
