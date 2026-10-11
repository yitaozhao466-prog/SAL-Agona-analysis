import os
import glob

def merge_klety_files(input_dir, output_file):
    # 获取所有以.fasta.KleTy结尾的文件
    file_pattern = os.path.join(input_dir, "*.fasta.KleTy")
    klety_files = glob.glob(file_pattern)
    
    if not klety_files:
        print(f"在目录 {input_dir} 中未找到任何以.fasta.KleTy结尾的文件")
        return
    
    # 写入输出文件
    with open(output_file, 'w') as out_f:
        # 处理第一个文件，保留标题行并添加文件名列
        first_file = True
        for file_path in klety_files:
            # 获取文件名（不包含路径）
            file_name = os.path.basename(file_path)
            
            with open(file_path, 'r') as in_f:
                for line_num, line in enumerate(in_f):
                    line = line.strip()
                    if not line:
                        continue
                    
                    # 处理标题行（只在第一个文件保留）
                    if line_num == 0:
                        if first_file:
                            # 在标题前添加"文件名"列
                            out_f.write(f"文件名\t{line}\n")
                            first_file = False
                        continue  # 跳过其他文件的标题行
                    
                    # 普通行，添加文件名作为第一列
                    out_f.write(f"{file_name}\t{line}\n")
    
    print(f"已成功将 {len(klety_files)} 个文件汇总到 {output_file}")

if __name__ == "__main__":
    # 设置输入目录（当前目录）和输出文件
    input_directory = "."  # 当前目录
    output_filename = "merged_klety_results.txt"
    
    # 调用函数执行合并操作
    merge_klety_files(input_directory, output_filename)
