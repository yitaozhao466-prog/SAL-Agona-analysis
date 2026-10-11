import os
import shutil

source_root = './'
target_dir = './PT578'
os.makedirs(target_dir, exist_ok=True)

for dir_name in os.listdir(source_root):
    dir_path = os.path.join(source_root, dir_name)
    if os.path.isdir(dir_path) and dir_name.startswith('SAL'):
        for file_name in os.listdir(dir_path):
            if 'PT_578' in file_name:
                src_file = os.path.join(dir_path, file_name)
                dst_file = os.path.join(target_dir, file_name)
                shutil.copy2(src_file, dst_file)
                print(f"Copied {src_file} to {dst_file}")

print('All PT_578 files copied successfully.')
