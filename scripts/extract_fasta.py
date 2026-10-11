import os
import shutil

source_dir = 'seq'
destination_dir = 'Funcnote'

if not os.path.exists(destination_dir):
    os.makedirs(destination_dir)

for root, dirs, files in os.walk(source_dir):
    for file in files:
        if file.endswith('.fasta.gz'):
            source_file_path = os.path.join(root, file)
            destination_file_path = os.path.join(destination_dir, file)
            try:
                shutil.copy2(source_file_path, destination_file_path)
                print(f"Successfully copied file: {source_file_path} to {destination_file_path}")
            except Exception as e:
                print(f"Error copying file {source_file_path}: {e}")



