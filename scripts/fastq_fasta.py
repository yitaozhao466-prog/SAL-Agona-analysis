import gzip
import tempfile
import os
import argparse

def decompress_file(input_file):
    file_ext = os.path.splitext(input_file)[1].lower()
    if file_ext == '.gz':
        with gzip.open(input_file, 'rt') as f:
            content = f.read()
    else:
        with open(input_file, 'r') as f:
            content = f.read()

    return content

def detect_file_type(content):
    first_line = content.split('\n', 1)[0]
    if first_line.startswith('>'):
        return 'fasta'
    elif first_line.startswith('@'):
        return 'fastq'
    else:
        raise ValueError("Unrecognized file format")

def fastq_to_fasta(content):
    lines = content.strip().split('\n')
    fasta_content = ''
    for i in range(0, len(lines), 4):
        header = lines[i]
        sequence = lines[i + 1]
        fasta_content += f'{header[1:]}\n{sequence}\n'
    return fasta_content

def write_temp_file(content, file_type, work_dir):
    with tempfile.NamedTemporaryFile(mode='w', delete=False, dir=work_dir, suffix=f'.{file_type}') as temp_file:
        temp_file.write(content)

    return temp_file.name

def process_input_file(input_file, work_dir):
    content = decompress_file(input_file)
    file_type = detect_file_type(content)
    if file_type == 'fastq':
        content = fastq_to_fasta(content)
        file_type = 'fasta'
    temp_file_path = write_temp_file(content, file_type, work_dir)
    return temp_file_path

def main():
    parser = argparse.ArgumentParser(description='Process FASTQ files and convert to FASTA format.')
    parser.add_argument('input_file', type=str, help='The input file to process')
    parser.add_argument('work_dir', type=str, help='The working directory for storing temporary files')

    args = parser.parse_args()

    temp_file_path = process_input_file(args.input_file, args.work_dir)
    print(f'Temporary file saved to: {temp_file_path}')

if __name__ == '__main__':
    main()

