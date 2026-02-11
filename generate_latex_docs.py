import os
import subprocess

ROOT_DIR = "/Users/tio/Documents/GitHub/UMEDCTA"
OUTPUT_DIR = os.path.join(ROOT_DIR, "Code_Documentation_LaTeX")
SKIP_DIRS = {'.git', 'node_modules', '.claude', 'Code_Documentation_LaTeX', 'files (3)', '.git-rewrite', '__pycache__', '.vscode', '.idea', '_minted'}
SKIP_EXTENSIONS = {'.zip', '.DS_Store', '.png', '.jpg', '.jpeg', '.pdf', '.tex', '.log', '.aux', '.out', '.toc', '.pyc', '.gz', '.svg', '.ico', '.mp3', '.wav'}

# Map extensions to minted languages
EXT_TO_LANG = {
    '.py': 'python',
    '.js': 'javascript',
    '.jsx': 'javascript',
    '.ts': 'typescript',
    '.tsx': 'typescript',
    '.html': 'html',
    '.css': 'css',
    '.md': 'markdown',
    '.json': 'json',
    '.pl': 'prolog',
    '.sh': 'bash',
    '.xml': 'xml',
    '.yml': 'yaml',
    '.yaml': 'yaml',
    '.c': 'c',
    '.cpp': 'cpp',
    '.h': 'cpp',
    '.java': 'java',
    '.txt': 'text'
}

def escape_latex(text):
    chars = {
        '&': r'\&',
        '%': r'\%',
        '$': r'\$',
        '#': r'\#',
        '_': r'\_',
        '{': r'\{',
        '}': r'\}',
        '~': r'\textasciitilde{}',
        '^': r'\textasciicircum{}',
        '\\': r'\textbackslash{}',
    }
    return ''.join(chars.get(c, c) for c in text)

def generate_latex_for_folder(folder_name, base_path, files_to_process):
    if not files_to_process:
        return

    tex_filename = f"{folder_name if folder_name else 'Root'}.tex"
    tex_path = os.path.join(OUTPUT_DIR, tex_filename)
    
    print(f"Generating {tex_path} with {len(files_to_process)} files...")

    with open(tex_path, 'w', encoding='utf-8') as f:
        f.write(r"""\documentclass{article}
\usepackage{fontspec}
\usepackage{minted}
\usepackage{hyperref}
\usepackage{geometry}
\usepackage{xcolor}
\usepackage{fancyhdr}

\geometry{a4paper, margin=1in}
\usemintedstyle{friendly}
\setmonofont{Menlo} [Scale=MatchLowercase]

\pagestyle{fancy}
\fancyhf{}
\lhead{Code Documentation}
\rhead{""" + escape_latex(folder_name if folder_name else "Root Directory") + r"""}
\cfoot{\thepage}

\title{Code Documentation: """ + escape_latex(folder_name if folder_name else "Root Directory") + r"""}
\author{UMEDCTA Repository}
\date{\today}

\begin{document}

\maketitle
\tableofcontents
\newpage
""")

        for file_path in sorted(files_to_process):
            rel_path = os.path.relpath(file_path, ROOT_DIR)
            ext = os.path.splitext(file_path)[1].lower()
            lang = EXT_TO_LANG.get(ext, 'text')
            
            f.write(f"\\section{{{escape_latex(rel_path)}}}\n")
            
            try:
                with open(file_path, 'r', encoding='utf-8', errors='replace') as source_file:
                    content = source_file.read()
                    # Remove null bytes and other non-printable characters that might confuse TeX
                    content = content.replace('\x00', '')
                    
                    # Basic check to avoid empty files or binary looking files
                    if not content.strip():
                        f.write("File is empty.\n")
                        continue
                        
                f.write(f"\\begin{{minted}}[breaklines, linenos, fontsize=\\small, frame=single]{{{lang}}}\n")
                f.write(content)
                # Split the end tag to avoid confusing minted when this script is documented
                end_tag = "\\end{" + "minted}"
                f.write(f"\n{end_tag}\n\\newpage\n")
            except Exception as e:
                f.write(f"Error reading file: {e}\n")

        f.write(r"\end{document}")

def main():
    if not os.path.exists(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)

    # 1. Identify top-level folders and root files
    subfolders = []
    root_files = []

    for item in os.listdir(ROOT_DIR):
        item_path = os.path.join(ROOT_DIR, item)
        if item in SKIP_DIRS or item.startswith('.'):
            continue
            
        if os.path.isdir(item_path):
            subfolders.append(item)
        elif os.path.isfile(item_path):
            ext = os.path.splitext(item)[1].lower()
            if ext not in SKIP_EXTENSIONS:
                root_files.append(item_path)

    # Process Root Files
    if root_files:
        generate_latex_for_folder("", ROOT_DIR, root_files)

    # Process Subfolders
    for folder in subfolders:
        folder_path = os.path.join(ROOT_DIR, folder)
        files_in_folder = []
        for root, dirs, files in os.walk(folder_path):
            # Modify dirs in-place to skip unwanted directories
            dirs[:] = [d for d in dirs if d not in SKIP_DIRS and not d.startswith('.')]
            
            for file in files:
                if file.startswith('.'):
                    continue
                ext = os.path.splitext(file)[1].lower()
                if ext not in SKIP_EXTENSIONS:
                    files_in_folder.append(os.path.join(root, file))
        
        if files_in_folder:
            generate_latex_for_folder(folder, folder_path, files_in_folder)

    compile_pdfs()

def compile_pdfs():
    print("\nCompiling PDFs...")
    for filename in os.listdir(OUTPUT_DIR):
        if filename.endswith(".tex"):
            tex_path = os.path.join(OUTPUT_DIR, filename)
            print(f"Compiling {filename}...")
            try:
                # Run xelatex twice to resolve TOC
                # Using -shell-escape is crucial for minted.
                # -interaction=nonstopmode prevents hanging on errors.
                cmd = ['xelatex', '-shell-escape', '-interaction=nonstopmode', filename]
                subprocess.run(cmd, cwd=OUTPUT_DIR, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                subprocess.run(cmd, cwd=OUTPUT_DIR, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                print(f"Successfully compiled {filename}")
            except subprocess.CalledProcessError:
                print(f"Error compiling {filename}. Check {filename.replace('.tex', '.log')} for details.")

if __name__ == "__main__":
    main()
