# merge_script.py
import glob

def merge_py_files(output_file="merged.py", exclude_files=["merge_script.py"]):
    with open(output_file, "w", encoding="utf-8") as outfile:
        for py_file in glob.glob("*.py"):
            if py_file in exclude_files:
                continue  # 跳过自己
            outfile.write(f"\n# ===== {py_file} =====\n\n")
            with open(py_file, "r", encoding="utf-8") as infile:
                outfile.write(infile.read())

if __name__ == "__main__":
    merge_py_files()
    print("合并完成！生成 merged.py")
