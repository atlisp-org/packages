import tkinter as tk
from tkinter import filedialog, messagebox
import os
import subprocess
import csv

class CADPlotterApp:
    def __init__(self, root):
        self.root = root
        self.root.title("CAD展点工具(CSV版)")
        
        # 文件选择
        self.file_path = tk.StringVar()
        tk.Label(root, text="CSV文件:").grid(row=0, column=0, padx=5, pady=5, sticky='e')
        tk.Entry(root, textvariable=self.file_path, width=40).grid(row=0, column=1, padx=5, pady=5)
        tk.Button(root, text="浏览...", command=self.browse_file).grid(row=0, column=2, padx=5, pady=5)
        
        # AutoCAD路径
        self.cad_path = tk.StringVar()
        self.cad_path.set(self.find_autocad())
        tk.Label(root, text="AutoCAD路径:").grid(row=1, column=0, padx=5, pady=5, sticky='e')
        tk.Entry(root, textvariable=self.cad_path, width=40).grid(row=1, column=1, padx=5, pady=5)
        tk.Button(root, text="浏览...", command=self.browse_cad).grid(row=1, column=2, padx=5, pady=5)
        
        # 操作按钮
        tk.Button(root, text="展点", command=self.plot_points, width=15).grid(row=2, column=1, padx=5, pady=5)
    
    def find_autocad(self):
        common_paths = [
            r"C:\Program Files\Autodesk\AutoCAD 2023\acad.exe",
            r"C:\Program Files\Autodesk\AutoCAD 2022\acad.exe",
            r"C:\Program Files\Autodesk\AutoCAD 2021\acad.exe",
            r"C:\Program Files\Autodesk\AutoCAD 2020\acad.exe"
        ]
        for path in common_paths:
            if os.path.exists(path):
                return path
        return ""
    
    def browse_file(self):
        filename = filedialog.askopenfilename(filetypes=[("CSV文件", "*.csv")])
        if filename:
            self.file_path.set(filename)
    
    def browse_cad(self):
        filename = filedialog.askopenfilename(filetypes=[("AutoCAD", "acad.exe")])
        if filename:
            self.cad_path.set(filename)
    
    def read_csv_data(self):
        try:
            with open(self.file_path.get(), 'r', encoding='ansi') as f:
                reader = csv.reader(f)
                data = []
                for row in reader:
                    if len(row) >= 3:
                        try:
                            point_number = row[0].strip()
                            x = float(row[1].strip())
                            y = float(row[2].strip())
                            data.append((point_number, x, y))
                        except ValueError:
                            continue
                return data
        except Exception as e:
            messagebox.showerror("错误", f"读取CSV文件失败: {str(e)}")
            return None
    
    def create_scr_file(self, commands, action_name):
        try:
            scr_filename = os.path.join(os.path.dirname(self.file_path.get()), f"{action_name}_commands.scr")
            with open(scr_filename, 'w', encoding='ansi') as f:
                f.write("\n".join(commands))
            return scr_filename
        except Exception as e:
            messagebox.showerror("错误", f"创建脚本文件失败: {str(e)}")
            return None
    
    def run_autocad_script(self, scr_file):
        if not os.path.exists(self.cad_path.get()):
            messagebox.showerror("错误", "找不到AutoCAD可执行文件")
            return False
        
        try:
            subprocess.Popen([self.cad_path.get(), "/b", scr_file])
            return True
        except Exception as e:
            messagebox.showerror("错误", f"启动AutoCAD失败: {str(e)}")
            return False
    
    def plot_points(self):
        if not self.file_path.get():
            messagebox.showwarning("警告", "请先选择CSV文件")
            return
        
        data = self.read_csv_data()
        if not data:
            return
        
        commands = []
        for point_number, x, y in data:
            commands.append(f"point {y},{x},0")  # 交换X和Y坐标
            commands.append(f"text {y},{x},0 2.5 0 {point_number}")
        
        scr_file = self.create_scr_file(commands, "plot_points")
        if scr_file and self.run_autocad_script(scr_file):
            messagebox.showinfo("成功", f"已展绘 {len(data)} 个点到AutoCAD中！")

if __name__ == "__main__":
    root = tk.Tk()
    app = CADPlotterApp(root)
    root.mainloop()
