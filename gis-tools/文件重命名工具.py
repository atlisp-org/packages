import os
import csv
import tkinter as tk
from tkinter import ttk, filedialog, messagebox, simpledialog

class FileRenamerApp:
    def __init__(self, root):
        self.root = root
        self.root.title("文件重命名工具")
        self.root.geometry("700x500")
        
        # 创建主框架
        main_frame = ttk.Frame(root, padding=10)
        main_frame.pack(fill=tk.BOTH, expand=True)
        
        # 按钮区域
        button_frame = ttk.Frame(main_frame)
        button_frame.pack(fill=tk.X, pady=5)
        
        ttk.Button(button_frame, text="提取文件", command=self.extract_filenames).pack(side=tk.LEFT, padx=5)
        ttk.Button(button_frame, text="导入新名", command=self.import_new_names).pack(side=tk.LEFT, padx=5)
        ttk.Button(button_frame, text="执行重命名", command=self.rename_files).pack(side=tk.LEFT, padx=5)
        ttk.Button(button_frame, text="导出CSV", command=self.export_to_csv).pack(side=tk.LEFT, padx=5)
        
        # 表格区域
        columns = ("原始名称", "新名称", "路径")
        self.tree = ttk.Treeview(main_frame, columns=columns, show="headings", height=15)
        
        # 设置列标题
        for col in columns:
            self.tree.heading(col, text=col)
            self.tree.column(col, width=150, anchor=tk.W)
        
        # 添加滚动条
        scrollbar = ttk.Scrollbar(main_frame, orient=tk.VERTICAL, command=self.tree.yview)
        self.tree.configure(yscroll=scrollbar.set)
        
        self.tree.pack(side=tk.LEFT, fill=tk.BOTH, expand=True, pady=5)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        
        # 状态栏
        self.status_var = tk.StringVar(value="就绪")
        ttk.Label(root, textvariable=self.status_var, relief=tk.SUNKEN, anchor=tk.W).pack(side=tk.BOTTOM, fill=tk.X)
        
        # 绑定编辑事件
        self.tree.bind("<Double-1>", self.on_double_click)
    
    def extract_filenames(self):
        """选择文件夹并提取文件名"""
        folder = filedialog.askdirectory(title="选择文件夹")
        if not folder: return
        
        # 清空现有数据
        for item in self.tree.get_children():
            self.tree.delete(item)
        
        count = 0
        for root_dir, _, files in os.walk(folder):
            for file in files:
                name, ext = os.path.splitext(file)
                self.tree.insert("", "end", values=(name, "", os.path.join(root_dir, file)))
                count += 1
        
        self.status_var.set(f"找到 {count} 个文件")
    
    def import_new_names(self):
        """从CSV文件导入新名称"""
        if not self.tree.get_children():
            messagebox.showinfo("提示", "请先提取文件")
            return
        
        file_path = filedialog.askopenfilename(title="选择CSV文件", filetypes=[("CSV文件", "*.csv")])
        if not file_path: return
        
        try:
            name_map = {}
            with open(file_path, 'r', encoding='ANSI') as f:
                reader = csv.reader(f)
                for row in reader:
                    if len(row) >= 2 and row[0] and row[1]:
                        name_map[row[0].strip()] = row[1].strip()
            
            update_count = 0
            for item in self.tree.get_children():
                values = list(self.tree.item(item, "values"))
                if values[0] in name_map:
                    values[1] = name_map[values[0]]
                    self.tree.item(item, values=values)
                    update_count += 1
            
            self.status_var.set(f"更新了 {update_count} 个新名称")
        except Exception as e:
            messagebox.showerror("错误", f"导入失败: {str(e)}")
    
    def export_to_csv(self):
        """导出当前数据到CSV"""
        if not self.tree.get_children():
            messagebox.showinfo("提示", "没有可导出的数据")
            return
        
        file_path = filedialog.asksaveasfilename(
            title="保存CSV文件",
            defaultextension=".csv",
            filetypes=[("CSV文件", "*.csv")]
        )
        if not file_path: return
        
        try:
            with open(file_path, 'w', encoding='ANSI', newline='') as f:
                writer = csv.writer(f)
                for item in self.tree.get_children():
                    values = self.tree.item(item, "values")
                    writer.writerow([values[0], values[1]])  # 只导出原始名称和新名称
            self.status_var.set(f"已导出到: {os.path.basename(file_path)}")
        except Exception as e:
            messagebox.showerror("错误", f"导出失败: {str(e)}")
    
    def rename_files(self):
        """批量重命名文件"""
        if not self.tree.get_children():
            messagebox.showinfo("提示", "没有可重命名的文件")
            return
        
        success = errors = 0
        error_msgs = []
        illegal_chars = r'\/:*?"<>|'
        
        for item in self.tree.get_children():
            orig, new, path = self.tree.item(item, "values")
            if not new: continue
            
            # 检查非法字符
            if any(char in new for char in illegal_chars):
                errors += 1
                error_msgs.append(f"非法字符: {new}")
                continue
            
            # 构建新路径
            dir_name = os.path.dirname(path)
            ext = os.path.splitext(path)[1]
            new_path = os.path.join(dir_name, new + ext)
            
            try:
                os.rename(path, new_path)
                self.tree.item(item, values=(orig, new, new_path))
                success += 1
            except Exception as e:
                errors += 1
                error_msgs.append(f"重命名失败: {os.path.basename(path)} -> {str(e)}")
        
        # 显示结果
        self.status_var.set(f"完成: {success} 成功, {errors} 失败")
        if error_msgs:
            msg = "\n".join(error_msgs[:5])  # 最多显示5条错误
            if errors > 5: msg += f"\n...共 {errors} 个错误"
            messagebox.showwarning("部分失败", msg)
    
    def on_double_click(self, event):
        """双击编辑新名称"""
        item = self.tree.identify_row(event.y)
        col = self.tree.identify_column(event.x)
        
        if not item or col != "#2": return  # 只编辑第二列
        
        # 获取当前值
        values = list(self.tree.item(item, "values"))
        new_name = simpledialog.askstring("编辑名称", "输入新名称:", initialvalue=values[1])
        
        if new_name is not None:  # 用户点击了确定
            values[1] = new_name
            self.tree.item(item, values=values)

if __name__ == "__main__":
    root = tk.Tk()
    app = FileRenamerApp(root)
    root.mainloop()
