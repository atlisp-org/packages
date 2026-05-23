import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import csv
from collections import Counter

class CSVToolApp:
    def __init__(self, root):
        self.root = root
        self.root.title("CSV桩号处理工具")
        self.root.geometry("700x550")
        
        # 创建标签页
        self.notebook = ttk.Notebook(root)
        
        # 三个功能标签页
        self.setup_tab("数据替换", self.replace_ui, self.replace_logic)
        self.setup_tab("桩号对比", self.compare_ui, self.compare_logic)
        self.setup_tab("桩号统一替换", self.unify_ui, self.unify_logic)
        
        self.notebook.pack(expand=True, fill="both", padx=10, pady=10)
    
    def setup_tab(self, name, ui_func, logic_func):
        """创建标签页的通用方法"""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text=name)
        ui_func(tab)
        logic_func(tab)
    
    # ========== 数据替换工具 ==========
    def replace_ui(self, tab):
        frame = ttk.LabelFrame(tab, text="数据替换功能")
        frame.pack(fill="both", expand=True, padx=10, pady=10)
        
        # 文件选择
        file_frame = ttk.Frame(frame)
        file_frame.pack(fill="x", padx=5, pady=5)
        ttk.Label(file_frame, text="CSV文件路径:").pack(side="left")
        self.file_path_replace = tk.StringVar()
        ttk.Entry(file_frame, textvariable=self.file_path_replace, width=50).pack(side="left", padx=5, fill="x", expand=True)
        ttk.Button(file_frame, text="浏览", command=lambda: self.browse_file(self.file_path_replace)).pack(side="left")
        
        # 操作按钮
        btn_frame = ttk.Frame(frame)
        btn_frame.pack(pady=10)
        ttk.Button(btn_frame, text="加载数据", command=self.load_replace_data).pack(side="left", padx=5)
        ttk.Button(btn_frame, text="执行替换", command=self.execute_replace).pack(side="left", padx=5)
        ttk.Button(btn_frame, text="保存结果", command=lambda: self.save_data(self.replace_data)).pack(side="left", padx=5)
        
        # 日志显示
        log_frame = ttk.LabelFrame(frame, text="操作日志")
        log_frame.pack(fill="both", expand=True, padx=5, pady=5)
        self.log_replace = tk.Text(log_frame, height=10)
        self.log_replace.pack(fill="both", expand=True, padx=5, pady=5)
        scroll = ttk.Scrollbar(log_frame, command=self.log_replace.yview)
        scroll.pack(side="right", fill="y")
        self.log_replace.config(yscrollcommand=scroll.set)
    
    def replace_logic(self, tab):
        self.replace_data = []
        self.replace_map = {}
    
    def load_replace_data(self):
        path = self.file_path_replace.get()
        if not path:
            messagebox.showerror("错误", "请先选择文件")
            return
        
        try:
            encodings = ['utf-8-sig', 'gbk', 'ANSI']
            for enc in encodings:
                try:
                    with open(path, 'r', encoding=enc) as f:
                        self.replace_data = [row for row in csv.reader(f) if row]
                    
                    self.replace_map = {}
                    for row in self.replace_data:
                        if len(row) > 7 and row[6].strip() and row[7].strip():
                            self.replace_map[row[6].strip()] = row[7].strip()
                    
                    self.log_replace.insert(tk.END, f"已加载 {len(self.replace_data)} 行数据 ({enc})\n")
                    self.log_replace.insert(tk.END, f"已建立 {len(self.replace_map)} 条替换规则\n")
                    return
                except:
                    continue
            raise Exception("无法读取文件，请检查编码")
        except Exception as e:
            self.log_replace.insert(tk.END, f"加载失败: {str(e)}\n")
    
    def execute_replace(self):
        if not self.replace_data:
            messagebox.showerror("错误", "请先加载数据")
            return
        
        replace_count = 0
        for row in self.replace_data:
            if row and row[0].strip() in self.replace_map:
                row[0] = self.replace_map[row[0].strip()]
                replace_count += 1
        
        self.log_replace.insert(tk.END, f"替换完成: 共修改 {replace_count} 处\n")
        messagebox.showinfo("完成", f"成功替换 {replace_count} 处数据")
    
    # ========== 桩号对比工具 ==========
    def compare_ui(self, tab):
        frame = ttk.LabelFrame(tab, text="桩号对比功能")
        frame.pack(fill="both", expand=True, padx=10, pady=10)
        
        # 文件选择
        file_frame = ttk.Frame(frame)
        file_frame.pack(fill="x", padx=5, pady=5)
        ttk.Label(file_frame, text="CSV文件路径:").pack(side="left")
        self.file_path_compare = tk.StringVar()
        ttk.Entry(file_frame, textvariable=self.file_path_compare, width=50).pack(side="left", padx=5, fill="x", expand=True)
        ttk.Button(file_frame, text="浏览", command=lambda: self.browse_file(self.file_path_compare)).pack(side="left")
        
        # 操作按钮
        btn_frame = ttk.Frame(frame)
        btn_frame.pack(pady=10)
        ttk.Button(btn_frame, text="加载并对比", command=self.load_compare_data).pack(side="left", padx=5)
        ttk.Button(btn_frame, text="保存结果", command=lambda: self.save_data(self.matches, headers="序号,桩号A,桩号B,差值")).pack(side="left", padx=5)
        
        # 结果表格
        table_frame = ttk.LabelFrame(frame, text="对比结果")
        table_frame.pack(fill="both", expand=True, padx=5, pady=5)
        
        cols = ("序号", "桩号A", "桩号B", "差值")
        self.tree = ttk.Treeview(table_frame, columns=cols, show="headings", height=12)
        
        # 设置列宽
        self.tree.column("序号", width=60, anchor="center")
        self.tree.column("桩号A", width=120, anchor="center")
        self.tree.column("桩号B", width=120, anchor="center")
        self.tree.column("差值", width=80, anchor="center")
        
        # 设置表头
        for col in cols:
            self.tree.heading(col, text=col)
        
        self.tree.pack(side="left", fill="both", expand=True)
        
        # 添加滚动条
        scroll = ttk.Scrollbar(table_frame, command=self.tree.yview)
        scroll.pack(side="right", fill="y")
        self.tree.config(yscrollcommand=scroll.set)
        
        # 状态栏
        self.status_compare = tk.StringVar(value="请选择包含两列桩号的CSV文件")
        ttk.Label(frame, textvariable=self.status_compare, relief="sunken").pack(fill="x", padx=5, pady=5)
    
    def compare_logic(self, tab):
        self.matches = []
    
    def load_compare_data(self):
        path = self.file_path_compare.get()
        if not path:
            messagebox.showerror("错误", "请先选择文件")
            return

        self.tree.delete(*self.tree.get_children())
        self.matches = []

        try:
            # 尝试多种编码
            encodings = ['utf-8-sig', 'gbk', 'ANSI']
            for enc in encodings:
                try:
                    with open(path, 'r', encoding=enc) as f:
                        reader = csv.reader(f)
                        next(reader)  # 跳过表头
                        list_a, list_b = [], []

                        for row in reader:
                            if row and row[0].strip():
                                try:
                                    list_a.append(float(row[0].strip()))
                                except ValueError:
                                    pass
                            if len(row) > 1 and row[1].strip():
                                try:
                                    list_b.append(float(row[1].strip()))
                                except ValueError:
                                    pass
                    break
                except:
                    continue
            else:
                raise Exception("无法读取文件，请检查编码")

            list_a.sort()
            list_b.sort()

            i = j = 0
            idx = 1
            while i < len(list_a) or j < len(list_b):
                if i < len(list_a) and j < len(list_b) and abs(list_a[i] - list_b[j]) <= 0.1:
                    self.matches.append((idx, list_a[i], list_b[j], f"{abs(list_a[i] - list_b[j]):.2f}"))
                    i += 1
                    j += 1
                elif j == len(list_b) or (i < len(list_a) and list_a[i] < list_b[j]):
                    self.matches.append((idx, list_a[i], "", ""))
                    i += 1
                else:
                    self.matches.append((idx, "", list_b[j], ""))
                    j += 1
                idx += 1

            for row in self.matches:
                self.tree.insert("", "end", values=row)

            matched = sum(1 for _,a,b,_ in self.matches if a != "" and b != "")
            self.status_compare.set(f"A列桩号: {len(list_a)}个 | B列桩号: {len(list_b)}个 | 匹配成功: {matched}个")

        except Exception as e:
            messagebox.showerror("错误", str(e))
    
    # ========== 桩号统一替换工具 ==========
    def unify_ui(self, tab):
        frame = ttk.LabelFrame(tab, text="桩号统一替换功能")
        frame.pack(fill="both", expand=True, padx=10, pady=10)
        
        # 文件选择
        file_frame = ttk.Frame(frame)
        file_frame.pack(fill="x", padx=5, pady=10)
        ttk.Label(file_frame, text="CSV文件路径:").pack(side="left")
        self.file_path_unify = tk.StringVar()
        ttk.Entry(file_frame, textvariable=self.file_path_unify, width=50).pack(side="left", padx=5, fill="x", expand=True)
        ttk.Button(file_frame, text="浏览", command=lambda: self.browse_file(self.file_path_unify)).pack(side="left")
        
        # 操作按钮
        btn_frame = ttk.Frame(frame)
        btn_frame.pack(pady=20)
        ttk.Button(btn_frame, text="开始替换并保存", command=self.unify_stakes).pack(padx=10, pady=5)
        
        # 说明信息
        info_frame = ttk.LabelFrame(frame, text="功能说明")
        info_frame.pack(fill="both", expand=True, padx=10, pady=10)
        info_text = tk.Text(info_frame, height=6, wrap="word")
        info_text.pack(fill="both", expand=True, padx=5, pady=5)
        info_text.insert("end", "功能说明：\n")
        info_text.insert("end", "1. 读取CSV文件（第一列为桩号）\n")
        info_text.insert("end", "2. 将桩号分组（相邻差值≤0.2为一组）\n")
        info_text.insert("end", "3. 每组用出现次数最多的数据作为统一值\n")
        info_text.insert("end", "4. 生成新的CSV文件（在原文件名后加'_统一.csv'）")
        info_text.config(state="disabled")
    
    def unify_logic(self, tab):
        pass
    
    def unify_stakes(self):
        path = self.file_path_unify.get()
        if not path:
            messagebox.showerror("错误", "请先选择文件")
            return

        try:
            # 读取文件
            encodings = ['utf-8-sig', 'gbk', 'ANSI']
            for enc in encodings:
                try:
                    with open(path, 'r', encoding=enc) as f:
                        reader = csv.reader(f)
                        header = next(reader)
                        rows = [row for row in reader if row]
                    break
                except:
                    continue
            else:
                raise Exception("无法读取文件，请检查编码")

            if not rows:
                messagebox.showinfo("提示", "文件内容为空")
                return

            # 提取第一列桩号并统计频率
            stake_values = []
            for row in rows:
                if row and row[0].strip() and self.is_number(row[0].strip()):
                    stake_values.append(float(row[0].strip()))
            
            if not stake_values:
                messagebox.showinfo("提示", "未找到有效的桩号数据")
                return

            # 排序并分组
            stake_values_sorted = sorted(stake_values)
            groups = []
            current_group = [stake_values_sorted[0]]
            
            for val in stake_values_sorted[1:]:
                if val - current_group[-1] <= 0.2:
                    current_group.append(val)
                else:
                    groups.append(current_group)
                    current_group = [val]
            groups.append(current_group)

            # 建立替换映射 - 使用每组中出现次数最多的值
            replace_map = {}
            for group in groups:
                # 统计组内每个值的出现次数
                counter = Counter(group)
                # 获取出现次数最多的值
                most_common = counter.most_common(1)[0][0]
                # 建立映射关系
                for val in group:
                    replace_map[val] = most_common

            # 替换数据
            for row in rows:
                if row and row[0].strip() and self.is_number(row[0].strip()):
                    old_val = float(row[0].strip())
                    row[0] = str(int(replace_map[old_val]) if replace_map[old_val] == int(replace_map[old_val]) 
                               else replace_map[old_val])

            # 保存结果
            out_path = path.rsplit(".", 1)[0] + "_统一.csv"
            with open(out_path, "w", encoding="utf-8", newline="") as f:
                writer = csv.writer(f)
                writer.writerow(header)
                writer.writerows(rows)

            messagebox.showinfo("完成", 
                f"桩号统一替换完成！\n"
                f"原始桩号数: {len(stake_values)}\n"
                f"分组数量: {len(groups)}\n"
                f"处理行数: {len(rows)}\n"
                f"保存路径: {out_path}")

        except Exception as e:
            messagebox.showerror("错误", str(e))
    
    def is_number(self, s):
        """检查字符串是否可以转换为数字"""
        try:
            float(s)
            return True
        except ValueError:
            return False
    
    # ========== 通用方法 ==========
    def browse_file(self, path_var):
        path = filedialog.askopenfilename(filetypes=[("CSV文件", "*.csv"), ("所有文件", "*.*")])
        if path:
            path_var.set(path)
    
    def save_data(self, data, headers=None):
        if not data:
            messagebox.showinfo("提示", "无数据可保存")
            return
        
        path = filedialog.asksaveasfilename(
            defaultextension=".csv",
            filetypes=[("CSV文件", "*.csv")],
            title="保存结果"
        )
        
        if path:
            try:
                with open(path, 'w', encoding='utf-8', newline='') as f:
                    if headers:
                        f.write(headers + "\n")
                    writer = csv.writer(f)
                    writer.writerows(data)
                messagebox.showinfo("保存成功", f"文件已保存至:\n{path}\n总行数: {len(data)}")
            except Exception as e:
                messagebox.showerror("保存失败", f"错误信息: {str(e)}")

if __name__ == "__main__":
    root = tk.Tk()
    app = CSVToolApp(root)
    root.mainloop()