import tkinter as tk
import csv
import re
from tkinter import messagebox, filedialog

class LiningApp:
    def __init__(self, root):
        self.root = root
        self.root.title("衬砌桩号计算工具")
        self.root.geometry("800x600")
        
        # 主框架
        main_frame = tk.Frame(root, bg="#f0f0f0")
        main_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # 控制面板
        control_frame = tk.Frame(main_frame, bg="#f0f0f0")
        control_frame.pack(fill=tk.X, pady=(0, 10))
        
        # 起始桩号输入
        tk.Label(control_frame, text="起始桩号:", bg="#f0f0f0").grid(row=0, column=0, padx=(0, 5), sticky="e")
        self.start_stake = tk.Entry(control_frame, width=15)
        self.start_stake.grid(row=0, column=1, padx=(0, 10), sticky="w")
        self.start_stake.insert(0, "K120+132")
        
        # 添加行按钮
        tk.Button(control_frame, text="添加行", command=self.add_row, width=10).grid(row=0, column=2, padx=5)
        
        # 按钮
        buttons = [
            ("导入CSV", self.import_csv),
            ("计算桩号", self.calculate_stakes),
            ("导出CSV", self.export_csv),
            ("清除数据", self.clear_data)
        ]
        
        for col, (text, command) in enumerate(buttons):
            tk.Button(control_frame, text=text, command=command, width=10).grid(
                row=0, column=col+3, padx=5)
        
        # 表格区域
        table_frame = tk.LabelFrame(main_frame, text="衬砌数据", bg="#f0f0f0")
        table_frame.pack(fill=tk.BOTH, expand=True, pady=(0, 10))
        
        # 创建表格标题
        header_frame = tk.Frame(table_frame, bg="#e0e0e0")
        header_frame.pack(fill=tk.X)
        
        headers = ["衬砌类型", "起点桩号", "终点桩号", "长度(米)"]
        widths = [150, 150, 150, 100]
        
        for i, (header, width) in enumerate(zip(headers, widths)):
            tk.Label(header_frame, text=header, width=width//10, anchor=tk.CENTER, 
                    bg="#e0e0e0").grid(row=0, column=i, sticky="ew")
        
        # 创建表格内容区域
        canvas_frame = tk.Frame(table_frame, bg="white")
        canvas_frame.pack(fill=tk.BOTH, expand=True)
        
        # 创建画布和滚动条
        self.canvas = tk.Canvas(canvas_frame, bg="white", highlightthickness=0)
        scrollbar = tk.Scrollbar(canvas_frame, orient="vertical", command=self.canvas.yview)
        self.scrollable_frame = tk.Frame(self.canvas, bg="white")
        
        self.scrollable_frame.bind(
            "<Configure>",
            lambda e: self.canvas.configure(scrollregion=self.canvas.bbox("all"))
        )
        
        self.canvas.create_window((0, 0), window=self.scrollable_frame, anchor="nw")
        self.canvas.configure(yscrollcommand=scrollbar.set)
        
        # 布局
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        self.canvas.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        
        # 状态栏
        self.status_var = tk.StringVar(value="就绪")
        status_bar = tk.Label(root, textvariable=self.status_var, relief=tk.SUNKEN, anchor=tk.W, bg="#e0e0e0")
        status_bar.pack(side=tk.BOTTOM, fill=tk.X)
        
        # 右键菜单
        self.context_menu = tk.Menu(root, tearoff=0)
        self.context_menu.add_command(label="删除行", command=self.delete_row)
        
        # 初始化数据
        self.rows = []
        self.add_row()
        self.add_row()
    
    def add_row(self):
        row_frame = tk.Frame(self.scrollable_frame, bg="white")
        row_frame.pack(fill=tk.X, padx=1, pady=1)
        
        # 衬砌类型输入框
        type_entry = tk.Entry(row_frame, width=20)
        type_entry.pack(side=tk.LEFT, padx=1, fill=tk.X, expand=True)
        
        # 起点桩号（只读）
        start_var = tk.StringVar()
        start_entry = tk.Entry(row_frame, textvariable=start_var, state="readonly", 
                             fg="blue", width=18)
        start_entry.pack(side=tk.LEFT, padx=1, fill=tk.X, expand=True)
        
        # 终点桩号（只读）
        end_var = tk.StringVar()
        end_entry = tk.Entry(row_frame, textvariable=end_var, state="readonly", 
                           fg="green", width=18)
        end_entry.pack(side=tk.LEFT, padx=1, fill=tk.X, expand=True)
        
        # 长度输入框
        length_entry = tk.Entry(row_frame, width=12)
        length_entry.pack(side=tk.LEFT, padx=1, fill=tk.X, expand=True)
        
        # 绑定右键菜单
        for widget in [type_entry, start_entry, end_entry, length_entry]:
            widget.bind("<Button-3>", lambda e, row=row_frame: self.show_context_menu(e, row))
        
        # 存储行数据
        row_data = {
            "frame": row_frame,
            "type": type_entry,
            "start": start_var,
            "end": end_var,
            "length": length_entry
        }
        
        self.rows.append(row_data)
        self.status_var.set(f"已添加新行 (总行数: {len(self.rows)})")
    
    def delete_row(self):
        if hasattr(self, 'current_row'):
            row_frame = self.current_row
            # 查找要删除的行
            for i, row_data in enumerate(self.rows):
                if row_data["frame"] == row_frame:
                    # 销毁所有小部件
                    row_data["frame"].destroy()
                    # 从列表中移除
                    self.rows.pop(i)
                    self.status_var.set(f"已删除行 (剩余行数: {len(self.rows)})")
                    return
    
    def show_context_menu(self, event, row_frame):
        self.current_row = row_frame
        self.context_menu.post(event.x_root, event.y_root)
    
    def import_csv(self):
        file_path = filedialog.askopenfilename(filetypes=[("CSV文件", "*.csv")])
        if not file_path:
            return
        
        try:
            with open(file_path, "r", encoding="ansi") as file:
                reader = csv.reader(file)
                next(reader)  # 跳过标题行
                
                # 清除现有数据
                self.clear_data()
                
                # 添加新数据
                for row in reader:
                    if row:  # 确保行不为空
                        # 添加新行
                        self.add_row()
                        # 填充数据
                        last_row = self.rows[-1]
                        last_row["type"].insert(0, row[0] if len(row) > 0 else "")
                        if len(row) > 1 and row[1]:
                            last_row["start"].set(row[1])
                        if len(row) > 2 and row[2]:
                            last_row["end"].set(row[2])
                        if len(row) > 3 and row[3]:
                            last_row["length"].insert(0, row[3])
                
                self.status_var.set(f"已导入 {len(self.rows)} 行数据")
                
        except Exception as e:
            messagebox.showerror("错误", f"导入CSV文件时出错: {str(e)}")
            self.status_var.set("导入失败")
    
    def export_csv(self):
        if not self.rows:
            messagebox.showwarning("警告", "没有数据可导出")
            return
            
        file_path = filedialog.asksaveasfilename(
            defaultextension=".csv",
            filetypes=[("CSV文件", "*.csv")],
            title="导出衬砌数据",
            initialfile="衬砌数据.csv"
        )
        if not file_path:
            return
            
        try:
            with open(file_path, "w", encoding="ansi", newline='') as file:
                writer = csv.writer(file)
                # 写入标题行
                writer.writerow(["衬砌类型", "起点桩号", "终点桩号", "长度(米)"])
                
                # 写入数据
                for row in self.rows:
                    row_data = [
                        row["type"].get(),
                        row["start"].get(),
                        row["end"].get(),
                        row["length"].get()
                    ]
                    writer.writerow(row_data)
            
            self.status_var.set(f"数据已导出到: {file_path}")
            messagebox.showinfo("导出成功", f"数据已成功导出为CSV文件:\n{file_path}")
        except Exception as e:
            messagebox.showerror("导出错误", f"导出文件时出错: {str(e)}")
            self.status_var.set("导出失败")
    
    def calculate_stakes(self):
        if not self.rows:
            messagebox.showwarning("警告", "没有数据可计算")
            return
            
        # 获取起始桩号
        start_stake_str = self.start_stake.get().strip()
        if not start_stake_str:
            messagebox.showerror("错误", "请输入起始桩号")
            return
            
        try:
            # 解析起始桩号
            current_stake = self.parse_stake(start_stake_str)
            
            # 遍历所有行
            for i, row in enumerate(self.rows):
                # 获取长度
                length_str = row["length"].get().strip()
                if not length_str:
                    # 如果没有输入长度，默认为0
                    length_str = "0"
                    row["length"].delete(0, tk.END)
                    row["length"].insert(0, "0")
                
                try:
                    length = float(length_str)
                    if length < 0:
                        raise ValueError("长度不能为负数")
                except ValueError:
                    messagebox.showerror("错误", f"第 {i+1} 行: 无效的长度值 '{length_str}'")
                    return
                
                # 设置起点桩号
                row["start"].set(self.format_stake(current_stake))
                
                # 计算终点桩号
                current_stake += length
                row["end"].set(self.format_stake(current_stake))
            
            # 更新起始桩号为最后一行的终点桩号
            self.start_stake.delete(0, tk.END)
            self.start_stake.insert(0, self.rows[-1]["end"].get())
            
            self.status_var.set("桩号计算完成")
            messagebox.showinfo("成功", "桩号计算完成")
                
        except ValueError as e:
            messagebox.showerror("错误", f"桩号格式错误: {str(e)}")
    
    def parse_stake(self, stake_str):
        """将桩号字符串解析为米数"""
        # 匹配桩号格式 Kxxx+xxx 或 xxx+xxx
        match = re.match(r'K?(\d+)\+(\d+)', stake_str)
        if not match:
            raise ValueError(f"无效的桩号格式: {stake_str}")
        
        km = int(match.group(1))
        m = int(match.group(2))
        return km * 1000 + m
    
    def format_stake(self, meters):
        """将米数格式化为桩号字符串"""
        km = meters // 1000
        m = meters % 1000
        return f"K{int(km)}+{int(m):03d}"
    
    def clear_data(self):
        # 清除表格数据
        for row in self.rows:
            row["frame"].destroy()
        self.rows = []
        
        # 重置起始桩号
        self.start_stake.delete(0, tk.END)
        self.start_stake.insert(0, "K120+132")
        
        # 添加两行空行
        self.add_row()
        self.add_row()
        
        self.status_var.set("数据已清除")

if __name__ == "__main__":
    root = tk.Tk()
    app = LiningApp(root)
    root.mainloop()
