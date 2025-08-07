import tkinter as tk
from tkinter import messagebox, filedialog
import re
from datetime import datetime, timedelta
import csv
import os

class TunnelConstructionRegister:
    def __init__(self, master):
        self.master = master
        master.title("隧道施工台账管理系统")
        master.geometry("1100x750")
        master.configure(bg="#f0f0f0")
        
        # 数据文件路径
        self.data_file = None
        self.direction = "增加"  # 全局桩号方向
        self.current_rows = 0    # 当前行数
        self.max_rows = 1000     # 最大行数限制
        
        # 主框架
        main_frame = tk.Frame(master, bg="#f0f0f0", padx=15, pady=15)
        main_frame.pack(fill=tk.BOTH, expand=True)
        
        # 标题
        title_frame = tk.Frame(main_frame, bg="#f0f0f0")
        title_frame.pack(fill=tk.X, pady=(0, 15))
        tk.Label(title_frame, text="隧道施工台账管理系统", font=("微软雅黑", 16, "bold"), 
                bg="#f0f0f0", fg="#333").pack()
        
        # 控制面板
        control_frame = tk.Frame(main_frame, bg="#e0e0e0", padx=10, pady=10, 
                               relief=tk.RIDGE, bd=1)
        control_frame.pack(fill=tk.X, pady=(0, 15))
        
        # 方向选择框
        direction_frame = tk.Frame(control_frame, bg="#e0e0e0")
        direction_frame.pack(side=tk.LEFT, padx=(0, 20))
        tk.Label(direction_frame, text="桩号方向:", bg="#e0e0e0", 
                font=("微软雅黑", 10)).pack(side=tk.LEFT)
        self.direction_var = tk.StringVar(value="增加")
        direction_menu = tk.OptionMenu(direction_frame, self.direction_var, "增加", "减小")
        direction_menu.config(width=8, font=("微软雅黑", 9), relief=tk.RAISED, 
                            bg="#f8f8f8", activebackground="#e0e0e0")
        direction_menu.pack(side=tk.LEFT)
        
        # 按钮
        buttons = [
            ("新建", self.new_file, "#5cb85c"),
            ("打开", self.open_file, "#5bc0de"),
            ("保存", self.save_file, "#5cb85c"),
            ("添加行", self.add_row, "#337ab7"),
            ("删除行", self.delete_row, "#d9534f"),
            ("计算桩号", self.recalculate_all, "#f0ad4e")
        ]
        
        btn_frame = tk.Frame(control_frame, bg="#e0e0e0")
        btn_frame.pack(side=tk.LEFT, expand=True)
        for text, command, color in buttons:
            btn = tk.Button(btn_frame, text=text, command=command, 
                           width=10, font=("微软雅黑", 9, "bold"),
                           bg=color, fg="white", activebackground=color,
                           relief=tk.RAISED, bd=1)
            btn.pack(side=tk.LEFT, padx=3)
        
        # 表格区域
        table_frame = tk.Frame(main_frame, bg="#f0f0f0")
        table_frame.pack(fill=tk.BOTH, expand=True)
        self.create_table_with_scrollbar(table_frame)
        
        # 状态栏
        self.status_var = tk.StringVar(value="就绪")
        status_bar = tk.Label(master, textvariable=self.status_var, 
                            relief=tk.SUNKEN, anchor=tk.W, bg="#e0e0e0",
                            font=("微软雅黑", 9), padx=10)
        status_bar.pack(side=tk.BOTTOM, fill=tk.X)
        
        # 初始化表格
        self.initialize_table()
    
    def create_table_with_scrollbar(self, parent):
        """创建带滚动条的表格"""
        # 外层框架
        outer_frame = tk.Frame(parent, bg="#f0f0f0")
        outer_frame.pack(fill=tk.BOTH, expand=True)
        
        # 创建Canvas和滚动条
        self.canvas = tk.Canvas(outer_frame, bg="#f0f0f0", highlightthickness=0)
        self.scrollbar = tk.Scrollbar(outer_frame, orient="vertical", 
                                    command=self.canvas.yview)
        
        # 可滚动的内层框架
        self.inner_frame = tk.Frame(self.canvas, bg="#f0f0f0")
        self.inner_frame.bind("<Configure>", lambda e: self.canvas.configure(
            scrollregion=self.canvas.bbox("all")))
        
        # 将内层框架放入Canvas
        self.canvas.create_window((0, 0), window=self.inner_frame, anchor="nw")
        self.canvas.configure(yscrollcommand=self.scrollbar.set)
        
        # 布局
        self.canvas.pack(side="left", fill="both", expand=True)
        self.scrollbar.pack(side="right", fill="y")
        
        # 绑定鼠标滚轮事件
        self.canvas.bind_all("<MouseWheel>", self._on_mousewheel)
        
        # 创建表头
        headers = ["序号", "日期", "线路类型", "部位", "起始桩号", "终止桩号", "每日完成量(m)", "备注"]
        header_font = ("微软雅黑", 10, "bold")
        for col, header in enumerate(headers):
            lbl = tk.Label(self.inner_frame, text=header, bg="#4a6ea9", fg="white",
                          relief=tk.RIDGE, width=15, height=2, font=header_font)
            lbl.grid(row=0, column=col, sticky="nsew", padx=1, pady=1)
        
        # 初始化表格数据存储
        self.tree = []
        
        # 配置网格行列权重
        for col in range(len(headers)):
            self.inner_frame.grid_columnconfigure(col, weight=1)
    
    def _on_mousewheel(self, event):
        """鼠标滚轮滚动事件"""
        self.canvas.yview_scroll(int(-1*(event.delta/120)), "units")
    
    def add_table_row(self, row_num):
        """添加一行表格元素"""
        row_widgets = []
        entry_font = ("微软雅黑", 9)
        for col in range(8):  # 共8列
            if col == 2:  # 线路类型列
                var = tk.StringVar()
                option = tk.OptionMenu(self.inner_frame, var, "左线", "右线")
                option.config(width=10, font=entry_font, bg="#f8f8f8", 
                            relief=tk.RAISED, bd=1)
                option.grid(row=row_num, column=col, sticky="nsew", padx=1, pady=1)
                var.trace("w", lambda *args, r=row_num-1: self.on_line_type_change(r))
                row_widgets.append({"widget": option, "var": var})
            elif col == 3:  # 部位列
                var = tk.StringVar()
                option = tk.OptionMenu(self.inner_frame, var, "初支", "二衬", "仰供", "其他")
                option.config(width=10, font=entry_font, bg="#f8f8f8", 
                            relief=tk.RAISED, bd=1)
                option.grid(row=row_num, column=col, sticky="nsew", padx=1, pady=1)
                row_widgets.append({"widget": option, "var": var})
            elif col == 6:  # 每日完成量列
                entry = tk.Entry(self.inner_frame, width=12, font=entry_font,
                               relief=tk.SUNKEN, bd=1, bg="white")
                entry.grid(row=row_num, column=col, sticky="nsew", padx=1, pady=1)
                row_widgets.append({"widget": entry, "var": None})
            else:
                entry = tk.Entry(self.inner_frame, width=15, font=entry_font,
                               relief=tk.SUNKEN, bd=1, bg="white")
                entry.grid(row=row_num, column=col, sticky="nsew", padx=1, pady=1)
                row_widgets.append({"widget": entry, "var": None})
        self.tree.append(row_widgets)
        return row_widgets
    
    def ensure_row_exists(self, row):
        """确保指定行存在"""
        while row >= len(self.tree):
            self.add_table_row(len(self.tree)+1)
    
    def on_line_type_change(self, row):
        """线路类型变化时的处理"""
        line_type = self.get_cell_value(row, 2)
        start_pile = self.get_cell_value(row, 4)
        end_pile = self.get_cell_value(row, 5)
        
        if line_type == "左线":
            if start_pile and not start_pile.startswith("ZK"):
                self.set_cell_value(row, 4, "ZK" + start_pile if not start_pile.startswith("K") else "Z" + start_pile)
            if end_pile and not end_pile.startswith("ZK"):
                self.set_cell_value(row, 5, "ZK" + end_pile if not end_pile.startswith("K") else "Z" + end_pile)
        else:  # 右线
            if start_pile and start_pile.startswith("ZK"):
                self.set_cell_value(row, 4, start_pile[1:])
            elif start_pile and start_pile.startswith("Z"):
                self.set_cell_value(row, 4, start_pile[1:])
            if end_pile and end_pile.startswith("ZK"):
                self.set_cell_value(row, 5, end_pile[1:])
            elif end_pile and end_pile.startswith("Z"):
                self.set_cell_value(row, 5, end_pile[1:])
    
    def get_cell_value(self, row, col):
        """获取单元格值"""
        self.ensure_row_exists(row)
        if col in [2, 3]:  # 下拉框
            return self.tree[row][col]["var"].get()
        else:  # 输入框
            return self.tree[row][col]["widget"].get()
    
    def set_cell_value(self, row, col, value):
        """设置单元格值"""
        self.ensure_row_exists(row)
        if col in [2, 3]:  # 下拉框
            self.tree[row][col]["var"].set(value)
        else:  # 输入框
            self.tree[row][col]["widget"].delete(0, tk.END)
            self.tree[row][col]["widget"].insert(0, value)
    
    def initialize_table(self):
        """初始化表格"""
        # 清除所有行
        for row in range(len(self.tree)):
            for col in range(8):
                self.set_cell_value(row, col, "")
        
        # 设置第一行
        current_date = datetime.now().strftime("%Y-%m-%d")
        self.set_cell_value(0, 0, "1")  # 序号
        self.set_cell_value(0, 1, current_date)  # 日期
        self.set_cell_value(0, 2, "左线")  # 线路类型
        self.set_cell_value(0, 3, "初支")  # 部位
        self.set_cell_value(0, 6, "1.0")  # 每日完成量
        self.set_cell_value(0, 7, "")  # 备注
        
        self.current_rows = 1
        self.status_var.set("表格已初始化 | 请填写起始桩号")
    
    def add_row(self):
        """添加新行"""
        if self.current_rows >= self.max_rows:
            messagebox.showwarning("警告", f"已达到最大行数限制({self.max_rows}行)!")
            return
        
        # 获取最后一行数据
        last_row = self.current_rows - 1
        if last_row < 0:
            messagebox.showwarning("警告", "请先初始化表格！")
            return
        
        # 使用上一行的终止桩号作为新行的起始桩号
        start_pile = self.get_cell_value(last_row, 5)
        
        # 获取上一行的其他信息
        line_type = self.get_cell_value(last_row, 2)
        part = self.get_cell_value(last_row, 3)
        daily_progress = self.get_cell_value(last_row, 6)
        
        # 计算新日期（上一行日期加一天）
        try:
            last_date = datetime.strptime(self.get_cell_value(last_row, 1), "%Y-%m-%d")
            new_date = (last_date + timedelta(days=1)).strftime("%Y-%m-%d")
        except:
            new_date = datetime.now().strftime("%Y-%m-%d")
        
        # 设置新行数据
        new_row = self.current_rows
        self.set_cell_value(new_row, 0, str(new_row+1))  # 序号
        self.set_cell_value(new_row, 1, new_date)  # 日期
        self.set_cell_value(new_row, 2, line_type)  # 线路类型
        self.set_cell_value(new_row, 3, part)  # 部位
        self.set_cell_value(new_row, 4, start_pile)  # 起始桩号
        self.set_cell_value(new_row, 6, daily_progress)  # 每日完成量
        
        self.current_rows += 1
        self.status_var.set(f"已添加第{new_row+1}行数据 | 总行数: {self.current_rows}")
        
        # 滚动到新行
        self.canvas.yview_moveto(1.0)
    
    def delete_row(self):
        """删除最后一行"""
        if self.current_rows <= 1:
            messagebox.showwarning("警告", "至少需要保留一行数据！")
            return
        
        # 清除最后一行数据
        row = self.current_rows - 1
        for col in range(8):
            self.set_cell_value(row, col, "")
        
        self.current_rows -= 1
        self.status_var.set(f"已删除行 | 总行数: {self.current_rows}")
    
    def recalculate_all(self):
        """重新计算所有桩号"""
        # 获取当前选择的桩号方向
        self.direction = self.direction_var.get()
        
        # 重新计算所有行的终止桩号
        for row in range(self.current_rows):
            if row == 0:
                start_pile = self.get_cell_value(row, 4)
            else:
                start_pile = self.get_cell_value(row-1, 5)
            
            if not start_pile:
                continue
                
            line_type = self.get_cell_value(row, 2)
            daily_progress = self.get_cell_value(row, 6)
            
            # 更新起始桩号
            self.set_cell_value(row, 4, start_pile)
            
            # 计算终止桩号
            try:
                progress = float(daily_progress)
            except:
                progress = 0
                
            if progress > 0:
                end_pile = self.calculate_end_pile(start_pile, progress, line_type)
                self.set_cell_value(row, 5, end_pile)
        
        self.status_var.set(f"已重新计算所有桩号 | 总行数: {self.current_rows}")
    
    def calculate_end_pile(self, start_pile, progress, line_type):
        """计算终止桩号"""
        # 处理前缀
        pile = start_pile.replace("ZK", "K").replace("Z", "")  # 统一处理为K前缀
        match = re.match(r'^([A-Za-z]*)(\d+)\+(\d+\.?\d*)$', pile)
        if not match:
            return ""
        
        prefix = match.group(1) or "K"  # 如果没有前缀，默认为"K"
        km = int(match.group(2))
        meter = float(match.group(3))
        
        # 根据方向计算新的米数
        if self.direction == "增加":
            total_meter = km * 1000 + meter + progress
        else:  # 减小
            total_meter = km * 1000 + meter - progress
        
        # 计算新的公里和米
        new_km = int(total_meter // 1000)
        new_meter = total_meter % 1000
        
        # 格式化桩号
        if new_meter.is_integer():
            end_pile = f"{prefix}{new_km}+{int(new_meter):03d}"
        else:
            end_pile = f"{prefix}{new_km}+{new_meter:06.3f}".rstrip('0').rstrip('.')
        
        # 根据线路类型添加前缀
        if line_type == "左线":
            end_pile = "ZK" + end_pile if not end_pile.startswith("K") else "Z" + end_pile
        
        return end_pile
    
    def new_file(self):
        """新建文件"""
        self.initialize_table()
        self.data_file = None
        self.status_var.set("已创建新文件")
    
    def open_file(self):
        """打开文件"""
        filepath = filedialog.askopenfilename(
            filetypes=[("CSV文件", "*.csv"), ("所有文件", "*.*")]
        )
        
        if not filepath:
            self.status_var.set("已取消打开文件")
            return
        
        try:
            with open(filepath, mode='r', newline='', encoding='ANSI') as file:
                reader = csv.reader(file)
                headers = next(reader)  # 跳过标题行
                
                # 清空表格
                for row in range(len(self.tree)):
                    for col in range(8):
                        self.set_cell_value(row, col, "")
                
                # 加载数据
                self.current_rows = 0
                for row_idx, row in enumerate(reader):
                    if row_idx >= self.max_rows:
                        break
                    
                    for col_idx, value in enumerate(row[:8]):
                        self.set_cell_value(row_idx, col_idx, value)
                    
                    # 从第一行读取桩号方向
                    if row_idx == 0 and len(row) > 7:
                        if "桩号方向:增加" in row[7]:
                            self.direction_var.set("增加")
                        elif "桩号方向:减小" in row[7]:
                            self.direction_var.set("减小")
                    
                    self.current_rows += 1
            
            self.data_file = filepath
            self.status_var.set(f"已加载文件: {os.path.basename(filepath)} | 总行数: {self.current_rows}")
        except Exception as e:
            messagebox.showerror("加载错误", f"加载文件时出错: {str(e)}")
            self.status_var.set(f"加载错误 | {str(e)}")
    
    def save_file(self):
        """保存文件"""
        if not self.data_file:
            self.save_as_file()
            return
        
        try:
            with open(self.data_file, mode='w', newline='', encoding='ANSI') as file:
                writer = csv.writer(file)
                # 写入标题行
                headers = ["序号", "日期", "线路类型", "部位", "起始桩号", "终止桩号", "每日完成量(m)", "备注"]
                writer.writerow(headers)
                
                # 写入数据
                for row in range(self.current_rows):
                    values = [self.get_cell_value(row, col) for col in range(8)]
                    # 第一行保存桩号方向
                    if row == 0:
                        values[7] = f"桩号方向:{self.direction_var.get()}"
                    writer.writerow(values)
            
            self.status_var.set(f"数据已保存到 {os.path.basename(self.data_file)} | 总行数: {self.current_rows}")
        except Exception as e:
            messagebox.showerror("保存错误", f"保存数据时出错: {str(e)}")
            self.status_var.set(f"保存错误 | {str(e)}")
    
    def save_as_file(self):
        """另存为文件"""
        filepath = filedialog.asksaveasfilename(
            defaultextension=".csv",
            filetypes=[("CSV文件", "*.csv"), ("所有文件", "*.*")]
        )
        
        if not filepath:
            self.status_var.set("已取消保存")
            return
        
        self.data_file = filepath
        self.save_file()

if __name__ == "__main__":
    root = tk.Tk()
    app = TunnelConstructionRegister(root)
    root.mainloop()
