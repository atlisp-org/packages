import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import csv

class FillAndCutApp:
    def __init__(self, root):
        self.root = root
        self.root.title("填挖方计算")
        self.style = ttk.Style()
        self.style.configure("TLabel", padding=4, font=('微软雅黑', 10))
        self.style.configure("TButton", font=('微软雅黑', 10))
        self.style.configure("Header.TLabel", font=('微软雅黑', 10, 'bold'), background='#f0f0f0')
        
        # Main container
        main_frame = ttk.Frame(root, padding="10")
        main_frame.pack(fill=tk.BOTH, expand=True)
        
        # Control panel
        control_frame = ttk.Frame(main_frame)
        control_frame.pack(fill=tk.X, pady=(0, 10))
        
        ttk.Label(control_frame, text="数据行数:").grid(row=0, column=0, padx=(0, 5), sticky="e")
        self.row_num = ttk.Entry(control_frame, width=10)
        self.row_num.grid(row=0, column=1, padx=(0, 10), sticky="w")
        
        # 按钮使用网格布局
        buttons = [
            ("生成表格", self.create_table),
            ("导入CSV", self.load_csv),
            ("计算汇总", self.calculate_total),
            ("导出Excel", self.save_to_excel),
            ("清除数据", self.clear_all)
        ]
        
        for col, (text, command) in enumerate(buttons):
            ttk.Button(control_frame, text=text, command=command, width=10).grid(
                row=0, column=col+2, padx=5, sticky="ew")
        
        # 配置列权重
        control_frame.columnconfigure(0, weight=0)
        control_frame.columnconfigure(1, weight=0)
        for i in range(2, len(buttons)+2):
            control_frame.columnconfigure(i, weight=1)

        # Table area with scrollbar
        table_container = ttk.LabelFrame(main_frame, text="填挖方数据", padding=5)
        table_container.pack(fill=tk.BOTH, expand=True, pady=(0, 10))
        
        # 创建滚动条和画布
        self.scrollbar = ttk.Scrollbar(table_container, orient="vertical")
        self.scrollbar.pack(side="right", fill="y")
        
        self.canvas = tk.Canvas(table_container, borderwidth=0, highlightthickness=0, 
                               yscrollcommand=self.scrollbar.set)
        self.canvas.pack(side="left", fill="both", expand=True)
        
        self.scrollbar.config(command=self.canvas.yview)
        
        self.scrollable_frame = ttk.Frame(self.canvas)
        self.canvas_frame = self.canvas.create_window((0, 0), window=self.scrollable_frame, anchor="nw")
        
        # 绑定事件处理
        self.scrollable_frame.bind("<Configure>", self.on_frame_configure)
        self.canvas.bind("<Configure>", self.on_canvas_configure)

        # Summary area
        summary_frame = ttk.LabelFrame(main_frame, text="计算结果汇总", padding=10)
        summary_frame.pack(fill=tk.X, pady=(0, 5))
        
        ttk.Label(summary_frame, text="总填方数量 (m³):").grid(row=0, column=0, padx=(0, 5), sticky="e")
        self.total_fill = ttk.Entry(summary_frame, width=15, state="readonly", font=('微软雅黑', 10, 'bold'))
        self.total_fill.grid(row=0, column=1, padx=(0, 20), sticky="w")
        
        ttk.Label(summary_frame, text="总挖方数量 (m³):").grid(row=0, column=2, padx=(0, 5), sticky="e")
        self.total_cut = ttk.Entry(summary_frame, width=15, state="readonly", font=('微软雅黑', 10, 'bold'))
        self.total_cut.grid(row=0, column=3, sticky="w")
        
        # 添加状态栏
        self.status_var = tk.StringVar(value="就绪")
        status_bar = ttk.Label(root, textvariable=self.status_var, relief=tk.SUNKEN, anchor=tk.W)
        status_bar.pack(side=tk.BOTTOM, fill=tk.X)

    def on_frame_configure(self, event):
        """更新滚动区域"""
        self.canvas.configure(scrollregion=self.canvas.bbox("all"))
    
    def on_canvas_configure(self, event):
        """调整内部框架宽度以适应画布"""
        canvas_width = event.width
        self.canvas.itemconfig(self.canvas_frame, width=canvas_width)

    def create_table(self):
        try:
            self.row_count = int(self.row_num.get())
            if self.row_count < 1:
                messagebox.showerror("错误", "数据行数不能少于1")
                return
        except ValueError:
            messagebox.showerror("错误", "请输入有效的数据行数")
            return

        # Clear old widgets
        self.clear_table()

        # Create headers
        headers = ["桩号", "填方面积 (m²)", "挖方面积 (m²)", "距离 (m)", "填方数量 (m³)", "挖方数量 (m³)"]
        for col, header in enumerate(headers):
            label = ttk.Label(self.scrollable_frame, text=header, style="Header.TLabel")
            label.grid(row=0, column=col, padx=5, pady=3, sticky="ew")
            # 确保列标题宽度一致
            self.scrollable_frame.columnconfigure(col, minsize=100)

        # Create input rows and result rows
        self.rows = []
        self.result_entries = []
        
        for row_idx in range(self.row_count):
            # Input columns (0-2) - each input row spans 2 visual rows
            row_entries = []
            for col_idx in range(3):
                entry = ttk.Entry(self.scrollable_frame, width=15)
                entry.grid(row=row_idx*2+1, column=col_idx, padx=5, pady=2, sticky="ew", rowspan=2)
                row_entries.append(entry)
            self.rows.append(row_entries)

            # Result columns (3-5) - each result spans 2 visual rows
            if row_idx < self.row_count - 1:
                result_entry = []
                for col_idx in range(3, 6):
                    entry = ttk.Entry(self.scrollable_frame, width=15, state="readonly")
                    entry.grid(row=row_idx*2+1, column=col_idx, padx=5, pady=2, sticky="ew", rowspan=2)
                    result_entry.append(entry)
                self.result_entries.append(result_entry)

        # Configure column weights
        for col in range(6):
            self.scrollable_frame.columnconfigure(col, weight=1)
            
        self.status_var.set(f"已创建 {self.row_count} 行数据表格")

    def clear_table(self):
        """Clear all widgets in the table"""
        for widget in self.scrollable_frame.winfo_children():
            widget.destroy()
        self.rows = []
        self.result_entries = []
        self.status_var.set("表格已清除")
        
        # 重新配置列
        for col in range(6):
            self.scrollable_frame.columnconfigure(col, weight=1)

    def load_csv(self):
        """Load data from CSV file with ANSI encoding"""
        file_path = filedialog.askopenfilename(filetypes=[("CSV文件", "*.csv")])
        if not file_path:
            return
        
        try:
            with open(file_path, "r", encoding="gbk") as file:
                reader = csv.reader(file)
                rows = [row for row in reader if any(field.strip() for field in row)]
                
                if not rows:
                    messagebox.showerror("错误", "CSV文件中没有有效数据")
                    return
                
                # Set row count and create table
                self.row_num.delete(0, tk.END)
                self.row_num.insert(0, str(len(rows)))
                self.create_table()
                
                # Fill data
                for i, row in enumerate(rows):
                    if i >= len(self.rows):
                        break
                    for j in range(min(3, len(row))):
                        self.rows[i][j].delete(0, tk.END)
                        self.rows[i][j].insert(0, row[j].strip())
                
                self.status_var.set(f"已从 {file_path.split('/')[-1]} 加载 {len(rows)} 行数据")
                
        except Exception as e:
            messagebox.showerror("错误", f"读取CSV文件时出错: {str(e)}")
            self.status_var.set("CSV导入失败")

    def calculate_total(self):
        if not hasattr(self, 'rows') or not self.rows:
            messagebox.showwarning("警告", "请先生成表格并输入数据")
            return
            
        total_fill = 0.0
        total_cut = 0.0
        
        # Clear previous results
        for group in self.result_entries:
            for entry in group:
                self._update_entry(entry, "")
        
        # Calculate between each pair of rows
        for i in range(1, len(self.rows)):
            prev = self.rows[i-1]
            current = self.rows[i]
            
            try:
                # 获取桩号值，处理可能的空值
                station1_str = prev[0].get()
                station2_str = current[0].get()
                
                if not station1_str or not station2_str:
                    continue
                    
                station1 = float(station1_str)
                fill1 = float(prev[1].get() or 0)
                cut1 = float(prev[2].get() or 0)
                
                station2 = float(station2_str)
                fill2 = float(current[1].get() or 0)
                cut2 = float(current[2].get() or 0)
                
                distance = station2 - station1
                if distance <= 0:
                    messagebox.showerror("错误", f"桩号必须递增 (行 {i} 到行 {i+1})")
                    continue
               
                fill_volume = (fill1 + fill2) / 2 * distance
                cut_volume = (cut1 + cut2) / 2 * distance
               
                if i-1 < len(self.result_entries):
                    self._update_entry(self.result_entries[i-1][0], f"{distance:.1f}")
                    self._update_entry(self.result_entries[i-1][1], f"{fill_volume:.2f}")
                    self._update_entry(self.result_entries[i-1][2], f"{cut_volume:.2f}")
               
                total_fill += fill_volume
                total_cut += cut_volume
               
            except ValueError as e:
                self.status_var.set(f"第{i+1}行数据错误: {str(e)}")
                continue
        
        self._update_entry(self.total_fill, f"{total_fill:.2f}")
        self._update_entry(self.total_cut, f"{total_cut:.2f}")
        self.status_var.set(f"计算完成: 填方 {total_fill:.2f} m³, 挖方 {total_cut:.2f} m³")

    def clear_all(self):
        """Clear all data"""
        self.row_num.delete(0, tk.END)
        self.clear_table()
        self._update_entry(self.total_fill, "")
        self._update_entry(self.total_cut, "")
        self.status_var.set("所有数据已清除")

    def _update_entry(self, entry, value):
        """Helper method to update entry content"""
        entry.config(state="normal")
        entry.delete(0, tk.END)
        entry.insert(0, value)
        entry.config(state="readonly")

    def save_to_excel(self):
        """Save data to Excel-compatible HTML format"""
        if not hasattr(self, 'rows') or not self.rows:
            messagebox.showwarning("警告", "没有数据可导出")
            return
            
        file_path = filedialog.asksaveasfilename(
            defaultextension=".xls",
            filetypes=[("Excel文件", "*.xls")],
            title="导出填挖方数据",
            initialfile="填挖方计算结果.xls"
        )
        if not file_path:
            return
            
        try:
            # 创建HTML表格结构
            html = """<html xmlns:o="urn:schemas-microsoft-com:office:office" 
            xmlns:x="urn:schemas-microsoft-com:office:excel" 
            xmlns="http://www.w3.org/TR/REC-html40">
            <head>
                <meta http-equiv=Content-Type content="text/html; charset=gb2312">
                <title>填挖方计算结果</title>
                <!--[if gte mso 9]><xml>
                <x:ExcelWorkbook>
                    <x:ExcelWorksheets>
                        <x:ExcelWorksheet>
                            <x:Name>填挖方计算</x:Name>
                            <x:WorksheetOptions>
                                <x:DisplayGridlines/>
                            </x:WorksheetOptions>
                        </x:ExcelWorksheet>
                    </x:ExcelWorksheets>
                </x:ExcelWorkbook>
                </xml><![endif]-->
                <style>
                    body {font-family: Arial, sans-serif; margin: 20px;}
                    table {border-collapse: collapse; width: 100%;}
                    th {background-color: #4CAF50; color: white; font-weight: bold; text-align: center; padding: 8px;}
                    td {border: 1px solid #ddd; padding: 6px; text-align: center;}
                    tr:nth-child(even) {background-color: #f2f2f2;}
                    .total-row {background-color: #FFD700; font-weight: bold;}
                    .header-row {background-color: #4CAF50; color: white;}
                </style>
            </head>
            <body>
            <h2 style="text-align: center;">填挖方计算结果</h2>
            <table>
            <tr class="header-row">
                <th>桩号</th>
                <th>填方面积 (m²)</th>
                <th>挖方面积 (m²)</th>
                <th>距离 (m)</th>
                <th>填方数量 (m³)</th>
                <th>挖方数量 (m³)</th>
            </tr>
            """
            
            # 添加数据行
            for i in range(len(self.rows)):
                # 原始数据行 - 合并两行
                html += f"<tr><td rowspan='2'>{self.rows[i][0].get() or ''}</td>"
                html += f"<td rowspan='2'>{self.rows[i][1].get() or ''}</td>"
                html += f"<td rowspan='2'>{self.rows[i][2].get() or ''}</td>"
                
                # 计算结果列 - 如果有计算结果则显示
                if i < len(self.result_entries):
                    html += f"<td rowspan='2'>{self.result_entries[i][0].get() or ''}</td>"
                    html += f"<td rowspan='2'>{self.result_entries[i][1].get() or ''}</td>"
                    html += f"<td rowspan='2'>{self.result_entries[i][2].get() or ''}</td>"
                else:
                    html += "<td rowspan='2'></td><td rowspan='2'></td><td rowspan='2'></td>"
                html += "</tr><tr></tr>"  # 添加一个空行来保持结构
            
            # 添加汇总行
            total_fill = self.total_fill.get() or ""
            total_cut = self.total_cut.get() or ""
            
            html += f"""
            <tr class="total-row">
                <td colspan="3" style="text-align: right;">总填方数量:</td>
                <td colspan="3">{total_fill}</td>
            </tr>
            <tr class="total-row">
                <td colspan="3" style="text-align: right;">总挖方数量:</td>
                <td colspan="3">{total_cut}</td>
            </tr>
            """
            
            html += "</table></body></html>"
            
            # 写入文件
            with open(file_path, "w", encoding="gb2312", errors="replace") as f:
                f.write(html)
            
            self.status_var.set(f"数据已导出到: {file_path}")
            messagebox.showinfo("导出成功", f"数据已成功导出为Excel格式:\n{file_path}")
        except Exception as e:
            messagebox.showerror("导出错误", f"导出文件时出错: {str(e)}")
            self.status_var.set("导出失败")

if __name__ == "__main__":
    root = tk.Tk()
    root.geometry("950x650")
    root.minsize(800, 500)
    app = FillAndCutApp(root)
    root.mainloop()