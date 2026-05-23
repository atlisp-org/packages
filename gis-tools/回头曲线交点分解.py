import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import math
import csv

class MainApp:
    def __init__(self, root):
        self.root = root
        self.root.title("回头曲线交点分解")
        self.root.geometry("800x700")
        self.root.minsize(700, 600)
        
        # 配置主窗口网格
        self.root.grid_columnconfigure(0, weight=1)
        self.root.grid_rowconfigure(0, weight=1)
        
        # 主容器
        main_container = ttk.Frame(root, padding=10)
        main_container.grid(row=0, column=0, sticky='nsew')
        main_container.grid_columnconfigure(0, weight=1)
        main_container.grid_rowconfigure(1, weight=1)
        
        # 标题栏
        header_frame = ttk.Frame(main_container)
        header_frame.grid(row=0, column=0, sticky='ew', pady=(0, 10))
        ttk.Label(header_frame, text="回头曲线交点分解", 
                 font=('Microsoft YaHei', 14, 'bold')).pack(side=tk.LEFT, padx=5)
        
        # 创建Notebook
        self.notebook = ttk.Notebook(main_container)
        self.notebook.grid(row=1, column=0, sticky='nsew')
        
        # 添加两个功能模块
        self._create_no_spiral_tab()    # 无缓和曲线
        self._create_with_spiral_tab()  # 有缓和曲线
        
        # 状态栏
        status_frame = ttk.Frame(main_container)
        status_frame.grid(row=2, column=0, sticky='ew')
        
        self.status_var = tk.StringVar(value="就绪")
        ttk.Label(status_frame, textvariable=self.status_var, 
                 anchor=tk.W).pack(side=tk.LEFT, padx=5)
    
    def _create_no_spiral_tab(self):
        """创建无缓和曲线标签页"""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text='无缓和曲线')
        
        # 配置网格
        tab.grid_columnconfigure(0, weight=1)
        tab.grid_rowconfigure(1, weight=1)
        
        # 输入框框架
        input_frame = ttk.LabelFrame(tab, text="圆弧三点坐标", padding=10)
        input_frame.grid(row=0, column=0, sticky='ew', padx=10, pady=5)
        
        # 三点坐标输入
        self.entries = []
        point_labels = ["圆弧起点", "圆弧中点", "圆弧终点"]
        
        for i, label in enumerate(point_labels):
            row_frame = ttk.Frame(input_frame)
            row_frame.pack(fill=tk.X, pady=5)
            
            ttk.Label(row_frame, text=f"{label} X:", width=10, anchor="e").pack(side=tk.LEFT, padx=5)
            x_entry = ttk.Entry(row_frame, width=15)
            x_entry.pack(side=tk.LEFT, padx=5)
            
            ttk.Label(row_frame, text="Y:", width=2).pack(side=tk.LEFT, padx=(10, 5))
            y_entry = ttk.Entry(row_frame, width=15)
            y_entry.pack(side=tk.LEFT, padx=5)
            
            self.entries.append((x_entry, y_entry))
        
        # 按钮框架
        btn_frame = ttk.Frame(input_frame)
        btn_frame.pack(fill=tk.X, pady=10)
        
        ttk.Button(btn_frame, text="加载CSV", command=self._load_csv_no_spiral).pack(side=tk.LEFT, padx=5)
        ttk.Button(btn_frame, text="计算", command=self._calculate_no_spiral).pack(side=tk.LEFT, padx=5)
        ttk.Button(btn_frame, text="清空输入", command=self._clear_input_no_spiral).pack(side=tk.LEFT, padx=5)
        
        # 结果框架
        result_frame = ttk.LabelFrame(tab, text="计算结果", padding=10)
        result_frame.grid(row=1, column=0, sticky='nsew', padx=10, pady=5)
        result_frame.grid_columnconfigure(0, weight=1)
        result_frame.grid_rowconfigure(0, weight=1)
        
        # 结果文本框
        self.no_spiral_result = tk.Text(result_frame, wrap=tk.WORD, height=10, 
                                      font=('Consolas', 10), padx=10, pady=10)
        
        # 滚动条
        scroll_y = ttk.Scrollbar(result_frame, orient=tk.VERTICAL, command=self.no_spiral_result.yview)
        self.no_spiral_result['yscrollcommand'] = scroll_y.set
        
        scroll_x = ttk.Scrollbar(result_frame, orient=tk.HORIZONTAL, command=self.no_spiral_result.xview)
        self.no_spiral_result['xscrollcommand'] = scroll_x.set
        
        # 网格布局
        self.no_spiral_result.grid(row=0, column=0, sticky='nsew')
        scroll_y.grid(row=0, column=1, sticky='ns')
        scroll_x.grid(row=1, column=0, sticky='ew')
        
        # 结果操作按钮
        btn_frame_result = ttk.Frame(result_frame)
        btn_frame_result.grid(row=2, column=0, columnspan=2, sticky='e', pady=5)
        
        ttk.Button(btn_frame_result, text="复制结果", command=self._copy_result_no_spiral).pack(side=tk.RIGHT, padx=5)
        ttk.Button(btn_frame_result, text="保存结果", command=self._save_result_no_spiral).pack(side=tk.RIGHT, padx=5)
        ttk.Button(btn_frame_result, text="清空结果", command=self._clear_result_no_spiral).pack(side=tk.RIGHT, padx=5)
    
    def _load_csv_no_spiral(self):
        """加载CSV文件（无缓和曲线）"""
        file_path = filedialog.askopenfilename(filetypes=[("CSV文件", "*.csv")])
        if not file_path: return
        
        try:
            with open(file_path, newline='') as f:
                points = [tuple(map(float, row)) for row in csv.reader(f) if len(row) >= 2]
                
            if len(points) < 3: 
                raise ValueError("需要至少3个有效坐标点")
                
            for i, (x, y) in enumerate(points[:3]):
                self.entries[i][0].delete(0, tk.END)
                self.entries[i][0].insert(0, str(x))
                self.entries[i][1].delete(0, tk.END)
                self.entries[i][1].insert(0, str(y))
                
            self.status_var.set(f"已加载CSV文件: {file_path}")
        except Exception as e: 
            messagebox.showerror("文件错误", str(e))
    
    def _clear_input_no_spiral(self):
        """清空输入框（无缓和曲线）"""
        for x_entry, y_entry in self.entries:
            x_entry.delete(0, tk.END)
            y_entry.delete(0, tk.END)
        self.status_var.set("已清空输入数据")
    
    def _clear_result_no_spiral(self):
        """清空结果（无缓和曲线）"""
        self.no_spiral_result.delete(1.0, tk.END)
        self.status_var.set("已清空计算结果")
    
    def _calculate_no_spiral(self):
        """计算交点（无缓和曲线）"""
        try:
            points = []
            for x_entry, y_entry in self.entries:
                x = x_entry.get()
                y = y_entry.get()
                if not x or not y:
                    raise ValueError("所有坐标值必须填写")
                points.append((float(x), float(y)))
            
            if len(points) != 3:
                raise ValueError("需要3个点坐标")
                
            inter1, inter2, center, radius = self._calculate_intersections(*points)
            
            result = f"交点1: {inter1[0]:.3f}, {inter1[1]:.3f}\n"
            result += f"交点2: {inter2[0]:.3f}, {inter2[1]:.3f}\n"
            result += f"圆心坐标: {center[0]:.3f}, {center[1]:.3f}\n"
            result += f"半径: {radius:.3f}"
            
            self.no_spiral_result.delete(1.0, tk.END)
            self.no_spiral_result.insert(tk.END, result)
            self.status_var.set("计算完成")
        except Exception as e: 
            messagebox.showerror("计算错误", str(e))
            self.status_var.set(f"计算错误: {str(e)}")
    
    def _calculate_intersections(self, p1, p2, p3):
        """计算交点坐标（无缓和曲线）"""
        x1, y1 = p1
        x2, y2 = p2
        x3, y3 = p3

        a1 = x2 - x1
        b1 = y2 - y1
        c1 = a1 * (x1 + x2) / 2 + b1 * (y1 + y2) / 2

        a2 = x3 - x2
        b2 = y3 - y2
        c2 = a2 * (x2 + x3) / 2 + b2 * (y2 + y3) / 2

        det = a1 * b2 - a2 * b1
        if abs(det) < 1e-9: 
            raise ValueError("三点共线，无法形成圆")
            
        h = (b2 * c1 - b1 * c2) / det
        k = (a1 * c2 - a2 * c1) / det
        radius = math.sqrt((x1 - h)**2 + (y1 - k)**2)

        alpha = math.atan2(y1 - k, x1 - h)
        beta = math.atan2(y2 - k, x2 - h)
        theta = 2 * (beta - alpha)

        inter1 = (
            h + radius * (math.cos(alpha) - math.tan(theta / 4) * math.sin(alpha)),
            k + radius * (math.sin(alpha) + math.tan(theta / 4) * math.cos(alpha))
        )
        inter2 = (
            h + radius * (2 * math.cos(beta) - math.cos(alpha) + math.tan(theta / 4) * math.sin(alpha)),
            k + radius * (2 * math.sin(beta) - math.sin(alpha) - math.tan(theta / 4) * math.cos(alpha))
        )

        return inter1, inter2, (h, k), radius
    
    def _copy_result_no_spiral(self):
        """复制结果（无缓和曲线）"""
        result = self.no_spiral_result.get(1.0, tk.END).strip()
        if result:
            self.root.clipboard_clear()
            self.root.clipboard_append(result)
            messagebox.showinfo("成功", "结果已复制到剪贴板")
            self.status_var.set("结果已复制")
        else:
            messagebox.showwarning("警告", "没有可复制的结果！")
    
    def _save_result_no_spiral(self):
        """保存结果（无缓和曲线）"""
        result = self.no_spiral_result.get(1.0, tk.END).strip()
        if not result:
            messagebox.showwarning("警告", "没有可保存的结果！")
            return

        file = filedialog.asksaveasfilename(
            defaultextension=".txt",
            filetypes=[("文本文件", "*.txt"), ("所有文件", "*.*")],
            title="保存计算结果"
        )

        if not file:
            return

        try:
            with open(file, 'w', encoding='ANSI') as f:
                f.write(result)
            self.status_var.set(f"结果已保存到: {file}")
            messagebox.showinfo("成功", f"文件已成功保存到:\n{file}")
        except Exception as e:
            messagebox.showerror("错误", f"保存文件失败: {str(e)}")
    
    def _create_with_spiral_tab(self):
        """创建有缓和曲线标签页"""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text='有缓和曲线')
        
        # 配置网格
        tab.grid_columnconfigure(0, weight=1)
        tab.grid_rowconfigure(1, weight=1)
        
        # 输入框框架
        input_frame = ttk.LabelFrame(tab, text="曲线参数", padding=10)
        input_frame.grid(row=0, column=0, sticky='ew', padx=10, pady=5)
        
        # 创建输入字段
        self.entries_spiral = {}
        fields = [
            ("圆弧起点 (S)", "sx", "sy"),
            ("圆弧中点 (M)", "mx", "my"),
            ("圆弧终点 (E)", "ex", "ey"),
            ("上一交点 (A)", "ax", "ay"),
            ("入缓起点 (B)", "bx", "by"),
            ("出缓终点 (C)", "cx", "cy"),
            ("下一交点 (D)", "dx", "dy")
        ]
        
        # 使用网格布局输入字段
        for i, (label, x_key, y_key) in enumerate(fields):
            row_frame = ttk.Frame(input_frame)
            row_frame.grid(row=i, column=0, sticky='ew', pady=2)
            
            ttk.Label(row_frame, text=label, width=15, anchor="e").pack(side=tk.LEFT, padx=5)
            
            ttk.Label(row_frame, text="X:").pack(side=tk.LEFT, padx=5)
            self.entries_spiral[x_key] = ttk.Entry(row_frame, width=15)
            self.entries_spiral[x_key].pack(side=tk.LEFT, padx=5)
            
            ttk.Label(row_frame, text="Y:").pack(side=tk.LEFT, padx=5)
            self.entries_spiral[y_key] = ttk.Entry(row_frame, width=15)
            self.entries_spiral[y_key].pack(side=tk.LEFT, padx=5)
        
        # 按钮框架
        btn_frame = ttk.Frame(input_frame)
        btn_frame.grid(row=len(fields), column=0, sticky='ew', pady=10)
        
        ttk.Button(btn_frame, text="加载CSV", command=self._load_csv_spiral).pack(side=tk.LEFT, padx=5)
        ttk.Button(btn_frame, text="计算", command=self._calculate_spiral).pack(side=tk.LEFT, padx=5)
        ttk.Button(btn_frame, text="清空输入", command=self._clear_input_spiral).pack(side=tk.LEFT, padx=5)
        
        # 结果框架
        result_frame = ttk.LabelFrame(tab, text="计算结果", padding=10)
        result_frame.grid(row=1, column=0, sticky='nsew', padx=10, pady=5)
        result_frame.grid_columnconfigure(0, weight=1)
        result_frame.grid_rowconfigure(0, weight=1)
        
        # 结果文本框
        self.spiral_result = tk.Text(result_frame, wrap=tk.WORD, height=10, 
                                   font=('Consolas', 10), padx=10, pady=10)
        
        # 滚动条
        scroll_y = ttk.Scrollbar(result_frame, orient=tk.VERTICAL, command=self.spiral_result.yview)
        self.spiral_result['yscrollcommand'] = scroll_y.set
        
        scroll_x = ttk.Scrollbar(result_frame, orient=tk.HORIZONTAL, command=self.spiral_result.xview)
        self.spiral_result['xscrollcommand'] = scroll_x.set
        
        # 网格布局
        self.spiral_result.grid(row=0, column=0, sticky='nsew')
        scroll_y.grid(row=0, column=1, sticky='ns')
        scroll_x.grid(row=1, column=0, sticky='ew')
        
        # 结果操作按钮
        btn_frame_result = ttk.Frame(result_frame)
        btn_frame_result.grid(row=2, column=0, columnspan=2, sticky='e', pady=5)
        
        ttk.Button(btn_frame_result, text="复制结果", command=self._copy_result_spiral).pack(side=tk.RIGHT, padx=5)
        ttk.Button(btn_frame_result, text="保存结果", command=self._save_result_spiral).pack(side=tk.RIGHT, padx=5)
        ttk.Button(btn_frame_result, text="清空结果", command=self._clear_result_spiral).pack(side=tk.RIGHT, padx=5)
    
    def _load_csv_spiral(self):
        """加载CSV文件（有缓和曲线）"""
        file_path = filedialog.askopenfilename(filetypes=[("CSV文件", "*.csv")])
        if not file_path: return
        
        try:
            with open(file_path, newline='') as f:
                rows = [row for row in csv.reader(f) if row]
                
            if len(rows) < 7: 
                raise ValueError("CSV文件需要至少7行数据")
            
            fields = ["sx", "sy", "mx", "my", "ex", "ey", "ax", "ay", "bx", "by", "cx", "cy", "dx", "dy"]
            
            if all(len(row) == 2 for row in rows[:7]):
                values = []
                for row in rows[:7]: 
                    values.extend(row)
            elif len(rows) == 1 and len(rows[0]) == 14:
                values = rows[0]
            elif len(rows) == 7 and all(len(row) == 2 for row in rows):
                values = []
                for row in rows: 
                    values.extend(row)
            else: 
                raise ValueError("CSV格式不识别")
            
            if len(values) != 14: 
                raise ValueError(f"需要14个数值，但得到{len(values)}个")
            
            for i, field in enumerate(fields):
                self.entries_spiral[field].delete(0, tk.END)
                self.entries_spiral[field].insert(0, values[i])
                
            self.status_var.set(f"已加载CSV文件: {file_path}")
        except Exception as e: 
            messagebox.showerror("文件错误", f"加载CSV文件时出错: {str(e)}")
    
    def _clear_input_spiral(self):
        """清空输入框（有缓和曲线）"""
        for entry in self.entries_spiral.values(): 
            entry.delete(0, tk.END)
        self.status_var.set("已清空输入数据")
    
    def _clear_result_spiral(self):
        """清空结果（有缓和曲线）"""
        self.spiral_result.delete(1.0, tk.END)
        self.status_var.set("已清空计算结果")
    
    def _calculate_spiral(self):
        """计算交点（有缓和曲线）"""
        try:
            S = (self._get_float_spiral("sx"), self._get_float_spiral("sy"))
            M = (self._get_float_spiral("mx"), self._get_float_spiral("my"))
            E = (self._get_float_spiral("ex"), self._get_float_spiral("ey"))
            A = (self._get_float_spiral("ax"), self._get_float_spiral("ay"))
            B = (self._get_float_spiral("bx"), self._get_float_spiral("by"))
            C = (self._get_float_spiral("cx"), self._get_float_spiral("cy"))
            D = (self._get_float_spiral("dx"), self._get_float_spiral("dy"))
            
            if None in S+M+E+A+B+C+D:
                messagebox.showerror("输入错误", "所有坐标值必须为有效数字")
                return
            
            O, radius = self._calculate_circle_spiral(S, M, E)
            dx_om = M[0] - O[0]
            dy_om = M[1] - O[1]
            tangent1_dx = -dy_om
            tangent1_dy = dx_om
            dx_ab = B[0] - A[0]
            dy_ab = B[1] - A[1]
            dx_cd = D[0] - C[0]
            dy_cd = D[1] - C[1]
            
            p1 = self._line_intersection_spiral(M, tangent1_dx, tangent1_dy, A, dx_ab, dy_ab)
            p2 = self._line_intersection_spiral(M, tangent1_dx, tangent1_dy, C, dx_cd, dy_cd)
            
            result = f"圆心坐标: {O[0]:.4f}, {O[1]:.4f}\n"
            result += f"半径: {radius:.3f}\n"
            result += f"交点1: {p1[0]:.4f}, {p1[1]:.4f}\n"
            result += f"交点2: {p2[0]:.4f}, {p2[1]:.4f}"
            
            self.spiral_result.delete(1.0, tk.END)
            self.spiral_result.insert(tk.END, result)
            self.status_var.set("计算完成")
            
        except Exception as e: 
            messagebox.showerror("计算错误", f"发生错误: {str(e)}")
            self.status_var.set(f"计算错误: {str(e)}")
    
    def _get_float_spiral(self, key):
        """获取浮点数（有缓和曲线）"""
        try: 
            return float(self.entries_spiral[key].get())
        except ValueError: 
            return None
    
    def _calculate_circle_spiral(self, p1, p2, p3):
        """计算圆心和半径（有缓和曲线）"""
        x1, y1 = p1
        x2, y2 = p2
        x3, y3 = p3

        a1 = x2 - x1
        b1 = y2 - y1
        c1 = a1 * (x1 + x2) / 2 + b1 * (y1 + y2) / 2

        a2 = x3 - x2
        b2 = y3 - y2
        c2 = a2 * (x2 + x3) / 2 + b2 * (y2 + y3) / 2

        det = a1 * b2 - a2 * b1
        if abs(det) < 1e-9: 
            raise ValueError("三点共线，无法形成圆")
            
        h = (b2 * c1 - b1 * c2) / det
        k = (a1 * c2 - a2 * c1) / det
        radius = math.sqrt((x1 - h)**2 + (y1 - k)**2)
        return (h, k), radius
    
    def _line_intersection_spiral(self, P0, dir1_x, dir1_y, P1, dir2_x, dir2_y):
        """计算直线交点（有缓和曲线）"""
        a1 = dir1_y
        b1 = -dir1_x
        c1 = dir1_x * P0[1] - dir1_y * P0[0]

        a2 = dir2_y
        b2 = -dir2_x
        c2 = dir2_x * P1[1] - dir2_y * P1[0]
        
        denominator = a1 * b2 - a2 * b1
        if abs(denominator) < 1e-10: 
            return (None, None)
        
        x = (b1 * c2 - b2 * c1) / denominator
        y = (a2 * c1 - a1 * c2) / denominator
        return (round(x, 4), round(y, 4))
    
    def _copy_result_spiral(self):
        """复制结果（有缓和曲线）"""
        result = self.spiral_result.get(1.0, tk.END).strip()
        if result:
            self.root.clipboard_clear()
            self.root.clipboard_append(result)
            messagebox.showinfo("成功", "结果已复制到剪贴板")
            self.status_var.set("结果已复制")
        else:
            messagebox.showwarning("警告", "没有可复制的结果！")
    
    def _save_result_spiral(self):
        """保存结果（有缓和曲线）"""
        result = self.spiral_result.get(1.0, tk.END).strip()
        if not result:
            messagebox.showwarning("警告", "没有可保存的结果！")
            return

        file = filedialog.asksaveasfilename(
            defaultextension=".txt",
            filetypes=[("文本文件", "*.txt"), ("所有文件", "*.*")],
            title="保存计算结果"
        )

        if not file:
            return

        try:
            with open(file, 'w', encoding='ANSI') as f:
                f.write(result)
            self.status_var.set(f"结果已保存到: {file}")
            messagebox.showinfo("成功", f"文件已成功保存到:\n{file}")
        except Exception as e:
            messagebox.showerror("错误", f"保存文件失败: {str(e)}")

if __name__ == "__main__":
    root = tk.Tk()
    app = MainApp(root)
    root.mainloop()
