# 工程计算工具箱 (右侧结果布局)
import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import math
import csv

# === 工具类 ===
class Utils:
    @staticmethod
    def validate_float(value_str: str, field_name: str) -> float:
        if not value_str:
            raise ValueError(f"{field_name}不能为空")
        try:
            return float(value_str)
        except ValueError:
            raise ValueError(f"{field_name}必须是有效数字")

# === 桥梁计算 ===
class BridgeCalculatorApp:
    def __init__(self, parent):
        self.parent = parent
        self.setup_ui()

    def setup_ui(self):
        main = ttk.PanedWindow(self.parent, orient=tk.HORIZONTAL)
        main.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # 左侧输入区
        input_frame = ttk.Frame(main)
        main.add(input_frame, weight=3)
        
        # 右侧结果区
        result_frame = ttk.Frame(main)
        main.add(result_frame, weight=2)
        
        # 结构定位
        self.setup_structure_positioning(input_frame)
        # 垫石计算
        self.setup_padstone(input_frame)
        # 柱宽计算
        self.setup_column_width(input_frame)
        # 结果显示
        self.setup_result_area(result_frame)

    def setup_structure_positioning(self, parent):
        frame = ttk.LabelFrame(parent, text="结构定位计算", padding=10)
        frame.pack(fill=tk.X, pady=5)

        # 类型选择
        type_frame = ttk.Frame(frame)
        type_frame.pack(fill=tk.X, pady=5)
        ttk.Label(type_frame, text="类型:").pack(side=tk.LEFT)
        self.struct_type = tk.StringVar(value="两点")
        ttk.OptionMenu(type_frame, self.struct_type, "两点", "两点", "四点", command=self.update_struct_fields).pack(side=tk.LEFT, padx=5)

        # 坐标输入区
        self.struct_input_frame = ttk.Frame(frame)
        self.struct_input_frame.pack(fill=tk.X, pady=5)
        self.update_struct_fields()

        # 尺寸输入
        size_frame = ttk.Frame(frame)
        size_frame.pack(fill=tk.X, pady=5)
        ttk.Label(size_frame, text="长(m):").pack(side=tk.LEFT, padx=(0, 5))
        self.length_entry = ttk.Entry(size_frame, width=10)
        self.length_entry.pack(side=tk.LEFT, padx=5)
        
        ttk.Label(size_frame, text="宽(m):").pack(side=tk.LEFT, padx=(10, 5))
        self.width_entry = ttk.Entry(size_frame, width=10)
        self.width_entry.pack(side=tk.LEFT, padx=5)

        # 按钮区
        btn_frame = ttk.Frame(frame)
        btn_frame.pack(fill=tk.X, pady=5)
        ttk.Button(btn_frame, text="加载CSV", command=self.load_struct_csv, width=10).pack(side=tk.LEFT, padx=5)
        ttk.Button(btn_frame, text="计算", command=self.calculate_structure, width=10).pack(side=tk.LEFT, padx=5)

    def update_struct_fields(self, *_):
        for w in self.struct_input_frame.winfo_children():
            w.destroy()
        points = 2 if self.struct_type.get() == "两点" else 4
        self.struct_entries = []
        
        for i in range(points):
            row = ttk.Frame(self.struct_input_frame)
            row.pack(fill=tk.X, pady=2)
            ttk.Label(row, text=f"点{i+1} X:").pack(side=tk.LEFT, padx=(0, 2))
            x = ttk.Entry(row, width=8)
            x.pack(side=tk.LEFT, padx=2)
            ttk.Label(row, text="Y:").pack(side=tk.LEFT, padx=(5, 2))
            y = ttk.Entry(row, width=8)
            y.pack(side=tk.LEFT, padx=2)
            self.struct_entries.append((x, y))

    def load_struct_csv(self):
        path = filedialog.askopenfilename(filetypes=[("CSV文件", "*.csv")])
        if not path: return
        try:
            with open(path, newline='') as f:
                points = [tuple(map(float, row)) for row in csv.reader(f) if len(row) >= 2]
            req = 2 if self.struct_type.get() == "两点" else 4
            if len(points) < req:
                raise ValueError(f"需要至少{req}个点")
            for i, (x, y) in enumerate(points[:req]):
                self.struct_entries[i][0].delete(0, tk.END)
                self.struct_entries[i][0].insert(0, str(x))
                self.struct_entries[i][1].delete(0, tk.END)
                self.struct_entries[i][1].insert(0, str(y))
        except Exception as e:
            messagebox.showerror("文件错误", str(e))

    def calculate_structure(self):
        try:
            points = [(float(x.get()), float(y.get())) for x, y in self.struct_entries]
            length = Utils.validate_float(self.length_entry.get(), "长度")
            width = Utils.validate_float(self.width_entry.get(), "宽度")
            result = self.calc_two_points(points, length, width) if len(points) == 2 else self.calc_four_points(points, length, width)
            self.show_result(result)
        except Exception as e:
            messagebox.showerror("计算错误", str(e))

    def calc_two_points(self, points, length, width):
        (x1, y1), (x2, y2) = points
        dx, dy = x2 - x1, y2 - y1
        cur = math.hypot(dx, dy)
        if cur == 0: raise ValueError("点重合")
        ext = (length - cur) / 2
        ux, uy = dx / cur, dy / cur
        p1 = (x1 - ext * ux, y1 - ext * uy)
        p2 = (x2 + ext * ux, y2 + ext * uy)
        px, py = -uy * width / 2, ux * width / 2
        result = [(p1[0] + px, p1[1] + py), (p2[0] + px, p2[1] + py),
                  (p2[0] - px, p2[1] - py), (p1[0] - px, p1[1] - py)]
        return "\n".join([f"点{i+1}: ({x:.3f}, {y:.3f})" for i, (x, y) in enumerate(result)])

    def calc_four_points(self, points, length, width):
        mid1 = ((points[0][0] + points[1][0]) / 2, (points[0][1] + points[1][1]) / 2)
        mid2 = ((points[2][0] + points[3][0]) / 2, (points[2][1] + points[3][1]) / 2)
        return self.calc_two_points([mid1, mid2], length, width)

    def setup_padstone(self, parent):
        frame = ttk.LabelFrame(parent, text="垫石计算", padding=10)
        frame.pack(fill=tk.X, pady=5)

        # 类型选择
        type_frame = ttk.Frame(frame)
        type_frame.pack(fill=tk.X, pady=5)
        ttk.Label(type_frame, text="类型:").pack(side=tk.LEFT)
        self.pad_type = tk.StringVar(value="两点")
        ttk.OptionMenu(type_frame, self.pad_type, "两点", "两点", "四点", command=self.update_pad_fields).pack(side=tk.LEFT, padx=5)

        # 坐标输入区
        self.pad_input_frame = ttk.Frame(frame)
        self.pad_input_frame.pack(fill=tk.X, pady=5)
        self.update_pad_fields()

        # 参数输入
        param_frame = ttk.Frame(frame)
        param_frame.pack(fill=tk.X, pady=5)
        ttk.Label(param_frame, text="间距(m):").pack(side=tk.LEFT, padx=(0, 5))
        self.spacing_entry = ttk.Entry(param_frame, width=10)
        self.spacing_entry.pack(side=tk.LEFT, padx=5)
        
        ttk.Label(param_frame, text="垫石个数:").pack(side=tk.LEFT, padx=(10, 5))
        self.count_entry = ttk.Entry(param_frame, width=10)
        self.count_entry.pack(side=tk.LEFT, padx=5)

        # 计算按钮
        ttk.Button(frame, text="计算", command=self.calculate_padstone, width=20).pack(pady=5)

    def update_pad_fields(self, *_):
        for w in self.pad_input_frame.winfo_children():
            w.destroy()
        points = 2 if self.pad_type.get() == "两点" else 4
        self.pad_entries = []
        
        for i in range(points):
            row = ttk.Frame(self.pad_input_frame)
            row.pack(fill=tk.X, pady=2)
            ttk.Label(row, text=f"点{i+1} X:").pack(side=tk.LEFT, padx=(0, 2))
            x = ttk.Entry(row, width=8)
            x.pack(side=tk.LEFT, padx=2)
            ttk.Label(row, text="Y:").pack(side=tk.LEFT, padx=(5, 2))
            y = ttk.Entry(row, width=8)
            y.pack(side=tk.LEFT, padx=2)
            self.pad_entries.append((x, y))

    def calculate_padstone(self):
        try:
            points = [(float(x.get()), float(y.get())) for x, y in self.pad_entries]
            spacing = Utils.validate_float(self.spacing_entry.get(), "间距")
            count = int(self.count_entry.get())
            mid = ((points[0][0] + points[-1][0]) / 2, (points[0][1] + points[-1][1]) / 2)
            dx = points[-1][0] - points[0][0]
            dy = points[-1][1] - points[0][1]
            length = math.hypot(dx, dy)
            if length == 0: raise ValueError("点重合")
            ux, uy = dx / length, dy / length
            start = -((count - 1) * spacing / 2)
            res = []
            for i in range(count):
                offset = start + i * spacing
                x = mid[0] + offset * ux
                y = mid[1] + offset * uy
                res.append(f"垫石{i+1}: ({x:.3f}, {y:.3f})")
            self.show_result("\n".join(res))
        except Exception as e:
            messagebox.showerror("计算错误", str(e))

    def setup_column_width(self, parent):
        frame = ttk.LabelFrame(parent, text="柱宽计算", padding=10)
        frame.pack(fill=tk.X, pady=5)
        
        # 输入区
        input_frame = ttk.Frame(frame)
        input_frame.pack(fill=tk.X, pady=5)
        
        ttk.Label(input_frame, text="柱顶宽度(m):").pack(side=tk.LEFT, padx=(0, 5))
        self.top_entry = ttk.Entry(input_frame, width=10)
        self.top_entry.pack(side=tk.LEFT, padx=5)
        
        ttk.Label(input_frame, text="高差(m):").pack(side=tk.LEFT, padx=(10, 5))
        self.diff_entry = ttk.Entry(input_frame, width=10)
        self.diff_entry.pack(side=tk.LEFT, padx=5)
        
        # 计算按钮
        ttk.Button(frame, text="计算", command=self.calculate_column, width=20).pack(pady=5)

    def calculate_column(self):
        try:
            top = Utils.validate_float(self.top_entry.get(), "柱顶宽度")
            diff = Utils.validate_float(self.diff_entry.get(), "高差")
            width = top + diff / 40
            self.show_result(f"柱宽 = {width:.3f} 米")
        except Exception as e:
            messagebox.showerror("计算错误", str(e))

    def setup_result_area(self, parent):
        frame = ttk.LabelFrame(parent, text="计算结果", padding=10)
        frame.pack(fill=tk.BOTH, expand=True, padx=5, pady=5)
        
        # 结果文本框
        self.result_text = tk.Text(frame, height=20, state='disabled', font=("Consolas", 10))
        scrollbar = ttk.Scrollbar(frame, command=self.result_text.yview)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        self.result_text.config(yscrollcommand=scrollbar.set)
        self.result_text.pack(fill=tk.BOTH, expand=True, padx=5, pady=5)
        
        # 操作按钮
        btn_frame = ttk.Frame(frame)
        btn_frame.pack(fill=tk.X, pady=5)
        ttk.Button(btn_frame, text="复制结果", command=self.copy_result, width=10).pack(side=tk.LEFT, padx=5)
        ttk.Button(btn_frame, text="清空结果", command=self.clear_result, width=10).pack(side=tk.RIGHT, padx=5)

    def show_result(self, text):
        self.result_text.config(state='normal')
        self.result_text.insert(tk.END, text + "\n\n")
        self.result_text.see(tk.END)
        self.result_text.config(state='disabled')

    def copy_result(self):
        text = self.result_text.get("1.0", tk.END).strip()
        if text:
            self.parent.clipboard_clear()
            self.parent.clipboard_append(text)
            messagebox.showinfo("复制成功", "已复制到剪贴板")

    def clear_result(self):
        self.result_text.config(state='normal')
        self.result_text.delete(1.0, tk.END)
        self.result_text.config(state='disabled')

# === 角度、曲率 ===
class AngleCurvatureCalculator:
    def __init__(self, parent):
        self.parent = parent
        self.setup_ui()

    def setup_ui(self):
        main = ttk.PanedWindow(self.parent, orient=tk.HORIZONTAL)
        main.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # 左侧曲率计算
        left_frame = ttk.Frame(main)
        main.add(left_frame, weight=1)
        self.setup_curvature(left_frame)
        
        # 右侧角度计算
        right_frame = ttk.Frame(main)
        main.add(right_frame, weight=1)
        self.setup_angle(right_frame)

    def setup_curvature(self, parent):
        frame = ttk.LabelFrame(parent, text="缓和曲线曲率计算", padding=10)
        frame.pack(fill=tk.BOTH, expand=True, padx=5, pady=5)

        # 输入区
        input_frame = ttk.Frame(frame)
        input_frame.pack(fill=tk.X, pady=10)
        
        ttk.Label(input_frame, text="参数A(m):").grid(row=0, column=0, sticky="e", padx=5, pady=5)
        self.A_entry = ttk.Entry(input_frame, width=12)
        self.A_entry.grid(row=0, column=1, pady=5)
        
        ttk.Label(input_frame, text="Ls(m):").grid(row=1, column=0, sticky="e", padx=5, pady=5)
        self.Ls_entry = ttk.Entry(input_frame, width=12)
        self.Ls_entry.grid(row=1, column=1, pady=5)
        
        # 半径选择
        radio_frame = ttk.Frame(frame)
        radio_frame.pack(fill=tk.X, pady=5)
        self.known = tk.StringVar(value="start")
        ttk.Radiobutton(radio_frame, text="已知起点半径R", variable=self.known, value="start").pack(side=tk.LEFT, padx=10)
        ttk.Radiobutton(radio_frame, text="已知终点半径R", variable=self.known, value="end").pack(side=tk.LEFT, padx=10)
        
        # 半径输入
        radius_frame = ttk.Frame(frame)
        radius_frame.pack(fill=tk.X, pady=5)
        ttk.Label(radius_frame, text="半径R(m):").pack(side=tk.LEFT, padx=(20, 5))
        self.radius_entry = ttk.Entry(radius_frame, width=15)
        self.radius_entry.pack(side=tk.LEFT, padx=5)
        
        # 计算按钮
        ttk.Button(frame, text="计算曲率", command=self.calc_curvature, width=15).pack(pady=10)
        
        # 结果显示
        result_frame = ttk.LabelFrame(frame, text="计算结果", padding=10)
        result_frame.pack(fill=tk.BOTH, expand=True, pady=10, padx=5)
        self.curvature_result = ttk.Label(result_frame, text="", font=("微软雅黑", 10, "bold"), foreground="#0066CC")
        self.curvature_result.pack(pady=10)

    def calc_curvature(self):
        try:
            A = Utils.validate_float(self.A_entry.get(), "参数A")
            Ls = Utils.validate_float(self.Ls_entry.get(), "Ls")
            R = Utils.validate_float(self.radius_entry.get(), "半径")
            if self.known.get() == "start":
                R2 = (A ** 2 * R) / (A ** 2 + R * Ls)
                self.curvature_result.config(text=f"终点半径: {R2:.2f} m")
            else:
                R1 = (A ** 2 * R) / (A ** 2 - R * Ls)
                self.curvature_result.config(text=f"起点半径: {R1:.2f} m")
        except Exception as e:
            messagebox.showerror("错误", str(e))

    def setup_angle(self, parent):
        frame = ttk.LabelFrame(parent, text="角度加减计算", padding=10)
        frame.pack(fill=tk.BOTH, expand=True, padx=5, pady=5)

        # 角度输入区
        input_frame = ttk.Frame(frame)
        input_frame.pack(fill=tk.X, pady=10)
        
        self.angle_entries = []
        for i in range(2):
            angle_frame = ttk.LabelFrame(input_frame, text=f"角度{i+1}")
            angle_frame.pack(side=tk.LEFT, fill=tk.BOTH, expand=True, padx=5)
            
            entry_frame = ttk.Frame(angle_frame)
            entry_frame.pack(padx=5, pady=5)
            
            d = ttk.Entry(entry_frame, width=5)
            d.pack(side=tk.LEFT)
            ttk.Label(entry_frame, text="°").pack(side=tk.LEFT, padx=2)
            
            m = ttk.Entry(entry_frame, width=5)
            m.pack(side=tk.LEFT, padx=(10, 0))
            ttk.Label(entry_frame, text="'").pack(side=tk.LEFT, padx=2)
            
            s = ttk.Entry(entry_frame, width=5)
            s.pack(side=tk.LEFT, padx=(10, 0))
            ttk.Label(entry_frame, text='"').pack(side=tk.LEFT, padx=2)
            
            self.angle_entries.append((d, m, s))

        # 操作选择
        op_frame = ttk.Frame(frame)
        op_frame.pack(pady=10)
        self.op = tk.StringVar(value="add")
        ttk.Radiobutton(op_frame, text="角度相加", variable=self.op, value="add").pack(side=tk.LEFT, padx=15)
        ttk.Radiobutton(op_frame, text="角度相减", variable=self.op, value="sub").pack(side=tk.LEFT, padx=15)

        # 计算按钮
        ttk.Button(frame, text="计算角度", command=self.calc_angle, width=15).pack(pady=10)
        
        # 结果显示
        result_frame = ttk.LabelFrame(frame, text="计算结果", padding=10)
        result_frame.pack(fill=tk.BOTH, expand=True, pady=10, padx=5)
        self.angle_result = ttk.Label(result_frame, text="", font=("微软雅黑", 10, "bold"), foreground="#0066CC")
        self.angle_result.pack(pady=10)

    def calc_angle(self):
        try:
            def to_sec(d, m, s):
                return int(d.get() or 0) * 3600 + int(m.get() or 0) * 60 + int(s.get() or 0)
            sec1 = to_sec(*self.angle_entries[0])
            sec2 = to_sec(*self.angle_entries[1])
            res = sec1 + sec2 if self.op.get() == "add" else sec1 - sec2
            sign = "-" if res < 0 else ""
            res = abs(res)
            d, m, s = res // 3600, (res % 3600) // 60, res % 60
            self.angle_result.config(text=f"结果: {sign}{d}° {m}' {s}\"")
        except Exception as e:
            messagebox.showerror("错误", str(e))

# === 主窗口 ===
class MainApp:
    def __init__(self, root):
        self.root = root
        self.root.title("工程计算工具箱")
        self.root.geometry("1200x1000")
        self.style = ttk.Style()
        self.style.configure("TNotebook.Tab", font=('微软雅黑', 10, 'bold'), padding=[10, 5])
        
        # 创建主界面
        self.notebook = ttk.Notebook(root, style="TNotebook")
        self.notebook.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # 桥梁计算标签页
        bridge_frame = ttk.Frame(self.notebook)
        self.notebook.add(bridge_frame, text="桥梁计算")
        BridgeCalculatorApp(bridge_frame)
        
        # 角度/曲率标签页
        angle_frame = ttk.Frame(self.notebook)
        self.notebook.add(angle_frame, text="角度/曲率计算")
        AngleCurvatureCalculator(angle_frame)
        
        # 状态栏
        self.status = ttk.Label(root, text="工程计算工具箱 v1.0 | 已就绪", relief=tk.SUNKEN, anchor=tk.W)
        self.status.pack(side=tk.BOTTOM, fill=tk.X)

if __name__ == "__main__":
    root = tk.Tk()
    MainApp(root)
    root.mainloop()
