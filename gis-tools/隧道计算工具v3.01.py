import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import math
import csv
import random
from typing import List, Tuple

class Utils:
    @staticmethod
    def validate_float(value_str: str, field_name: str) -> float:
        """验证浮点数输入"""
        if not value_str:
            raise ValueError(f"{field_name}不能为空")
        try:
            return float(value_str)
        except ValueError:
            raise ValueError(f"{field_name}必须是有效的数字")

class SingleRadiusCalculator:
    """正洞计算器"""
    def __init__(self, x_offset: float = 5.32, y_offset: float = 1.61):
        self.x_offset = x_offset
        self.y_offset = y_offset
        self.angles = None

    def generate_angles(self, total_points: int) -> List[float]:
        """生成均匀分布的角度"""
        if total_points < 2:
            raise ValueError("总点数至少为2")
        step = 180.0 / (total_points - 1)
        return [round(i * step, 2) for i in range(total_points)]

    def generate_radius(self, base_value: float) -> float:
        """生成随机半径"""
        if base_value <= 5.5:
            return round(base_value + random.uniform(0.02, 0.05), 4)
        return round(base_value + random.uniform(0.04, 0.1), 4)

    def calculate(self, base_radius: float, total_points: int) -> List[Tuple]:
        """计算坐标点"""
        self.angles = self.generate_angles(total_points)
        return [self._calculate_point(angle, self.generate_radius(base_radius), "随机半径") for angle in self.angles]

    def calculate_fixed(self, radius_values: List[float]) -> List[Tuple]:
        """使用固定半径计算坐标点"""
        if not self.angles or len(radius_values) != len(self.angles):
            raise ValueError("半径值数量与角度数量不匹配")
        return [self._calculate_point(angle, radius, "固定半径") for angle, radius in zip(self.angles, radius_values)]

    def _calculate_point(self, angle: float, radius: float, remark: str) -> Tuple:
        """计算单个点的坐标"""
        rad = math.radians(angle)
        x_original = round(radius * math.cos(rad), 4)
        y_original = round(radius * math.sin(rad), 4)
        x = round(x_original + self.x_offset, 4)
        y = round(y_original + self.y_offset, 4)
        x_prime = round(-x_original + self.x_offset, 4)
        y_prime = round(-y_original + self.y_offset, 4)
        return (angle, radius, f"{x:.4f},{y:.4f}", f"{x_prime:.4f},{y_prime:.4f}", remark)

class MultiRadiusCalculator:
    """紧急停车带计算器"""
    def __init__(self, x_offset: float = 6.82, y_offset: float = 1.61):
        self.x_offset = x_offset
        self.y_offset = y_offset
        self.angles = [180, 171.16, 162.28, 153.32, 144.22, 134.94, 125.36, 115.49,
                       105.39, 95.15, 84.85, 74.61, 64.51, 54.64, 45.06, 35.78,
                       26.68, 17.72, 8.84, 0]
        self.radius_map = {
            '5jt': [8.04, 8.0178, 7.9498, 7.839, 7.6864, 7.4987, 7.3244, 7.1885,
                    7.0942, 7.0462, 7.0462, 7.0942, 7.1885, 7.3244, 7.4987, 7.6864,
                    7.839, 7.9498, 8.0178, 8.04],
            '4jt': [7.89, 7.8678, 7.7998, 7.689, 7.5364, 7.3487, 7.1744, 7.0385,
                    6.9442, 6.8962, 6.8962, 6.9442, 7.0385, 7.1744, 7.3487, 7.5364,
                    7.689, 7.7998, 7.8678, 7.89],
            '二衬': [7, 6.9778, 6.9098, 6.799, 6.6464, 6.4587, 6.2844, 6.1485,
                   6.0542, 6.0062, 6.0062, 6.0542, 6.1485, 6.2844, 6.4587, 6.6464,
                   6.799, 6.9098, 6.9778, 7]
        }

    def generate_radius(self, base_value: float) -> float:
        """生成随机半径"""
        return round(base_value + random.uniform(0.04, 0.1), 4)

    def calculate(self, radius_type: str) -> List[Tuple]:
        """计算指定半径类型的坐标点"""
        if radius_type not in self.radius_map:
            raise ValueError("半径类型必须是 '5jt', '4jt' 或 '二衬'")
        
        results = []
        for angle, radius in zip(self.angles, self.radius_map[radius_type]):
            radius = self.generate_radius(radius)
            rad = math.radians(angle)
            x = round(self.x_offset + radius * math.cos(rad), 4)
            y = round(self.y_offset + radius * math.sin(rad), 4)
            x_prime = round(self.x_offset - radius * math.cos(rad), 4)
            y_prime = round(self.y_offset - radius * math.sin(rad), 4)
            results.append((
                angle, 
                radius, 
                f"{x:.4f},{y:.4f}", 
                f"{x_prime:.4f},{y_prime:.4f}",
                f"紧急停车带-{radius_type}"
            ))
        return results

    def calculate_fixed(self, radius_values: List[float]) -> List[Tuple]:
        """使用固定半径计算坐标点"""
        if len(radius_values) != len(self.angles):
            raise ValueError("半径值数量与角度数量不匹配")
        
        results = []
        for angle, radius in zip(self.angles, radius_values):
            rad = math.radians(angle)
            x = round(self.x_offset + radius * math.cos(rad), 4)
            y = round(self.y_offset + radius * math.sin(rad), 4)
            x_prime = round(self.x_offset - radius * math.cos(rad), 4)
            y_prime = round(self.y_offset - radius * math.sin(rad), 4)
            results.append((
                angle, 
                radius, 
                f"{x:.4f},{y:.4f}", 
                f"{x_prime:.4f},{y_prime:.4f}",
                "紧急停车带-固定半径"
            ))
        return results

# 隧道计算工具箱
class TunnelCalculator:
    def __init__(self, root):
        self.root = root
        self.root.title("隧道计算工具箱 v3.0")
        self.setup_ui()
        self.center_window(1200, 800)
        
        # 初始化计算器
        self.single_calc = SingleRadiusCalculator()
        self.multi_calc = MultiRadiusCalculator()

    def center_window(self, width, height):
        """居中显示窗口"""
        screen_width = self.root.winfo_screenwidth()
        screen_height = self.root.winfo_screenheight()
        x = (screen_width - width) // 2
        y = (screen_height - height) // 2
        self.root.geometry(f"{width}x{height}+{x}+{y}")

    def setup_ui(self):
        """设置主界面UI"""
        main_frame = ttk.Frame(self.root, padding="10")
        main_frame.pack(fill=tk.BOTH, expand=True)

        input_frame = ttk.Frame(main_frame, width=350)
        input_frame.pack(side=tk.LEFT, fill=tk.Y, padx=(0, 10))
        input_frame.pack_propagate(False)

        result_frame = ttk.Frame(main_frame)
        result_frame.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True)

        self.create_input_sections(input_frame)
        self.create_result_sections(result_frame)

    def create_input_sections(self, parent):
        """创建输入区域"""
        # 正洞计算
        self.create_single_input(parent)
        # 紧急停车带计算
        self.create_multi_input(parent)
        # 待测点计算
        self.create_points_input(parent)

    def create_single_input(self, parent):
        """正洞计算输入区域"""
        frame = ttk.LabelFrame(parent, text="正洞计算", padding=10)
        frame.pack(fill=tk.X, pady=5, padx=5)

        mode_frame = ttk.Frame(frame)
        mode_frame.pack(fill=tk.X, pady=(0, 5))
        ttk.Label(mode_frame, text="计算模式:").pack(side=tk.LEFT)
        self.single_mode_var = tk.StringVar(value="随机值")
        ttk.OptionMenu(mode_frame, self.single_mode_var, "随机值", "随机值", "固定值", 
                      command=self.toggle_single_mode).pack(side=tk.LEFT, padx=5)

        self.base_radius_frame = ttk.Frame(frame)
        self.base_radius_frame.pack(fill=tk.X, pady=2)
        ttk.Label(self.base_radius_frame, text="基础半径(m):").pack(side=tk.LEFT)
        self.base_radius_entry = ttk.Entry(self.base_radius_frame, width=12)
        self.base_radius_entry.pack(side=tk.LEFT, padx=5)
        self.base_radius_entry.insert(0, "5.5")

        self.radius_file_frame = ttk.Frame(frame)
        ttk.Label(self.radius_file_frame, text="半径文件:").pack(side=tk.LEFT)
        self.radius_file_entry = ttk.Entry(self.radius_file_frame)
        self.radius_file_entry.pack(side=tk.LEFT, fill=tk.X, expand=True, padx=5)
        ttk.Button(self.radius_file_frame, text="浏览", command=self.browse_radius_file, width=6).pack(side=tk.LEFT)

        params = [
            ("X偏移量(m):", "x_offset_entry", "5.32"),
            ("Y偏移量(m):", "y_offset_entry", "1.61"),
            ("计算点数:", "points_entry", "20")
        ]
        for label, name, default in params:
            row = ttk.Frame(frame)
            row.pack(fill=tk.X, pady=2)
            ttk.Label(row, text=label).pack(side=tk.LEFT)
            entry = ttk.Entry(row, width=12)
            entry.insert(0, default)
            entry.pack(side=tk.LEFT, padx=5)
            setattr(self, name, entry)

        ttk.Button(frame, text="计算正洞", command=self.calculate_single).pack(pady=(5, 0), fill=tk.X)

    def create_multi_input(self, parent):
        """紧急停车带计算输入区域"""
        frame = ttk.LabelFrame(parent, text="紧急停车带计算", padding=10)
        frame.pack(fill=tk.X, pady=5, padx=5)

        mode_frame = ttk.Frame(frame)
        mode_frame.pack(fill=tk.X, pady=(0, 5))
        ttk.Label(mode_frame, text="计算模式:").pack(side=tk.LEFT)
        self.multi_mode_var = tk.StringVar(value="随机值")
        ttk.OptionMenu(mode_frame, self.multi_mode_var, "随机值", "随机值", "固定值", 
                      command=self.toggle_multi_mode).pack(side=tk.LEFT, padx=5)

        type_frame = ttk.Frame(frame)
        type_frame.pack(fill=tk.X, pady=2)
        ttk.Label(type_frame, text="半径类型:").pack(side=tk.LEFT)
        self.radius_type_var = tk.StringVar()
        ttk.Combobox(type_frame, textvariable=self.radius_type_var, 
                    values=["5jt", "4jt", "二衬"], width=8, state="readonly").pack(side=tk.LEFT, padx=5)
        self.radius_type_var.set("5jt")

        self.multi_file_frame = ttk.Frame(frame)
        ttk.Label(self.multi_file_frame, text="半径文件:").pack(side=tk.LEFT)
        self.multi_file_entry = ttk.Entry(self.multi_file_frame)
        self.multi_file_entry.pack(side=tk.LEFT, fill=tk.X, expand=True, padx=5)
        ttk.Button(self.multi_file_frame, text="浏览", command=self.browse_multi_file, width=6).pack(side=tk.LEFT)

        params = [
            ("X偏移量(m):", "multi_x_offset_entry", "6.82"),
            ("Y偏移量(m):", "multi_y_offset_entry", "1.61")
        ]
        for label, name, default in params:
            row = ttk.Frame(frame)
            row.pack(fill=tk.X, pady=2)
            ttk.Label(row, text=label).pack(side=tk.LEFT)
            entry = ttk.Entry(row, width=12)
            entry.insert(0, default)
            entry.pack(side=tk.LEFT, padx=5)
            setattr(self, name, entry)

        ttk.Button(frame, text="计算停车带", command=self.calculate_multi).pack(pady=(5, 0), fill=tk.X)

    def create_points_input(self, parent):
        """待测点计算输入区域"""
        frame = ttk.LabelFrame(parent, text="待测点计算", padding=10)
        frame.pack(fill=tk.X, pady=5, padx=5)

        points = [
            ("测设线点 X:", "x1_entry"),
            ("测设线点 Y:", "y1_entry"),
            ("衬砌中线点 X:", "x2_entry"),
            ("衬砌中线点 Y:", "y2_entry"),
            ("设计高程(m):", "elevation_entry")
        ]
        for label, name in points:
            row = ttk.Frame(frame)
            row.pack(fill=tk.X, pady=2)
            ttk.Label(row, text=label).pack(side=tk.LEFT)
            entry = ttk.Entry(row)
            entry.pack(side=tk.LEFT, fill=tk.X, expand=True, padx=5)
            setattr(self, name, entry)

        ttk.Label(frame, text="批量输入(距离,高差):").pack(anchor=tk.W, pady=(5, 0))
        self.bulk_entry = tk.Text(frame, height=4, width=30)
        self.bulk_entry.pack(fill=tk.X, pady=2)

        btn_frame = ttk.Frame(frame)
        btn_frame.pack(fill=tk.X, pady=(5, 0))
        ttk.Button(btn_frame, text="加载CSV", command=self.load_csv).pack(side=tk.LEFT, padx=2)
        ttk.Button(btn_frame, text="计算待测点", command=self.calculate_points).pack(side=tk.LEFT, padx=2)

    def create_result_sections(self, parent):
        """创建结果展示区域"""
        notebook = ttk.Notebook(parent)
        notebook.pack(fill=tk.BOTH, expand=True)

        coord_frame = ttk.Frame(notebook)
        notebook.add(coord_frame, text="坐标结果")

        # 增加备注列
        columns = ("角度", "半径", "X,Y坐标", "X',Y'坐标", "备注")
        self.tree = ttk.Treeview(coord_frame, columns=columns, show="headings")
        for col in columns:
            self.tree.heading(col, text=col)
            self.tree.column(col, width=120, anchor=tk.CENTER)
        vsb = ttk.Scrollbar(coord_frame, orient="vertical", command=self.tree.yview)
        hsb = ttk.Scrollbar(coord_frame, orient="horizontal", command=self.tree.xview)
        self.tree.configure(yscrollcommand=vsb.set, xscrollcommand=hsb.set)
        self.tree.grid(row=0, column=0, sticky="nsew")
        vsb.grid(row=0, column=1, sticky="ns")
        hsb.grid(row=1, column=0, sticky="ew")
        coord_frame.grid_rowconfigure(0, weight=1)
        coord_frame.grid_columnconfigure(0, weight=1)

        btn_frame = ttk.Frame(coord_frame)
        btn_frame.grid(row=2, column=0, columnspan=2, sticky="ew", pady=(5, 0))
        ttk.Button(btn_frame, text="保存CSV", command=self.save_results_csv).pack(side=tk.LEFT, padx=2)
        ttk.Button(btn_frame, text="清空结果", command=self.clear_results).pack(side=tk.LEFT, padx=2)

        points_frame = ttk.Frame(notebook)
        notebook.add(points_frame, text="待测点结果")

        self.points_text = tk.Text(points_frame, height=15, state='disabled')
        scrollbar = ttk.Scrollbar(points_frame, command=self.points_text.yview)
        self.points_text.configure(yscrollcommand=scrollbar.set)
        self.points_text.pack(side=tk.TOP, fill=tk.BOTH, expand=True)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)

        btn_frame_points = ttk.Frame(points_frame)
        btn_frame_points.pack(side=tk.BOTTOM, fill=tk.X, pady=(5, 0))
        ttk.Button(btn_frame_points, text="保存CSV", command=self.save_points_csv).pack(side=tk.LEFT, padx=2)
        ttk.Button(btn_frame_points, text="清空结果", command=self.clear_points_results).pack(side=tk.LEFT, padx=2)

    def toggle_single_mode(self, mode):
        """切换正洞计算模式"""
        if mode == "随机值":
            self.base_radius_frame.pack(fill=tk.X, pady=2)
            self.radius_file_frame.pack_forget()
        else:
            self.base_radius_frame.pack_forget()
            self.radius_file_frame.pack(fill=tk.X, pady=2)

    def toggle_multi_mode(self, mode):
        """切换多半径计算模式"""
        if mode == "随机值":
            self.radius_type_var.set("5jt")
            self.multi_file_frame.pack_forget()
        else:
            self.multi_file_frame.pack(fill=tk.X, pady=2)

    def browse_radius_file(self):
        """浏览半径数据文件"""
        filename = filedialog.askopenfilename(filetypes=[("CSV文件", "*.csv")])
        if filename:
            self.radius_file_entry.delete(0, tk.END)
            self.radius_file_entry.insert(0, filename)

    def browse_multi_file(self):
        """浏览多半径数据文件"""
        filename = filedialog.askopenfilename(filetypes=[("CSV文件", "*.csv")])
        if filename:
            self.multi_file_entry.delete(0, tk.END)
            self.multi_file_entry.insert(0, filename)

    def load_csv(self):
        """加载CSV文件"""
        file_path = filedialog.askopenfilename(filetypes=[("CSV文件", "*.csv")])
        if not file_path:
            return

        try:
            with open(file_path, newline='') as csvfile:
                points = []
                for row in csv.reader(csvfile):
                    if len(row) != 2:
                        raise ValueError("每行必须包含两个坐标值")
                    points.append((float(row[0]), float(row[1])))

                if len(points) < 2:
                    raise ValueError("需要至少两个点")

                self.x1_entry.delete(0, tk.END)
                self.y1_entry.delete(0, tk.END)
                self.x1_entry.insert(0, points[0][0])
                self.y1_entry.insert(0, points[0][1])

                self.x2_entry.delete(0, tk.END)
                self.y2_entry.delete(0, tk.END)
                self.x2_entry.insert(0, points[1][0])
                self.y2_entry.insert(0, points[1][1])
        except Exception as e:
            messagebox.showerror("错误", str(e))

    def calculate_single(self):
        """执行正洞计算"""
        try:
            x_offset = Utils.validate_float(self.x_offset_entry.get(), "X偏移量")
            y_offset = Utils.validate_float(self.y_offset_entry.get(), "Y偏移量")
            point_count = int(self.points_entry.get())
            
            self.single_calc.x_offset = x_offset
            self.single_calc.y_offset = y_offset
            
            if self.single_mode_var.get() == "随机值":
                base_radius = Utils.validate_float(self.base_radius_entry.get(), "基础半径")
                results = self.single_calc.calculate(base_radius, point_count)
            else:
                filename = self.radius_file_entry.get()
                if not filename:
                    raise ValueError("请选择半径数据文件")
                
                with open(filename, 'r') as f:
                    radius_values = [float(row[0]) for row in csv.reader(f) if row]
                
                results = self.single_calc.calculate_fixed(radius_values)
            
            self.tree.delete(*self.tree.get_children())
            for row in results:
                self.tree.insert("", tk.END, values=(
                    f"{row[0]:.2f}", 
                    f"{row[1]:.4f}", 
                    row[2],
                    row[3],
                    row[4]  # 备注信息
                ))
        except Exception as e:
            messagebox.showerror("计算错误", str(e))

    def calculate_multi(self):
        """执行紧急停车带计算"""
        try:
            x_offset = Utils.validate_float(self.multi_x_offset_entry.get(), "X偏移量")
            y_offset = Utils.validate_float(self.multi_y_offset_entry.get(), "Y偏移量")
            
            self.multi_calc.x_offset = x_offset
            self.multi_calc.y_offset = y_offset
            
            if self.multi_mode_var.get() == "随机值":
                radius_type = self.radius_type_var.get()
                results = self.multi_calc.calculate(radius_type)
            else:
                filename = self.multi_file_entry.get()
                if not filename:
                    raise ValueError("请选择半径数据文件")
                
                with open(filename, 'r') as f:
                    radius_values = [float(row[0]) for row in csv.reader(f) if row]
                
                results = self.multi_calc.calculate_fixed(radius_values)
            
            self.tree.delete(*self.tree.get_children())
            for row in results:
                self.tree.insert("", tk.END, values=(
                    f"{row[0]:.2f}", 
                    f"{row[1]:.4f}", 
                    row[2],
                    row[3],
                    row[4]  # 备注信息
                ))
        except Exception as e:
            messagebox.showerror("计算错误", str(e))

    def calculate_points(self):
        """计算待测点"""
        try:
            x1 = Utils.validate_float(self.x1_entry.get(), "X1")
            y1 = Utils.validate_float(self.y1_entry.get(), "Y1")
            x2 = Utils.validate_float(self.x2_entry.get(), "X2")
            y2 = Utils.validate_float(self.y2_entry.get(), "Y2")
            elevation = Utils.validate_float(self.elevation_entry.get(), "高程")
            
            L = math.hypot(x2 - x1, y2 - y1)
            if L == 0:
                raise ValueError("点1和点2重合")

            bulk_data = self.bulk_entry.get("1.0", tk.END).strip()
            if not bulk_data:
                raise ValueError("请输入批量距离和高差数据")

            data_pairs = []
            for line in bulk_data.split('\n'):
                if line.strip():
                    parts = line.split(',')
                    if len(parts) != 2:
                        raise ValueError("每行必须包含两个值（距离,高差）")
                    d = Utils.validate_float(parts[0], "距离")
                    height_diff = Utils.validate_float(parts[1], "高差")
                    data_pairs.append((d, height_diff))

            results = []
            for d, height_diff in data_pairs:
                t = d / L
                x = x1 + t * (x2 - x1)
                y = y1 + t * (y2 - y1)
                results.append((x, y, elevation + height_diff))

            self.points_text.config(state='normal')
            self.points_text.delete(1.0, tk.END)
            result_str = "\n".join(
                f"待测点 {i+1}: X={x:.3f}m, Y={y:.3f}m, 高程={e:.3f}m" 
                for i, (x, y, e) in enumerate(results))
            self.points_text.insert(tk.END, result_str)
            self.points_text.config(state='disabled')
        except Exception as e:
            messagebox.showerror("输入错误", str(e))

    def save_results_csv(self):
        """保存计算结果到CSV"""
        if not self.tree.get_children():
            messagebox.showwarning("无数据", "没有计算结果可保存")
            return
        
        try:
            filename = filedialog.asksaveasfilename(
                defaultextension=".csv", 
                filetypes=[("CSV文件", "*.csv")], 
                title="保存计算结果")
            
            if not filename:
                return
            
            with open(filename, 'w', newline='', encoding='ANSI') as f:
                writer = csv.writer(f)
                writer.writerow([self.tree.heading(col)['text'] for col in self.tree['columns']])
                for item in self.tree.get_children():
                    writer.writerow(self.tree.item(item)['values'])
            
            messagebox.showinfo("保存成功", f"结果已保存到 {filename}")
        except Exception as e:
            messagebox.showerror("保存错误", str(e))

    def save_points_csv(self):
        """保存待测点结果到CSV"""
        if not self.points_text.get("1.0", tk.END).strip():
            messagebox.showwarning("无数据", "没有待测点结果可保存")
            return
        
        try:
            filename = filedialog.asksaveasfilename(
                defaultextension=".csv",
                filetypes=[("CSV文件", "*.csv")],
                title="保存待测点结果")
            
            if not filename:
                return
            
            self.points_text.config(state='normal')
            content = self.points_text.get("1.0", tk.END).strip()
            self.points_text.config(state='disabled')
            
            lines = content.split('\n')
            data = []
            for line in lines:
                parts = line.split(':')
                if len(parts) != 2:
                    continue
                
                point_info = parts[1].strip()
                x_part = point_info.split('X=')[1].split('m')[0]
                y_part = point_info.split('Y=')[1].split('m')[0]
                e_part = point_info.split('高程=')[1].split('m')[0]
                
                data.append([
                    float(x_part.strip()),
                    float(y_part.strip()),
                    float(e_part.strip())
                ])
            
            with open(filename, 'w', newline='', encoding='ANSI') as f:
                writer = csv.writer(f)
                writer.writerow(["X坐标(m)", "Y坐标(m)", "高程(m)"])
                writer.writerows(data)
            
            messagebox.showinfo("保存成功", f"待测点结果已保存到 {filename}")
        except Exception as e:
            messagebox.showerror("保存错误", str(e))

    def clear_results(self):
        """清空坐标计算结果"""
        self.tree.delete(*self.tree.get_children())

    def clear_points_results(self):
        """清空待测点结果"""
        self.points_text.config(state='normal')
        self.points_text.delete(1.0, tk.END)
        self.points_text.config(state='disabled')

# 主程序
if __name__ == "__main__":
    root = tk.Tk()
    app = TunnelCalculator(root)
    root.mainloop()
