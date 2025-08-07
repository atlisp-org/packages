import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import math
import csv

# ====================== 通用工具函数 ======================
def dms_to_deg(dms_str):
    """将度分秒字符串转换为十进制角度"""
    if not dms_str:
        return None

    dms_str = dms_str.replace('°', ' ').replace('′', ' ').replace("'", ' ') \
                     .replace('″', ' ').replace('"', ' ')
    parts = dms_str.split()

    degrees = minutes = seconds = 0.0
    try:
        if len(parts) >= 1:
            degrees = float(parts[0])
        if len(parts) >= 2:
            minutes = float(parts[1])
        if len(parts) >= 3:
            seconds = float(parts[2])
    except ValueError:
        return None

    total_deg = degrees + minutes / 60 + seconds / 3600
    return total_deg

def deg_to_dms(deg, decimal_places=1):
    """将十进制角度转换为度分秒字符串"""
    sign = '' if deg >= 0 else '-'
    deg = abs(deg)
    degrees = int(deg)
    fractional = deg - degrees
    minutes = int(fractional * 60)
    seconds = round((fractional * 60 - minutes) * 60, decimal_places)

    if seconds >= 60:
        minutes += 1
        seconds -= 60

    return f"{sign}{degrees}°{minutes}′{seconds:.{decimal_places}f}″"

# ====================== 新版度分秒输入对话框 ======================
class AngleInputDialog(tk.Toplevel):
    def __init__(self, parent, callback):
        super().__init__(parent)
        self.title("度分秒输入辅助")
        self.callback = callback
        self.geometry(f"300x180+{parent.winfo_rootx()+50}+{parent.winfo_rooty()+50}")
        self.resizable(False, False)
        self.transient(parent)
        self.grab_set()

        main_frame = tk.Frame(self, padx=10, pady=10)
        main_frame.pack(fill=tk.BOTH, expand=True)

        labels = ["度:", "分:", "秒:"]
        self.entries = []
        for idx, text in enumerate(labels):
            tk.Label(main_frame, text=text).grid(row=idx, column=0, padx=5, pady=5, sticky="e")
            ent = tk.Entry(main_frame, width=8, justify="center")
            ent.grid(row=idx, column=1, padx=5, pady=5, sticky="w")
            self.entries.append(ent)

        btn_frame = tk.Frame(self, pady=10)
        btn_frame.pack(fill=tk.X)
        tk.Button(btn_frame, text="确定", width=10, bg="#4a6baf", fg="white",
                  command=self.on_confirm).pack(side=tk.LEFT, padx=20)
        tk.Button(btn_frame, text="取消", width=10, bg="#cccccc",
                  command=self.destroy).pack(side=tk.RIGHT, padx=20)

        self.bind("<Return>", lambda e: self.on_confirm())
        self.entries[0].focus_set()

    def on_confirm(self):
        try:
            deg = float(self.entries[0].get())
            mins = float(self.entries[1].get())
            secs = float(self.entries[2].get())

            if not (0 <= mins < 60) or not (0 <= secs < 60):
                raise ValueError("分/秒必须在 0–60 之间")

            result = f"{int(deg)}°{int(mins)}′{secs:.2f}″"
            self.callback(result)
            self.destroy()

        except ValueError as e:
            messagebox.showerror("输入错误", f"请输入有效的角度值:\n{e}", parent=self)

# ====================== 带滚动条的表格组件 ======================
class ScrollableTable(tk.Frame):
    def __init__(self, parent, headers, col_widths, editable_cols=()):
        super().__init__(parent)
        self.headers = headers
        self.col_widths = col_widths
        self.editable_cols = editable_cols
        self.rows = 0
        self.cells = []

        # 表头
        header_frame = tk.Frame(self)
        header_frame.pack(fill=tk.X)

        for col, (header, width) in enumerate(zip(headers, col_widths)):
            tk.Label(
                header_frame, text=header, width=width,
                relief="ridge", bg="#3F51B5", fg="white", font=("Arial", 10, "bold")
            ).grid(row=0, column=col, padx=1, pady=1, sticky="nsew")

        # 可滚动区域
        container = tk.Frame(self)
        container.pack(fill=tk.BOTH, expand=True)

        self.canvas = tk.Canvas(container)
        scrollbar = ttk.Scrollbar(container, orient="vertical", command=self.canvas.yview)
        scrollable_frame = tk.Frame(self.canvas)

        scrollable_frame.bind(
            "<Configure>",
            lambda e: self.canvas.configure(scrollregion=self.canvas.bbox("all"))
        )

        self.canvas.create_window((0, 0), window=scrollable_frame, anchor="nw")
        self.canvas.configure(yscrollcommand=scrollbar.set)
        self.canvas.pack(side="left", fill="both", expand=True)
        scrollbar.pack(side="right", fill="y")

        self.inner_frame = scrollable_frame

    def add_row(self, defaults=None):
        if defaults is None:
            defaults = [""] * len(self.headers)

        row_cells = []
        for col, width in enumerate(self.col_widths):
            if col in self.editable_cols:
                cell = tk.Entry(self.inner_frame, width=width, justify="center", relief="sunken")
                cell.insert(0, defaults[col])
            else:
                cell = tk.Label(self.inner_frame, width=width, relief="sunken", text=defaults[col])

            cell.grid(row=self.rows, column=col, padx=1, pady=1, sticky="nsew")
            row_cells.append(cell)

        self.rows += 1
        self.cells.append(row_cells)
        return row_cells

    def clear(self):
        for row in self.cells:
            for cell in row:
                cell.destroy()
        self.cells = []
        self.rows = 0

    def get_values(self):
        return [
            [cell.get() if isinstance(cell, tk.Entry) else cell.cget("text") for cell in row]
            for row in self.cells
        ]

# ====================== 主应用 ======================
class SurveyCalculator(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("导线计算工具")
        self.geometry("1200x800")
        self.configure(bg="#f5f5f5")

        self.notebook = ttk.Notebook(self)
        self.notebook.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)

        self.create_elevation_tab()
        self.create_leveling_tab()
        self.create_traverse_tab()

        self.status_var = tk.StringVar(value="就绪 | 导线计算工具 v1.0")
        status_bar = tk.Label(self, textvariable=self.status_var, relief=tk.SUNKEN,
                              anchor=tk.W, bg="#e0e0e0", font=("Arial", 9))
        status_bar.pack(fill=tk.X, side=tk.BOTTOM)

        # 绑定度分秒右键输入
        attach_angle_popup(self.elevation_table, [6, 7])
        attach_angle_popup(self.traverse_table,   [3, 4])

    # ---------- 三角高程 ----------
    def create_elevation_tab(self):
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="三角高程测量")

        tk.Label(tab, text="三角高程测量计算", font=("Arial", 14, "bold"), bg="#f5f5f5").pack(pady=(10, 5))

        control_frame = tk.Frame(tab, bg="#f5f5f5")
        control_frame.pack(fill=tk.X, padx=10, pady=5)

        tk.Label(control_frame, text="每组测回数:", bg="#f5f5f5").pack(side=tk.LEFT, padx=5)
        self.rounds_var = tk.StringVar(value="2")
        ttk.OptionMenu(control_frame, self.rounds_var, "4", "1", "2", "3", "4", "5", "6").pack(side=tk.LEFT, padx=5)

        btn_frame = tk.Frame(control_frame, bg="#f5f5f5")
        btn_frame.pack(side=tk.RIGHT, padx=10)

        for text, cmd in [("添加组", self.add_elevation_group), ("计算", self.calculate_elevation),
                          ("清空", self.clear_elevation), ("导入CSV", lambda: self.import_csv("elevation")),
                          ("导出", lambda: self.export_data("elevation"))]:
            tk.Button(btn_frame, text=text, width=10, bg="#4CAF50", fg="white", command=cmd).pack(side=tk.LEFT, padx=2)

        headers = ["测站", "照准点", "测回", "仪器高(m)", "觇标高(m)", "平距(m)",
                   "盘左读数", "盘右读数", "指标差", "竖直角", "测回高差(m)",
                   "平均竖直角", "平均高差(m)", "两点高差(m)"]
        widths = [8, 8, 6, 8, 8, 8, 10, 10, 8, 10, 10, 10, 10, 10]
        self.elevation_table = ScrollableTable(tab, headers, widths, editable_cols=(0, 1, 3, 4, 5, 6, 7))
        self.elevation_table.pack(fill=tk.BOTH, expand=True, padx=10, pady=5)

    def add_elevation_group(self):
        rounds = int(self.rounds_var.get())
        group_num = len(self.elevation_table.cells) // rounds + 1
        for i in range(rounds):
            row = self.elevation_table.add_row()
            if i == 0:
                row[0].insert(0, f"L{group_num}")
                row[1].insert(0, f"NY{group_num}")
            row[2].config(text=f"第{i + 1}测回")

    def calculate_elevation(self):
        try:
            self.status_var.set("计算中...")
            self.update_idletasks()

            rounds = int(self.rounds_var.get())
            rows = self.elevation_table.cells
            total_rows = len(rows)
            if total_rows == 0:
                messagebox.showwarning("警告", "没有可计算的数据")
                return

            group_results = []
            for group_start in range(0, total_rows, rounds):
                group_rows = rows[group_start: group_start + rounds]
                instr_height = float(group_rows[0][3].get() or 0)
                target_height = float(group_rows[0][4].get() or 0)

                angles = []
                diffs = []
                for i, row in enumerate(group_rows):
                    distance = float(row[5].get() or 0)
                    left_deg = dms_to_deg(row[6].get())
                    right_deg = dms_to_deg(row[7].get())
                    if left_deg is None or right_deg is None:
                        raise ValueError(f"第{group_start + i + 1}行角度数据无效")

                    index_error = (left_deg + right_deg - 360) / 2 * 3600
                    vertical_angle = (90 - left_deg + right_deg - 270) / 2
                    height_diff = (distance * math.tan(math.radians(vertical_angle))
                                   + instr_height - target_height)

                    row[8].config(text=f"{index_error:.1f}″")
                    row[9].config(text=deg_to_dms(vertical_angle))
                    row[10].config(text=f"{height_diff:.4f}")
                    angles.append(vertical_angle)
                    diffs.append(height_diff)

                if angles and diffs:
                    avg_angle = sum(angles) / len(angles)
                    avg_diff = sum(diffs) / len(diffs)
                    group_rows[0][11].config(text=deg_to_dms(avg_angle))
                    group_rows[0][12].config(text=f"{avg_diff:.4f}")
                    group_results.append(avg_diff)

            for group_start in range(0, total_rows, rounds):
                group_idx = group_start // rounds
                if group_idx < len(group_results) - 1:
                    diff = group_results[group_idx + 1] - group_results[group_idx]
                    rows[group_start][13].config(text=f"{diff:.4f}")

            self.status_var.set("计算完成 | 三角高程测量")
        except Exception as e:
            messagebox.showerror("计算错误", f"计算过程中发生错误:\n{str(e)}")
            self.status_var.set(f"错误 | {str(e)}")

    def clear_elevation(self):
        self.elevation_table.clear()
        self.status_var.set("已清空三角高程数据")

    # ---------- 水准测量 ----------
    def create_leveling_tab(self):
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="水准测量平差")

        tk.Label(tab, text="水准测量平差计算", font=("Arial", 14, "bold"), bg="#f5f5f5").pack(pady=(10, 5))

        control_frame = tk.Frame(tab, bg="#f5f5f5")
        control_frame.pack(fill=tk.X, padx=10, pady=5)

        input_frame = tk.Frame(control_frame, bg="#f5f5f5")
        input_frame.pack(side=tk.LEFT, padx=10)

        tk.Label(input_frame, text="起点高程(m):", bg="#f5f5f5").grid(row=0, column=0, padx=5, sticky="e")
        self.start_elev = tk.Entry(input_frame, width=10)
        self.start_elev.grid(row=0, column=1, padx=5)

        tk.Label(input_frame, text="终点高程(m):", bg="#f5f5f5").grid(row=0, column=2, padx=5, sticky="e")
        self.end_elev = tk.Entry(input_frame, width=10)
        self.end_elev.grid(row=0, column=3, padx=5)

        tk.Label(input_frame, text="水准等级:", bg="#f5f5f5").grid(row=0, column=4, padx=5, sticky="e")
        self.level_var = tk.StringVar(value="三等")
        ttk.OptionMenu(input_frame, self.level_var, "三等", "三等", "四等").grid(row=0, column=5, padx=5)

        btn_frame = tk.Frame(control_frame, bg="#f5f5f5")
        btn_frame.pack(side=tk.RIGHT, padx=10)
        for text, cmd in [("添加点", self.add_leveling_point), ("计算", self.calculate_leveling),
                          ("清空", self.clear_leveling), ("导入CSV", lambda: self.import_csv("leveling")),
                          ("导出", lambda: self.export_data("leveling"))]:
            tk.Button(btn_frame, text=text, width=10, bg="#4CAF50", fg="white", command=cmd).pack(side=tk.LEFT, padx=2)

        headers = ["点号", "距离(m)", "观测高差(m)", "改正数(mm)", "改正后高差(m)", "高程(m)"]
        widths = [10, 10, 12, 10, 12, 12]
        self.leveling_table = ScrollableTable(tab, headers, widths, editable_cols=(0, 1, 2))
        self.leveling_table.pack(fill=tk.BOTH, expand=True, padx=10, pady=5)

    def add_leveling_point(self):
        row = self.leveling_table.add_row()
        if len(self.leveling_table.cells) == 1:
            self.start_elev.insert(0, "0.000")

    def calculate_leveling(self):
        try:
            self.status_var.set("计算中...")
            self.update_idletasks()

            start_elev = float(self.start_elev.get() or 0)
            end_elev = float(self.end_elev.get() or 0)
            level = self.level_var.get()

            rows = self.leveling_table.cells
            if not rows:
                raise ValueError("没有输入数据")

            total_dist = total_obs = 0
            for row in rows:
                total_dist += float(row[1].get() or 0)
                total_obs += float(row[2].get() or 0)

            fh = total_obs - (end_elev - start_elev)
            fh_allow = 12 * math.sqrt(total_dist / 1000) if level == "三等" else 20 * math.sqrt(total_dist / 1000)

            current_elev = start_elev
            rows[0][5].config(text=f"{current_elev:.3f}")

            for i, row in enumerate(rows):
                if i == 0:
                    continue
                dist = float(row[1].get() or 0)
                obs = float(row[2].get() or 0)
                correction = -fh * dist / total_dist
                corrected_obs = obs + correction
                current_elev += corrected_obs
                row[3].config(text=f"{correction * 1000:.1f}")
                row[4].config(text=f"{corrected_obs:.4f}")
                row[5].config(text=f"{current_elev:.3f}")

            result = (f"闭合差: {fh * 1000:.1f} mm\n"
                      f"允许闭合差: ±{fh_allow:.1f} mm\n"
                      f"{'满足要求' if abs(fh * 1000) <= fh_allow else '不满足要求'}")
            self.status_var.set(f"计算完成 | 闭合差: {fh * 1000:.1f}mm (允许: ±{fh_allow:.1f}mm)")
            messagebox.showinfo("计算结果", result)
        except Exception as e:
            messagebox.showerror("计算错误", f"计算过程中发生错误:\n{str(e)}")
            self.status_var.set(f"错误 | {str(e)}")

    def clear_leveling(self):
        self.leveling_table.clear()
        self.start_elev.delete(0, tk.END)
        self.end_elev.delete(0, tk.END)
        self.status_var.set("已清空水准测量数据")

    # ---------- 导线观测 ----------
    def create_traverse_tab(self):
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="导线观测计算")

        tk.Label(tab, text="导线观测计算", font=("Arial", 14, "bold"), bg="#f5f5f5").pack(pady=(10, 5))

        control_frame = tk.Frame(tab, bg="#f5f5f5")
        control_frame.pack(fill=tk.X, padx=10, pady=5)

        tk.Label(control_frame, text="每组测回数:", bg="#f5f5f5").pack(side=tk.LEFT, padx=5)
        self.traverse_rounds_var = tk.StringVar(value="4")
        ttk.OptionMenu(control_frame, self.traverse_rounds_var, "4", "2", "3", "4", "5", "6").pack(side=tk.LEFT, padx=5)

        btn_frame = tk.Frame(control_frame, bg="#f5f5f5")
        btn_frame.pack(side=tk.RIGHT, padx=10)
        for text, cmd in [("添加组", self.add_traverse_group), ("计算", self.calculate_traverse),
                          ("清空", self.clear_traverse), ("导入CSV", lambda: self.import_csv("traverse")),
                          ("导出", lambda: self.export_data("traverse"))]:
            tk.Button(btn_frame, text=text, width=10, bg="#4CAF50", fg="white", command=cmd).pack(side=tk.LEFT, padx=2)

        headers = ["测站", "测回", "目标", "盘左读数", "盘右读数", "2C", "半测回角值", "一测回角值", "各测回平均值"]
        widths = [8, 6, 8, 10, 10, 8, 12, 12, 12]
        self.traverse_table = ScrollableTable(tab, headers, widths, editable_cols=(0, 2, 3, 4))
        self.traverse_table.pack(fill=tk.BOTH, expand=True, padx=10, pady=5)

    def add_traverse_group(self):
        rounds = int(self.traverse_rounds_var.get())
        group_num = len(self.traverse_table.cells) // (2 * rounds) + 1
        for i in range(rounds):
            back_row = self.traverse_table.add_row()
            back_row[0].insert(0, f"测站{group_num}")
            back_row[1].config(text=f"第{i + 1}测回")
            back_row[2].insert(0, f"后视{i + 1}")
            front_row = self.traverse_table.add_row()
            front_row[2].insert(0, f"前视{i + 1}")

    def calculate_traverse(self):
        try:
            self.status_var.set("计算中...")
            self.update_idletasks()

            rounds = int(self.traverse_rounds_var.get())
            rows = self.traverse_table.cells
            total_rows = len(rows)
            if total_rows % (2 * rounds) != 0:
                messagebox.showerror('错误', f'数据行数({total_rows})必须是测回数({rounds})的两倍的整数倍！')
                self.status_var.set("错误 | 数据行数与测回数不匹配")
                return

            for group_start in range(0, total_rows, 2 * rounds):
                group_angles = []
                for round_idx in range(rounds):
                    back_idx = group_start + 2 * round_idx
                    front_idx = back_idx + 1
                    back_row, front_row = rows[back_idx], rows[front_idx]

                    left_a = dms_to_deg(back_row[3].get())
                    right_a = dms_to_deg(back_row[4].get())
                    left_b = dms_to_deg(front_row[3].get())
                    right_b = dms_to_deg(front_row[4].get())
                    if None in (left_a, right_a, left_b, right_b):
                        raise ValueError(f"第{back_idx + 1}-{front_idx + 1}行角度数据无效")

                    c2_a = abs(left_a - (right_a - 180)) if right_a > 180 else abs(left_a - (right_a + 180))
                    c2_b = abs(left_b - (right_b - 180)) if right_b > 180 else abs(left_b - (right_b + 180))

                    half1 = left_b - left_a
                    if half1 < 0:
                        half1 += 360
                    half2 = right_b - right_a
                    if half2 < 0:
                        half2 += 360
                    angle = (half1 + half2) / 2
                    group_angles.append(angle)

                    back_row[5].config(text=f"{c2_a * 3600:.1f}″")
                    front_row[5].config(text=f"{c2_b * 3600:.1f}″")
                    back_row[6].config(text=deg_to_dms(half1))
                    front_row[6].config(text=deg_to_dms(half2))
                    back_row[7].config(text=deg_to_dms(angle))

                if group_angles:
                    avg = sum(group_angles) / len(group_angles)
                    rows[group_start][8].config(text=deg_to_dms(avg))

            self.status_var.set("计算完成 | 导线观测计算")
        except Exception as e:
            messagebox.showerror("计算错误", f"计算过程中发生错误:\n{str(e)}")
            self.status_var.set(f"错误 | {str(e)}")

    def clear_traverse(self):
        self.traverse_table.clear()
        self.status_var.set("已清空导线观测数据")

    # ---------- 通用 ----------
    def import_csv(self, tab_name):
        file_path = filedialog.askopenfilename(filetypes=[("CSV文件", "*.csv")])
        if not file_path:
            return
        try:
            table = {"elevation": self.elevation_table, "leveling": self.leveling_table, "traverse": self.traverse_table}[tab_name]
            with open(file_path, "r", encoding="ANSI") as file:
                reader = csv.reader(file)
                next(reader)
                table.clear()
                for row in reader:
                    table.add_row(row)
            self.status_var.set(f"成功导入 {len(table.cells)} 行数据")
        except Exception as e:
            messagebox.showerror("导入错误", f"导入CSV文件时出错:\n{str(e)}")
            self.status_var.set(f"导入失败 | {str(e)}")

    def export_data(self, tab_name):
        file_path = filedialog.asksaveasfilename(defaultextension=".csv",
                                                 filetypes=[("CSV文件", "*.csv"), ("文本文件", "*.txt")])
        if not file_path:
            return
        try:
            table = {"elevation": self.elevation_table, "leveling": self.leveling_table, "traverse": self.traverse_table}[tab_name]
            headers = table.headers
            rows = table.get_values()
            with open(file_path, "w", encoding="ANSI", newline="") as file:
                writer = csv.writer(file)
                writer.writerow(headers)
                writer.writerows(rows)
            self.status_var.set(f"数据已导出到: {file_path}")
            messagebox.showinfo("导出成功", f"{tab_name}数据已成功导出")
        except Exception as e:
            messagebox.showerror("导出错误", f"导出数据时出错:\n{str(e)}")
            self.status_var.set(f"导出失败 | {str(e)}")

# ====================== 度分秒右键绑定函数 ======================
def attach_angle_popup(table, col_list):
    """
    为指定表格的指定列绑定右键度分秒输入
    :param table: ScrollableTable 实例
    :param col_list: 需要绑定的列索引列表
    """
    def _on_right_click(event, entry):
        def _callback(dms_str):
            entry.delete(0, tk.END)
            entry.insert(0, dms_str)

        AngleInputDialog(table, _callback)

    # 重写 add_row，使新增行也自动绑定右键
    orig_add_row = table.add_row

    def add_row_wrapper(defaults=None):
        new_cells = orig_add_row(defaults)
        for col in col_list:
            if isinstance(new_cells[col], tk.Entry):
                new_cells[col].bind("<Button-3>",
                                    lambda e, ent=new_cells[col]: _on_right_click(e, ent))
        return new_cells

    table.add_row = add_row_wrapper

    # 为现有行绑定
    for row_cells in table.cells:
        for col in col_list:
            if isinstance(row_cells[col], tk.Entry):
                row_cells[col].bind("<Button-3>",
                                    lambda e, ent=row_cells[col]: _on_right_click(e, ent))

# ====================== 入口 ======================
if __name__ == "__main__":
    app = SurveyCalculator()
    app.mainloop()
