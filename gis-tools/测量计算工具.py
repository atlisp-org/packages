import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import math
import csv
import os

class MeasureToolApp:
    def __init__(self, master):
        self.master = master
        master.title("测量计算工具")
        master.geometry("900x700")
        master.resizable(True, True)

        try:
            master.iconbitmap("measure_icon.ico")
        except:
            pass

        main = ttk.Frame(master, padding=10)
        main.pack(fill='both', expand=True)

        ttk.Label(main, text="测量计算工具", font=('Arial', 14, 'bold'), foreground='#0066CC').pack(pady=10)

        self.nb = ttk.Notebook(main)
        self.nb.pack(fill='both', expand=True, padx=10, pady=5)

        self.status = tk.StringVar(value="就绪")
        status_bar = ttk.Label(main, textvariable=self.status, relief='sunken', anchor='w', padding=3)
        status_bar.pack(fill='x', pady=(5, 0))

        self.build_coord_tab()
        self.build_geom_tab()
        self.build_level_tab()
        self.build_area_tab()

    # ==================== 坐标计算 ====================
    def build_coord_tab(self):
        tab = ttk.Frame(self.nb)
        self.nb.add(tab, text='坐标计算')

        op_frame = ttk.LabelFrame(tab, text='计算类型', padding=10)
        op_frame.pack(fill='x', padx=10, pady=5)

        self.coord_op = tk.IntVar(value=1)
        ttk.Radiobutton(op_frame, text='坐标正算', variable=self.coord_op, value=1, command=self.refresh_coord).pack(side=tk.LEFT, padx=10)
        ttk.Radiobutton(op_frame, text='坐标反算', variable=self.coord_op, value=2, command=self.refresh_coord).pack(side=tk.LEFT, padx=10)

        input_frame = ttk.LabelFrame(tab, text='参数输入', padding=10)
        input_frame.pack(fill='both', expand=True, padx=10, pady=5)

        self.coord_in = ttk.Frame(input_frame)
        self.coord_in.pack(fill='both', expand=True)

        result_frame = ttk.LabelFrame(tab, text='计算结果', padding=10)
        result_frame.pack(fill='both', expand=True, padx=10, pady=5)

        self.coord_out = tk.Text(result_frame, height=6, font=('Consolas', 10), wrap='word', bg='#f9f9f9', padx=10, pady=10)
        scroll_y = ttk.Scrollbar(result_frame, command=self.coord_out.yview)
        self.coord_out['yscrollcommand'] = scroll_y.set
        self.coord_out.pack(side=tk.LEFT, fill='both', expand=True)
        scroll_y.pack(side=tk.RIGHT, fill='y')
        self.coord_out.insert('1.0', '计算结果将显示在此处\n')

        btn_frame = ttk.Frame(tab, padding=5)
        btn_frame.pack(pady=5)
        ttk.Button(btn_frame, text='计算', width=10, command=self.calc_coord).pack(side=tk.LEFT, padx=5)
        ttk.Button(btn_frame, text='清空', width=10, command=self.clear_coord_out).pack(side=tk.LEFT, padx=5)

        self.refresh_coord()

    def refresh_coord(self):
        for w in self.coord_in.winfo_children():
            w.destroy()
        self.coord_vars = []
        if self.coord_op.get() == 1:
            labels = ['起点 X:', '起点 Y:', '边长(m):', '方位角:']
            for i, label in enumerate(labels):
                row = ttk.Frame(self.coord_in)
                row.pack(fill='x', pady=5)
                ttk.Label(row, text=label, width=8, anchor='e').pack(side=tk.LEFT, padx=5)
                if label == '方位角:':
                    angle_frame = ttk.Frame(row)
                    angle_frame.pack(side=tk.LEFT, fill='x', expand=True)
                    for unit in ['°', '′', '″']:
                        e = ttk.Entry(angle_frame, width=5)
                        e.pack(side=tk.LEFT, padx=2)
                        ttk.Label(angle_frame, text=unit).pack(side=tk.LEFT)
                        self.coord_vars.append(e)
                else:
                    e = ttk.Entry(row)
                    e.pack(fill='x', expand=True, padx=2)
                    self.coord_vars.append(e)
        else:
            for label in ['A点 X:', 'A点 Y:', 'B点 X:', 'B点 Y:']:
                row = ttk.Frame(self.coord_in)
                row.pack(fill='x', pady=5)
                ttk.Label(row, text=label, width=8, anchor='e').pack(side=tk.LEFT, padx=5)
                e = ttk.Entry(row)
                e.pack(fill='x', expand=True, padx=2)
                self.coord_vars.append(e)

    def calc_coord(self):
        try:
            self.clear_coord_out()
            if self.coord_op.get() == 1:
                x0, y0, d = map(float, [self.coord_vars[i].get() for i in [0, 1, 2]])
                dms = sum(float(self.coord_vars[i].get()) / 60 ** (i - 3) for i in [3, 4, 5])
                rad = math.radians(dms)
                x1 = x0 + d * math.cos(rad)
                y1 = y0 + d * math.sin(rad)
                self.coord_out.insert('end', f"终点坐标:\nX: {x1:.4f}\nY: {y1:.4f}")
                self.status.set("坐标正算完成")
            else:
                ax, ay, bx, by = map(float, [v.get() for v in self.coord_vars])
                dx, dy = bx - ax, by - ay
                dist = math.sqrt(dx ** 2 + dy ** 2)
                az = math.degrees(math.atan2(dy, dx))
                if az < 0:
                    az += 360
                d = int(az)
                m = int((az - d) * 60)
                s = round(((az - d) * 60 - m) * 60, 1)
                self.coord_out.insert('end', f"距离: {dist:.4f} m\n方位角: {d}°{m}′{s}″")
                self.status.set("坐标反算完成")
        except Exception as e:
            messagebox.showerror('错误', str(e))
            self.status.set(f"错误: {str(e)}")

    def clear_coord_out(self):
        self.coord_out.delete('1.0', 'end')
        self.coord_out.insert('1.0', '计算结果将显示在此处\n')

    # ==================== 几何计算 ====================
    def build_geom_tab(self):
        tab = ttk.Frame(self.nb)
        self.nb.add(tab, text='几何计算')

        op_frame = ttk.LabelFrame(tab, text='计算类型', padding=10)
        op_frame.pack(fill='x', padx=10, pady=5)

        self.geom_op = tk.IntVar(value=1)
        options = [('度分秒→十进', 1), ('十进→度分秒', 2), ('圆弧长', 3), ('扇形面积', 4),
                   ('弓形面积', 5), ('圆面积', 6), ('圆周长', 7), ('三角形面积', 8), ('三角函数', 9)]
        for i, (text, val) in enumerate(options):
            ttk.Radiobutton(op_frame, text=text, variable=self.geom_op, value=val, command=self.refresh_geom).grid(
                row=i // 4, column=i % 4, padx=5, pady=3, sticky='w')

        input_frame = ttk.LabelFrame(tab, text='参数输入', padding=10)
        input_frame.pack(fill='both', expand=True, padx=10, pady=5)
        self.geom_in = ttk.Frame(input_frame)
        self.geom_in.pack(fill='both', expand=True)

        result_frame = ttk.LabelFrame(tab, text='计算结果', padding=10)
        result_frame.pack(fill='both', expand=True, padx=10, pady=5)
        self.geom_out = tk.Text(result_frame, height=6, font=('Consolas', 10), wrap='word', bg='#f9f9f9', padx=10, pady=10)
        scroll = ttk.Scrollbar(result_frame, command=self.geom_out.yview)
        self.geom_out['yscrollcommand'] = scroll.set
        self.geom_out.pack(side=tk.LEFT, fill='both', expand=True)
        scroll.pack(side=tk.RIGHT, fill='y')
        self.geom_out.insert('1.0', '计算结果将显示在此处\n')

        btn_frame = ttk.Frame(tab, padding=5)
        btn_frame.pack(pady=5)
        ttk.Button(btn_frame, text='计算', width=10, command=self.calc_geom).pack(side=tk.LEFT, padx=5)
        ttk.Button(btn_frame, text='清空', width=10, command=self.clear_geom_out).pack(side=tk.LEFT, padx=5)

        self.refresh_geom()

    def refresh_geom(self):
        for w in self.geom_in.winfo_children():
            w.destroy()
        self.geom_vars = []
        op = self.geom_op.get()
        if op == 1:
            for label in ['度:', '分:', '秒:']:
                self.add_input_field(label)
        elif op == 2:
            self.add_input_field('十进制度:')
        elif op in (3, 4, 5):
            self.add_input_field('半径(m):')
            self.add_angle_input('角度:')
        elif op in (6, 7):
            self.add_input_field('半径(m):')
        elif op == 8:
            for label in ['边a(m):', '边b(m):', '边c(m):']:
                self.add_input_field(label)
        elif op == 9:
            self.add_angle_input('角度:')

    def add_input_field(self, label, default=''):
        row = ttk.Frame(self.geom_in)
        row.pack(fill='x', pady=5)
        ttk.Label(row, text=label, width=10, anchor='e').pack(side=tk.LEFT, padx=5)
        var = tk.StringVar(value=default)
        ttk.Entry(row, textvariable=var).pack(fill='x', expand=True, padx=2)
        self.geom_vars.append(var)

    def add_angle_input(self, label):
        row = ttk.Frame(self.geom_in)
        row.pack(fill='x', pady=5)
        ttk.Label(row, text=label, width=10, anchor='e').pack(side=tk.LEFT, padx=5)
        angle_frame = ttk.Frame(row)
        angle_frame.pack(side=tk.LEFT, fill='x', expand=True)
        for unit in ['°', '′', '″']:
            var = tk.StringVar()
            ttk.Entry(angle_frame, width=5, textvariable=var).pack(side=tk.LEFT, padx=2)
            ttk.Label(angle_frame, text=unit).pack(side=tk.LEFT)
            self.geom_vars.append(var)

    def calc_geom(self):
        try:
            self.clear_geom_out()
            op = self.geom_op.get()
            if op == 1:
                d, m, s = map(float, [v.get() for v in self.geom_vars])
                self.geom_out.insert('end', f'{d + m/60 + s/3600:.6f}°')
            elif op == 2:
                dec = float(self.geom_vars[0].get())
                d = int(dec)
                m = int((dec - d) * 60)
                s = round(((dec - d) * 60 - m) * 60, 2)
                self.geom_out.insert('end', f'{d}°{m}′{s}″')
            elif op == 3:
                r, d, m, s = map(float, [v.get() for v in self.geom_vars])
                ang = d + m/60 + s/3600
                arc = 2 * math.pi * r * ang / 360
                self.geom_out.insert('end', f'圆弧长度: {arc:.4f} m')
            elif op == 4:
                r, d, m, s = map(float, [v.get() for v in self.geom_vars])
                ang = d + m/60 + s/3600
                area = math.pi * r**2 * ang / 360
                self.geom_out.insert('end', f'扇形面积: {area:.4f} m²')
            elif op == 5:
                r, d, m, s = map(float, [v.get() for v in self.geom_vars])
                ang = math.radians(d + m/60 + s/3600)
                seg = (math.pi * r**2 * math.degrees(ang) / 360) - 0.5 * r**2 * math.sin(ang)
                self.geom_out.insert('end', f'弓形面积: {seg:.4f} m²')
            elif op == 6:
                r = float(self.geom_vars[0].get())
                self.geom_out.insert('end', f'圆面积: {math.pi * r**2:.4f} m²')
            elif op == 7:
                r = float(self.geom_vars[0].get())
                self.geom_out.insert('end', f'圆周长: {2 * math.pi * r:.4f} m')
            elif op == 8:
                a, b, c = map(float, [v.get() for v in self.geom_vars])
                s = (a + b + c) / 2
                area = math.sqrt(s * (s - a) * (s - b) * (s - c))
                self.geom_out.insert('end', f'三角形面积: {area:.4f} m²')
            elif op == 9:
                d, m, s = map(float, [v.get() for v in self.geom_vars])
                ang = math.radians(d + m/60 + s/3600)
                self.geom_out.insert('end', f'sin={math.sin(ang):.6f}\ncos={math.cos(ang):.6f}\ntan={math.tan(ang):.6f}')
            self.status.set("几何计算完成")
        except Exception as e:
            messagebox.showerror('错误', str(e))
            self.status.set(f"错误: {str(e)}")

    def clear_geom_out(self):
        self.geom_out.delete('1.0', 'end')
        self.geom_out.insert('1.0', '计算结果将显示在此处\n')

    # ==================== 水准测量 ====================
    def build_level_tab(self):
        tab = ttk.Frame(self.nb)
        self.nb.add(tab, text='水准测量')

        top = ttk.Frame(tab, padding=5)
        top.pack(fill='x', padx=10, pady=5)
        ttk.Button(top, text='导入CSV', command=self.import_csv).pack(side=tk.LEFT, padx=3)
        ttk.Button(top, text='导出表格', command=self.export_table).pack(side=tk.LEFT, padx=3)
        ttk.Button(top, text='清空表格', command=self.clear_level_table).pack(side=tk.LEFT, padx=3)
        ttk.Button(top, text='加载示例', command=self.load_level_sample).pack(side=tk.LEFT, padx=3)
        ttk.Button(top, text='计算高程', command=self.calc_level).pack(side=tk.RIGHT, padx=3)

        cols = ['测站', '后视(m)', '前视(m)', '高程(m)', '距离(m)', '备注']
        self.level_tree = ttk.Treeview(tab, columns=cols, show='headings', height=12)
        for col, w in zip(cols, [70, 70, 70, 80, 60, 120]):
            self.level_tree.heading(col, text=col)
            self.level_tree.column(col, width=w, anchor='center')
        vsb = ttk.Scrollbar(tab, orient='vertical', command=self.level_tree.yview)
        hsb = ttk.Scrollbar(tab, orient='horizontal', command=self.level_tree.xview)
        self.level_tree.configure(yscrollcommand=vsb.set, xscrollcommand=hsb.set)
        self.level_tree.pack(side=tk.TOP, fill='both', expand=True, padx=10, pady=5)
        vsb.pack(side=tk.RIGHT, fill='y')
        hsb.pack(side=tk.BOTTOM, fill='x')

        menu = tk.Menu(self.level_tree, tearoff=0)
        menu.add_command(label="插入行", command=self.insert_level_row)
        menu.add_command(label="删除行", command=self.delete_level_rows)
        self.level_tree.bind('<Button-3>', lambda e: menu.post(e.x_root, e.y_root))
        self.level_tree.bind('<Double-1>', self.edit_level_cell)

        self.insert_level_row()

    def edit_level_cell(self, event):
        """双击单元格编辑"""
        region = self.level_tree.identify_region(event.x, event.y)
        if region != 'cell':
            return
        col = self.level_tree.identify_column(event.x)
        item = self.level_tree.identify_row(event.y)
        col_index = int(col.replace('#', '')) - 1
        x, y, width, height = self.level_tree.bbox(item, col)
        value = self.level_tree.set(item, col)

        entry = tk.Entry(self.level_tree)
        entry.place(x=x, y=y, width=width, height=height)
        entry.insert(0, value)
        entry.focus_set()
        entry.select_range(0, tk.END)

        def save_edit():
            self.level_tree.set(item, col, entry.get())
            entry.destroy()
        entry.bind('<Return>', lambda e: save_edit())
        entry.bind('<FocusOut>', lambda e: save_edit())
        entry.bind('<Escape>', lambda e: entry.destroy())

    def insert_level_row(self):
        self.level_tree.insert('', 'end', values=("", "", "", "", "", ""))
        self.status.set("已插入新行")

    def delete_level_rows(self):
        for item in self.level_tree.selection():
            self.level_tree.delete(item)
        self.status.set("已删除选中行")

    def calc_level(self):
        try:
            items = self.level_tree.get_children()
            if not items:
                raise ValueError('表格中没有数据')
            values = list(self.level_tree.item(items[0], 'values'))
            if not values[3]:
                raise ValueError('首行需输入起始高程')
            start_elev = float(values[3])
            values[3] = f"{start_elev:.3f}"
            self.level_tree.item(items[0], values=values)

            prev_bs = float(values[1] or 0)
            curr_elev = start_elev
            for i in range(1, len(items)):
                values = list(self.level_tree.item(items[i], 'values'))
                fs = float(values[2] or 0)
                curr_elev += prev_bs - fs
                values[3] = f"{curr_elev:.3f}"
                self.level_tree.item(items[i], values=values)
                prev_bs = float(values[1] or 0)
            self.status.set("高程计算完成")
            messagebox.showinfo('完成', '高程计算完毕')
        except Exception as e:
            messagebox.showerror('错误', str(e))
            self.status.set(f"错误: {str(e)}")

    def import_csv(self):
        try:
            path = filedialog.askopenfilename(filetypes=[("CSV文件", "*.csv")])
            if not path:
                return
            with open(path, newline='', encoding='ANSI') as f:
                reader = csv.reader(f)
                next(reader, None)
                self.clear_level_table()
                for row in reader:
                    row += [''] * (6 - len(row))
                    self.level_tree.insert('', 'end', values=row[:6])
            self.status.set(f"已导入 {len(self.level_tree.get_children())} 条数据")
        except Exception as e:
            messagebox.showerror('导入错误', str(e))

    def export_table(self):
        if not self.level_tree.get_children():
            messagebox.showwarning('提示', '表格中没有数据可导出')
            return
        try:
            path = filedialog.asksaveasfilename(defaultextension=".csv", filetypes=[("CSV", "*.csv")])
            if not path:
                return
            with open(path, 'w', newline='', encoding='ANSI') as f:
                writer = csv.writer(f)
                headers = [self.level_tree.heading(c)['text'] for c in self.level_tree['columns']]
                writer.writerow(headers)
                for item in self.level_tree.get_children():
                    writer.writerow(self.level_tree.item(item, 'values'))
            self.status.set(f"已导出到 {path}")
            messagebox.showinfo('导出成功', f'数据已导出到:\n{path}')
        except Exception as e:
            messagebox.showerror('导出错误', str(e))

    def clear_level_table(self):
        for item in self.level_tree.get_children():
            self.level_tree.delete(item)
        self.status.set("表格已清空")

    def load_level_sample(self):
        sample = [["BM1", "1.235", "", "100.000", "50", "基准点"],
                  ["TP1", "1.456", "0.789", "", "60", "转点1"],
                  ["TP2", "1.678", "1.234", "", "55", "转点2"],
                  ["BM2", "", "1.567", "", "40", "闭合点"]]
        self.clear_level_table()
        for row in sample:
            self.level_tree.insert('', 'end', values=row)
        self.status.set("已加载示例数据")

    # ==================== 面积转换 ====================
    def build_area_tab(self):
        tab = ttk.Frame(self.nb)
        self.nb.add(tab, text='面积转换')

        op_frame = ttk.LabelFrame(tab, text='转换方向', padding=10)
        op_frame.pack(fill='x', padx=10, pady=5)
        self.area_dir = tk.IntVar(value=1)
        ttk.Radiobutton(op_frame, text='平方米 → 亩', variable=self.area_dir, value=1).pack(side=tk.LEFT, padx=10)
        ttk.Radiobutton(op_frame, text='亩 → 平方米', variable=self.area_dir, value=2).pack(side=tk.LEFT, padx=10)

        input_frame = ttk.LabelFrame(tab, text='输入数值', padding=10)
        input_frame.pack(fill='x', padx=10, pady=5)
        ttk.Label(input_frame, text='数值:').pack(side=tk.LEFT, padx=5)
        self.area_val = ttk.Entry(input_frame, width=20)
        self.area_val.pack(side=tk.LEFT, fill='x', expand=True, padx=5)

        result_frame = ttk.LabelFrame(tab, text='转换结果', padding=10)
        result_frame.pack(fill='both', expand=True, padx=10, pady=5)
        self.area_res = tk.Text(result_frame, height=2, font=('Arial', 11), wrap='word', bg='white', padx=10, pady=10)
        self.area_res.pack(fill='both', expand=True)
        self.area_res.config(state='disabled')

        btn_frame = ttk.Frame(tab, padding=5)
        btn_frame.pack(pady=5)
        ttk.Button(btn_frame, text='计算', width=8, command=self.calc_area).pack(side=tk.LEFT, padx=5)
        ttk.Button(btn_frame, text='复制', width=8, command=self.copy_area).pack(side=tk.LEFT, padx=5)
        ttk.Button(btn_frame, text='清空', width=8, command=self.clear_area).pack(side=tk.LEFT, padx=5)

    def calc_area(self):
        try:
            val = float(self.area_val.get())
            self.area_res.config(state='normal')
            self.area_res.delete('1.0', 'end')
            if self.area_dir.get() == 1:
                mu = val / 666.67
                self.area_res.insert('end', f'{val:.2f} 平方米 = {mu:.4f} 亩')
            else:
                sqm = val * 666.67
                self.area_res.insert('end', f'{val:.2f} 亩 = {sqm:.2f} 平方米')
            self.area_res.config(state='disabled')
            self.status.set("面积转换完成")
        except Exception as e:
            messagebox.showerror('错误', str(e))

    def copy_area(self):
        text = self.area_res.get('1.0', 'end-1c').strip()
        if text:
            self.master.clipboard_clear()
            self.master.clipboard_append(text)
            self.status.set("结果已复制")
        else:
            messagebox.showwarning('提示', '没有可复制的结果')

    def clear_area(self):
        self.area_val.delete(0, 'end')
        self.area_res.config(state='normal')
        self.area_res.delete('1.0', 'end')
        self.area_res.config(state='disabled')
        self.status.set("已清空")


if __name__ == '__main__':
    root = tk.Tk()
    app = MeasureToolApp(root)
    root.mainloop()
