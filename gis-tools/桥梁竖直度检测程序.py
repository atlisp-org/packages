import tkinter as tk
from tkinter import ttk, filedialog, messagebox
import csv

class BridgeVerticalityApp(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("📐 桥梁竖直度检测计算程序")
        self.geometry("1050x680")
        self.configure(bg="#ffffff")

        # ---------- 全局样式 ----------
        style = ttk.Style(self)
        # 确保使用默认主题，兼容所有平台
        style.theme_use("default")

        # 让表格出现细线
        style.layout("Treeview.Item",
                     [('Treeitem.padding', {'sticky': 'nswe'}),
                      ('Treeitem.indicator', {'side': 'left', 'sticky': ''}),
                      ('Treeitem.text', {'side': 'left', 'sticky': 'nswe'})])
        style.configure("Treeview",
                        background="#ffffff",
                        fieldbackground="#ffffff",
                        rowheight=28,
                        font=("Segoe UI", 10),
                        relief="solid",
                        borderwidth=1)
        style.map("Treeview",
                  background=[("selected", "#e0f2fe")],
                  foreground=[("selected", "#000000")])
        style.configure("Treeview.Heading",
                        background="#f1f3f4",
                        foreground="#0077b6",
                        font=("Segoe UI", 10, "bold"))

        # ---------- 界面 ----------
        main = tk.Frame(self, bg="#ffffff")
        main.pack(fill=tk.BOTH, expand=True, padx=20, pady=15)

        tk.Label(main, text="📐 桥梁竖直度检测计算",
                 bg="#ffffff", fg="#0077b6",
                 font=("Segoe UI", 22, "bold")).pack(pady=(0, 15))

        # 控制按钮
        ctrl = tk.Frame(main, bg="#ffffff")
        ctrl.pack(fill=tk.X, pady=(0, 12))
        for txt, cmd in [
            ("➕ 2行",  lambda: self.add_group(2)),
            ("➕ 4行",  lambda: self.add_group(4)),
            ("📂 导入CSV", self.load_from_csv),
            ("💾 导出Excel", self.save_to_excel),
            ("🧮 计算全部", self.calculate_all),
            ("🗑️ 清空",  self.clear_table),
        ]:
            tk.Button(ctrl, text=txt, command=cmd,
                      bg="#0077b6", fg="#ffffff",
                      activebackground="#0096c7",
                      bd=0, padx=12, pady=6,
                      font=("Segoe UI", 10)).pack(side=tk.LEFT, padx=4)

        # 表格
        table_wrap = tk.Frame(main, bg="#ffffff")
        table_wrap.pack(fill=tk.BOTH, expand=True)

        scroll = ttk.Scrollbar(table_wrap)
        scroll.pack(side=tk.RIGHT, fill=tk.Y)

        self.tree = ttk.Treeview(table_wrap, yscrollcommand=scroll.set,
                                 show="headings")
        scroll.config(command=self.tree.yview)

        self.columns = [
            ("构件编号", 90),
            ("测试方向", 80),
            ("设计墩高(m)", 100),
            ("柱顶平距(m)", 100),
            ("柱顶高差(m)", 100),
            ("柱底平距(m)", 100),
            ("柱底高差(m)", 100),
            ("高差(m)", 80),
            ("偏差值(mm)", 100),
            ("允许偏差(mm)", 100),
            ("备注", 150),
        ]
        self.tree["columns"] = [c[0] for c in self.columns]
        for col, w in self.columns:
            self.tree.heading(col, text=col, anchor=tk.CENTER)
            self.tree.column(col, width=w, anchor=tk.CENTER)
        self.tree.pack(fill=tk.BOTH, expand=True)

        # 状态栏
        self.status_var = tk.StringVar(value="就绪")
        tk.Label(self, textvariable=self.status_var,
                 bg="#f1f3f4", fg="#1f2937",
                 font=("Segoe UI", 9), anchor=tk.W, padx=10, pady=6)\
            .pack(fill=tk.X, side=tk.BOTTOM)

        # 数据
        self.groups = {}
        self.group_count = 0

        # 事件
        self.tree.bind("<Double-1>", self.on_cell_edit)
        self.tree.bind("<Return>", self.on_cell_edit)
        self.tree.bind("<FocusOut>", self.calculate_row)
        self.bind_all("<Control-s>", lambda e: self.save_to_excel())

    # ---------- 以下为原逻辑，保持不变 ----------
    def add_group(self, row_count):
        group_id = f"G{self.group_count + 1}"
        self.group_count += 1
        group_data = {"id": group_id, "design_height": "", "rows": []}
        for i in range(row_count):
            direction = "横向" if i % 2 == 0 else "纵向"
            values = [group_id, direction] + [""] * 9
            item_id = self.tree.insert("", tk.END, values=values)
            group_data["rows"].append({"item_id": item_id, "direction": direction})
        self.groups[group_id] = group_data
        self.status_var.set(f"已添加组: {group_id} ({row_count} 行)")

    def clear_table(self):
        for item in self.tree.get_children():
            self.tree.delete(item)
        self.groups.clear()
        self.group_count = 0
        self.status_var.set("已清空表格")

    def calculate_all(self):
        for item in self.tree.get_children():
            self.calculate_row_by_item(item)
        self.status_var.set("已完成所有计算")

    def calculate_row(self, event=None):
        selected = self.tree.selection()
        if selected:
            self.calculate_row_by_item(selected[0])

    def calculate_row_by_item(self, item_id):
        values = self.tree.item(item_id, "values")
        if not values:
            return
        try:
            group_id = values[0]
            design_height = float(self.groups[group_id]["design_height"] or 0)
            top_distance = float(values[3] or 0)
            top_height_diff = float(values[4] or 0)
            bottom_distance = float(values[5] or 0)
            bottom_height_diff = float(values[6] or 0)

            height_diff = top_height_diff - bottom_height_diff
            deviation = abs(top_distance - bottom_distance) * 1000
            allowed_deviation = min(design_height, 20) if design_height <= 60 else min(design_height / 3, 30)

            new_values = list(values)
            new_values[7] = f"{height_diff:.4f}"
            new_values[8] = f"{deviation:.1f}"
            new_values[9] = f"{allowed_deviation:.1f}"
            self.tree.item(item_id, values=new_values)
        except ValueError:
            pass

    def on_cell_edit(self, event):
        region = self.tree.identify("region", event.x, event.y)
        if region != "cell":
            return
        col = int(self.tree.identify_column(event.x)[1:]) - 1
        row_id = self.tree.focus()
        if not row_id or col in [7, 8, 9]:
            return

        self.edit_window = tk.Toplevel(self)
        self.edit_window.title("编辑")
        self.edit_window.geometry("300x120")
        self.edit_window.transient(self)
        self.edit_window.grab_set()

        tk.Label(self.edit_window, text=f"编辑: {self.columns[col][0]}").pack(pady=5)
        self.edit_var = tk.StringVar(value=self.tree.item(row_id, "values")[col])
        if col == 1:
            combo = ttk.Combobox(self.edit_window, textvariable=self.edit_var,
                                 values=["横向", "纵向"], state="readonly")
            combo.pack(fill=tk.X, padx=20)
            combo.focus()
        else:
            entry = ttk.Entry(self.edit_window, textvariable=self.edit_var)
            entry.pack(fill=tk.X, padx=20)
            entry.focus()
            entry.select_range(0, tk.END)

        tk.Button(self.edit_window, text="确定",
                  command=lambda: self.save_edit(row_id, col)).pack(side=tk.LEFT, padx=20, pady=10)
        tk.Button(self.edit_window, text="取消",
                  command=self.edit_window.destroy).pack(side=tk.RIGHT, padx=20, pady=10)
        self.edit_window.bind("<Return>", lambda e: self.save_edit(row_id, col))

    def save_edit(self, row_id, col):
        new_value = self.edit_var.get()
        values = list(self.tree.item(row_id, "values"))
        values[col] = new_value
        self.tree.item(row_id, values=values)
        if col == 2:
            group_id = values[0]
            if group_id in self.groups:
                self.groups[group_id]["design_height"] = new_value
        self.edit_window.destroy()
        self.calculate_row_by_item(row_id)
        self.status_var.set("数据已更新")

    def load_from_csv(self):
        file_path = filedialog.askopenfilename(filetypes=[("CSV文件", "*.csv")])
        if not file_path:
            return
        try:
            self.clear_table()
            with open(file_path, "r", encoding="ANSI") as f:
                reader = csv.reader(f)
                next(reader)
                for row in reader:
                    if len(row) < 11:
                        continue
                    group_id = row[0]
                    if group_id not in self.groups:
                        self.group_count += 1
                        self.groups[group_id] = {"id": group_id, "design_height": row[2], "rows": []}
                    item_id = self.tree.insert("", tk.END, values=row)
                    self.groups[group_id]["rows"].append({"item_id": item_id, "direction": row[1]})
            self.calculate_all()
            self.status_var.set(f"已导入 {len(self.tree.get_children())} 行数据")
        except Exception as e:
            messagebox.showerror("导入失败", str(e))

    def save_to_excel(self):
        file_path = filedialog.asksaveasfilename(defaultextension=".xls",
                                                 filetypes=[("Excel文件", "*.xls")])
        if not file_path:
            return
        try:
            html = """<html xmlns:o="urn:schemas-microsoft-com:office:office"
xmlns:x="urn:schemas-microsoft-com:office:excel"
xmlns="http://www.w3.org/TR/REC-html40">
<head><meta charset="ANSI"><title>桥梁竖直度检测数据</title></head><body>
<table border="1"><tr>"""
            headers = [c[0] for c in self.columns]
            html += "".join(f"<th>{h}</th>" for h in headers) + "</tr>"
            for item in self.tree.get_children():
                values = self.tree.item(item, "values")
                html += "<tr>" + "".join(f"<td>{v}</td>" for v in values) + "</tr>"
            html += "</table></body></html>"
            with open(file_path, "w", encoding="ANSI") as f:
                f.write(html)
            messagebox.showinfo("导出成功", f"已保存为: {file_path}")
            self.status_var.set("导出成功")
        except Exception as e:
            messagebox.showerror("导出失败", str(e))


if __name__ == "__main__":
    BridgeVerticalityApp().mainloop()
