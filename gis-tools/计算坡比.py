import tkinter as tk
from tkinter import filedialog
import csv
from math import sqrt

def load_csv():
    # 打开文件选择对话框，选择 CSV 文件
    file_path = filedialog.askopenfilename(filetypes=[("CSV files", "*.csv")])
    if not file_path:
        label_result.config(text="未选择文件")
        return

    try:
        # 读取 CSV 文件
        with open(file_path, newline='', encoding='utf-8') as csvfile:
            csv_reader = csv.reader(csvfile)
            rows = list(csv_reader)  # 将所有行读取为列表

        # 检查 CSV 文件是否至少有两行数据
        if len(rows) < 2:
            label_result.config(text="CSV 文件中数据不足，至少需要两行数据。")
            return

        # 假设 CSV 文件的第一行是点 A 的数据，第二行是点 B 的数据
        # 格式为：x, y, H
        data_a = rows[0]
        data_b = rows[1]

        # 将数据填充到输入框中
        entry_x1.delete(0, tk.END)
        entry_x1.insert(0, data_a[0])
        entry_y1.delete(0, tk.END)
        entry_y1.insert(0, data_a[1])
        entry_H1.delete(0, tk.END)
        entry_H1.insert(0, data_a[2])

        entry_x2.delete(0, tk.END)
        entry_x2.insert(0, data_b[0])
        entry_y2.delete(0, tk.END)
        entry_y2.insert(0, data_b[1])
        entry_H2.delete(0, tk.END)
        entry_H2.insert(0, data_b[2])

        label_result.config(text="CSV 数据已加载，请点击“计算坡比”按钮进行计算。")
    except Exception as e:
        label_result.config(text=f"读取 CSV 文件失败：{e}")

def calculate_slope():
    try:
        # 获取输入数据
        x1 = float(entry_x1.get())
        y1 = float(entry_y1.get())
        H1 = float(entry_H1.get())
        x2 = float(entry_x2.get())
        y2 = float(entry_y2.get())
        H2 = float(entry_H2.get())
        
        # 计算水平距离 L
        L = sqrt((x2 - x1)**2 + (y2 - y1)**2)
        
        # 计算高差 ΔH
        delta_H = H2 - H1
        
        if delta_H == 0:
            result = "两点高程相同，坡比为 1 : ∞（平坡）"
        else:
            # 计算坡比 1 : n
            n = L / abs(delta_H)
            result = f"坡比: 1 : {n:.3f}"
        
        # 显示结果
        label_result.config(text=result)
    except ValueError:
        label_result.config(text="输入错误，请检查数据！")

# 创建主窗口
root = tk.Tk()
root.title("坡比计算器 (1 : n)")

# 输入框和标签
tk.Label(root, text="点 A (x1, y1, H1)").grid(row=0, column=0, columnspan=3, pady=5)
tk.Label(root, text="x1:").grid(row=1, column=0)
entry_x1 = tk.Entry(root)
entry_x1.grid(row=1, column=1)
tk.Label(root, text="y1:").grid(row=1, column=2)
entry_y1 = tk.Entry(root)
entry_y1.grid(row=1, column=3)
tk.Label(root, text="H1:").grid(row=1, column=4)
entry_H1 = tk.Entry(root)
entry_H1.grid(row=1, column=5)

tk.Label(root, text="点 B (x2, y2, H2)").grid(row=2, column=0, columnspan=3, pady=5)
tk.Label(root, text="x2:").grid(row=3, column=0)
entry_x2 = tk.Entry(root)
entry_x2.grid(row=3, column=1)
tk.Label(root, text="y2:").grid(row=3, column=2)
entry_y2 = tk.Entry(root)
entry_y2.grid(row=3, column=3)
tk.Label(root, text="H2:").grid(row=3, column=4)
entry_H2 = tk.Entry(root)
entry_H2.grid(row=3, column=5)

# 按钮：加载 CSV 文件
btn_load = tk.Button(root, text="加载 CSV 文件", command=load_csv)
btn_load.grid(row=4, column=0, columnspan=3, pady=5)

# 按钮：计算坡比
btn_calculate = tk.Button(root, text="计算坡比", command=calculate_slope)
btn_calculate.grid(row=4, column=3, columnspan=3, pady=5)

# 结果显示
label_result = tk.Label(root, text="", justify="left")
label_result.grid(row=5, column=0, columnspan=6, pady=10)

root.mainloop()
