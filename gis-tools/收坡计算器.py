import tkinter as tk
from tkinter import messagebox

def calculate():
    try:
        # 获取输入值
        ratio = entry_ratio.get()
        height = entry_height.get()
        
        # 解析坡比
        h_ratio, l_ratio = map(float, ratio.split(':'))
        if h_ratio <= 0 or l_ratio <= 0:
            raise ValueError("坡比必须为正数")
            
        # 转换高差为浮点数
        delta_h = float(height)
        if delta_h <= 0:
            raise ValueError("高差必须为正数")
        
        # 计算水平收进量
        delta_l = (delta_h * l_ratio) / h_ratio
        
        # 显示结果（保留4位小数）
        result_label.config(text=f"水平收进量：{delta_l:.4f} 米")
        
    except ValueError as ve:
        messagebox.showerror("输入错误", f"请输入有效数字\n{str(ve)}")
    except Exception as e:
        messagebox.showerror("错误", f"发生未知错误：{str(e)}")

# 创建主窗口
window = tk.Tk()
window.title("收坡计算器")
window.geometry("300x200")

# 使用Frame容器整理布局
frame = tk.Frame(window, padx=20, pady=20)
frame.pack(expand=True)

# 坡比输入组件
tk.Label(frame, text="坡比（格式 垂直:水平）").grid(row=0, column=0, sticky="w")
entry_ratio = tk.Entry(frame)
entry_ratio.grid(row=0, column=1)
entry_ratio.insert(0, "80:1")  # 默认示例值

# 高差输入组件
tk.Label(frame, text="垂直高差（米）").grid(row=1, column=0, sticky="w", pady=10)
entry_height = tk.Entry(frame)
entry_height.grid(row=1, column=1)
entry_height.insert(0, "1")  # 默认示例值

# 计算按钮
calc_btn = tk.Button(frame, text="计算", command=calculate)
calc_btn.grid(row=2, columnspan=2, pady=10)

# 结果显示
result_label = tk.Label(frame, text="水平收进量：")
result_label.grid(row=3, columnspan=2)

window.mainloop()
