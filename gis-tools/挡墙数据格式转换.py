import tkinter as tk
from tkinter import scrolledtext, messagebox, filedialog

def convert_format():
    # 获取输入框内容
    input_data = input_text.get("1.0", tk.END).strip()
    
    if not input_data:
        messagebox.showwarning("警告", "请输入需要转换的数据")
        return
    
    try:
        # 分割输入数据为行
        lines = input_data.split('\n')
        output_lines = []
        
        # 处理所有数据行
        for line in lines:
            if not line.strip():
                continue  # 跳过空行
                
            # 分割数据列
            columns = line.split('\t')
            
            # 处理数值和插入点
            formatted_columns = []
            for i, col in enumerate(columns):
                stripped = col.strip()
                
                # 特殊处理插入点列
                if i == len(columns) - 1 and stripped.startswith('(') and stripped.endswith(')'):
                    # 提取坐标并格式化为浮点数
                    coords = stripped[1:-1].split()
                    formatted_coords = [f"{float(coord.strip()):.2f}" for coord in coords]
                    formatted_columns.append(f"({' '.join(formatted_coords)})")
                else:
                    # 数值列格式化为浮点数
                    try:
                        num_val = float(stripped)
                        # 整数显示为整数，小数保留两位
                        if num_val.is_integer():
                            formatted_columns.append(str(int(num_val)))
                        else:
                            formatted_columns.append(f"{num_val:.2f}")
                    except ValueError:
                        formatted_columns.append(stripped)
            
            # 构建输出行
            output_lines.append(f"({' '.join(formatted_columns)})")
        
        # 更新输出框
        output_text.delete("1.0", tk.END)
        output_text.insert(tk.END, '\n'.join(output_lines))
        
    except Exception as e:
        messagebox.showerror("转换错误", f"格式转换出错: {str(e)}")

def paste_data():
    try:
        # 从剪贴板获取数据
        clipboard_data = root.clipboard_get()
        input_text.insert(tk.END, clipboard_data)
    except tk.TclError:
        messagebox.showwarning("警告", "剪贴板中没有数据")

def clear_data():
    input_text.delete("1.0", tk.END)
    output_text.delete("1.0", tk.END)

def save_data():
    output_data = output_text.get("1.0", tk.END).strip()
    if not output_data:
        messagebox.showwarning("警告", "没有数据可保存")
        return
    
    file_path = filedialog.asksaveasfilename(
        defaultextension=".txt",
        filetypes=[("Text Files", "*.txt"), ("All Files", "*.*")],
        title="保存文件"
    )
    
    if file_path:
        try:
            with open(file_path, 'w', encoding='utf-8') as file:
                file.write(output_data)
            messagebox.showinfo("成功", "文件保存成功")
        except Exception as e:
            messagebox.showerror("错误", f"保存文件时出错: {str(e)}")

# 创建主窗口
root = tk.Tk()
root.title("挡墙数据格式转换")
root.geometry("800x600")

# 按钮框架
button_frame = tk.Frame(root)
button_frame.pack(pady=5)

# 功能按钮
paste_btn = tk.Button(button_frame, text="粘贴数据", command=paste_data, 
                     bg="#2196F3", fg="white", font=("Arial", 10))
paste_btn.pack(side=tk.LEFT, padx=5)

clear_btn = tk.Button(button_frame, text="清空数据", command=clear_data, 
                     bg="#f44336", fg="white", font=("Arial", 10))
clear_btn.pack(side=tk.LEFT, padx=5)

convert_btn = tk.Button(button_frame, text="转换格式", command=convert_format, 
                       bg="#4CAF50", fg="white", font=("Arial", 10, "bold"))
convert_btn.pack(side=tk.LEFT, padx=5)

save_btn = tk.Button(button_frame, text="保存结果", command=save_data, 
                    bg="#FF9800", fg="white", font=("Arial", 10))
save_btn.pack(side=tk.LEFT, padx=5)

# 输入区域
input_label = tk.Label(root, text="输入CSV格式数据:", font=("Arial", 10))
input_label.pack(pady=(10, 5), anchor="w")

input_text = scrolledtext.ScrolledText(root, width=90, height=10, font=("Consolas", 10))
input_text.pack(padx=10, fill=tk.BOTH, expand=True)

# 输出区域
output_label = tk.Label(root, text="转换后的TXT格式:", font=("Arial", 10))
output_label.pack(pady=(10, 5), anchor="w")

output_text = scrolledtext.ScrolledText(root, width=90, height=10, font=("Consolas", 10))
output_text.pack(padx=10, pady=(0, 10), fill=tk.BOTH, expand=True)

# 启动主循环
root.mainloop()