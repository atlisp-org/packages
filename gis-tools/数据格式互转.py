import tkinter as tk
from tkinter import scrolledtext, messagebox, filedialog

class DataConverter:
    def __init__(self, root):
        self.root = root
        self.root.title("数据格式互转工具")
        self.root.geometry("800x700")
        
        # 创建UI组件
        self.create_widgets()
    
    def create_widgets(self):
        """创建所有UI组件"""
        # 输入框区域
        input_frame = tk.Frame(self.root)
        input_frame.pack(pady=(10, 5), fill=tk.X, padx=10)
        
        tk.Label(input_frame, text="输入数据（支持格式：1,,y,x,z 或 x y z 或 x y）：").pack(anchor="w")
        
        # 输入框按钮栏
        input_btn_frame = tk.Frame(input_frame)
        input_btn_frame.pack(fill=tk.X, pady=5)
        
        buttons = [
            ("粘贴数据", self.paste_data, "#4CAF50"),
            ("清空输入", self.clear_input, "#f44336")
        ]
        
        for text, command, color in buttons:
            tk.Button(input_btn_frame, text=text, command=command, 
                     bg=color, fg="white", relief=tk.FLAT).pack(side=tk.LEFT, padx=5)
        
        self.input_text = scrolledtext.ScrolledText(input_frame, width=80, height=15)
        self.input_text.pack(fill=tk.BOTH, expand=True)
        
        # 转换按钮区域
        convert_frame = tk.Frame(self.root)
        convert_frame.pack(pady=5, fill=tk.X, padx=10)
        
        convert_buttons = [
            ("转为 x y z 格式", self.convert_to_xyz, "#2196F3"),
            ("转为 1,,y,x,z 格式", self.convert_to_csv, "#2196F3"),
            ("转为 y,x 格式", self.convert_to_yx, "#2196F3")
        ]
        
        for text, command, color in convert_buttons:
            tk.Button(convert_frame, text=text, command=command,
                     bg=color, fg="white", relief=tk.FLAT).pack(side=tk.LEFT, padx=5, ipadx=10)
        
        # 输出框区域
        output_frame = tk.Frame(self.root)
        output_frame.pack(pady=(5, 10), fill=tk.BOTH, expand=True, padx=10)
        
        tk.Label(output_frame, text="转换结果：").pack(anchor="w")
        
        # 输出框按钮栏
        output_btn_frame = tk.Frame(output_frame)
        output_btn_frame.pack(fill=tk.X, pady=5)
        
        output_buttons = [
            ("复制结果", self.copy_result, "#FF9800"),
            ("清空输出", self.clear_output, "#f44336")
        ]
        
        for text, command, color in output_buttons:
            tk.Button(output_btn_frame, text=text, command=command,
                     bg=color, fg="white", relief=tk.FLAT).pack(side=tk.LEFT, padx=5)
        
        self.output_text = scrolledtext.ScrolledText(output_frame, width=80, height=15)
        self.output_text.pack(fill=tk.BOTH, expand=True)
        
        # 底部按钮
        bottom_frame = tk.Frame(self.root)
        bottom_frame.pack(pady=10, fill=tk.X, padx=10)
        
        bottom_buttons = [
            ("清空全部", self.clear_all, "#607D8B"),
            ("保存为TXT", lambda: self.save_as_file("txt"), "#9C27B0"),
            ("保存为DAT", lambda: self.save_as_file("dat"), "#9C27B0")
        ]
        
        for text, command, color in reversed(bottom_buttons):
            tk.Button(bottom_frame, text=text, command=command,
                     bg=color, fg="white", relief=tk.FLAT).pack(side=tk.RIGHT, padx=5, ipadx=10)
    
    def get_input_data(self):
        """获取输入数据并返回处理后的行列表"""
        input_data = self.input_text.get("1.0", tk.END).strip()
        if not input_data:
            messagebox.showwarning("警告", "请输入要转换的数据！")
            return None
        return [line.strip() for line in input_data.split('\n') if line.strip()]
    
    def process_lines(self, lines, format_type):
        """
        通用行处理函数
        format_type: 'xyz', 'csv' 或 'yx'
        """
        results = []
        error_lines = []
        
        for i, line in enumerate(lines, 1):
            try:
                if format_type == 'xyz':
                    # 转为 x y z 格式
                    if line.count(',') >= 3:  # 1,,y,x,z 格式
                        parts = line.split(',')
                        y, x, z = parts[2], parts[3], parts[4]
                        results.append(f"{x} {y} {z}")
                    elif line.count(' ') >= 2:  # 已经是 x y z 格式
                        parts = line.split()
                        if len(parts) >= 3:
                            results.append(line)
                        else:
                            raise ValueError
                    else:
                        raise ValueError
                
                elif format_type == 'csv':
                    # 转为 1,,y,x,z 格式
                    if line.count(' ') >= 2:  # x y z 格式
                        parts = line.split()
                        x, y, z = parts[0], parts[1], parts[2]
                        results.append(f"1,,{y},{x},{z}")
                    elif line.count(',') >= 3:  # 已经是 1,,y,x,z 格式
                        parts = line.split(',')
                        if len(parts) >= 5:
                            results.append(line)
                        else:
                            raise ValueError
                    else:
                        raise ValueError
                
                elif format_type == 'yx':
                    # 转为 y,x 格式
                    if line.count(' ') >= 1:  # x y 格式
                        parts = line.split()
                        x, y = parts[0], parts[1]
                        results.append(f"{y},{x}")
                    elif line.count(',') >= 1:  # 已经是 y,x 格式
                        parts = line.split(',')
                        if len(parts) >= 2:
                            results.append(line)
                        else:
                            raise ValueError
                    else:
                        raise ValueError
                
            except (IndexError, ValueError):
                error_lines.append(i)
                continue
        
        return results, error_lines
    
    def show_results(self, results, error_lines, success_message):
        """显示处理结果"""
        if not results:
            messagebox.showerror("错误", "没有找到有效格式的数据！")
            return
        
        self.output_text.delete("1.0", tk.END)
        self.output_text.insert(tk.END, "\n".join(results))
        
        if error_lines:
            messagebox.showwarning("警告", 
                f"{success_message}\n以下行无法转换：{', '.join(map(str, error_lines))}")
        else:
            messagebox.showinfo("成功", success_message)
    
    def convert_to_xyz(self):
        """将1,,y,x,z格式转换为x y z格式"""
        lines = self.get_input_data()
        if not lines:
            return
        
        results, error_lines = self.process_lines(lines, 'xyz')
        self.show_results(results, error_lines, f"转换完成！共处理 {len(results)} 行数据。")
    
    def convert_to_csv(self):
        """将x y z格式转换为1,,y,x,z格式"""
        lines = self.get_input_data()
        if not lines:
            return
        
        results, error_lines = self.process_lines(lines, 'csv')
        self.show_results(results, error_lines, f"转换完成！共处理 {len(results)} 行数据。")
    
    def convert_to_yx(self):
        """将x y格式转换为y,x格式"""
        lines = self.get_input_data()
        if not lines:
            return
        
        results, error_lines = self.process_lines(lines, 'yx')
        self.show_results(results, error_lines, f"转换完成！共处理 {len(results)} 行数据。")
    
    def paste_data(self):
        """粘贴剪贴板内容到输入框"""
        try:
            clipboard_content = self.root.clipboard_get()
            if clipboard_content:
                self.input_text.delete("1.0", tk.END)
                self.input_text.insert(tk.END, clipboard_content)
                messagebox.showinfo("成功", "数据已粘贴到输入框！")
            else:
                messagebox.showwarning("警告", "剪贴板为空！")
        except:
            messagebox.showwarning("警告", "无法获取剪贴板内容！")
    
    def clear_input(self):
        """清空输入框"""
        self.input_text.delete("1.0", tk.END)
    
    def clear_output(self):
        """清空输出框"""
        self.output_text.delete("1.0", tk.END)
    
    def clear_all(self):
        """清空所有内容"""
        self.clear_input()
        self.clear_output()
    
    def copy_result(self):
        """复制结果到剪贴板"""
        result = self.output_text.get("1.0", tk.END).strip()
        if result:
            self.root.clipboard_clear()
            self.root.clipboard_append(result)
            messagebox.showinfo("成功", "结果已复制到剪贴板！")
        else:
            messagebox.showwarning("警告", "没有可复制的结果！")
    
    def save_as_file(self, file_type):
        """保存结果为文件
        file_type: 'txt' 或 'dat'
        """
        result = self.output_text.get("1.0", tk.END).strip()
        if not result:
            messagebox.showwarning("警告", "没有可保存的结果！")
            return
        
        file_path = filedialog.asksaveasfilename(
            defaultextension=f".{file_type}",
            filetypes=[(f"{file_type.upper()}文件", f"*.{file_type}"), ("所有文件", "*.*")],
            title=f"保存为{file_type.upper()}文件"
        )
        
        if not file_path:
            return
        
        try:
            # DAT文件通常不需要特殊编码，使用二进制写入
            mode = 'wb' if file_type == 'dat' else 'w'
            encoding = None if file_type == 'dat' else 'utf-8'
            
            with open(file_path, mode, encoding=encoding) as f:
                if file_type == 'dat':
                    # 对于DAT文件，可以按需添加文件头或其他二进制数据
                    # 这里简单地将文本转换为bytes写入
                    f.write(result.encode('utf-8'))
                else:
                    f.write(result)
            
            messagebox.showinfo("成功", f"结果已保存到：\n{file_path}")
        except Exception as e:
            messagebox.showerror("错误", f"保存文件时出错：\n{str(e)}")

if __name__ == "__main__":
    root = tk.Tk()
    app = DataConverter(root)
    root.mainloop()