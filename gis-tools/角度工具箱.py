import math
import re
import tkinter as tk
from tkinter import ttk, messagebox, Menu, scrolledtext

class AngleConverterTab(ttk.Frame):
    """角度弧度转换标签页"""
    def __init__(self, parent):
        super().__init__(parent)
        self.create_widgets()
        
    def create_widgets(self):
        # 主框架
        main_frame = ttk.Frame(self)
        main_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # 操作选择区域
        conversion_frame = ttk.LabelFrame(main_frame, text="选择转换类型", padding=5)
        conversion_frame.pack(fill=tk.X, pady=5)
        
        self.conversion_var = tk.IntVar(value=1)
        
        conversions = [
            ("度分秒 → 弧度", 1),
            ("弧度 → 度分秒", 2)
        ]
        
        for i, (text, val) in enumerate(conversions):
            rb = ttk.Radiobutton(
                conversion_frame, 
                text=text, 
                variable=self.conversion_var, 
                value=val,
                command=self.conversion_changed
            )
            rb.grid(row=0, column=i, padx=5, pady=3, sticky="w")
        
        # 输入区域
        self.input_frame = ttk.LabelFrame(main_frame, text="输入参数", padding=5)
        self.input_frame.pack(fill=tk.X, pady=5)
        
        # 结果区域
        result_frame = ttk.LabelFrame(main_frame, text="转换结果", padding=5)
        result_frame.pack(fill=tk.BOTH, expand=True, pady=5)
        
        self.result_display = tk.Text(
            result_frame,
            font=('微软雅黑', 10),
            bg='#f0f0f0',
            relief=tk.SUNKEN,
            padx=8,
            pady=8,
            wrap=tk.WORD,
            height=3,
            state='disabled'
        )
        self.result_display.pack(fill=tk.BOTH, expand=True, padx=3, pady=3)
        
        # 右键菜单
        self.context_menu = Menu(self.result_display, tearoff=0)
        self.context_menu.add_command(label="复制", command=self.copy_result)
        self.result_display.bind("<Button-3>", self.show_context_menu)
        
        # 按钮区域
        button_frame = ttk.Frame(main_frame)
        button_frame.pack(fill=tk.X, pady=5)
        
        ttk.Button(
            button_frame, 
            text="计算", 
            command=self.convert_units,
            width=8
        ).pack(side=tk.LEFT, padx=3)
        
        ttk.Button(
            button_frame, 
            text="复制结果", 
            command=self.copy_result,
            width=8
        ).pack(side=tk.LEFT, padx=3)
        
        ttk.Button(
            button_frame, 
            text="清空", 
            command=self.clear_all,
            width=8
        ).pack(side=tk.LEFT, padx=3)
        
        # 默认显示第一个转换类型的输入字段
        self.current_conversion = 1
        self.create_input_field()
        
        # 绑定回车键计算
        self.master.bind('<Return>', lambda event: self.convert_units())
    
    def show_context_menu(self, event):
        """显示右键菜单"""
        try:
            self.context_menu.tk_popup(event.x_root, event.y_root)
        finally:
            self.context_menu.grab_release()
    
    def copy_result(self):
        """复制结果到剪贴板"""
        result = self.result_display.get("1.0", "end-1c")
        if result.strip():
            self.master.clipboard_clear()
            self.master.clipboard_append(result)
            messagebox.showinfo("复制成功", "结果已复制到剪贴板")
    
    def conversion_changed(self):
        """转换类型改变时更新界面"""
        new_conversion = self.conversion_var.get()
        if new_conversion != self.current_conversion:
            self.current_conversion = new_conversion
            self.create_input_field()
            self.clear_result()
    
    def create_input_field(self):
        """创建输入字段"""
        # 清除旧的输入字段
        for widget in self.input_frame.winfo_children():
            widget.destroy()
        
        # 创建新的输入字段
        if self.current_conversion == 1:  # 度分秒转弧度
            self.create_dms_input()
        elif self.current_conversion == 2:  # 弧度转度分秒
            self.create_simple_input("弧度 (rad):")
    
    def create_simple_input(self, label_text):
        """创建简单输入框(单个数值)"""
        frame = ttk.Frame(self.input_frame)
        frame.pack(fill=tk.X, padx=5, pady=5)
        
        ttk.Label(frame, text=label_text, width=10).pack(side=tk.LEFT, padx=5)
        
        self.entry = ttk.Entry(frame)
        self.entry.pack(side=tk.LEFT, fill=tk.X, expand=True, padx=5)
        self.entry.focus()
    
    def create_dms_input(self):
        """创建度分秒输入框"""
        frame = ttk.Frame(self.input_frame)
        frame.pack(fill=tk.X, padx=5, pady=5)
        
        ttk.Label(frame, text="度分秒:", width=10).pack(side=tk.LEFT, padx=5)
        
        # 度
        self.entry_deg = ttk.Entry(frame, width=6)
        self.entry_deg.pack(side=tk.LEFT, padx=2)
        ttk.Label(frame, text="°").pack(side=tk.LEFT)
        
        # 分
        self.entry_min = ttk.Entry(frame, width=6)
        self.entry_min.pack(side=tk.LEFT, padx=2)
        ttk.Label(frame, text="′").pack(side=tk.LEFT)
        
        # 秒
        self.entry_sec = ttk.Entry(frame, width=8)
        self.entry_sec.pack(side=tk.LEFT, padx=2)
        ttk.Label(frame, text="″").pack(side=tk.LEFT)
        
        self.entry_deg.focus()
    
    def radians_to_degrees(self, radians):
        """弧度转角度"""
        return radians * (180 / math.pi)
    
    def degrees_to_radians(self, degrees):
        """角度转弧度"""
        return degrees * (math.pi / 180)
    
    def dms_to_radians(self, degrees, minutes, seconds):
        """度分秒转弧度"""
        decimal = self.dms_to_decimal(degrees, minutes, seconds)
        return self.degrees_to_radians(decimal)
    
    def radians_to_dms(self, radians):
        """弧度转度分秒"""
        degrees = self.radians_to_degrees(radians)
        return self.decimal_to_dms(degrees)
    
    def convert_units(self):
        """执行单位转换"""
        try:
            if self.current_conversion == 1:  # 度分秒转弧度
                deg = float(self.entry_deg.get())
                minutes = float(self.entry_min.get())
                seconds = float(self.entry_sec.get())
                radians = self.dms_to_radians(deg, minutes, seconds)
                dms_str = f"{deg}°{minutes}′{seconds}″"
                result_str = f"{dms_str} = {radians:.8f} rad"
                
            elif self.current_conversion == 2:  # 弧度转度分秒
                radians = float(self.entry.get())
                d, m, s = self.radians_to_dms(radians)
                result_str = f"{radians} rad = {d}°{m}′{s:.4f}″"
            
            # 更新结果显示
            self.result_display.config(state='normal')
            self.result_display.delete("1.0", tk.END)
            self.result_display.insert(tk.END, result_str)
            self.result_display.config(state='disabled')
            
        except ValueError:
            messagebox.showerror("输入错误", "请输入有效的数字！")
        except Exception as e:
            messagebox.showerror("转换错误", f"发生意外错误: {str(e)}")
    
    def clear_all(self):
        """清空所有输入和结果"""
        if self.current_conversion == 1:  # 度分秒输入
            self.entry_deg.delete(0, tk.END)
            self.entry_min.delete(0, tk.END)
            self.entry_sec.delete(0, tk.END)
        else:
            self.entry.delete(0, tk.END)
        self.clear_result()
    
    def clear_result(self):
        """清空结果显示"""
        self.result_display.config(state='normal')
        self.result_display.delete("1.0", tk.END)
        self.result_display.config(state='disabled')
    
    @staticmethod
    def dms_to_decimal(degrees, minutes, seconds):
        """将度分秒转换为十进制度数"""
        sign = 1 if degrees >= 0 else -1
        return degrees + sign * minutes / 60 + sign * seconds / 3600
    
    @staticmethod
    def decimal_to_dms(decimal_angle):
        """将十进制度数转换为度分秒"""
        sign = 1 if decimal_angle >= 0 else -1
        decimal_angle = abs(decimal_angle)
        
        degrees = int(decimal_angle)
        remaining = (decimal_angle - degrees) * 60
        minutes = int(remaining)
        seconds = round((remaining - minutes) * 60, 4)  # 保留4位小数
        
        # 处理60秒进位
        if seconds >= 60:
            seconds -= 60
            minutes += 1
        if minutes >= 60:
            minutes -= 60
            degrees += 1
        
        return sign * degrees, minutes, seconds


class FormatConversionTab(ttk.Frame):
    """角度格式转换标签页"""
    def __init__(self, parent):
        super().__init__(parent)
        self.create_widgets()
        
    def create_widgets(self):
        # 主框架
        main_frame = ttk.Frame(self)
        main_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # 输入区域
        input_frame = ttk.LabelFrame(main_frame, text="输入角度(每行一个，格式如: 237 56 06.0)", padding=5)
        input_frame.pack(fill=tk.BOTH, expand=True, pady=5)
        
        self.input_text = scrolledtext.ScrolledText(
            input_frame,
            font=('微软雅黑', 9),
            padx=5,
            pady=5,
            height=6
        )
        self.input_text.pack(fill=tk.BOTH, expand=True, padx=3, pady=3)
        
        # 按钮区域
        btn_frame = ttk.Frame(main_frame)
        btn_frame.pack(fill=tk.X, pady=5)
        
        ttk.Button(
            btn_frame, 
            text="转换为标准格式", 
            command=self.convert,
            width=15
        ).pack(side=tk.LEFT, padx=3)
        
        ttk.Button(
            btn_frame, 
            text="清空", 
            command=self.clear_input,
            width=8
        ).pack(side=tk.RIGHT, padx=3)
        
        # 结果区域
        result_frame = ttk.LabelFrame(main_frame, text="转换结果(237°56′06.0″格式)", padding=5)
        result_frame.pack(fill=tk.BOTH, expand=True, pady=5)
        
        self.result_text = scrolledtext.ScrolledText(
            result_frame,
            font=('微软雅黑', 10),
            padx=5,
            pady=5,
            height=6
        )
        self.result_text.pack(fill=tk.BOTH, expand=True, padx=3, pady=3)
        
        # 底部按钮
        bottom_frame = ttk.Frame(main_frame)
        bottom_frame.pack(fill=tk.X, pady=5)
        
        ttk.Button(
            bottom_frame, 
            text="复制结果", 
            command=self.copy_result,
            width=10
        ).pack(pady=3)
    
    def convert_angle(self, angle_str):
        """转换单个角度字符串"""
        angle_str = angle_str.strip()
        if not angle_str:
            return ""
            
        # 如果已经是标准格式，直接返回
        if "°" in angle_str and "′" in angle_str and "″" in angle_str:
            return angle_str
        
        # 替换各种分隔符为空格
        for char in ["°", "′", "″", ",", ";", ":", "度", "分", "秒"]:
            angle_str = angle_str.replace(char, " ")
        
        # 分割字符串
        parts = angle_str.split()
        if len(parts) < 3:
            return f"格式错误: {angle_str}"
        
        try:
            # 尝试转换为数字
            deg = float(parts[0])
            min = float(parts[1])
            sec = float(parts[2])
            
            # 添加符号前缀：正数不加+号，负数保留-号
            if deg < 0:
                return f"-{abs(deg)}°{min}′{sec}″"
            else:
                return f"{deg}°{min}′{sec}″"
        except:
            return f"转换失败: {angle_str}"
    
    def convert(self):
        """批量转换所有角度"""
        input_data = self.input_text.get("1.0", tk.END).splitlines()
        self.result_text.delete("1.0", tk.END)
        
        count = 0
        for line in input_data:
            converted = self.convert_angle(line)
            if converted:
                self.result_text.insert(tk.END, converted + "\n")
                count += 1
        
        messagebox.showinfo("完成", f"已转换 {count} 个角度")
    
    def copy_result(self):
        """复制转换结果到剪贴板"""
        result = self.result_text.get("1.0", tk.END).strip()
        if result:
            self.master.clipboard_clear()
            self.master.clipboard_append(result)
            messagebox.showinfo("已复制", "转换结果已复制到剪贴板")
        else:
            messagebox.showwarning("无内容", "没有可复制的内容")
    
    def clear_input(self):
        """清空输入和结果"""
        self.input_text.delete("1.0", tk.END)
        self.result_text.delete("1.0", tk.END)


class AngleSubtractionTab(ttk.Frame):
    """角度减法计算标签页"""
    def __init__(self, parent):
        super().__init__(parent)
        self.create_widgets()
        
    def create_widgets(self):
        # 主框架
        main_frame = ttk.Frame(self)
        main_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # 参考角度输入
        ref_frame = ttk.LabelFrame(main_frame, text="参考角度", padding=5)
        ref_frame.pack(fill=tk.X, pady=5)
        
        ttk.Label(ref_frame, text="角度值:").pack(side=tk.LEFT, padx=5)
        self.angle_entry = ttk.Entry(ref_frame, width=8)
        self.angle_entry.pack(side=tk.LEFT, padx=5)
        self.angle_entry.insert(0, "90")
        ttk.Label(ref_frame, text="°").pack(side=tk.LEFT)
        
        # 按钮区域
        btn_frame = ttk.Frame(ref_frame)
        btn_frame.pack(side=tk.RIGHT, padx=5)
        ttk.Button(btn_frame, text="计算", command=self.calculate_angles, width=6).pack(side=tk.LEFT, padx=2)
        ttk.Button(btn_frame, text="清空", command=self.clear_all, width=6).pack(side=tk.LEFT, padx=2)
        
        # 输入输出框架
        io_frame = ttk.Frame(main_frame)
        io_frame.pack(fill=tk.BOTH, expand=True, pady=5)
        
        # 输入区域
        input_frame = ttk.LabelFrame(io_frame, text="输入角度(每行一个，格式如: +14°4′15.50″)", padding=5)
        input_frame.pack(side=tk.LEFT, fill=tk.BOTH, expand=True, padx=(0, 5))
        
        self.sub_input_text = scrolledtext.ScrolledText(
            input_frame,
            font=('微软雅黑', 9),
            padx=5,
            pady=5,
            height=8
        )
        self.sub_input_text.pack(fill=tk.BOTH, expand=True, padx=3, pady=3)
        
        # 输出区域
        output_frame = ttk.LabelFrame(io_frame, text="计算结果(度.分秒组合格式)", padding=5)
        output_frame.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True, padx=(5, 0))
        
        self.output_textbox = scrolledtext.ScrolledText(
            output_frame,
            font=('微软雅黑', 9),
            padx=5,
            pady=5,
            height=8
        )
        self.output_textbox.pack(fill=tk.BOTH, expand=True, padx=3, pady=3)
        
        # 底部按钮
        bottom_frame = ttk.Frame(main_frame)
        bottom_frame.pack(fill=tk.X, pady=5)
        
        ttk.Button(
            bottom_frame, 
            text="复制结果", 
            command=self.copy_result,
            width=10
        ).pack(pady=3)
    
    def calculate_angles(self):
        """角度减法计算 - 输出格式为度.分秒组合"""
        try:
            # 获取参考角度（默认90°）
            ref_angle = float(self.angle_entry.get())
        except ValueError:
            messagebox.showerror("错误", "参考角度必须是数字")
            return
        
        # 获取输入的角度文本
        input_text = self.sub_input_text.get("1.0", tk.END).strip()
        if not input_text:
            messagebox.showinfo("提示", "请输入角度数据")
            return
        
        # 处理每一行
        results = []
        lines = input_text.split('\n')
        valid_count = 0
        
        for line in lines:
            line = line.strip()
            if not line:
                continue
            
            # 尝试匹配标准格式
            match = re.match(r'([+-]?)(\d+)°(\d+)′(\d+\.?\d*)″', line)
            if match:
                # 提取符号和度分秒
                sign_str, deg_str, min_str, sec_str = match.groups()
                sign = -1 if sign_str == '-' else 1
                
                try:
                    deg = float(deg_str)
                    min = float(min_str)
                    sec = float(sec_str)
                    
                    # 计算总角度（以度为单位）
                    total_angle = sign * (deg + min/60 + sec/3600)
                    
                    # 计算参考角度减去当前角度
                    result_deg = ref_angle - total_angle
                    
                    # 处理负角度
                    output_sign = '-' if result_deg < 0 else ''
                    result_deg = abs(result_deg)
                    
                    # 转换回度分秒
                    deg_int = int(result_deg)
                    remainder = (result_deg - deg_int) * 60
                    min_int = int(remainder)
                    sec_float = round((remainder - min_int) * 60, 2)
                    
                    # 处理进位
                    if sec_float >= 60:
                        sec_float -= 60
                        min_int += 1
                    if min_int >= 60:
                        min_int -= 60
                        deg_int += 1
                    
                    # 格式化为度.分秒组合形式 (如75.554450)
                    # 分格式化为2位整数
                    min_formatted = f"{min_int:02d}"
                    
                    # 秒格式化为2位整数+2位小数（去掉小数点）
                    sec_whole = int(sec_float)
                    sec_fraction = sec_float - sec_whole
                    sec_formatted = f"{sec_whole:02d}{int(round(sec_fraction * 100)):02d}"
                    
                    # 组合结果
                    results.append(f"{output_sign}{deg_int}.{min_formatted}{sec_formatted}")
                    valid_count += 1
                except Exception as e:
                    results.append(f"计算错误: {line} ({str(e)})")
            else:
                # 尝试处理非标准格式
                try:
                    # 替换分隔符为空格
                    for char in ["°", "′", "″", ",", ";", ":", "度", "分", "秒"]:
                        line = line.replace(char, " ")
                    
                    parts = line.split()
                    if len(parts) < 3:
                        results.append(f"格式错误: {line}")
                        continue
                    
                    sign = 1
                    deg = float(parts[0])
                    min = float(parts[1])
                    sec = float(parts[2])
                    
                    # 检查是否有负号
                    if deg < 0 or (deg == 0 and min < 0) or (deg == 0 and min == 0 and sec < 0):
                        sign = -1
                    
                    # 计算总角度（以度为单位）
                    total_angle = sign * (abs(deg) + min/60 + sec/3600)
                    
                    # 计算参考角度减去当前角度
                    result_deg = ref_angle - total_angle
                    
                    # 处理负角度
                    output_sign = '-' if result_deg < 0 else ''
                    result_deg = abs(result_deg)
                    
                    # 转换回度分秒
                    deg_int = int(result_deg)
                    remainder = (result_deg - deg_int) * 60
                    min_int = int(remainder)
                    sec_float = round((remainder - min_int) * 60, 2)
                    
                    # 处理进位
                    if sec_float >= 60:
                        sec_float -= 60
                        min_int += 1
                    if min_int >= 60:
                        min_int -= 60
                        deg_int += 1
                    
                    # 格式化为度.分秒组合形式
                    min_formatted = f"{min_int:02d}"
                    sec_whole = int(sec_float)
                    sec_fraction = sec_float - sec_whole
                    sec_formatted = f"{sec_whole:02d}{int(round(sec_fraction * 100)):02d}"
                    
                    results.append(f"{output_sign}{deg_int}.{min_formatted}{sec_formatted}")
                    valid_count += 1
                except Exception as e:
                    results.append(f"解析错误: {line} ({str(e)})")
        
        # 显示结果
        self.output_textbox.delete("1.0", tk.END)
        self.output_textbox.insert(tk.END, "\n".join(results))
        
        if valid_count > 0:
            messagebox.showinfo("计算完成", f"成功计算 {valid_count} 个角度\n错误: {len(lines) - valid_count} 个")
        else:
            messagebox.showwarning("计算失败", "没有成功计算任何角度")
    
    def copy_result(self):
        """复制减法结果到剪贴板"""
        result = self.output_textbox.get("1.0", tk.END).strip()
        if result:
            self.master.clipboard_clear()
            self.master.clipboard_append(result)
            messagebox.showinfo("已复制", "计算结果已复制到剪贴板")
        else:
            messagebox.showwarning("无内容", "没有可复制的内容")
    
    def clear_all(self):
        """清空减法标签页内容"""
        self.sub_input_text.delete("1.0", tk.END)
        self.output_textbox.delete("1.0", tk.END)
        self.angle_entry.delete(0, tk.END)
        self.angle_entry.insert(0, "90")


class AngleToolbox:
    """角度工具箱主应用"""
    def __init__(self, root):
        self.root = root
        self.root.title("角度工具箱")
        self.root.geometry("900x600")
        
        # 设置应用图标
        try:
            self.root.iconbitmap("angle_toolbox.ico")
        except:
            pass
        
        # 创建标签页
        self.notebook = ttk.Notebook(root)
        self.notebook.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # 添加三个功能标签页
        self.create_tabs()
        
        # 状态栏
        self.status_bar = tk.Label(root, text="就绪", bd=1, relief=tk.SUNKEN, anchor=tk.W)
        self.status_bar.pack(side=tk.BOTTOM, fill=tk.X)
    
    def create_tabs(self):
        """创建三个功能标签页"""
        # 角度弧度转换
        converter_tab = AngleConverterTab(self.notebook)
        self.notebook.add(converter_tab, text="角度弧度转换")
        
        # 格式转换
        format_tab = FormatConversionTab(self.notebook)
        self.notebook.add(format_tab, text="格式转换")
        
        # 角度减法
        subtraction_tab = AngleSubtractionTab(self.notebook)
        self.notebook.add(subtraction_tab, text="角度减法")


if __name__ == "__main__":
    root = tk.Tk()
    app = AngleToolbox(root)
    root.mainloop()