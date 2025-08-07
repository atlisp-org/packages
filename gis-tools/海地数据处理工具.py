import tkinter as tk
from tkinter import ttk, messagebox, scrolledtext
import re

class CrossSectionTool:
    def __init__(self, root):
        self.root = root
        root.title("海地数据处理工具")
        root.geometry("950x680")
        root.minsize(880, 600)
        root['bg'] = '#f0f0f0'

        # Main Notebook
        nb = ttk.Notebook(root)
        nb.pack(fill='both', expand=1, padx=6, pady=6)
        
        # Create tabs
        tabs = [
            ('数据转换', self.build_conversion),
            ('海地dmx格式', self.build_haiti),
            ('海地jdx格式', self.build_jdx)
        ]
        for name, builder in tabs:
            tab = ttk.Frame(nb)
            nb.add(tab, text=name)
            builder(tab)

        # Status bar
        self.status = tk.StringVar(value='就绪')
        ttk.Label(root, textvariable=self.status, anchor='w',
                 relief='sunken', font=('Arial', 9)).pack(fill='x', padx=6, pady=2)

    def create_btn_bar(self, parent, specs):
        bar = ttk.Frame(parent)
        bar.pack(fill='x', pady=4)
        for txt, cmd, clr in specs:
            style = f'{clr}.TButton' if clr in ('success', 'warning', 'danger') else 'primary.TButton'
            ttk.Button(bar, text=txt, command=cmd, style=style).pack(side='left', padx=2)
        return bar

    # ========== 1. Data Conversion Tab ==========
    def build_conversion(self, parent):
        buttons = [
            ('粘贴', self.conv_paste, 'primary'),
            ('CASS↔标准', self.toggle_cass, 'warning'),
            ('排序', self.sort_data, 'primary'),
            ('复制', self.conv_copy, 'primary'),
            ('清空', self.conv_clear, 'danger')
        ]
        self.create_btn_bar(parent, buttons)

        nb = ttk.Notebook(parent)
        nb.pack(fill='both', expand=1, pady=2)
        
        # Input/Output areas
        for name, attr in (('输入数据', 'conv_in'), ('输出结果', 'conv_out')):
            frm = ttk.Frame(nb)
            nb.add(frm, text=name)
            setattr(self, attr, scrolledtext.ScrolledText(frm, height=10, font=('Consolas', 9)))
            getattr(self, attr).pack(fill='both', expand=1, padx=2, pady=2)

    def conv_paste(self):
        try:
            self.conv_in.delete('1.0', 'end')
            self.conv_in.insert('end', self.root.clipboard_get())
            self.status.set("已粘贴")
        except:
            messagebox.showwarning("警告", "剪贴板为空")

    def conv_copy(self):
        if txt := self.conv_out.get('1.0', 'end').strip():
            self.root.clipboard_clear()
            self.root.clipboard_append(txt)
            self.status.set("已复制")
        else:
            messagebox.showwarning("提示", "无内容")

    def conv_clear(self):
        self.conv_in.delete('1.0', 'end')
        self.conv_out.delete('1.0', 'end')
        self.status.set("已清空")

    def lines(self):
        return self.conv_in.get('1.0', 'end').strip().splitlines()

    def toggle_cass(self):
        self.cass_to_std() if any('BEGIN' in ln for ln in self.lines()) else self.std_to_cass()

    def std_to_cass(self):
        data = {}
        for ln in self.lines():
            parts = re.split(r'[, \t]+', ln.strip())
            if len(parts) >= 3:
                try:
                    d, o, e = map(float, parts[:3])
                    data.setdefault(d, []).append((o, e))
                except:
                    continue
        
        out = []
        for d in sorted(data):
            out.append(f'BEGIN,{d}:')
            out += [f'{o},{e}' for o, e in data[d]]
            out.append('END')
        
        self.show_result(out)

    def cass_to_std(self):
        res, mile = [], None
        for ln in self.lines():
            ln = ln.strip()
            if ln.startswith('BEGIN'):
                try:
                    mile = float(re.split(r'[:,]', ln)[1])
                except:
                    mile = None
            elif ln == 'END':
                mile = None
            elif mile is not None:
                parts = re.split(r'[, \t]+', ln)
                if len(parts) >= 2:
                    try:
                        res.append(f'{mile},{float(parts[0])},{float(parts[1])}')
                    except:
                        continue
        self.show_result(res)

    def sort_data(self):
        try:
            parsed = []
            for ln in self.lines():
                parts = re.split(r'[, \t]+', ln.strip())
                if len(parts) >= 3:
                    try:
                        parsed.append(tuple(map(float, parts[:3])))
                    except:
                        continue
            
            if not parsed:
                raise ValueError("没有有效的三维数据")
            
            lst = sorted(parsed, key=lambda x: (x[0], x[1]))
            self.show_result([f'{a}\t{b}\t{c}' for a, b, c in lst])
            self.status.set(f"排序完成: {len(lst)}个点")
        except Exception as e:
            messagebox.showerror("错误", str(e))

    def show_result(self, lines):
        self.conv_out.delete('1.0', 'end')
        self.conv_out.insert('end', '\n'.join(lines))

    # ========== 2. Haiti DMX Format Tab ==========
    def build_haiti(self, parent):
        buttons = [
            ('粘贴', self.haiti_paste, 'primary'),
            ('格式转换', self.haiti_conv, 'success'),
            ('中桩内插', self.haiti_interp, 'warning'),
            ('清空', self.haiti_clear, 'danger'),
            ('复制', self.haiti_copy, 'primary')
        ]
        self.create_btn_bar(parent, buttons)
        
        # Text areas
        self.haiti_in = scrolledtext.ScrolledText(parent, height=8, font=('Consolas', 9))
        self.haiti_in.pack(fill='both', expand=0, padx=2, pady=2)
        self.haiti_out = scrolledtext.ScrolledText(parent, height=10, font=('Consolas', 9))
        self.haiti_out.pack(fill='both', expand=1, padx=2, pady=2)

    def haiti_paste(self):
        self.paste_to_text(self.haiti_in)

    def haiti_clear(self):
        self.clear_texts(self.haiti_in, self.haiti_out)

    def haiti_copy(self):
        self.copy_from_text(self.haiti_out)

    def parse_haiti(self):
        text = self.haiti_in.get('1.0', 'end')
        return [tuple(map(float, m)) for m in re.findall(
            r'(-?\d+\.?\d*)[,\s]+(-?\d+\.?\d*)[,\s]+(-?\d+\.?\d*)', text)]

    def haiti_conv(self):
        try:
            data = self.parse_haiti()
            if not data:
                raise ValueError("无数据")
            
            grp = {}
            for mile, off, elev in data:
                km = round(mile, 3)
                grp.setdefault(km, {'L': [], 'R': []})
                if abs(off) >= 0.001:  # Skip center points
                    side = 'L' if off < 0 else 'R'
                    grp[km][side].append((abs(off), elev))
            
            out = []
            for k in sorted(grp):
                out.append(f'{k:.3f}')
                for side in ['L', 'R']:
                    if grp[k][side]:
                        grp[k][side].sort()
                        out.append(' '.join(f'{o:.3f} {e:.3f}' for o, e in grp[k][side]))
            
            self.show_haiti_result(out, f"转换完成: {len(data)}点 → {len(out)}行")
        except Exception as e:
            messagebox.showerror('错误', str(e))

    def haiti_interp(self):
        try:
            data = self.parse_haiti()
            if not data:
                raise ValueError("无数据")
            
            grp = {}
            for mile, off, elev in data:
                grp.setdefault(round(mile, 3), []).append((off, elev))
            
            out = []
            for k in sorted(grp):
                pts = sorted(grp[k], key=lambda x: abs(x[0]))
                center = [e for o, e in pts if abs(o) < 0.001]
                
                if center:
                    ce = center[0]
                else:
                    left = [(o, e) for o, e in pts if o < 0]
                    right = [(o, e) for o, e in pts if o >= 0]
                    if left and right:
                        lo, le = min(left, key=lambda x: abs(x[0]))
                        ro, re = min(right, key=lambda x: abs(x[0]))
                        ce = (le * ro + re * abs(lo)) / (abs(lo) + ro)
                    elif left:
                        ce = left[0][1]
                    elif right:
                        ce = right[0][1]
                    else:
                        continue
                
                out.append(f'{k:.3f} {ce:.3f}')
            
            self.show_haiti_result(out, f"内插完成: {len(out)}个中桩")
        except Exception as e:
            messagebox.showerror('错误', str(e))

    def show_haiti_result(self, lines, status):
        self.haiti_out.delete('1.0', 'end')
        self.haiti_out.insert('end', '\n'.join(lines))
        self.status.set(status)

    # ========== 3. Haiti JDX Format Tab ==========
    def build_jdx(self, parent):
        buttons = [
            ('转换', self.jdx_convert, 'success'),
            ('复制', self.jdx_copy, 'primary'),
            ('清空', self.jdx_clear, 'danger'),
            ('粘贴', self.jdx_paste, 'primary')
        ]
        self.create_btn_bar(parent, buttons)
        
        # Text areas
        self.jdx_in = scrolledtext.ScrolledText(parent, height=8, font=('Consolas', 9))
        self.jdx_in.pack(fill='both', expand=0, padx=2, pady=2)
        self.jdx_out = scrolledtext.ScrolledText(parent, height=10, font=('Consolas', 9))
        self.jdx_out.pack(fill='both', expand=1, padx=2, pady=2)

    def jdx_paste(self):
        self.paste_to_text(self.jdx_in)

    def jdx_clear(self):
        self.clear_texts(self.jdx_in, self.jdx_out)

    def jdx_copy(self):
        self.copy_from_text(self.jdx_out)

    def jdx_convert(self):
        try:
            input_text = self.jdx_in.get("1.0", tk.END).strip()
            if not input_text:
                raise ValueError("输入为空")
            
            output_lines = ["NE", "1"]  # JDX header
            
            for line in input_text.split('\n'):
                parts = re.split(r'\s+', line.strip())
                if not parts:
                    continue
                
                try:
                    point_name = parts[0]
                    x_coord = parts[1]
                    y_coord = parts[2]
                    vals = [0.0] * 5  # Default values
                    
                    for i in range(5):
                        if len(parts) > 3 + i:
                            try:
                                vals[i] = float(parts[3 + i])
                            except ValueError:
                                pass
                    
                    formatted_line = (f"{point_name:>10} {x_coord:>15} {y_coord:>15} "
                                    f"{vals[0]:>15.3f}{vals[1]:>15.3f}{vals[2]:>15.3f}{vals[3]:>15.3f}{vals[4]:>15.3f}")
                    
                    output_lines.append(formatted_line)
                except Exception as e:
                    raise ValueError(f"处理行时出错: {line}\n{e}")
            
            self.jdx_out.delete("1.0", tk.END)
            self.jdx_out.insert(tk.END, "\n".join(output_lines))
            self.status.set("转换完成")
        except Exception as e:
            messagebox.showerror("错误", str(e))

    # ========== Common Methods ==========
    def paste_to_text(self, text_widget):
        try:
            text_widget.delete('1.0', 'end')
            text_widget.insert('end', self.root.clipboard_get())
            self.status.set("已粘贴")
        except:
            messagebox.showwarning("警告", "无法粘贴")

    def copy_from_text(self, text_widget):
        if txt := text_widget.get('1.0', 'end').strip():
            self.root.clipboard_clear()
            self.root.clipboard_append(txt)
            self.status.set("已复制")
        else:
            messagebox.showwarning("提示", "无内容")

    def clear_texts(self, *text_widgets):
        for widget in text_widgets:
            widget.delete('1.0', 'end')
        self.status.set("已清空")

if __name__ == '__main__':
    root = tk.Tk()
    style = ttk.Style()
    style.theme_use('clam')
    
    # Configure styles
    style.configure('TButton', font=('Arial', 9))
    style.configure('primary.TButton', foreground='white', background='#3498db')
    style.configure('success.TButton', foreground='white', background='#27ae60')
    style.configure('warning.TButton', foreground='white', background='#f39c12')
    style.configure('danger.TButton', foreground='white', background='#e74c3c')
    
    CrossSectionTool(root)
    root.mainloop()