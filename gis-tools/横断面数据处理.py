import tkinter as tk
from tkinter import ttk, messagebox, filedialog, scrolledtext
import csv, re, os

class CrossSectionTool:
    def __init__(self, root):
        self.root = root
        root.title("横断面数据处理工具")
        root.geometry("950x680")
        root.minsize(880, 600)
        root['bg'] = '#f0f0f0'

        # ---------- 主 Notebook ----------
        nb = ttk.Notebook(root)
        nb.pack(fill='both', expand=1, padx=6, pady=6)
        for name, builder in (('数据转换', self.build_conversion),
                              ('高程计算', self.build_elev),
                              ('路基设计', self.build_design)):
            tab = ttk.Frame(nb)
            nb.add(tab, text=name)
            builder(tab)

        # ---------- 状态栏 ----------
        self.status = tk.StringVar(value='就绪')
        ttk.Label(root, textvariable=self.status, anchor='w',
                  relief='sunken', font=('Arial', 9)).pack(fill='x', padx=6, pady=2)

    # ------------ 通用 ------------
    def open_file(self, var, save=False):
        op = filedialog.asksaveasfilename if save else filedialog.askopenfilename
        fn = op(filetypes=[('CSV文件', '*.csv'), ('所有文件', '*.*')])
        if fn:
            var.set(fn)

    def create_file_entry(self, parent, label, var, save=False):
        f = ttk.Frame(parent)
        ttk.Label(f, text=label, width=14, anchor='w').pack(side='left')
        ttk.Entry(f, textvariable=var).pack(side='left', fill='x', expand=1, padx=3)
        ttk.Button(f, text='浏览', width=6,
                   command=lambda: self.open_file(var, save)).pack(side='left')
        return f

    def create_btn_bar(self, parent, specs):
        bar = ttk.Frame(parent)
        bar.pack(fill='x', pady=4)
        for txt, cmd, clr in specs:
            style = 'success.TButton' if clr == 'success' else \
                    'warning.TButton' if clr == 'warning' else \
                    'danger.TButton' if clr == 'danger' else 'primary.TButton'
            btn = ttk.Button(bar, text=txt, command=cmd, style=style)
            btn.pack(side='left', padx=2, ipadx=6, ipady=2)
        return bar

    # ------------ 1. 数据转换 ------------
    def build_conversion(self, parent):
        specs = [('粘贴', self.conv_paste, 'primary'),
                 ('标准化', self.to_std, 'success'),
                 ('CASS↔标准', self.toggle_cass, 'warning'),
                 ('轻松格式', self.to_qs, 'success'),
                 ('排序', self.sort_data, 'primary'),
                 ('展点', self.to_zd, 'primary'),
                 ('保存DAT', self.save_dat, 'success'),
                 ('复制', self.conv_copy, 'primary'),
                 ('清空', self.conv_clear, 'danger')]
        self.create_btn_bar(parent, specs)

        nb = ttk.Notebook(parent)
        nb.pack(fill='both', expand=1, pady=2)
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
            self.status.set("粘贴失败")

    def conv_copy(self):
        txt = self.conv_out.get('1.0', 'end').strip()
        if txt:
            self.root.clipboard_clear()
            self.root.clipboard_append(txt)
            self.status.set("已复制")
        else:
            messagebox.showwarning("提示", "无内容")
            self.status.set("复制失败")

    def conv_clear(self):
        self.conv_in.delete('1.0', 'end')
        self.conv_out.delete('1.0', 'end')
        self.status.set("已清空")

    def lines(self):
        return self.conv_in.get('1.0', 'end').strip().splitlines()

    def to_std(self):
        try:
            res = [','.join(re.split(r'[,\s]+', ln.strip())[:3]) for ln in self.lines()]
            self.conv_out.delete('1.0', 'end')
            self.conv_out.insert('end', '\n'.join(res))
            self.status.set("标准化完成")
        except Exception as e:
            messagebox.showerror("错误", str(e))

    def toggle_cass(self):
        self.cass_to_std() if any('BEGIN' in ln for ln in self.lines()) else self.std_to_cass()

    def std_to_cass(self):
        data = {}
        for ln in self.lines():
            p = ln.split(',')
            if len(p) >= 3:
                try:
                    d, o, e = map(float, p[:3])
                    data.setdefault(d, []).append((o, e))
                except:
                    continue
        out = []
        for d in sorted(data):
            out.append(f'BEGIN,{d}:')
            out += [f'{o},{e}' for o, e in data[d]]
            out.append('END')
        self.conv_out.delete('1.0', 'end')
        self.conv_out.insert('end', '\n'.join(out))
        self.status.set("转为CASS")

    def cass_to_std(self):
        res, mile = [], None
        for ln in self.lines():
            ln = ln.strip()
            if ln.startswith('BEGIN'):
                try:
                    mile = float(ln.split(',')[1].split(':')[0])
                except:
                    mile = None
            elif ln == 'END':
                mile = None
            elif mile is not None:
                p = ln.split(',')
                if len(p) >= 2:
                    try:
                        res.append(f'{mile},{float(p[0])},{float(p[1])}')
                    except:
                        continue
        self.conv_out.delete('1.0', 'end')
        self.conv_out.insert('end', '\n'.join(res))
        self.status.set("转为标准")

    def to_qs(self):
        data = {}
        for ln in self.lines():
            p = ln.split(',')
            if len(p) >= 3:
                try:
                    d, o, e = p[0], p[1], p[2]
                    data.setdefault(float(d), {'e': [], 'o': []})
                    data[float(d)]['e'].append(e)
                    data[float(d)]['o'].append(o)
                except:
                    continue
        out = []
        for d in sorted(data):
            km, m = int(d // 1000), d % 1000
            out.append(f'K{km}+{m:.3f}\t高程\t' + '\t'.join(data[d]['e']))
            out.append('\t偏距\t' + '\t'.join(data[d]['o']))
        self.conv_out.delete('1.0', 'end')
        self.conv_out.insert('end', '\n'.join(out))
        self.status.set("轻松格式完成")

    def sort_data(self):
        try:
            lst = sorted([tuple(map(float, ln.split(',')[:3])) for ln in self.lines() if len(ln.split(',')) >= 3], key=lambda x: (x[0], x[1]))
            self.conv_out.delete('1.0', 'end')
            self.conv_out.insert('end', '\n'.join(f'{a},{b},{c}' for a, b, c in lst))
            self.status.set("排序完成")
        except Exception as e:
            messagebox.showerror("错误", str(e))

    def to_zd(self):
        res = []
        for i, ln in enumerate(self.lines()):
            nums = re.findall(r'-?\d+\.?\d*', ln)
            if len(nums) >= 3:
                # 修改点：交换前两列数据，即X和Y坐标互换
                res.append(f'{i+1},,{nums[1]},{nums[0]},{nums[2]}')
        self.conv_out.delete('1.0', 'end')
        self.conv_out.insert('end', '\n'.join(res))
        self.status.set("展点完成")

    def save_dat(self):
        txt = self.conv_out.get('1.0', 'end').strip()
        if not txt:
            messagebox.showwarning("提示", "无内容")
            return
        fn = filedialog.asksaveasfilename(defaultextension='.dat', filetypes=[('DAT', '*.dat')])
        if fn:
            with open(fn, 'w', encoding='gbk') as f:
                f.write(txt)
            messagebox.showinfo("完成", "已保存")
            self.status.set("已保存DAT")

    # ------------ 2. 高程计算 ------------
    def build_elev(self, parent):
        self.elev_f1 = tk.StringVar()
        self.elev_f2 = tk.StringVar()
        self.elev_f3 = tk.StringVar()
        self.elev_out = tk.StringVar()
        ttk.Label(parent, text='文件选择').pack(anchor='w', pady=2)
        for lab, var, sv in [('中桩高程文件', self.elev_f1, False),
                             ('偏距高差文件', self.elev_f2, False),
                             ('或偏距高程文件', self.elev_f3, False),
                             ('输出文件', self.elev_out, True)]:
            self.create_file_entry(parent, lab + ':', var, sv).pack(fill='x', pady=1)
        self.create_btn_bar(parent, [('计算高程', self.elev_calc, 'success'),
                                     ('清空', lambda: self.elev_txt.delete('1.0', 'end'), 'danger')])
        self.elev_txt = scrolledtext.ScrolledText(parent, height=10, font=('Consolas', 9))
        self.elev_txt.pack(fill='both', expand=1, padx=2, pady=2)

    def elev_calc(self):
        try:
            f1, f2, f3, out = self.elev_f1.get(), self.elev_f2.get(), self.elev_f3.get(), self.elev_out.get()
            if not (f1 and out):
                raise ValueError("缺少文件")
            center = {}
            with open(f1, encoding='ANSI') as f:
                next(csv.reader(f))
                for r in csv.reader(f):
                    if len(r) >= 2:
                        try:
                            center[float(r[0])] = float(r[1])
                        except:
                            continue
            rows = []
            for fp, col in [(f2, 2), (f3, 2)]:
                if not fp:
                    continue
                with open(fp, encoding='ANSI') as f:
                    next(csv.reader(f))
                    for r in csv.reader(f):
                        if len(r) >= 3:
                            try:
                                mile, offset = float(r[0]), r[1]
                                val = float(r[col])
                                if mile in center:
                                    elev = center[mile] + val if col == 2 else val
                                    rows.append([f'{mile}', offset, f'{elev:.3f}'])
                            except:
                                continue
            with open(out, 'w', newline='', encoding='ANSI') as f:
                csv.writer(f).writerows([['里程', '偏距', '高程']] + rows)
            self.elev_txt.delete('1.0', 'end')
            self.elev_txt.insert('end', f'共处理 {len(rows)} 个点，已保存')
            messagebox.showinfo('完成', f'处理 {len(rows)} 个点')
            self.status.set("高程计算完成")
        except Exception as e:
            messagebox.showerror('错误', str(e))

    # ------------ 3. 路基设计 ------------
    def build_design(self, parent):
        self.dsg_in  = tk.StringVar()
        self.dsg_out = tk.StringVar()

        ttk.Label(parent, text='文件选择').pack(anchor='w', pady=2)
        self.create_file_entry(parent, '输入CSV:', self.dsg_in,  False).pack(fill='x', pady=1)
        self.create_file_entry(parent, '输出文件:', self.dsg_out, True).pack(fill='x', pady=1)

        self.create_btn_bar(parent, [
            ('处理数据', self.dsg_run,  'success'),
            ('复制结果', self.dsg_copy, 'primary'),
            ('清空',     self.dsg_clear,'danger')
        ])

        self.dsg_txt = scrolledtext.ScrolledText(parent, height=10, font=('Consolas', 9))
        self.dsg_txt.pack(fill='both', expand=1, padx=2, pady=2)

    def dsg_run(self):
        in_f  = self.dsg_in.get()
        out_f = self.dsg_out.get()
        if not (in_f and out_f):
            messagebox.showerror('错误', '请选择输入/输出文件'); return
        try:
            pts = []
            with open(in_f, encoding='ANSI') as f:
                next(csv.reader(f))  # 跳过表头
                for row in csv.reader(f):
                    if len(row) < 15: continue
                    try:
                        m   = float(row[0])
                        de  = float(row[1])
                        # 左侧宽度
                        wl1 = float(row[2]) if row[2] else 0
                        wl2 = float(row[3]) if row[3] else 0
                        wl3 = float(row[4]) if row[4] else 0
                        wo  = float(row[5]) if row[5] else 0
                        # 右侧宽度
                        wr3 = float(row[6]) if row[6] else 0
                        wr2 = float(row[7]) if row[7] else 0
                        wr1 = float(row[8]) if row[8] else 0
                        # 左侧高差
                        hl1 = float(row[9])  if row[9]  else 0
                        hl2 = float(row[10]) if row[10] else 0
                        hl3 = float(row[11]) if row[11] else 0
                        # 右侧高差
                        hr3 = float(row[12]) if row[12] else 0
                        hr2 = float(row[13]) if row[13] else 0
                        hr1 = float(row[14]) if row[14] else 0

                        # 7 个特征点
                        pts.extend([
                            (m, -(wl1+wl2+wl3+wo), de+hl1),
                            (m, -(wl2+wl3+wo),       de+hl2),
                            (m, -(wl3+wo),           de+hl3),
                            (m,  0,                  de),
                            (m,  wr3,                de+hr3),
                            (m,  wr3+wr2,            de+hr2),
                            (m,  wr3+wr2+wr1,        de+hr1)
                        ])
                    except:
                        continue

            pts = sorted(set((round(a,3),round(b,3),round(c,3)) for a,b,c in pts),
                         key=lambda x:(x[0],x[1]))

            with open(out_f,'w',newline='',encoding='ANSI') as f:
                w=csv.writer(f); w.writerow(['里程','偏距','高程'])
                w.writerows([f'{a:.3f}',f'{b:.3f}',f'{c:.3f}'] for a,b,c in pts)

            self.dsg_txt.delete('1.0','end')
            self.dsg_txt.insert('end', f'生成 {len(pts)} 个特征点，已保存')
            messagebox.showinfo('完成', f'{len(pts)} 个点')
            self.status.set("路基设计完成")
        except Exception as e:
            messagebox.showerror('错误', str(e))

    def dsg_copy(self):
        t = self.dsg_txt.get('1.0','end').strip()
        if t:
            self.root.clipboard_clear(); self.root.clipboard_append(t)
            messagebox.showinfo('完成','已复制'); self.status.set('结果已复制')
        else:
            messagebox.showwarning('提示','无内容')

    def dsg_clear(self):
        self.dsg_txt.delete('1.0','end'); self.status.set('已清空')

# ---------------- 启动 ----------------
if __name__ == '__main__':
    root = tk.Tk()
    style = ttk.Style()
    style.theme_use('clam')
    style.configure('TButton', font=('Arial', 9))
    style.configure('TLabel', font=('Arial', 9))
    style.configure('TEntry', font=('Arial', 9))
    style.configure('primary.TButton', foreground='white', background='#3498db')
    style.configure('success.TButton', foreground='white', background='#27ae60')
    style.configure('warning.TButton', foreground='white', background='#f39c12')
    style.configure('danger.TButton', foreground='white', background='#e74c3c')
    CrossSectionTool(root)
    root.mainloop()