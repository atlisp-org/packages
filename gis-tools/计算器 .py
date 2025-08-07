import tkinter as tk
from tkinter import Menu
from tkinter import ttk

class GUI:
    def __init__(self):
        self.root = tk.Tk()
        self.root.title('计算器v0.1')
        self.root.geometry("250x400+1100+150")
        self.root.resizable(False, False)  # 禁止最大化
        # 创建主菜单实例
        self.menubar = Menu(self.root)
        # 显示菜单,将root根窗口的主菜单设置为menu
        self.root.config(menu=self.menubar)
        style = ttk.Style(self.root)
        style.configure("TButton", font=("Helvetica", 20))
        self.shu = ""  # 初始化存储表达式的变量
        self.interface()

    def interface(self):
        """界面编写位置"""
        self.lab = tk.Label(self.root, text="", font=("黑体", 26, "bold"))
        self.lab.place(x=5, y=200)

        self.w1 = tk.Text(self.root, width=33, height=15)
        self.w1.place(x=7, y=0)
        self.w1.insert("insert", "")
        self.w1.tag_configure("font", font=("黑体", 20))
        self.w1.tag_add("font", "1.0", "end")

        # 数字按钮
        self.btn1 = ttk.Button(self.root, text="1", command=lambda: self.append_number("1"))
        self.btn1.place(x=10, y=270, relwidth=0.2, relheight=0.1)

        self.btn2 = ttk.Button(self.root, text="2", command=lambda: self.append_number("2"))
        self.btn2.place(x=65, y=270, relwidth=0.2, relheight=0.1)

        self.btn3 = ttk.Button(self.root, text="3", command=lambda: self.append_number("3"))
        self.btn3.place(x=120, y=270, relwidth=0.2, relheight=0.1)

        self.btn4 = ttk.Button(self.root, text="4", command=lambda: self.append_number("4"))
        self.btn4.place(x=10, y=317, relwidth=0.2, relheight=0.1)

        self.btn5 = ttk.Button(self.root, text="5", command=lambda: self.append_number("5"))
        self.btn5.place(x=65, y=317, relwidth=0.2, relheight=0.1)

        self.btn6 = ttk.Button(self.root, text="6", command=lambda: self.append_number("6"))
        self.btn6.place(x=120, y=317, relwidth=0.2, relheight=0.1)

        self.btn7 = ttk.Button(self.root, text="7", command=lambda: self.append_number("7"))
        self.btn7.place(x=10, y=364, relwidth=0.2, relheight=0.1)

        self.btn8 = ttk.Button(self.root, text="8", command=lambda: self.append_number("8"))
        self.btn8.place(x=65, y=364, relwidth=0.2, relheight=0.1)

        self.btn9 = ttk.Button(self.root, text="9", command=lambda: self.append_number("9"))
        self.btn9.place(x=120, y=364, relwidth=0.2, relheight=0.1)

        self.btn0 = ttk.Button(self.root, text="0", command=lambda: self.append_number("0"))
        self.btn0.place(x=175, y=364, relwidth=0.27, relheight=0.1)

        # 操作按钮
        self.btn_ac = ttk.Button(self.root, text="AC", command=self.clear_all)
        self.btn_ac.place(x=10, y=235, relwidth=0.2, relheight=0.08)

        self.btn_mul = ttk.Button(self.root, text="×", command=lambda: self.append_operator("*"))
        self.btn_mul.place(x=65, y=235, relwidth=0.2, relheight=0.08)

        self.btn_div = ttk.Button(self.root, text="÷", command=lambda: self.append_operator("/"))
        self.btn_div.place(x=120, y=235, relwidth=0.2, relheight=0.08)

        self.btn_sub = ttk.Button(self.root, text="-", command=lambda: self.append_operator("-"))
        self.btn_sub.place(x=175, y=235, relwidth=0.27, relheight=0.08)

        self.btn_add = ttk.Button(self.root, text="+", command=lambda: self.append_operator("+"))
        self.btn_add.place(x=175, y=270, relwidth=0.27, relheight=0.1)

        self.btn_eq = ttk.Button(self.root, text="=", command=self.calculate)
        self.btn_eq.place(x=175, y=317, relwidth=0.27, relheight=0.1)

        self.btn_clear = ttk.Button(self.root, text="清空", command=self.clear_history)
        self.btn_clear.place(x=175, y=200, relwidth=0.27, relheight=0.08)

    def append_number(self, num):
        """添加数字到表达式"""
        self.shu += num
        self.lab.config(text=self.shu)

    def append_operator(self, op):
        """添加运算符到表达式"""
        if not self.shu:
            return
            
        # 检查最后一个字符是否是运算符
        if self.shu[-1] in '+-*/':
            # 替换最后一个运算符
            self.shu = self.shu[:-1] + op
        else:
            self.shu += op
            
        self.lab.config(text=self.shu)

    def calculate(self):
        """计算结果"""
        if not self.shu or self.shu[-1] in '+-*/':
            return
            
        try:
            result = eval(self.shu)
            self.w1.insert("end", f"{self.shu}={result}\n")
            self.w1.see("end")  # 滚动到最后一行
            self.shu = str(result)
            self.lab.config(text=self.shu)
        except Exception as e:
            self.w1.insert("end", f"错误: {e}\n")
            self.shu = ""
            self.lab.config(text="")

    def clear_all(self):
        """清除当前表达式"""
        self.shu = ""
        self.lab.config(text="")

    def clear_history(self):
        """清除历史记录"""
        self.w1.delete(1.0, tk.END)

if __name__ == '__main__':
    app = GUI()
    app.root.mainloop()