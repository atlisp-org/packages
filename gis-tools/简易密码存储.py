import tkinter as tk
from tkinter import ttk, messagebox
import json
import base64
import hashlib
import os

class SimplePasswordManager:
    def __init__(self, root):
        self.root = root
        self.root.title("简易密码存储")
        self.root.geometry("600x400")
        
        # 检查主密码是否已设置
        if not os.path.exists("master.pw"):
            self.setup_master_password()
        else:
            self.verify_master_password()
    
    def hash_password(self, password):
        """使用SHA256哈希密码"""
        return hashlib.sha256(password.encode()).hexdigest()
    
    def simple_encrypt(self, text):
        """简单的加密函数"""
        key = self.hash_password(self.master_password)[:32]
        encoded = []
        for i, c in enumerate(text):
            key_c = key[i % len(key)]
            encoded.append(chr(ord(c) + ord(key_c) % 128))
        return base64.urlsafe_b64encode("".join(encoded).encode()).decode()
    
    def simple_decrypt(self, text):
        """简单的解密函数"""
        key = self.hash_password(self.master_password)[:32]
        decoded = base64.urlsafe_b64decode(text.encode()).decode()
        result = []
        for i, c in enumerate(decoded):
            key_c = key[i % len(key)]
            result.append(chr(ord(c) - ord(key_c) % 128))
        return "".join(result)
    
    def setup_master_password(self):
        """设置主密码窗口"""
        self.setup_win = tk.Toplevel(self.root)
        self.setup_win.title("设置主密码")
        self.setup_win.geometry("300x200")
        
        tk.Label(self.setup_win, text="设置您的主密码（请牢记）").pack(pady=10)
        
        tk.Label(self.setup_win, text="主密码:").pack()
        self.master_entry = tk.Entry(self.setup_win, show="*")
        self.master_entry.pack()
        
        tk.Label(self.setup_win, text="确认密码:").pack()
        self.confirm_entry = tk.Entry(self.setup_win, show="*")
        self.confirm_entry.pack()
        
        tk.Button(self.setup_win, text="确定", command=self.save_master).pack(pady=10)
    
    def save_master(self):
        """保存主密码"""
        pw1 = self.master_entry.get()
        pw2 = self.confirm_entry.get()
        
        if not pw1:
            messagebox.showerror("错误", "密码不能为空")
            return
        
        if pw1 != pw2:
            messagebox.showerror("错误", "两次输入不一致")
            return
            
        with open("master.pw", "w") as f:
            f.write(self.hash_password(pw1))
            
        self.master_password = pw1
        self.setup_win.destroy()
        self.create_main_ui()
    
    def verify_master_password(self):
        """验证主密码窗口"""
        self.verify_win = tk.Toplevel(self.root)
        self.verify_win.title("验证主密码")
        self.verify_win.geometry("300x150")
        
        tk.Label(self.verify_win, text="输入主密码").pack(pady=10)
        
        tk.Label(self.verify_win, text="主密码:").pack()
        self.verify_entry = tk.Entry(self.verify_win, show="*")
        self.verify_entry.pack()
        
        tk.Button(self.verify_win, text="确定", command=self.check_master).pack(pady=10)
    
    def check_master(self):
        """检查主密码是否正确"""
        input_pw = self.verify_entry.get()
        
        with open("master.pw", "r") as f:
            stored_hash = f.read().strip()
            
        if self.hash_password(input_pw) != stored_hash:
            messagebox.showerror("错误", "密码错误")
            return
            
        self.master_password = input_pw
        self.verify_win.destroy()
        self.create_main_ui()
    
    def create_main_ui(self):
        """创建主界面"""
        self.main_frame = ttk.Frame(self.root, padding="10")
        self.main_frame.pack(fill=tk.BOTH, expand=True)
        
        # 工具栏
        toolbar = ttk.Frame(self.main_frame)
        toolbar.pack(fill=tk.X, pady=5)
        
        ttk.Button(toolbar, text="添加", command=self.add_item).pack(side=tk.LEFT, padx=5)
        ttk.Button(toolbar, text="编辑", command=self.edit_item).pack(side=tk.LEFT, padx=5)
        ttk.Button(toolbar, text="删除", command=self.delete_item).pack(side=tk.LEFT, padx=5)
        ttk.Button(toolbar, text="显示密码", command=self.show_password).pack(side=tk.LEFT, padx=5)
        ttk.Button(toolbar, text="清空", command=self.clear_all).pack(side=tk.LEFT, padx=5)
        
        # 密码列表
        self.tree = ttk.Treeview(self.main_frame, columns=("name"), show="headings")
        self.tree.heading("name", text="名称")
        self.tree.column("name", width=550)
        
        scrollbar = ttk.Scrollbar(self.main_frame, orient=tk.VERTICAL, command=self.tree.yview)
        self.tree.configure(yscroll=scrollbar.set)
        
        self.tree.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        
        # 加载数据
        self.load_data()
    
    def load_data(self):
        """加载密码数据"""
        self.tree.delete(*self.tree.get_children())
        
        if os.path.exists("passwords.dat"):
            with open("passwords.dat", "r") as f:
                encrypted_data = json.load(f)
                self.items = []
                for item in encrypted_data:
                    try:
                        decrypted = {
                            "name": self.simple_decrypt(item["name"]),
                            "password": self.simple_decrypt(item["password"])
                        }
                        self.items.append(decrypted)
                    except:
                        continue
                
                for item in self.items:
                    self.tree.insert("", tk.END, values=(item["name"],))
        else:
            self.items = []
    
    def save_data(self):
        """保存密码数据"""
        encrypted_data = []
        for item in self.items:
            encrypted = {
                "name": self.simple_encrypt(item["name"]),
                "password": self.simple_encrypt(item["password"])
            }
            encrypted_data.append(encrypted)
        
        with open("passwords.dat", "w") as f:
            json.dump(encrypted_data, f)
    
    def add_item(self):
        """添加新密码"""
        dialog = tk.Toplevel(self.root)
        dialog.title("添加密码")
        dialog.geometry("300x200")
        
        tk.Label(dialog, text="名称:").pack()
        name_entry = tk.Entry(dialog)
        name_entry.pack(fill=tk.X, padx=5, pady=5)
        
        tk.Label(dialog, text="密码:").pack()
        pw_entry = tk.Entry(dialog, show="*")
        pw_entry.pack(fill=tk.X, padx=5, pady=5)
        
        def save():
            name = name_entry.get()
            password = pw_entry.get()
            
            if not name:
                messagebox.showerror("错误", "名称不能为空")
                return
                
            self.items.append({
                "name": name,
                "password": password
            })
            
            self.save_data()
            self.load_data()
            dialog.destroy()
        
        tk.Button(dialog, text="保存", command=save).pack(pady=10)
    
    def edit_item(self):
        """编辑密码"""
        selected = self.tree.selection()
        if not selected:
            messagebox.showerror("错误", "请选择一项")
            return
            
        index = self.tree.index(selected[0])
        current = self.items[index]
        
        dialog = tk.Toplevel(self.root)
        dialog.title("编辑密码")
        dialog.geometry("300x200")
        
        tk.Label(dialog, text="名称:").pack()
        name_entry = tk.Entry(dialog)
        name_entry.insert(0, current["name"])
        name_entry.pack(fill=tk.X, padx=5, pady=5)
        
        tk.Label(dialog, text="密码:").pack()
        pw_entry = tk.Entry(dialog, show="*")
        pw_entry.insert(0, current["password"])
        pw_entry.pack(fill=tk.X, padx=5, pady=5)
        
        def save():
            name = name_entry.get()
            password = pw_entry.get()
            
            if not name:
                messagebox.showerror("错误", "名称不能为空")
                return
                
            self.items[index] = {
                "name": name,
                "password": password
            }
            
            self.save_data()
            self.load_data()
            dialog.destroy()
        
        tk.Button(dialog, text="保存", command=save).pack(pady=10)
    
    def delete_item(self):
        """删除密码"""
        selected = self.tree.selection()
        if not selected:
            messagebox.showerror("错误", "请选择一项")
            return
            
        if messagebox.askyesno("确认", "确定要删除吗？"):
            index = self.tree.index(selected[0])
            del self.items[index]
            self.save_data()
            self.load_data()
    
    def show_password(self):
        """显示选中密码"""
        selected = self.tree.selection()
        if not selected:
            messagebox.showerror("错误", "请选择一项")
            return
            
        index = self.tree.index(selected[0])
        password = self.items[index]["password"]
        
        messagebox.showinfo("密码", f"密码: {password}")
    
    def clear_all(self):
        """清空所有数据"""
        if messagebox.askyesno("警告", "确定要清空所有数据吗？"):
            self.items = []
            self.save_data()
            self.load_data()

if __name__ == "__main__":
    root = tk.Tk()
    app = SimplePasswordManager(root)
    root.mainloop()