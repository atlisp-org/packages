import os
import csv
import tkinter as tk
from tkinter import filedialog, messagebox, ttk, simpledialog
from collections import defaultdict
import threading
from pathlib import Path

def read_csv(file_path):
    """读取CSV文件，返回字典列表和表头"""
    data = []
    with open(file_path, 'r', encoding='utf-8-sig', errors='replace') as f:
        reader = csv.DictReader(f)
        headers = reader.fieldnames or []
        for row in reader:
            data.append(row)
    return data, headers

def get_complete_row(rows):
    """从重复行中选择数据最完整的行"""
    max_count = -1
    best_row = None
    
    for row in rows:
        non_null_count = sum(1 for value in row.values() if value is not None and value != '')
        if non_null_count > max_count:
            max_count = non_null_count
            best_row = row.copy()  # 复制行数据
    
    return best_row

def process_tables(folder_path, output_folder, match_column, log_callback=None):
    """处理文件夹中的所有CSV表格，提取重复值并保留最完整的记录"""
    # 检查文件夹是否存在
    if not os.path.exists(folder_path):
        if log_callback:
            log_callback(f"错误: 文件夹 {folder_path} 不存在")
        return False, None

    output_path = os.path.join(output_folder, f"duplicate_result_{match_column}.csv")
    
    # 存储所有匹配列值及其对应的行
    value_dict = defaultdict(list)
    file_count = 0
    total_rows = 0
    files_with_column = 0
    all_headers = set()

    # 遍历文件夹中的所有CSV文件
    if log_callback:
        log_callback(f"开始遍历文件夹 {folder_path} 中的CSV文件...")
    
    for root, _, files in os.walk(folder_path):
        for file in files:
            if file.lower().endswith('.csv'):
                file_path = os.path.join(root, file)
                file_count += 1
                try:
                    data, headers = read_csv(file_path)
                    if headers and match_column in headers:
                        files_with_column += 1
                        total_rows += len(data)
                        
                        # 添加来源文件信息并更新所有表头
                        all_headers.update(headers)
                        all_headers.add('来源文件')
                        
                        # 遍历每行，收集匹配列值及其对应的行
                        for row in data:
                            value = row.get(match_column)
                            if value:
                                row['来源文件'] = file
                                value_dict[value].append(row)
                    else:
                        if log_callback:
                            log_callback(f"文件 {file} 中未找到 '{match_column}' 列")
                except Exception as e:
                    if log_callback:
                        log_callback(f"读取文件 {file} 时出错: {str(e)}")

    if log_callback:
        log_callback(f"已处理 {file_count} 个CSV文件，其中 {files_with_column} 个文件包含'{match_column}'列")
        log_callback(f"共处理 {total_rows} 行数据")

    # 提取重复的值
    duplicate_values = {value: rows for value, rows in value_dict.items() if len(rows) > 1}
    if log_callback:
        log_callback(f"发现 {len(duplicate_values)} 个重复的'{match_column}'值")

    if not duplicate_values:
        if log_callback:
            log_callback(f"没有找到重复的'{match_column}'值，无需进一步处理")
        return False, None

    # 准备结果数据
    final_headers = sorted(all_headers)
    result_data = []

    # 为每个重复的值选择最完整的行，并收集所有来源文件
    for value, rows in duplicate_values.items():
        # 获取最完整的行
        complete_row = get_complete_row(rows)
        
        # 收集该值所有来源文件
        source_files = set()
        for row in rows:
            if '来源文件' in row:
                source_files.add(row['来源文件'])
        
        # 更新来源文件信息
        complete_row['来源文件'] = ', '.join(sorted(source_files))
        result_data.append(complete_row)

    # 保存结果到CSV
    if result_data:
        try:
            with open(output_path, 'w', encoding='utf-8', newline='') as f:
                writer = csv.DictWriter(f, fieldnames=final_headers)
                writer.writeheader()
                writer.writerows(result_data)
            
            if log_callback:
                log_callback(f"已将结果保存至 {output_path}，共 {len(result_data)} 条记录")
            return True, output_path
        except Exception as e:
            if log_callback:
                log_callback(f"保存结果时出错: {str(e)}")
            return False, None
    else:
        if log_callback:
            log_callback("无法生成结果数据")
        return False, None

class ColumnSelectorDialog(simpledialog.Dialog):
    """选择列名的对话框"""
    def __init__(self, parent, title, columns):
        self.columns = columns
        self.selected_column = None
        super().__init__(parent, title)
    
    def body(self, master):
        ttk.Label(master, text="请选择要匹配的列:").pack(anchor=tk.W, padx=5, pady=5)
        
        self.var = tk.StringVar()
        column_frame = ttk.Frame(master)
        column_frame.pack(fill=tk.X, padx=5, pady=5)
        
        # 创建滚动条
        canvas = tk.Canvas(column_frame)
        scrollbar = ttk.Scrollbar(column_frame, orient="vertical", command=canvas.yview)
        scrollable_frame = ttk.Frame(canvas)
        
        scrollable_frame.bind(
            "<Configure>",
            lambda e: canvas.configure(
                scrollregion=canvas.bbox("all")
            )
        )
        
        canvas.create_window((0, 0), window=scrollable_frame, anchor="nw")
        canvas.configure(yscrollcommand=scrollbar.set)
        
        canvas.pack(side="left", fill="both", expand=True)
        scrollbar.pack(side="right", fill="y")
        
        # 创建列名单选按钮
        for col in self.columns:
            ttk.Radiobutton(scrollable_frame, text=col, variable=self.var, value=col).pack(anchor=tk.W)
        
        if self.columns:
            self.var.set(self.columns[0])  # 默认选择第一个列
    
    def apply(self):
        self.selected_column = self.var.get()

class TableProcessorApp:
    def __init__(self, root):
        self.root = root
        self.root.title("CSV数据处理工具")
        self.root.geometry("700x550")
        self.root.resizable(True, True)
        
        # 设置字体确保中文显示正常
        self.style = ttk.Style()
        self.style.configure("TButton", font=("SimHei", 10))
        self.style.configure("TLabel", font=("SimHei", 10))
        self.style.configure("TEntry", font=("SimHei", 10))
        
        # 创建主框架
        main_frame = ttk.Frame(root, padding="20")
        main_frame.pack(fill=tk.BOTH, expand=True)
        
        # 输入文件夹选择
        input_frame = ttk.Frame(main_frame)
        input_frame.pack(fill=tk.X, pady=5)
        
        ttk.Label(input_frame, text="输入文件夹:", width=15).pack(side=tk.LEFT, padx=5)
        
        self.input_path_var = tk.StringVar()
        input_entry = ttk.Entry(input_frame, textvariable=self.input_path_var, width=50)
        input_entry.pack(side=tk.LEFT, padx=5, fill=tk.X, expand=True)
        
        input_btn = ttk.Button(input_frame, text="浏览...", command=self.select_input_folder)
        input_btn.pack(side=tk.LEFT, padx=5)
        
        # 输出文件夹选择
        output_frame = ttk.Frame(main_frame)
        output_frame.pack(fill=tk.X, pady=5)
        
        ttk.Label(output_frame, text="输出文件夹:", width=15).pack(side=tk.LEFT, padx=5)
        
        self.output_path_var = tk.StringVar()
        output_entry = ttk.Entry(output_frame, textvariable=self.output_path_var, width=50)
        output_entry.pack(side=tk.LEFT, padx=5, fill=tk.X, expand=True)
        
        output_btn = ttk.Button(output_frame, text="浏览...", command=self.select_output_folder)
        output_btn.pack(side=tk.LEFT, padx=5)
        
        # 匹配列选择
        column_frame = ttk.Frame(main_frame)
        column_frame.pack(fill=tk.X, pady=5)
        
        ttk.Label(column_frame, text="匹配列:", width=15).pack(side=tk.LEFT, padx=5)
        
        self.column_var = tk.StringVar()
        column_entry = ttk.Entry(column_frame, textvariable=self.column_var, width=50)
        column_entry.pack(side=tk.LEFT, padx=5, fill=tk.X, expand=True)
        
        self.detect_btn = ttk.Button(column_frame, text="自动检测列", command=self.detect_columns)
        self.detect_btn.pack(side=tk.LEFT, padx=5)
        
        # 处理按钮
        btn_frame = ttk.Frame(main_frame)
        btn_frame.pack(fill=tk.X, pady=15)
        
        self.process_btn = ttk.Button(btn_frame, text="开始处理", command=self.start_processing)
        self.process_btn.pack(side=tk.LEFT, padx=5)
        
        # 日志区域
        log_frame = ttk.LabelFrame(main_frame, text="处理日志")
        log_frame.pack(fill=tk.BOTH, expand=True, pady=10)
        
        self.log_text = tk.Text(log_frame, wrap=tk.WORD, height=15)
        self.log_text.pack(side=tk.LEFT, fill=tk.BOTH, expand=True, padx=5, pady=5)
        
        scrollbar = ttk.Scrollbar(log_frame, command=self.log_text.yview)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        self.log_text.config(yscrollcommand=scrollbar.set)
        
        # 状态栏
        self.status_var = tk.StringVar()
        self.status_var.set("就绪")
        status_bar = ttk.Label(root, textvariable=self.status_var, relief=tk.SUNKEN, anchor=tk.W)
        status_bar.pack(side=tk.BOTTOM, fill=tk.X)
        
        # 默认值
        self.input_path_var.set("")
        self.output_path_var.set(os.getcwd())
        
        # 记录上次选择的文件夹路径
        self.last_input_dir = ""
        self.last_output_dir = ""
        
        # 存储检测到的列
        self.detected_columns = []
    
    def select_input_folder(self):
        folder_path = filedialog.askdirectory(initialdir=self.last_input_dir, title="选择输入文件夹")
        if folder_path:
            self.input_path_var.set(folder_path)
            self.last_input_dir = folder_path
    
    def select_output_folder(self):
        folder_path = filedialog.askdirectory(initialdir=self.last_output_dir, title="选择输出文件夹")
        if folder_path:
            self.output_path_var.set(folder_path)
            self.last_output_dir = folder_path
    
    def log_message(self, message):
        """向日志区域添加消息"""
        self.log_text.insert(tk.END, message + "\n")
        self.log_text.see(tk.END)
        self.root.update_idletasks()  # 实时更新界面
    
    def update_status(self, message):
        """更新状态栏消息"""
        self.status_var.set(message)
        self.root.update_idletasks()
    
    def detect_columns(self):
        """自动检测输入文件夹中所有CSV文件的列"""
        input_path = self.input_path_var.get().strip()
        
        if not input_path:
            messagebox.showerror("错误", "请先选择输入文件夹")
            return
        
        if not os.path.exists(input_path):
            messagebox.showerror("错误", "输入文件夹不存在")
            return
        
        self.log_text.delete(1.0, tk.END)
        self.log_message("正在检测列...")
        
        # 收集所有CSV文件的列名
        all_columns = set()
        file_count = 0
        
        for root, _, files in os.walk(input_path):
            for file in files:
                if file.lower().endswith('.csv'):
                    file_path = os.path.join(root, file)
                    file_count += 1
                    try:
                        with open(file_path, 'r', encoding='utf-8-sig', errors='replace') as f:
                            reader = csv.reader(f)
                            headers = next(reader, [])
                            all_columns.update(headers)
                    except Exception as e:
                        self.log_message(f"读取文件 {file} 时出错: {str(e)}")
        
        if not all_columns:
            self.log_message("未检测到任何列")
            return
        
        self.detected_columns = sorted(list(all_columns))
        self.log_message(f"从 {file_count} 个CSV文件中检测到 {len(all_columns)} 个不同的列")
        
        # 显示列选择对话框
        if self.detected_columns:
            dialog = ColumnSelectorDialog(self.root, "选择匹配列", self.detected_columns)
            if dialog.selected_column:
                self.column_var.set(dialog.selected_column)
                self.log_message(f"已选择列: {dialog.selected_column}")
        else:
            self.log_message("未检测到有效列名")
    
    def start_processing(self):
        """开始处理CSV数据"""
        input_path = self.input_path_var.get().strip()
        output_path = self.output_path_var.get().strip()
        match_column = self.column_var.get().strip()
        
        if not input_path:
            messagebox.showerror("错误", "请选择输入文件夹")
            return
        
        if not output_path:
            messagebox.showerror("错误", "请选择输出文件夹")
            return
        
        if not match_column:
            messagebox.showerror("错误", "请指定要匹配的列名")
            return
        
        # 清空日志
        self.log_text.delete(1.0, tk.END)
        
        # 在新线程中处理，避免界面卡顿
        self.process_btn.config(state=tk.DISABLED)
        self.update_status("处理中...")
        threading.Thread(target=self._process_thread, args=(input_path, output_path, match_column), daemon=True).start()
    
    def _process_thread(self, input_path, output_path, match_column):
        """在线程中执行处理任务"""
        try:
            success, result_file = process_tables(input_path, output_path, match_column, self.log_message)
            
            if success:
                self.update_status("处理完成")
                self.root.after(0, lambda: messagebox.showinfo("成功", f"处理完成！\n结果已保存至: {result_file}"))
            else:
                self.update_status("处理完成（无结果）")
                self.root.after(0, lambda: messagebox.showinfo("提示", f"处理完成，但没有找到重复的'{match_column}'值"))
        except Exception as e:
            self.log_message(f"发生错误: {str(e)}")
            self.update_status("处理失败")
            self.root.after(0, lambda: messagebox.showerror("错误", f"处理过程中发生错误: {str(e)}"))
        finally:
            self.root.after(0, lambda: self.process_btn.config(state=tk.NORMAL))

if __name__ == "__main__":
    root = tk.Tk()
    app = TableProcessorApp(root)
    root.mainloop()