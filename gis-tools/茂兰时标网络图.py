import tkinter as tk
from tkinter import Canvas, Frame, Label
from datetime import datetime, timedelta
import math

class ProjectSchedule:
    def __init__(self, root):
        self.root = root
        self.root.title("公路建设项目进度时标网络图")
        self.root.geometry("1200x700")  # 适合大多数显示器的尺寸
        
        # 项目基础数据（基于用户提供的时间范围）
        self.start_date = datetime(2025, 10, 1)  # 项目开始日期
        
        # 活动数据（包括自由时差计算）
        self.activities = {
            # 关键路径活动
            "A": {"name": "施工准备、清表施工", "duration": 3, 
                 "start": "2025-10-01", "end": "2025-12-31", 
                 "predecessors": [], "color": "#E91E63", "y": 250, "free_float": 0},
            
            "C": {"name": "路基施工", "duration": 9, 
                 "start": "2026-01-01", "end": "2026-09-30", 
                 "predecessors": ["A"], "color": "#2196F3", "y": 250, "free_float": 0},
            
            "D": {"name": "路面施工", "duration": 8, 
                 "start": "2026-07-01", "end": "2027-02-28", 
                 "predecessors": ["C"], "color": "#4CAF50", "y": 250, "free_float": 0},
            
            "E": {"name": "交安及其他附属工程", "duration": 4, 
                 "start": "2027-01-01", "end": "2027-04-30", 
                 "predecessors": ["D"], "color": "#9C27B0", "y": 250, "free_float": 0},
            
            # 非关键路径活动（含自由时差）
            "B": {"name": "防护工程、圆管涵施工", "duration": 5, 
                 "start": "2025-11-01", "end": "2026-03-31", 
                 "predecessors": ["A"], "color": "#FF9800", "y": 350, "free_float": 6}  # 6个月自由时差
        }
        
        # 计算项目总天数
        self.total_days = (datetime.strptime(self.activities["E"]["end"], "%Y-%m-%d") - 
                         datetime.strptime(self.activities["A"]["start"], "%Y-%m-%d")).days + 1
        
        # 设置绘图参数
        self.scale_factor = 2  # 每1天对应的像素数
        self.time_axis_height = 100
        self.main_path_y = 250
        self.parallel_paths = {
            "B": {"y": 350, "color": "#FF9800"}  # 橙色
        }
        
        # 创建主画布（带滚动条）
        self.canvas_frame = Frame(root)
        self.canvas_frame.pack(fill=tk.BOTH, expand=True)
        
        self.canvas = Canvas(self.canvas_frame, bg="white", width=1500, height=800)
        self.hbar = tk.Scrollbar(self.canvas_frame, orient=tk.HORIZONTAL, command=self.canvas.xview)
        self.vbar = tk.Scrollbar(self.canvas_frame, orient=tk.VERTICAL, command=self.canvas.yview)
        self.canvas.configure(xscrollcommand=self.hbar.set, yscrollcommand=self.vbar.set)
        
        self.hbar.pack(side=tk.BOTTOM, fill=tk.X)
        self.vbar.pack(side=tk.RIGHT, fill=tk.Y)
        self.canvas.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        
        # 设置画布滚动区域
        self.canvas.configure(scrollregion=(0, 0, 100 + self.total_days * self.scale_factor + 200, 900))
        
        # 创建标题
        title_label = Label(root, text="公路建设项目进度时标网络图", 
                          font=("Microsoft YaHei", 18, "bold"), fg="#333333")
        title_label.pack(pady=(10, 0))
        
        # 绘制组件
        self.draw_time_axis()
        self.draw_main_path()
        self.draw_parallel_activities()
        self.draw_dependencies()
        self.create_legend()
        
        # 绑定鼠标滚轮滚动
        self.canvas.bind("<MouseWheel>", self._on_mousewheel)
    
    def _on_mousewheel(self, event):
        """鼠标滚轮滚动事件"""
        if event.delta > 0:
            self.canvas.yview_scroll(-1, "units")
        else:
            self.canvas.yview_scroll(1, "units")
    
    def date_to_x(self, date_str):
        """日期转x坐标"""
        date = datetime.strptime(date_str, "%Y-%m-%d")
        delta = (date - self.start_date).days
        return 150 + delta * self.scale_factor
    
    def format_date(self, date_str):
        """格式化日期为YYYY-MM"""
        date = datetime.strptime(date_str, "%Y-%m-%d")
        return date.strftime("%Y-%m")
    
    def draw_time_axis(self):
        """绘制时间轴"""
        start_x = 150
        end_x = start_x + self.total_days * self.scale_factor
        
        # 时间轴基线
        self.canvas.create_line(start_x, self.time_axis_height, end_x, self.time_axis_height, 
                               width=3, fill="#333333")
        
        # 添加开始和结束标记
        self.canvas.create_text(start_x, self.time_axis_height - 40, 
                              text=f"开始\n{self.format_date(self.activities['A']['start'])}",
                              font=("Microsoft YaHei", 12, "bold"), fill="#2196F3", anchor=tk.SW)
        self.canvas.create_text(end_x, self.time_axis_height - 40, 
                              text=f"结束\n{self.format_date(self.activities['E']['end'])}",
                              font=("Microsoft YaHei", 12, "bold"), fill="#2196F3", anchor=tk.SE)
        
        # 每月刻度标记
        current_date = self.start_date.replace(day=1)
        end_date = datetime.strptime(self.activities["E"]["end"], "%Y-%m-%d")
        
        while current_date <= end_date:
            x = self.date_to_x(current_date.strftime("%Y-%m-%d"))
            if x >= 150 and x <= end_x:
                # 月份刻度线
                self.canvas.create_line(x, self.time_axis_height-15, x, self.time_axis_height+15, 
                                     width=2, fill="#F44336")
                # 月份标签
                month_text = current_date.strftime("%Y-%m")
                self.canvas.create_text(x, self.time_axis_height-30, text=month_text, 
                                     font=("Microsoft YaHei", 10, "bold"), fill="#F44336")
            
            # 移动到下个月
            next_month = current_date.month + 1
            next_year = current_date.year
            if next_month > 12:
                next_month = 1
                next_year += 1
            current_date = current_date.replace(year=next_year, month=next_month, day=1)
    
    def draw_main_path(self):
        """绘制关键路径"""
        key_activities = ["A", "C", "D", "E"]
        nodes = {}
        
        # 创建节点位置
        for i, code in enumerate(key_activities):
            act = self.activities[code]
            x = self.date_to_x(act["start"])
            nodes[i+1] = (x, self.main_path_y)
        
        # 添加结束节点
        nodes[len(key_activities)+1] = (self.date_to_x(self.activities["E"]["end"]), self.main_path_y)
        
        # 绘制关键路径
        for i in range(1, len(nodes)):
            x1, y1 = nodes[i]
            x2, y2 = nodes[i+1]
            code = key_activities[i-1]
            act = self.activities[code]
            
            # 带箭头的线
            self.canvas.create_line(
                x1, y1, x2, y2, 
                width=5, 
                fill=act["color"], 
                arrow=tk.LAST, 
                arrowshape=(15, 20, 8)
            )
            
            # 活动信息
            mid_x = (x1 + x2) / 2
            label = f"{act['name']} ({code})\n{act['duration']}个月"
            self.canvas.create_text(
                mid_x, y1 - 50, 
                text=label, 
                font=("Microsoft YaHei", 11, "bold"), 
                fill=act["color"], 
                justify=tk.CENTER
            )
            
            # 日期范围
            date_range = f"{self.format_date(act['start'])} - {self.format_date(act['end'])}"
            self.canvas.create_text(
                mid_x, y1 - 25, 
                text=date_range, 
                font=("Microsoft YaHei", 10), 
                fill="#2196F3", 
                justify=tk.CENTER
            )
        
        # 绘制节点
        for i, (x, y) in nodes.items():
            self.canvas.create_oval(
                x-15, y-15, x+15, y+15, 
                fill="#FFFFFF", 
                outline="#333333", 
                width=3
            )
            self.canvas.create_text(
                x, y, 
                text=str(i), 
                font=("Microsoft YaHei", 10, "bold"), 
                fill="#C2185B"
            )
    
    def draw_parallel_activities(self):
        """绘制并行工程（非关键路径）"""
        for code, config in self.parallel_paths.items():
            act = self.activities[code]
            x1 = self.date_to_x(act["start"])
            x2 = self.date_to_x(act["end"])
            y = config["y"]
            color = config["color"]
            
            # 带箭头的线
            self.canvas.create_line(
                x1, y, x2, y, 
                width=4, 
                fill=color, 
                arrow=tk.LAST,
                arrowshape=(12, 15, 6)
            )
            
            # 活动信息
            mid_x = (x1 + x2) / 2
            label = f"{act['name']} ({code})\n{act['duration']}个月"
            self.canvas.create_text(
                mid_x, y - 50, 
                text=label, 
                font=("Microsoft YaHei", 10, "bold"), 
                fill=color, 
                justify=tk.CENTER
            )
            
            # 日期范围
            date_range = f"{self.format_date(act['start'])}-{self.format_date(act['end'])}"
            self.canvas.create_text(
                mid_x, y - 25, 
                text=date_range, 
                font=("Microsoft YaHei", 9), 
                fill="#2196F3", 
                justify=tk.CENTER
            )
            
            # 绘制节点
            for x in [x1, x2]:
                self.canvas.create_oval(
                    x-12, y-12, x+12, y+12, 
                    fill="#FFFFFF", 
                    outline=color, 
                    width=2
                )
            
            # 节点标签
            self.canvas.create_text(
                x1, y, 
                text="B1", 
                font=("Microsoft YaHei", 9, "bold"), 
                fill=color
            )
            self.canvas.create_text(
                x2, y, 
                text="B2", 
                font=("Microsoft YaHei", 9, "bold"), 
                fill=color
            )
            
            # 绘制自由时差（弹簧线）
            if act["free_float"] > 0:
                self.draw_free_float(act)
    
    def draw_free_float(self, act):
        """绘制自由时差弹簧线"""
        end_x = self.date_to_x(act["end"])
        start_x = end_x
        end_date = datetime.strptime(act["end"], "%Y-%m-%d")
        float_end_date = end_date + timedelta(days=act["free_float"]*30)  # 近似每月30天
        float_end_x = self.date_to_x(float_end_date.strftime("%Y-%m-%d"))
        
        y = self.parallel_paths["B"]["y"]
        color = "#FF5722"  # 橙色
        
        # 绘制弹簧线
        self.draw_spring_line(end_x, y, float_end_x, y, color)
        
        # 添加自由时差标签
        label_x = (end_x + float_end_x) / 2
        self.canvas.create_text(
            label_x, y - 35, 
            text=f"自由时差: {act['free_float']}个月", 
            font=("Microsoft YaHei", 9, "bold"), 
            fill=color
        )
    
    def draw_spring_line(self, x1, y1, x2, y2, color):
        """绘制弹簧形状的线"""
        # 计算线条参数
        length = abs(x2 - x1)
        segments = 10
        segment_length = length / segments
        
        # 绘制弹簧线
        points = []
        direction = 1 if x2 > x1 else -1
        amplitude = 10
        
        for i in range(segments + 1):
            x = x1 + i * segment_length * direction
            # 正弦波动
            if i % 2 == 0:
                y = y1 - amplitude
            else:
                y = y1 + amplitude
            points.extend([x, y])
        
        # 添加最后一个点
        points.extend([x2, y1])
        
        self.canvas.create_line(points, fill=color, width=2, smooth=True, dash=(4, 2))
        
        # 添加箭头
        self.canvas.create_line(
            points[-4], points[-3], points[-2], points[-1], 
            fill=color, 
            width=2, 
            arrow=tk.LAST, 
            arrowshape=(8, 10, 5)
        )
    
    def draw_dependencies(self):
        """绘制工作依赖关系"""
        # A -> B 依赖关系（部分搭接）
        a_end_x = self.date_to_x(self.activities["A"]["end"])
        b_start_x = self.date_to_x(self.activities["B"]["start"])
        self.canvas.create_line(
            a_end_x, self.main_path_y, 
            b_start_x, self.parallel_paths["B"]["y"], 
            dash=(4, 2), 
            width=1, 
            fill="#757575"
        )
        
        # C -> D 依赖关系（50%完成时开始）
        c_start_x = self.date_to_x(self.activities["C"]["start"])
        c_end_x = self.date_to_x(self.activities["C"]["end"])
        c_mid_x = c_start_x + (c_end_x - c_start_x) * 0.5
        
        d_start_x = self.date_to_x(self.activities["D"]["start"])
        
        self.canvas.create_line(
            c_mid_x, self.main_path_y, 
            d_start_x, self.main_path_y, 
            dash=(4, 2), 
            width=1, 
            fill="#757575"
        )
        
        # 添加依赖关系标签
        self.canvas.create_text(
            (a_end_x + b_start_x) / 2, 
            (self.main_path_y + self.parallel_paths["B"]["y"]) / 2, 
            text="搭接开始", 
            font=("Microsoft YaHei", 9), 
            fill="#757575"
        )
        
        self.canvas.create_text(
            (c_mid_x + d_start_x) / 2, 
            self.main_path_y - 20, 
            text="50%完成时开始", 
            font=("Microsoft YaHei", 9), 
            fill="#757575"
        )
    
    def create_legend(self):
        """创建图例"""
        legend_frame = Frame(self.root, bg="#F5F5F5", padx=10, pady=5)
        legend_frame.pack(fill=tk.X, padx=20, pady=10)
        
        # 关键路径图例
        tk.Label(
            legend_frame, 
            text="关键路径：", 
            font=("Microsoft YaHei", 11, "bold"), 
            bg="#F5F5F5"
        ).grid(row=0, column=0, padx=5, sticky="w")
        
        key_path = " → ".join([
            f"{self.activities['A']['name']} (A)",
            f"{self.activities['C']['name']} (C)",
            f"{self.activities['D']['name']} (D)",
            f"{self.activities['E']['name']} (E)"
        ])
        tk.Label(
            legend_frame, 
            text=key_path, 
            font=("Microsoft YaHei", 11), 
            bg="#F5F5F5"
        ).grid(row=0, column=1, padx=5, sticky="w")
        
        # 并行工程图例
        tk.Label(
            legend_frame, 
            text="并行工程：", 
            font=("Microsoft YaHei", 11, "bold"), 
            bg="#F5F5F5"
        ).grid(row=1, column=0, padx=5, sticky="w")
        
        tk.Label(
            legend_frame, 
            text=f"{self.activities['B']['name']} (B)", 
            font=("Microsoft YaHei", 11), 
            fg="#FF9800", 
            bg="#F5F5F5"
        ).grid(row=1, column=1, padx=5, sticky="w")
        
        # 自由时差图例
        tk.Label(
            legend_frame, 
            text="自由时差：", 
            font=("Microsoft YaHei", 11, "bold"), 
            bg="#F5F5F5"
        ).grid(row=2, column=0, padx=5, sticky="w")
        
        tk.Label(
            legend_frame, 
            text="波浪线表示自由时差（B有6个月自由时差）", 
            font=("Microsoft YaHei", 11), 
            fg="#FF5722", 
            bg="#F5F5F5"
        ).grid(row=2, column=1, padx=5, sticky="w")
        
        # 项目信息说明
        info_frame = Frame(self.root, bg="#E3F2FD", padx=10, pady=5)
        info_frame.pack(fill=tk.X, padx=20, pady=10)
        
        tk.Label(
            info_frame, 
            text="项目信息：", 
            font=("Microsoft YaHei", 11, "bold"), 
            bg="#E3F2FD", 
            fg="#0D47A1"
        ).pack(anchor="w")
        
        info_text = tk.Text(
            info_frame, 
            height=4, 
            font=("Microsoft YaHei", 10), 
            wrap=tk.WORD, 
            padx=5, 
            pady=2,
            bg="#E3F2FD",
            highlightthickness=0,
            borderwidth=0
        )
        info_text.pack(fill=tk.X)
        
        # 计算项目总工期（月）
        total_months = (datetime.strptime(self.activities["E"]["end"], "%Y-%m-%d") - 
                      datetime.strptime(self.activities["A"]["start"], "%Y-%m-%d")).days // 30
        
        project_info = f"• 项目总工期：{total_months}个月（2025年10月至2027年6月）\n"
        project_info += f"• 关键路径：A → C → D → E\n"
        project_info += f"• 非关键路径：B（防护工程、圆管涵施工）有6个月自由时差\n"
        project_info += f"• 特殊依赖关系：B在A开始1个月后开始（搭接），D在C完成50%时开始"
        
        info_text.insert(tk.END, project_info)
        info_text.config(state=tk.DISABLED)

if __name__ == "__main__":
    root = tk.Tk()
    app = ProjectSchedule(root)
    root.mainloop()
