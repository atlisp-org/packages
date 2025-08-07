import tkinter as tk
from tkinter import Canvas, Frame, Label
from datetime import datetime, timedelta

class ProjectSchedule:
    def __init__(self, root):
        self.root = root
        self.root.title("公路建设项目进度时标网络图")
        self.root.geometry("1600x900")  # 适合大多数显示器的尺寸
        
        # 项目基础数据（2025年时间范围）
        self.start_date = datetime(2025, 8, 1)  # 调整为最早开始月份
        self.activities = {
            # 关键路径
            "路基处理": {"duration": 78, "start": "2025-08-11", "end": "2025-10-28", "desc": ""},
            "路基填筑": {"duration": 81, "start": "2025-08-15", "end": "2025-11-04", "desc": ""},
            "路面基层": {"duration": 74, "start": "2025-09-16", "end": "2025-11-29", "desc": ""},
            "路面面层": {"duration": 88, "start": "2025-10-03", "end": "2025-12-30", "desc": ""},
            
            # 非关键路径
            "涵洞施工": {"duration": 43, "start": "2025-08-13", "end": "2025-09-25", "desc": ""},
            "防护及排水": {"duration": 61, "start": "2025-08-16", "end": "2025-10-16", "desc": ""},
            "其他工程": {"duration": 99, "start": "2025-09-05", "end": "2025-12-13", "desc": ""}
        }
        
        # 计算项目总天数
        self.total_days = (datetime.strptime(self.activities["路面面层"]["end"], "%Y-%m-%d") - 
                         datetime.strptime(self.activities["路基处理"]["start"], "%Y-%m-%d")).days + 1
        
        # 设置绘图参数
        self.scale_factor = 6  # 每1天对应的像素数
        self.time_axis_height = 100
        self.main_path_y = 250
        self.parallel_paths = {
            "涵洞施工": {"y": 350, "color": "#4CAF50"},  # 绿色
            "防护及排水": {"y": 450, "color": "#9C27B0"},  # 紫色
            "其他工程": {"y": 550, "color": "#FF9800"}   # 橙色
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
        """格式化日期为MM-DD"""
        date = datetime.strptime(date_str, "%Y-%m-%d")
        return date.strftime("%m-%d")
    
    def draw_time_axis(self):
        """绘制时间轴"""
        start_x = 150
        end_x = start_x + self.total_days * self.scale_factor
        
        # 时间轴基线
        self.canvas.create_line(start_x, self.time_axis_height, end_x, self.time_axis_height, 
                               width=3, fill="#333333")
        
        # 添加开始和结束标记
        self.canvas.create_text(start_x, self.time_axis_height - 40, 
                              text=f"开始\n{self.format_date(self.activities['路基处理']['start'])}",
                              font=("Microsoft YaHei", 12, "bold"), fill="#2196F3", anchor=tk.SW)
        self.canvas.create_text(end_x, self.time_axis_height - 40, 
                              text=f"结束\n{self.format_date(self.activities['路面面层']['end'])}",
                              font=("Microsoft YaHei", 12, "bold"), fill="#2196F3", anchor=tk.SE)
        
        # 每月刻度标记
        current_date = self.start_date.replace(day=1)
        while current_date <= datetime.strptime(self.activities["路面面层"]["end"], "%Y-%m-%d"):
            x = self.date_to_x(current_date.strftime("%Y-%m-%d"))
            if x >= 150:
                # 月份刻度线
                self.canvas.create_line(x, self.time_axis_height-15, x, self.time_axis_height+15, 
                                     width=2, fill="#F44336")
                # 月份标签
                month_text = current_date.strftime("%Y-%m")
                self.canvas.create_text(x, self.time_axis_height-30, text=month_text, 
                                     font=("Microsoft YaHei", 10, "bold"), fill="#F44336")
            current_date += timedelta(days=32)
            current_date = current_date.replace(day=1)
    
    def draw_main_path(self):
        """绘制关键路径"""
        key_activities = ["路基处理", "路基填筑", "路面基层", "路面面层"]
        nodes = {}
        
        # 创建节点位置
        for i, name in enumerate(key_activities):
            act = self.activities[name]
            x = self.date_to_x(act["start"])
            nodes[i+1] = (x, self.main_path_y)
        
        # 添加结束节点
        nodes[len(key_activities)+1] = (self.date_to_x(self.activities["路面面层"]["end"]), self.main_path_y)
        
        # 绘制关键路径
        for i in range(1, len(nodes)):
            x1, y1 = nodes[i]
            x2, y2 = nodes[i+1]
            act = self.activities[key_activities[i-1]]
            
            # 带箭头的线
            self.canvas.create_line(
                x1, y1, x2, y2, 
                width=5, 
                fill="#E91E63", 
                arrow=tk.LAST, 
                arrowshape=(15, 20, 8)
            )
            
            # 活动信息
            mid_x = (x1 + x2) / 2
            label = f"{key_activities[i-1]}\n{act['duration']}天"
            self.canvas.create_text(
                mid_x, y1 - 50, 
                text=label, 
                font=("Microsoft YaHei", 11, "bold"), 
                fill="#E91E63", 
                justify=tk.CENTER
            )
            
            # 日期范围
            date_range = f"{self.format_date(act['start'])}-{self.format_date(act['end'])}"
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
                outline="#E91E63", 
                width=3
            )
            self.canvas.create_text(
                x, y, 
                text=str(i), 
                font=("Microsoft YaHei", 10, "bold"), 
                fill="#C2185B"
            )
    
    def draw_parallel_activities(self):
        """绘制并行工程"""
        for i, (name, config) in enumerate(self.parallel_paths.items()):
            act = self.activities[name]
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
            label = f"{name}\n{act['duration']}天"
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
                text=f"A{i+1}", 
                font=("Microsoft YaHei", 9, "bold"), 
                fill=color
            )
            self.canvas.create_text(
                x2, y, 
                text=f"B{i+1}", 
                font=("Microsoft YaHei", 9, "bold"), 
                fill=color
            )
            
            # 绘制与关键路径的依赖关系
            if name == "涵洞施工":
                ref_x = self.date_to_x("2025-08-13")
            elif name == "防护及排水":
                ref_x = self.date_to_x("2025-08-16")
            else:
                ref_x = self.date_to_x("2025-09-05")
            
            self.canvas.create_line(
                ref_x, self.main_path_y+15, 
                ref_x, y-12, 
                dash=(4, 2), 
                width=1, 
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
        
        key_path = " → ".join(["路基处理", "路基填筑", "路面基层", "路面面层"])
        tk.Label(
            legend_frame, 
            text=key_path, 
            font=("Microsoft YaHei", 11), 
            fg="#E91E63", 
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
            text="涵洞施工", 
            font=("Microsoft YaHei", 11), 
            fg="#4CAF50", 
            bg="#F5F5F5"
        ).grid(row=1, column=1, padx=5, sticky="w")
        
        tk.Label(
            legend_frame, 
            text="防护及排水", 
            font=("Microsoft YaHei", 11), 
            fg="#9C27B0", 
            bg="#F5F5F5"
        ).grid(row=1, column=2, padx=5, sticky="w")
        
        tk.Label(
            legend_frame, 
            text="其他工程", 
            font=("Microsoft YaHei", 11), 
            fg="#FF9800", 
            bg="#F5F5F5"
        ).grid(row=1, column=3, padx=5, sticky="w")
        
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
        
        project_info = f"• 总工期：{self.total_days}天（{self.format_date(self.activities['路基处理']['start'])}至{self.format_date(self.activities['路面面层']['end'])}）\n"
        project_info += "• 关键路径：路基处理→路基填筑→路面基层→路面面层\n"
        project_info += "• 并行工程：涵洞施工(43天)、防护及排水(61天)、其他工程(99天)"
        
        info_text.insert(tk.END, project_info)
        info_text.config(state=tk.DISABLED)

if __name__ == "__main__":
    root = tk.Tk()
    app = ProjectSchedule(root)
    root.mainloop()