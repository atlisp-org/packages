using System;
// using System.Windows;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Threading.Tasks;

using System.Diagnostics;
using System.Drawing;
using System.Threading;
using System.Text;
using System.IO;
using System.Windows.Forms;
using Microsoft.Web.WebView2.Core;
using Microsoft.Web.WebView2.WinForms;
using System.Linq;
using Autodesk.AutoCAD.DatabaseServices;// (Database, DBPoint, Line, Spline) 
using Autodesk.AutoCAD.Geometry;//(Point3d, Line3d, Curve3d) 
using Autodesk.AutoCAD.ApplicationServices;// (Application, Document) 
using Autodesk.AutoCAD.Runtime;// (CommandMethodAttribute, RXObject, CommandFlag) 
using Autodesk.AutoCAD.EditorInput;//(Editor, PromptXOptions, PromptXResult)
using System.Runtime.InteropServices;
using Application = Autodesk.AutoCAD.ApplicationServices.Application;


namespace AtPaletteSet
{
    public partial class AtLispWv : UserControl
    {
	Database db = HostApplicationServices.WorkingDatabase;
        Editor ed = Application.DocumentManager.MdiActiveDocument.Editor;
        Document doc = Application.DocumentManager.MdiActiveDocument;

	private WebView2 webView = new WebView2();
	public AtLispWv(String uri)
        {
	    ed.WriteMessage("加载 @lisp 侧边栏\n");
            // InitializeComponent();
            Resize += new EventHandler(Form_Resize);
            webView.CoreWebView2InitializationCompleted += WebView21_CoreWebView2InitializationCompleted;
	    try{
		Initialize(uri);
		this.Controls.Add(webView);
	    }
	    catch (System.Exception ex)
	    {
		MessageBox.Show($"WebView2 initial faile:{ex.Message}");
	    }
        }
         /// <summary>
         /// 实现自适应页面缩放
         /// </summary>
        private void Form_Resize(object sender, EventArgs e)
        {
            webView.Size = ClientSize - new Size(webView.Location);
        }
        /// <summary>
        /// webview 加载完毕
        /// </summary>
        private void WebView21_CoreWebView2InitializationCompleted(object sender, CoreWebView2InitializationCompletedEventArgs e)
        {
            webView.CoreWebView2.Navigate("https://atlisp.cn/palette.html");
        }
        /// <summary>
        /// WebView2初始化
        /// </summary>
        async void Initialize(String uri)
        {
	    string userDataFolder = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "WebView2", "UserData");
	    Directory.CreateDirectory(userDataFolder); // 确保目录存在
	    var environment = await CoreWebView2Environment.CreateAsync(null, userDataFolder);
	    // webView.CoreWebView2.Navigate("https://atlisp.cn/palette.html");

	    await webView.EnsureCoreWebView2Async(environment);
	    webView.CoreWebView2.WebMessageReceived += (s, e) =>
	    {
		string msg = e.TryGetWebMessageAsString();
		Console.WriteLine($"[JS] {msg}");
	    };
	    // 加载本地 HTML
	    // string htmlPath = System.IO.Path.Combine(AppContext.BaseDirectory, "index.html");
	    webView.Source = new Uri(uri);
        }
	// 向 JS 发送消息
	private void SendToJs(string message)
	{
	    webView.CoreWebView2.PostWebMessageAsString(message);
	}
	private void WebView2_WebMessageReceived(object sender, CoreWebView2WebMessageReceivedEventArgs e)
	{
	    string message = e.WebMessageAsJson; // 获取来自 JavaScript 的消息
	    /// <script>
            /// window.chrome.webview.postMessage("Message from JavaScript!");
	    ///  </script>
	    // MessageBox.Show("Received message from JavaScript: " + message);
	    doc.SendStringToExecute(message+" ",true,false,false);
	    // Application.ShowAlertDialog(message);
	    // Application.ActiveDocument.SendCommand(message+" ");
	    // var cmdmsg =  message.Split(":")
	}
    }
    
}
