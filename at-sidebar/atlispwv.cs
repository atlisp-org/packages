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
			// InitializeComponent();
			Resize += new EventHandler(Form_Resize);
			webView.CoreWebView2InitializationCompleted += WebView21_CoreWebView2InitializationCompleted;
			try
			{
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
			webView.CoreWebView2.AddHostObjectToScript("host", new ScriptHost());
			await webView.CoreWebView2.AddScriptToExecuteOnDocumentCreatedAsync("window.host = window.chrome.webview.hostObjects.host;");
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
			doc.SendStringToExecute(message + " ", true, false, false);
			// ed.Command(message);
		}
	}
	[ClassInterface(ClassInterfaceType.AutoDual)]
	[ComVisible(true)]
	public class ScriptHost
	{
		Database db = HostApplicationServices.WorkingDatabase;
		Editor ed = Application.DocumentManager.MdiActiveDocument.Editor;
		Document doc = Application.DocumentManager.MdiActiveDocument;

		public string SendCommand(string message)
		{
			doc.SendStringToExecute(message + " ", true, false, false);

			return message;
		}
	}



}
