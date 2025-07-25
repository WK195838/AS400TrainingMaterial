# 第四週：ASP.NET Core MVC基礎 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 理解MVC架構模式和ASP.NET Core框架
- 掌握Controller、Action和路由系統的設計
- 熟練使用Razor View Engine建立動態網頁
- 實作完整的資料驗證和錯誤處理機制
- 建立可維護和可測試的Web應用程式架構
- 理解依賴注入和中介軟體的概念

---

## 🏗️ 第一節：MVC架構概念

### 1.1 MVC模式基礎

#### 1.1.1 MVC架構原理

```
MVC (Model-View-Controller) 架構模式：

┌─────────────────────────────────────────┐
│              使用者 (User)               │
└─────────────┬───────────────────────────┘
              │ HTTP Request
              ▼
┌─────────────────────────────────────────┐
│            路由系統 (Routing)            │
│  • URL分析和解析                       │
│  • Controller和Action決定              │
└─────────────┬───────────────────────────┘
              │
              ▼
┌─────────────────────────────────────────┐
│          Controller (控制器)            │
│  • 處理HTTP請求                        │
│  • 商業邏輯協調                        │
│  • Model數據獲取                       │
│  • View選擇和渲染                      │
└─────┬───────────────────────────────┬───┘
      │                               │
      ▼                               ▼
┌───────────────┐               ┌─────────────────┐
│  Model (模型) │               │   View (視圖)   │
│  • 數據實體   │               │   • UI呈現      │
│  • 業務邏輯   │               │   • 用戶交互    │
│  • 數據驗證   │               │   • 模板引擎    │
│  • 數據存取   │               │   • HTML生成    │
└───────────────┘               └─────────────────┘
```

#### 1.1.2 ASP.NET Core專案結構

```csharp
/*
標準ASP.NET Core MVC專案結構：

ECommerceApp/
├── Controllers/           // 控制器
│   ├── HomeController.cs
│   ├── ProductController.cs
│   └── AccountController.cs
├── Models/               // 模型和資料類別
│   ├── Entities/         // 實體類別
│   ├── ViewModels/       // 視圖模型
│   └── DTOs/            // 資料傳輸物件
├── Views/               // 視圖檔案
│   ├── Home/
│   ├── Product/
│   ├── Shared/          // 共用視圖
│   └── _ViewStart.cshtml
├── Services/            // 業務邏輯服務
├── Data/               // 資料存取層
├── wwwroot/            // 靜態檔案
│   ├── css/
│   ├── js/
│   └── images/
├── appsettings.json    // 組態設定
├── Program.cs          // 應用程式進入點
└── Startup.cs         // 服務和中介軟體設定
*/

using Microsoft.EntityFrameworkCore;
using ECommerceApp.Data;
using ECommerceApp.Services;

namespace ECommerceApp
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);
            
            // 設定服務
            ConfigureServices(builder.Services, builder.Configuration);
            
            var app = builder.Build();
            
            // 設定中介軟體管線
            ConfigureMiddleware(app);
            
            app.Run();
        }
        
        private static void ConfigureServices(IServiceCollection services, IConfiguration configuration)
        {
            // 資料庫連接
            services.AddDbContext<ApplicationDbContext>(options =>
                options.UseSqlServer(configuration.GetConnectionString("DefaultConnection")));
            
            // MVC服務
            services.AddControllersWithViews(options =>
            {
                // 全域篩選器
                options.Filters.Add<GlobalExceptionFilter>();
            });
            
            // 依賴注入
            services.AddScoped<IProductService, ProductService>();
            services.AddScoped<ICustomerService, CustomerService>();
            services.AddScoped<IOrderService, OrderService>();
            
            // 身分驗證和授權
            services.AddAuthentication()
                .AddCookie(options =>
                {
                    options.LoginPath = "/Account/Login";
                    options.LogoutPath = "/Account/Logout";
                    options.AccessDeniedPath = "/Account/AccessDenied";
                });
            
            // 快取
            services.AddMemoryCache();
            services.AddResponseCaching();
            
            // 其他服務
            services.AddAutoMapper(typeof(MappingProfile));
            services.AddHttpClient();
        }
        
        private static void ConfigureMiddleware(WebApplication app)
        {
            // 開發環境設定
            if (app.Environment.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }
            else
            {
                app.UseExceptionHandler("/Home/Error");
                app.UseHsts(); // HTTP Strict Transport Security
            }
            
            // 靜態檔案
            app.UseStaticFiles();
            
            // 路由
            app.UseRouting();
            
            // 身分驗證和授權
            app.UseAuthentication();
            app.UseAuthorization();
            
            // 快取
            app.UseResponseCaching();
            
            // MVC路由設定
            app.MapControllerRoute(
                name: "areas",
                pattern: "{area:exists}/{controller=Home}/{action=Index}/{id?}");
                
            app.MapControllerRoute(
                name: "default",
                pattern: "{controller=Home}/{action=Index}/{id?}");
        }
    }
}
```

### 1.2 ASP.NET Core請求生命週期

#### 1.2.1 HTTP請求處理流程

```csharp
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Filters;

namespace ECommerceApp.Infrastructure
{
    // 自訂中介軟體示範請求處理流程
    public class RequestLoggingMiddleware
    {
        private readonly RequestDelegate _next;
        private readonly ILogger<RequestLoggingMiddleware> _logger;
        
        public RequestLoggingMiddleware(RequestDelegate next, ILogger<RequestLoggingMiddleware> logger)
        {
            _next = next;
            _logger = logger;
        }
        
        public async Task InvokeAsync(HttpContext context)
        {
            var stopwatch = System.Diagnostics.Stopwatch.StartNew();
            
            // 請求開始
            _logger.LogInformation("請求開始: {Method} {Path} from {RemoteIP}",
                context.Request.Method,
                context.Request.Path,
                context.Connection.RemoteIpAddress);
            
            try
            {
                // 呼叫下一個中介軟體
                await _next(context);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "請求處理發生錯誤");
                throw;
            }
            finally
            {
                stopwatch.Stop();
                
                // 請求完成
                _logger.LogInformation("請求完成: {Method} {Path} - {StatusCode} in {ElapsedMs}ms",
                    context.Request.Method,
                    context.Request.Path,
                    context.Response.StatusCode,
                    stopwatch.ElapsedMilliseconds);
            }
        }
    }
    
    // 動作篩選器 - 示範MVC請求管線
    public class ActionLoggingFilter : ActionFilterAttribute
    {
        private readonly ILogger<ActionLoggingFilter> _logger;
        
        public ActionLoggingFilter(ILogger<ActionLoggingFilter> logger)
        {
            _logger = logger;
        }
        
        public override void OnActionExecuting(ActionExecutingContext context)
        {
            _logger.LogInformation("Action執行前: {Controller}.{Action}",
                context.Controller.GetType().Name,
                context.ActionDescriptor.DisplayName);
            
            // 記錄參數
            foreach (var parameter in context.ActionArguments)
            {
                _logger.LogDebug("參數: {Key} = {Value}",
                    parameter.Key,
                    parameter.Value);
            }
        }
        
        public override void OnActionExecuted(ActionExecutedContext context)
        {
            _logger.LogInformation("Action執行後: {Controller}.{Action} - Result: {Result}",
                context.Controller.GetType().Name,
                context.ActionDescriptor.DisplayName,
                context.Result?.GetType().Name);
                
            if (context.Exception != null)
            {
                _logger.LogError(context.Exception, "Action執行發生異常");
            }
        }
        
        public override void OnResultExecuting(ResultExecutingContext context)
        {
            _logger.LogInformation("Result執行前: {ResultType}",
                context.Result.GetType().Name);
        }
        
        public override void OnResultExecuted(ResultExecutedContext context)
        {
            _logger.LogInformation("Result執行後: {ResultType}",
                context.Result.GetType().Name);
        }
    }
    
    // 全域異常篩選器
    public class GlobalExceptionFilter : IExceptionFilter
    {
        private readonly ILogger<GlobalExceptionFilter> _logger;
        private readonly IWebHostEnvironment _environment;
        
        public GlobalExceptionFilter(ILogger<GlobalExceptionFilter> logger, IWebHostEnvironment environment)
        {
            _logger = logger;
            _environment = environment;
        }
        
        public void OnException(ExceptionContext context)
        {
            _logger.LogError(context.Exception, "未處理的異常發生在 {Controller}.{Action}",
                context.RouteData.Values["controller"],
                context.RouteData.Values["action"]);
            
            var result = new ViewResult
            {
                ViewName = "Error"
            };
            
            // 開發環境顯示詳細錯誤資訊
            if (_environment.IsDevelopment())
            {
                result.ViewData.Add("Exception", context.Exception);
            }
            
            context.Result = result;
            context.ExceptionHandled = true;
        }
    }
}
```

---

## 🎮 第二節：Controller和Action

### 2.1 Controller基礎

#### 2.1.1 Controller類別設計

```csharp
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Authorization;
using ECommerceApp.Models;
using ECommerceApp.Services;
using ECommerceApp.ViewModels;
using AutoMapper;

namespace ECommerceApp.Controllers
{
    [Route("products")]
    public class ProductController : Controller
    {
        private readonly IProductService _productService;
        private readonly ICategoryService _categoryService;
        private readonly IMapper _mapper;
        private readonly ILogger<ProductController> _logger;
        
        public ProductController(
            IProductService productService,
            ICategoryService categoryService,
            IMapper mapper,
            ILogger<ProductController> logger)
        {
            _productService = productService ?? throw new ArgumentNullException(nameof(productService));
            _categoryService = categoryService ?? throw new ArgumentNullException(nameof(categoryService));
            _mapper = mapper ?? throw new ArgumentNullException(nameof(mapper));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }
        
        // GET: /products
        [HttpGet("")]
        public async Task<IActionResult> Index(ProductSearchViewModel model)
        {
            try
            {
                _logger.LogInformation("載入產品列表頁面，搜尋條件: {@SearchModel}", model);
                
                // 設定預設值
                model.PageSize = model.PageSize <= 0 ? 12 : Math.Min(model.PageSize, 100);
                model.Page = Math.Max(model.Page, 1);
                
                // 載入類別列表
                var categories = await _categoryService.GetActiveCategoriesAsync();
                
                // 搜尋產品
                var searchResult = await _productService.SearchProductsAsync(
                    searchTerm: model.SearchTerm,
                    categoryId: model.CategoryId,
                    minPrice: model.MinPrice,
                    maxPrice: model.MaxPrice,
                    sortBy: model.SortBy,
                    page: model.Page,
                    pageSize: model.PageSize);
                
                // 建立ViewModel
                var viewModel = new ProductIndexViewModel
                {
                    SearchModel = model,
                    Products = _mapper.Map<List<ProductCardViewModel>>(searchResult.Products),
                    Categories = _mapper.Map<List<CategoryViewModel>>(categories),
                    TotalCount = searchResult.TotalCount,
                    TotalPages = (int)Math.Ceiling((double)searchResult.TotalCount / model.PageSize),
                    HasPreviousPage = model.Page > 1,
                    HasNextPage = model.Page < (int)Math.Ceiling((double)searchResult.TotalCount / model.PageSize)
                };
                
                return View(viewModel);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "載入產品列表時發生錯誤");
                TempData["ErrorMessage"] = "載入產品列表時發生錯誤，請稍後再試。";
                return View(new ProductIndexViewModel());
            }
        }
        
        // GET: /products/{id}
        [HttpGet("{id:int}")]
        public async Task<IActionResult> Details(int id)
        {
            if (id <= 0)
            {
                _logger.LogWarning("無效的產品ID: {ProductId}", id);
                return BadRequest("無效的產品ID");
            }
            
            try
            {
                var product = await _productService.GetProductByIdAsync(id);
                if (product == null)
                {
                    _logger.LogWarning("找不到產品: {ProductId}", id);
                    return NotFound("找不到指定的產品");
                }
                
                // 載入相關產品
                var relatedProducts = await _productService.GetRelatedProductsAsync(product.CategoryId, id, 4);
                
                // 載入產品評論
                var reviews = await _productService.GetProductReviewsAsync(id, 1, 10);
                
                var viewModel = new ProductDetailsViewModel
                {
                    Product = _mapper.Map<ProductViewModel>(product),
                    RelatedProducts = _mapper.Map<List<ProductCardViewModel>>(relatedProducts),
                    Reviews = _mapper.Map<List<ProductReviewViewModel>>(reviews.Reviews),
                    AverageRating = reviews.AverageRating,
                    TotalReviews = reviews.TotalCount
                };
                
                // 記錄產品檢視
                await _productService.RecordProductViewAsync(id, GetUserIdOrDefault(), GetUserIpAddress());
                
                return View(viewModel);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "載入產品詳情時發生錯誤，產品ID: {ProductId}", id);
                TempData["ErrorMessage"] = "載入產品詳情時發生錯誤。";
                return RedirectToAction(nameof(Index));
            }
        }
        
        // GET: /products/create
        [HttpGet("create")]
        [Authorize(Roles = "Admin,Manager")]
        public async Task<IActionResult> Create()
        {
            var viewModel = new ProductCreateViewModel();
            await PopulateCategoriesAsync(viewModel);
            return View(viewModel);
        }
        
        // POST: /products/create
        [HttpPost("create")]
        [Authorize(Roles = "Admin,Manager")]
        [ValidateAntiForgeryToken]
        public async Task<IActionResult> Create(ProductCreateViewModel model)
        {
            if (!ModelState.IsValid)
            {
                await PopulateCategoriesAsync(model);
                return View(model);
            }
            
            try
            {
                // 檢查產品代碼是否重複
                if (await _productService.IsProductCodeExistsAsync(model.ProductCode))
                {
                    ModelState.AddModelError(nameof(model.ProductCode), "產品代碼已存在");
                    await PopulateCategoriesAsync(model);
                    return View(model);
                }
                
                // 建立產品實體
                var product = _mapper.Map<Product>(model);
                product.CreatedBy = User.Identity.Name;
                product.CreatedAt = DateTime.UtcNow;
                
                // 處理圖片上傳
                if (model.ImageFile != null)
                {
                    var imagePath = await _productService.SaveProductImageAsync(model.ImageFile);
                    product.ImagePath = imagePath;
                }
                
                // 儲存產品
                var createdProduct = await _productService.CreateProductAsync(product);
                
                _logger.LogInformation("產品已建立: {ProductId} - {ProductName}", 
                    createdProduct.ProductId, createdProduct.ProductName);
                
                TempData["SuccessMessage"] = $"產品 '{createdProduct.ProductName}' 已成功建立。";
                return RedirectToAction(nameof(Details), new { id = createdProduct.ProductId });
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "建立產品時發生錯誤: {@Model}", model);
                ModelState.AddModelError("", "建立產品時發生錯誤，請稍後再試。");
                await PopulateCategoriesAsync(model);
                return View(model);
            }
        }
        
        // GET: /products/{id}/edit
        [HttpGet("{id:int}/edit")]
        [Authorize(Roles = "Admin,Manager")]
        public async Task<IActionResult> Edit(int id)
        {
            var product = await _productService.GetProductByIdAsync(id);
            if (product == null)
            {
                return NotFound();
            }
            
            var viewModel = _mapper.Map<ProductEditViewModel>(product);
            await PopulateCategoriesAsync(viewModel);
            
            return View(viewModel);
        }
        
        // POST: /products/{id}/edit
        [HttpPost("{id:int}/edit")]
        [Authorize(Roles = "Admin,Manager")]
        [ValidateAntiForgeryToken]
        public async Task<IActionResult> Edit(int id, ProductEditViewModel model)
        {
            if (id != model.ProductId)
            {
                return BadRequest();
            }
            
            if (!ModelState.IsValid)
            {
                await PopulateCategoriesAsync(model);
                return View(model);
            }
            
            try
            {
                var existingProduct = await _productService.GetProductByIdAsync(id);
                if (existingProduct == null)
                {
                    return NotFound();
                }
                
                // 檢查產品代碼是否重複（排除自己）
                if (model.ProductCode != existingProduct.ProductCode &&
                    await _productService.IsProductCodeExistsAsync(model.ProductCode))
                {
                    ModelState.AddModelError(nameof(model.ProductCode), "產品代碼已存在");
                    await PopulateCategoriesAsync(model);
                    return View(model);
                }
                
                // 更新產品資訊
                _mapper.Map(model, existingProduct);
                existingProduct.UpdatedBy = User.Identity.Name;
                existingProduct.UpdatedAt = DateTime.UtcNow;
                
                // 處理圖片更新
                if (model.ImageFile != null)
                {
                    // 刪除舊圖片
                    if (!string.IsNullOrEmpty(existingProduct.ImagePath))
                    {
                        await _productService.DeleteProductImageAsync(existingProduct.ImagePath);
                    }
                    
                    // 上傳新圖片
                    var imagePath = await _productService.SaveProductImageAsync(model.ImageFile);
                    existingProduct.ImagePath = imagePath;
                }
                
                await _productService.UpdateProductAsync(existingProduct);
                
                _logger.LogInformation("產品已更新: {ProductId} - {ProductName}", 
                    existingProduct.ProductId, existingProduct.ProductName);
                
                TempData["SuccessMessage"] = $"產品 '{existingProduct.ProductName}' 已成功更新。";
                return RedirectToAction(nameof(Details), new { id = existingProduct.ProductId });
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "更新產品時發生錯誤，產品ID: {ProductId}", id);
                ModelState.AddModelError("", "更新產品時發生錯誤，請稍後再試。");
                await PopulateCategoriesAsync(model);
                return View(model);
            }
        }
        
        // POST: /products/{id}/delete
        [HttpPost("{id:int}/delete")]
        [Authorize(Roles = "Admin")]
        [ValidateAntiForgeryToken]
        public async Task<IActionResult> Delete(int id)
        {
            try
            {
                var product = await _productService.GetProductByIdAsync(id);
                if (product == null)
                {
                    return NotFound();
                }
                
                await _productService.DeleteProductAsync(id);
                
                _logger.LogInformation("產品已刪除: {ProductId} - {ProductName}", 
                    product.ProductId, product.ProductName);
                
                TempData["SuccessMessage"] = $"產品 '{product.ProductName}' 已成功刪除。";
                return RedirectToAction(nameof(Index));
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "刪除產品時發生錯誤，產品ID: {ProductId}", id);
                TempData["ErrorMessage"] = "刪除產品時發生錯誤，請稍後再試。";
                return RedirectToAction(nameof(Details), new { id });
            }
        }
        
        // AJAX: /products/search-suggestions
        [HttpGet("search-suggestions")]
        public async Task<IActionResult> GetSearchSuggestions(string term)
        {
            if (string.IsNullOrWhiteSpace(term) || term.Length < 2)
            {
                return Json(new List<string>());
            }
            
            try
            {
                var suggestions = await _productService.GetSearchSuggestionsAsync(term, 10);
                return Json(suggestions);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "取得搜尋建議時發生錯誤，搜尋詞: {SearchTerm}", term);
                return Json(new List<string>());
            }
        }
        
        // 私有輔助方法
        private async Task PopulateCategoriesAsync<T>(T viewModel) where T : IHasCategorySelectList
        {
            var categories = await _categoryService.GetActiveCategoriesAsync();
            viewModel.Categories = _mapper.Map<List<CategorySelectViewModel>>(categories);
        }
        
        private int? GetUserIdOrDefault()
        {
            if (User.Identity.IsAuthenticated && 
                int.TryParse(User.FindFirst("UserId")?.Value, out int userId))
            {
                return userId;
            }
            return null;
        }
        
        private string GetUserIpAddress()
        {
            return HttpContext.Connection.RemoteIpAddress?.ToString() ?? "Unknown";
        }
    }
}
```

#### 2.1.2 Action Results和回應類型

```csharp
using Microsoft.AspNetCore.Mvc;

namespace ECommerceApp.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class ProductApiController : ControllerBase
    {
        private readonly IProductService _productService;
        
        public ProductApiController(IProductService productService)
        {
            _productService = productService;
        }
        
        // 回傳JSON資料
        [HttpGet]
        public async Task<ActionResult<ApiResponse<List<ProductDto>>>> GetProducts(
            [FromQuery] int page = 1,
            [FromQuery] int pageSize = 10,
            [FromQuery] string searchTerm = null)
        {
            try
            {
                var result = await _productService.SearchProductsAsync(searchTerm, null, null, null, "name", page, pageSize);
                
                var response = new ApiResponse<List<ProductDto>>
                {
                    Success = true,
                    Data = _mapper.Map<List<ProductDto>>(result.Products),
                    Message = "產品載入成功",
                    TotalCount = result.TotalCount,
                    Page = page,
                    PageSize = pageSize
                };
                
                return Ok(response);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "API載入產品時發生錯誤");
                
                return StatusCode(500, new ApiResponse<List<ProductDto>>
                {
                    Success = false,
                    Message = "內部伺服器錯誤",
                    Errors = new[] { "載入產品時發生錯誤" }
                });
            }
        }
        
        // 回傳特定產品
        [HttpGet("{id}")]
        public async Task<ActionResult<ApiResponse<ProductDto>>> GetProduct(int id)
        {
            if (id <= 0)
            {
                return BadRequest(new ApiResponse<ProductDto>
                {
                    Success = false,
                    Message = "無效的產品ID",
                    Errors = new[] { "產品ID必須大於0" }
                });
            }
            
            var product = await _productService.GetProductByIdAsync(id);
            if (product == null)
            {
                return NotFound(new ApiResponse<ProductDto>
                {
                    Success = false,
                    Message = "找不到指定的產品"
                });
            }
            
            return Ok(new ApiResponse<ProductDto>
            {
                Success = true,
                Data = _mapper.Map<ProductDto>(product),
                Message = "產品載入成功"
            });
        }
        
        // 建立新產品
        [HttpPost]
        [Authorize(Roles = "Admin,Manager")]
        public async Task<ActionResult<ApiResponse<ProductDto>>> CreateProduct([FromBody] ProductCreateDto createDto)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(new ApiResponse<ProductDto>
                {
                    Success = false,
                    Message = "輸入資料驗證失敗",
                    Errors = ModelState.Values.SelectMany(v => v.Errors).Select(e => e.ErrorMessage)
                });
            }
            
            try
            {
                // 檢查產品代碼是否重複
                if (await _productService.IsProductCodeExistsAsync(createDto.ProductCode))
                {
                    return Conflict(new ApiResponse<ProductDto>
                    {
                        Success = false,
                        Message = "產品代碼已存在"
                    });
                }
                
                var product = _mapper.Map<Product>(createDto);
                product.CreatedBy = User.Identity.Name;
                
                var createdProduct = await _productService.CreateProductAsync(product);
                
                var response = new ApiResponse<ProductDto>
                {
                    Success = true,
                    Data = _mapper.Map<ProductDto>(createdProduct),
                    Message = "產品建立成功"
                };
                
                return CreatedAtAction(nameof(GetProduct), new { id = createdProduct.ProductId }, response);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "API建立產品時發生錯誤: {@CreateDto}", createDto);
                
                return StatusCode(500, new ApiResponse<ProductDto>
                {
                    Success = false,
                    Message = "建立產品時發生錯誤"
                });
            }
        }
        
        // 檔案上傳
        [HttpPost("{id}/image")]
        [Authorize(Roles = "Admin,Manager")]
        public async Task<ActionResult<ApiResponse<string>>> UploadProductImage(int id, [FromForm] IFormFile imageFile)
        {
            if (imageFile == null || imageFile.Length == 0)
            {
                return BadRequest(new ApiResponse<string>
                {
                    Success = false,
                    Message = "請選擇要上傳的圖片檔案"
                });
            }
            
            // 檔案類型驗證
            var allowedExtensions = new[] { ".jpg", ".jpeg", ".png", ".gif" };
            var fileExtension = Path.GetExtension(imageFile.FileName).ToLower();
            
            if (!allowedExtensions.Contains(fileExtension))
            {
                return BadRequest(new ApiResponse<string>
                {
                    Success = false,
                    Message = "不支援的檔案格式，請上傳 JPG、PNG 或 GIF 檔案"
                });
            }
            
            // 檔案大小驗證 (5MB)
            if (imageFile.Length > 5 * 1024 * 1024)
            {
                return BadRequest(new ApiResponse<string>
                {
                    Success = false,
                    Message = "檔案過大，請上傳5MB以內的圖片"
                });
            }

            try
            {
                var imagePath = await _productService.SaveProductImageAsync(imageFile);
                return Ok(new ApiResponse<string>
                {
                    Success = true,
                    Data = imagePath,
                    Message = "圖片上傳成功"
                });
            }
            catch (Exception ex)
            {
                return StatusCode(500, new ApiResponse<string>
                {
                    Success = false,
                    Message = "圖片上傳失敗"
                });
            }
        }
    }
}
```

---

## 🖼️ 第三節：Razor View 與前端整合

### 3.1 Razor語法基礎

- `@{ ... }`：C#程式區塊
- `@Model`：存取ViewModel
- `@Html.*`：輔助方法產生表單、連結等
- `@foreach`、`@if`：流程控制

**範例：**
```cshtml
@model ProductIndexViewModel
<h2>產品列表</h2>
<ul>
@foreach (var product in Model.Products)
{
    <li>@product.ProductName - @product.UnitPrice.ToString("C")</li>
}
</ul>
```

### 3.2 Layout與Partial View

- `_Layout.cshtml`：全站共用版型
- `@RenderBody()`：主內容區
- `@RenderSection()`：可選區塊
- `Partial View`：重複區塊（如產品卡片、表單）

---

## ✅ 第四節：資料驗證與錯誤處理

- ModelState驗證、DataAnnotations屬性
- 自訂驗證屬性與伺服器端/用戶端驗證
- 全域例外處理（Exception Filter）、自訂錯誤頁

---

## 🧩 第五節：依賴注入與中介軟體

- 依賴注入（DI）設計模式，提升可維護性與測試性
- 服務註冊（AddScoped/AddSingleton/AddTransient）
- 中介軟體（Middleware）請求管線設計

---

## 📝 本週總結與學習建議

本週我們學習了ASP.NET Core MVC的架構、Controller/Action設計、Razor View、資料驗證、依賴注入與中介軟體。

**學習建議：**
- 多練習建立Controller與View，體驗MVC流程
- 嘗試自訂驗證屬性與錯誤處理
- 練習使用依賴注入與服務註冊
- 預習ASP.NET Core身份驗證與授權

---

*本教材版權所有，僅供學習使用。如有疑問，請聯繫課程講師。*
                