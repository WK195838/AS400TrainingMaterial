# 第六週：專案實戰開發 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 整合前面所學的所有技術建立完整的Web應用程式
- 實作完整的電商系統核心功能
- 掌握專案架構設計和模組化開發
- 運用最佳實務進行程式碼組織和管理
- 實作安全性、效能優化和使用者體驗
- 具備完整的全端Web開發能力

---

## 🚀 第一節：專案架構設計

### 1.1 電商系統架構規劃

#### 1.1.1 系統架構圖

```
電商系統整體架構：
┌─────────────────────────────────────────┐
│           前端層 (Presentation)         │
│ ┌─────────────────────────────────────┐ │
│ │ • 響應式Web界面 (Bootstrap + JS)    │ │
│ │ • 管理後台界面                     │ │
│ │ • RESTful API接口                  │ │
│ │ • 移動端友好界面                   │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↕ HTTP/HTTPS
┌─────────────────────────────────────────┐
│         應用服務層 (Application)        │
│ ┌─────────────────────────────────────┐ │
│ │ • ASP.NET Core MVC                 │ │
│ │ • 依賴注入容器                     │ │
│ │ • 身分驗證與授權                   │ │
│ │ • 中介軟體管線                     │ │
│ │ • AutoMapper對象映射               │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↕ Service Layer
┌─────────────────────────────────────────┐
│         業務邏輯層 (Business Logic)     │
│ ┌─────────────────────────────────────┐ │
│ │ • 產品管理服務                     │ │
│ │ • 訂單處理服務                     │ │
│ │ • 購物車管理                       │ │
│ │ │ 支付處理服務                     │ │
│ │ • 庫存管理服務                     │ │
│ │ • 會員管理服務                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↕ Repository Pattern
┌─────────────────────────────────────────┐
│         資料存取層 (Data Access)        │
│ ┌─────────────────────────────────────┐ │
│ │ • Entity Framework Core            │ │
│ │ • Repository Pattern              │ │
│ │ • Unit of Work Pattern            │ │
│ │ • 資料庫連接池                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↕ Database Connection
┌─────────────────────────────────────────┐
│            資料庫層 (Database)          │
│ ┌─────────────────────────────────────┐ │
│ │ • SQL Server 資料庫                │ │
│ │ • Redis 快取系統                   │ │
│ │ • 檔案儲存系統                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

#### 1.1.2 專案結構設計

```csharp
/*
ECommerceApp 專案結構：

ECommerceApp/
├── ECommerceApp.Web/              // 主要Web應用程式
│   ├── Controllers/               // MVC控制器
│   │   ├── HomeController.cs
│   │   ├── ProductController.cs
│   │   ├── CartController.cs
│   │   ├── OrderController.cs
│   │   ├── AccountController.cs
│   │   └── AdminController.cs
│   ├── Views/                     // Razor視圖
│   │   ├── Home/
│   │   ├── Product/
│   │   ├── Cart/
│   │   ├── Order/
│   │   ├── Account/
│   │   ├── Admin/
│   │   └── Shared/
│   ├── ViewModels/               // 視圖模型
│   ├── wwwroot/                  // 靜態資源
│   │   ├── css/
│   │   ├── js/
│   │   ├── images/
│   │   └── lib/
│   ├── Program.cs                // 應用程式進入點
│   ├── appsettings.json         // 組態設定
│   └── web.config
│
├── ECommerceApp.Core/            // 核心業務邏輯
│   ├── Entities/                 // 實體類別
│   │   ├── Product.cs
│   │   ├── Category.cs
│   │   ├── Customer.cs
│   │   ├── Order.cs
│   │   ├── OrderItem.cs
│   │   └── ShoppingCart.cs
│   ├── Interfaces/               // 服務介面
│   │   ├── IProductService.cs
│   │   ├── IOrderService.cs
│   │   ├── ICartService.cs
│   │   └── IEmailService.cs
│   ├── Services/                 // 業務服務實作
│   │   ├── ProductService.cs
│   │   ├── OrderService.cs
│   │   ├── CartService.cs
│   │   └── EmailService.cs
│   ├── DTOs/                     // 資料傳輸物件
│   └── Exceptions/               // 自訂例外
│
├── ECommerceApp.Infrastructure/  // 基礎設施層
│   ├── Data/                     // 資料存取
│   │   ├── ApplicationDbContext.cs
│   │   ├── Repositories/
│   │   │   ├── IRepository.cs
│   │   │   ├── Repository.cs
│   │   │   └── UnitOfWork.cs
│   │   └── Configurations/       // EF設定
│   ├── Services/                 // 外部服務
│   │   ├── EmailService.cs
│   │   ├── FileService.cs
│   │   └── PaymentService.cs
│   └── Extensions/               // 擴展方法
│
└── ECommerceApp.Tests/           // 測試專案
    ├── UnitTests/
    ├── IntegrationTests/
    └── TestData/
*/

using Microsoft.EntityFrameworkCore;
using ECommerceApp.Core.Interfaces;
using ECommerceApp.Core.Services;
using ECommerceApp.Infrastructure.Data;
using ECommerceApp.Infrastructure.Services;

namespace ECommerceApp.Web
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);
            
            // 註冊服務
            ConfigureServices(builder.Services, builder.Configuration);
            
            var app = builder.Build();
            
            // 設定中介軟體管線
            ConfigureMiddleware(app);
            
            // 初始化資料庫
            await InitializeDatabaseAsync(app);
            
            app.Run();
        }
        
        private static void ConfigureServices(IServiceCollection services, IConfiguration configuration)
        {
            // 資料庫服務
            services.AddDbContext<ApplicationDbContext>(options =>
                options.UseSqlServer(configuration.GetConnectionString("DefaultConnection"),
                    b => b.MigrationsAssembly("ECommerceApp.Infrastructure")));
            
            // Identity服務
            services.AddDefaultIdentity<ApplicationUser>(options =>
            {
                // 密碼要求
                options.Password.RequireDigit = true;
                options.Password.RequiredLength = 8;
                options.Password.RequireNonAlphanumeric = false;
                options.Password.RequireUppercase = true;
                options.Password.RequireLowercase = true;
                
                // 使用者要求
                options.User.RequireUniqueEmail = true;
                
                // 登入要求
                options.SignIn.RequireConfirmedEmail = false;
                options.SignIn.RequireConfirmedAccount = false;
            })
            .AddRoles<IdentityRole>()
            .AddEntityFrameworkStores<ApplicationDbContext>();
            
            // Repository和Unit of Work
            services.AddScoped(typeof(IRepository<>), typeof(Repository<>));
            services.AddScoped<IUnitOfWork, UnitOfWork>();
            
            // 業務服務
            services.AddScoped<IProductService, ProductService>();
            services.AddScoped<ICategoryService, CategoryService>();
            services.AddScoped<ICartService, CartService>();
            services.AddScoped<IOrderService, OrderService>();
            services.AddScoped<ICustomerService, CustomerService>();
            services.AddScoped<IEmailService, EmailService>();
            services.AddScoped<IFileService, FileService>();
            services.AddScoped<IPaymentService, PaymentService>();
            
            // AutoMapper
            services.AddAutoMapper(typeof(Program), typeof(MappingProfile));
            
            // 快取服務
            services.AddMemoryCache();
            services.AddStackExchangeRedisCache(options =>
            {
                options.Configuration = configuration.GetConnectionString("Redis");
            });
            
            // HTTP客戶端
            services.AddHttpClient();
            
            // MVC服務
            services.AddControllersWithViews(options =>
            {
                // 全域篩選器
                options.Filters.Add<GlobalExceptionFilter>();
                options.Filters.Add<ModelStateValidationFilter>();
            });
            
            // API服務
            services.AddEndpointsApiExplorer();
            services.AddSwaggerGen();
            
            // 其他服務
            services.AddAntiforgery(options =>
            {
                options.HeaderName = "X-CSRF-TOKEN";
            });
            
            // 自訂組態
            services.Configure<EmailSettings>(configuration.GetSection("EmailSettings"));
            services.Configure<PaymentSettings>(configuration.GetSection("PaymentSettings"));
            services.Configure<FileUploadSettings>(configuration.GetSection("FileUploadSettings"));
        }
        
        private static void ConfigureMiddleware(WebApplication app)
        {
            // 開發環境設定
            if (app.Environment.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
                app.UseSwagger();
                app.UseSwaggerUI();
            }
            else
            {
                app.UseExceptionHandler("/Home/Error");
                app.UseHsts();
            }
            
            // 安全性中介軟體
            app.UseHttpsRedirection();
            app.UseSecurityHeaders();
            
            // 靜態檔案
            app.UseStaticFiles();
            
            // 路由
            app.UseRouting();
            
            // 身分驗證和授權
            app.UseAuthentication();
            app.UseAuthorization();
            
            // 自訂中介軟體
            app.UseRequestLogging();
            app.UsePerformanceMonitoring();
            
            // MVC路由
            app.MapControllerRoute(
                name: "areas",
                pattern: "{area:exists}/{controller=Home}/{action=Index}/{id?}");
                
            app.MapControllerRoute(
                name: "default",
                pattern: "{controller=Home}/{action=Index}/{id?}");
            
            // API路由
            app.MapControllers();
            
            // Razor Pages (如果需要)
            app.MapRazorPages();
        }
        
        private static async Task InitializeDatabaseAsync(WebApplication app)
        {
            using (var scope = app.Services.CreateScope())
            {
                var context = scope.ServiceProvider.GetRequiredService<ApplicationDbContext>();
                var userManager = scope.ServiceProvider.GetRequiredService<UserManager<ApplicationUser>>();
                var roleManager = scope.ServiceProvider.GetRequiredService<RoleManager<IdentityRole>>();
                
                // 執行資料庫遷移
                await context.Database.MigrateAsync();
                
                // 初始化種子資料
                await SeedDataAsync(context, userManager, roleManager);
            }
        }
        
        private static async Task SeedDataAsync(
            ApplicationDbContext context,
            UserManager<ApplicationUser> userManager,
            RoleManager<IdentityRole> roleManager)
        {
            // 建立角色
            var roles = new[] { "Admin", "Manager", "Customer" };
            foreach (var roleName in roles)
            {
                if (!await roleManager.RoleExistsAsync(roleName))
                {
                    await roleManager.CreateAsync(new IdentityRole(roleName));
                }
            }
            
            // 建立管理員帳號
            var adminEmail = "admin@ecommerce.com";
            var adminUser = await userManager.FindByEmailAsync(adminEmail);
            if (adminUser == null)
            {
                adminUser = new ApplicationUser
                {
                    UserName = adminEmail,
                    Email = adminEmail,
                    EmailConfirmed = true,
                    FirstName = "System",
                    LastName = "Administrator"
                };
                
                var result = await userManager.CreateAsync(adminUser, "Admin123!");
                if (result.Succeeded)
                {
                    await userManager.AddToRoleAsync(adminUser, "Admin");
                }
            }
            
            // 初始化產品類別
            if (!context.Categories.Any())
            {
                var categories = new[]
                {
                    new Category { Name = "電子產品", Description = "手機、電腦、相機等電子產品" },
                    new Category { Name = "服飾配件", Description = "男女服飾、鞋子、包包等" },
                    new Category { Name = "家居用品", Description = "家具、裝飾品、清潔用品等" },
                    new Category { Name = "運動健身", Description = "運動器材、健身用品、戶外用品" },
                    new Category { Name = "書籍文具", Description = "書籍、文具用品、辦公用品" }
                };
                
                context.Categories.AddRange(categories);
                await context.SaveChangesAsync();
            }
            
            // 初始化示範產品
            if (!context.Products.Any())
            {
                var electronics = context.Categories.First(c => c.Name == "電子產品");
                var clothing = context.Categories.First(c => c.Name == "服飾配件");
                
                var products = new[]
                {
                    new Product
                    {
                        Name = "智慧型手機 Pro Max",
                        Description = "最新旗艦智慧型手機，搭載頂級處理器和相機系統",
                        Price = 32900,
                        CategoryId = electronics.Id,
                        StockQuantity = 50,
                        SKU = "PHONE001",
                        IsActive = true,
                        CreatedAt = DateTime.UtcNow
                    },
                    new Product
                    {
                        Name = "無線藍牙耳機",
                        Description = "高品質無線耳機，提供卓越音質和長時間續航",
                        Price = 4990,
                        CategoryId = electronics.Id,
                        StockQuantity = 100,
                        SKU = "AUDIO001",
                        IsActive = true,
                        CreatedAt = DateTime.UtcNow
                    },
                    new Product
                    {
                        Name = "休閒T恤",
                        Description = "100%純棉材質，舒適透氣，多色可選",
                        Price = 590,
                        CategoryId = clothing.Id,
                        StockQuantity = 200,
                        SKU = "CLOTH001",
                        IsActive = true,
                        CreatedAt = DateTime.UtcNow
                    }
                };
                
                context.Products.AddRange(products);
                await context.SaveChangesAsync();
            }
        }
    }
}
```

---

## 🛒 第二節：核心功能實作

### 2.1 產品管理系統

#### 2.1.1 產品實體和資料模型

```csharp
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace ECommerceApp.Core.Entities
{
    // 產品實體
    public class Product : BaseEntity
    {
        [Required]
        [MaxLength(200)]
        public string Name { get; set; }
        
        [MaxLength(50)]
        public string SKU { get; set; }
        
        public string Description { get; set; }
        
        [Column(TypeName = "decimal(18,2)")]
        public decimal Price { get; set; }
        
        [Column(TypeName = "decimal(18,2)")]
        public decimal? OriginalPrice { get; set; }
        
        public int StockQuantity { get; set; }
        public int ReorderLevel { get; set; } = 10;
        
        // 產品屬性
        [MaxLength(100)]
        public string Brand { get; set; }
        
        [MaxLength(50)]
        public string Color { get; set; }
        
        [MaxLength(20)]
        public string Size { get; set; }
        
        [Column(TypeName = "decimal(10,3)")]
        public decimal? Weight { get; set; }
        
        [MaxLength(100)]
        public string Dimensions { get; set; }
        
        // SEO和行銷
        [MaxLength(200)]
        public string MetaTitle { get; set; }
        
        [MaxLength(500)]
        public string MetaDescription { get; set; }
        
        [MaxLength(500)]
        public string Tags { get; set; }
        
        // 狀態管理
        public bool IsActive { get; set; } = true;
        public bool IsFeatured { get; set; } = false;
        public bool IsDiscontinued { get; set; } = false;
        
        // 統計資訊
        public int ViewCount { get; set; } = 0;
        public int SalesCount { get; set; } = 0;
        public decimal AverageRating { get; set; } = 0;
        public int ReviewCount { get; set; } = 0;
        
        // 關聯
        public int CategoryId { get; set; }
        public virtual Category Category { get; set; }
        
        public virtual ICollection<ProductImage> Images { get; set; } = new List<ProductImage>();
        public virtual ICollection<ProductReview> Reviews { get; set; } = new List<ProductReview>();
        public virtual ICollection<OrderItem> OrderItems { get; set; } = new List<OrderItem>();
        public virtual ICollection<CartItem> CartItems { get; set; } = new List<CartItem>();
        public virtual ICollection<WishlistItem> WishlistItems { get; set; } = new List<WishlistItem>();
        
        // 計算屬性
        [NotMapped]
        public bool IsOnSale => OriginalPrice.HasValue && OriginalPrice > Price;
        
        [NotMapped]
        public decimal DiscountPercentage => IsOnSale ? 
            Math.Round(((OriginalPrice.Value - Price) / OriginalPrice.Value) * 100, 2) : 0;
        
        [NotMapped]
        public bool IsInStock => StockQuantity > 0;
        
        [NotMapped]
        public bool IsLowStock => StockQuantity <= ReorderLevel && StockQuantity > 0;
        
        [NotMapped]
        public string MainImageUrl => Images?.FirstOrDefault(i => i.IsMain)?.ImageUrl ?? "/images/no-image.jpg";
    }
    
    // 產品圖片
    public class ProductImage : BaseEntity
    {
        [Required]
        [MaxLength(500)]
        public string ImageUrl { get; set; }
        
        [MaxLength(200)]
        public string AltText { get; set; }
        
        public bool IsMain { get; set; } = false;
        public int DisplayOrder { get; set; } = 0;
        
        // 關聯
        public int ProductId { get; set; }
        public virtual Product Product { get; set; }
    }
    
    // 產品評論
    public class ProductReview : BaseEntity
    {
        [Range(1, 5)]
        public int Rating { get; set; }
        
        [MaxLength(200)]
        public string Title { get; set; }
        
        public string ReviewText { get; set; }
        
        public bool IsApproved { get; set; } = false;
        public int HelpfulCount { get; set; } = 0;
        
        // 關聯
        public int ProductId { get; set; }
        public virtual Product Product { get; set; }
        
        public string CustomerId { get; set; }
        public virtual ApplicationUser Customer { get; set; }
    }
    
    // 產品類別
    public class Category : BaseEntity
    {
        [Required]
        [MaxLength(100)]
        public string Name { get; set; }
        
        public string Description { get; set; }
        
        [MaxLength(500)]
        public string ImageUrl { get; set; }
        
        public bool IsActive { get; set; } = true;
        public int DisplayOrder { get; set; } = 0;
        
        // 階層式類別
        public int? ParentCategoryId { get; set; }
        public virtual Category ParentCategory { get; set; }
        public virtual ICollection<Category> SubCategories { get; set; } = new List<Category>();
        
        // 關聯
        public virtual ICollection<Product> Products { get; set; } = new List<Product>();
        
        // 計算屬性
        [NotMapped]
        public int ProductCount => Products?.Count(p => p.IsActive) ?? 0;
    }
    
    // 基底實體類別
    public abstract class BaseEntity
    {
        public int Id { get; set; }
        public DateTime CreatedAt { get; set; } = DateTime.UtcNow;
        public DateTime? UpdatedAt { get; set; }
        public string CreatedBy { get; set; }
        public string UpdatedBy { get; set; }
    }
}
```

#### 2.1.2 產品服務實作

```csharp
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Caching.Memory;
using ECommerceApp.Core.Entities;
using ECommerceApp.Core.Interfaces;
using ECommerceApp.Core.DTOs;

namespace ECommerceApp.Core.Services
{
    public class ProductService : IProductService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IMemoryCache _cache;
        private readonly IFileService _fileService;
        private readonly ILogger<ProductService> _logger;
        
        public ProductService(
            IUnitOfWork unitOfWork,
            IMemoryCache cache,
            IFileService fileService,
            ILogger<ProductService> logger)
        {
            _unitOfWork = unitOfWork;
            _cache = cache;
            _fileService = fileService;
            _logger = logger;
        }
        
        // 取得產品清單（分頁和篩選）
        public async Task<PagedResult<Product>> GetProductsAsync(ProductSearchCriteria criteria)
        {
            var query = _unitOfWork.Repository<Product>().GetQueryable()
                .Include(p => p.Category)
                .Include(p => p.Images)
                .Where(p => p.IsActive);
            
            // 搜尋條件
            if (!string.IsNullOrWhiteSpace(criteria.SearchTerm))
            {
                var searchTerm = criteria.SearchTerm.ToLower();
                query = query.Where(p => 
                    p.Name.ToLower().Contains(searchTerm) ||
                    p.Description.ToLower().Contains(searchTerm) ||
                    p.Tags.ToLower().Contains(searchTerm));
            }
            
            // 類別篩選
            if (criteria.CategoryId.HasValue)
            {
                query = query.Where(p => p.CategoryId == criteria.CategoryId.Value);
            }
            
            // 價格範圍篩選
            if (criteria.MinPrice.HasValue)
            {
                query = query.Where(p => p.Price >= criteria.MinPrice.Value);
            }
            
            if (criteria.MaxPrice.HasValue)
            {
                query = query.Where(p => p.Price <= criteria.MaxPrice.Value);
            }
            
            // 品牌篩選
            if (!string.IsNullOrWhiteSpace(criteria.Brand))
            {
                query = query.Where(p => p.Brand == criteria.Brand);
            }
            
            // 特色產品篩選
            if (criteria.IsFeatured.HasValue)
            {
                query = query.Where(p => p.IsFeatured == criteria.IsFeatured.Value);
            }
            
            // 庫存狀態篩選
            if (criteria.InStock.HasValue && criteria.InStock.Value)
            {
                query = query.Where(p => p.StockQuantity > 0);
            }
            
            // 排序
            query = criteria.SortBy?.ToLower() switch
            {
                "name" => criteria.SortDirection == "desc" 
                    ? query.OrderByDescending(p => p.Name)
                    : query.OrderBy(p => p.Name),
                "price" => criteria.SortDirection == "desc"
                    ? query.OrderByDescending(p => p.Price)
                    : query.OrderBy(p => p.Price),
                "created" => criteria.SortDirection == "desc"
                    ? query.OrderByDescending(p => p.CreatedAt)
                    : query.OrderBy(p => p.CreatedAt),
                "rating" => criteria.SortDirection == "desc"
                    ? query.OrderByDescending(p => p.AverageRating)
                    : query.OrderBy(p => p.AverageRating),
                "popularity" => query.OrderByDescending(p => p.ViewCount),
                _ => query.OrderByDescending(p => p.CreatedAt)
            };
            
            // 分頁
            var totalCount = await query.CountAsync();
            var products = await query
                .Skip((criteria.Page - 1) * criteria.PageSize)
                .Take(criteria.PageSize)
                .ToListAsync();
            
            return new PagedResult<Product>
            {
                Items = products,
                TotalCount = totalCount,
                Page = criteria.Page,
                PageSize = criteria.PageSize,
                TotalPages = (int)Math.Ceiling(totalCount / (double)criteria.PageSize)
            };
        }
        
        // 取得單一產品詳情
        public async Task<Product> GetProductByIdAsync(int id)
        {
            var cacheKey = $"product_{id}";
            
            if (_cache.TryGetValue(cacheKey, out Product cachedProduct))
            {
                return cachedProduct;
            }
            
            var product = await _unitOfWork.Repository<Product>().GetQueryable()
                .Include(p => p.Category)
                .Include(p => p.Images.OrderBy(i => i.DisplayOrder))
                .Include(p => p.Reviews.Where(r => r.IsApproved))
                    .ThenInclude(r => r.Customer)
                .FirstOrDefaultAsync(p => p.Id == id && p.IsActive);
            
            if (product != null)
            {
                // 快取30分鐘
                _cache.Set(cacheKey, product, TimeSpan.FromMinutes(30));
            }
            
            return product;
        }
        
        // 建立新產品
        public async Task<Product> CreateProductAsync(CreateProductDto dto, string createdBy)
        {
            try
            {
                // 檢查SKU是否重複
                if (!string.IsNullOrWhiteSpace(dto.SKU))
                {
                    var existingSku = await _unitOfWork.Repository<Product>()
                        .GetQueryable()
                        .AnyAsync(p => p.SKU == dto.SKU);
                    
                    if (existingSku)
                    {
                        throw new BusinessException($"SKU '{dto.SKU}' 已存在");
                    }
                }
                
                var product = new Product
                {
                    Name = dto.Name,
                    SKU = dto.SKU,
                    Description = dto.Description,
                    Price = dto.Price,
                    OriginalPrice = dto.OriginalPrice,
                    StockQuantity = dto.StockQuantity,
                    ReorderLevel = dto.ReorderLevel,
                    Brand = dto.Brand,
                    Color = dto.Color,
                    Size = dto.Size,
                    Weight = dto.Weight,
                    Dimensions = dto.Dimensions,
                    MetaTitle = dto.MetaTitle,
                    MetaDescription = dto.MetaDescription,
                    Tags = dto.Tags,
                    CategoryId = dto.CategoryId,
                    IsFeatured = dto.IsFeatured,
                    CreatedBy = createdBy
                };
                
                // 處理圖片上傳
                if (dto.ImageFiles?.Any() == true)
                {
                    var images = await ProcessProductImagesAsync(