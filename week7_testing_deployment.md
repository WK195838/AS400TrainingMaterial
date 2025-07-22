# 第七週：測試和部署 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 理解軟體測試的重要性和各種測試類型
- 實作單元測試、整合測試和端對端測試
- 掌握測試驅動開發(TDD)和行為驅動開發(BDD)
- 建立持續整合和持續部署(CI/CD)管線
- 部署Web應用程式到雲端平台
- 監控應用程式效能和錯誤處理

---

## 🧪 第一節：軟體測試基礎

### 1.1 測試類型和測試金字塔

#### 1.1.1 測試金字塔概念

```
測試金字塔 (Test Pyramid)：
┌─────────────────────────────────────────┐
│           E2E Tests (端對端測試)         │
│                                         │
│ • 模擬真實使用者行為                   │
│ • 測試完整的使用者流程                 │
│ • 執行速度慢，維護成本高               │
│ • 數量較少，但價值很高                 │
└─────────────────────────────────────────┘
            ↑ 較少數量，高價值
┌─────────────────────────────────────────┐
│      Integration Tests (整合測試)       │
│                                         │
│ • 測試組件間的互動                     │
│ • 測試資料庫存取和API呼叫              │
│ • 測試第三方服務整合                   │
│ • 執行速度中等，中等維護成本           │
└─────────────────────────────────────────┘
            ↑ 中等數量，中等價值
┌─────────────────────────────────────────┐
│        Unit Tests (單元測試)            │
│                                         │
│ • 測試個別方法和類別                   │
│ • 快速執行，低維護成本                 │
│ • 高覆蓋率，快速回饋                   │
│ • 數量最多，是測試的基礎               │
└─────────────────────────────────────────┘
            ↑ 大量數量，基礎價值

其他測試類型：
• Performance Tests (效能測試)
• Security Tests (安全性測試)  
• Usability Tests (可用性測試)
• Load Tests (負載測試)
*/
```

#### 1.1.2 測試框架和工具設定

```csharp
// ECommerceApp.Tests.csproj - 測試專案設定
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <IsPackable>false</IsPackable>
    <IsTestProject>true</IsTestProject>
  </PropertyGroup>

  <ItemGroup>
    <!-- 測試框架 -->
    <PackageReference Include="Microsoft.NET.Test.Sdk" Version="17.8.0" />
    <PackageReference Include="xunit" Version="2.4.2" />
    <PackageReference Include="xunit.runner.visualstudio" Version="2.4.5" />
    
    <!-- Mock框架 -->
    <PackageReference Include="Moq" Version="4.20.69" />
    <PackageReference Include="AutoFixture" Version="4.18.0" />
    <PackageReference Include="AutoFixture.AutoMoq" Version="4.18.0" />
    <PackageReference Include="AutoFixture.Xunit2" Version="4.18.0" />
    
    <!-- ASP.NET Core測試工具 -->
    <PackageReference Include="Microsoft.AspNetCore.Mvc.Testing" Version="8.0.0" />
    <PackageReference Include="Microsoft.EntityFrameworkCore.InMemory" Version="8.0.0" />
    
    <!-- 斷言輔助工具 -->
    <PackageReference Include="FluentAssertions" Version="6.12.0" />
    
    <!-- 測試覆蓋率 -->
    <PackageReference Include="coverlet.collector" Version="6.0.0" />
    <PackageReference Include="coverlet.msbuild" Version="6.0.0" />
    
    <!-- 整合測試 -->
    <PackageReference Include="Testcontainers.SqlServer" Version="3.6.0" />
    
    <!-- 端對端測試 -->
    <PackageReference Include="Selenium.WebDriver" Version="4.15.0" />
    <PackageReference Include="Selenium.WebDriver.ChromeDriver" Version="119.0.6045.10500" />
    <PackageReference Include="Microsoft.Playwright" Version="1.40.0" />
  </ItemGroup>

  <ItemGroup>
    <ProjectReference Include="..\ECommerceApp.Core\ECommerceApp.Core.csproj" />
    <ProjectReference Include="..\ECommerceApp.Infrastructure\ECommerceApp.Infrastructure.csproj" />
    <ProjectReference Include="..\ECommerceApp.Web\ECommerceApp.Web.csproj" />
  </ItemGroup>

</Project>

// 測試基底類別設定
using AutoFixture;
using AutoFixture.AutoMoq;
using Microsoft.Extensions.Logging;
using Moq;

namespace ECommerceApp.Tests.Common
{
    public abstract class TestBase
    {
        protected readonly IFixture Fixture;
        protected readonly Mock<ILogger> MockLogger;
        
        protected TestBase()
        {
            Fixture = new Fixture().Customize(new AutoMoqCustomization());
            MockLogger = new Mock<ILogger>();
            
            // 設定AutoFixture忽略循環參考
            Fixture.Behaviors.OfType<ThrowingRecursionBehavior>().ToList()
                .ForEach(b => Fixture.Behaviors.Remove(b));
            Fixture.Behaviors.Add(new OmitOnRecursionBehavior());
        }
        
        protected Mock<T> CreateMock<T>() where T : class
        {
            return new Mock<T>();
        }
        
        protected T CreateInstance<T>()
        {
            return Fixture.Create<T>();
        }
        
        protected List<T> CreateMany<T>(int count = 3)
        {
            return Fixture.CreateMany<T>(count).ToList();
        }
    }
    
    // 測試資料建造者
    public class ProductTestDataBuilder
    {
        private Product _product;
        
        public ProductTestDataBuilder()
        {
            _product = new Product
            {
                Id = 1,
                Name = "Test Product",
                SKU = "TEST001",
                Description = "Test product description",
                Price = 99.99m,
                StockQuantity = 10,
                CategoryId = 1,
                IsActive = true,
                CreatedAt = DateTime.UtcNow
            };
        }
        
        public ProductTestDataBuilder WithId(int id)
        {
            _product.Id = id;
            return this;
        }
        
        public ProductTestDataBuilder WithName(string name)
        {
            _product.Name = name;
            return this;
        }
        
        public ProductTestDataBuilder WithPrice(decimal price)
        {
            _product.Price = price;
            return this;
        }
        
        public ProductTestDataBuilder WithStock(int quantity)
        {
            _product.StockQuantity = quantity;
            return this;
        }
        
        public ProductTestDataBuilder OutOfStock()
        {
            _product.StockQuantity = 0;
            return this;
        }
        
        public ProductTestDataBuilder Inactive()
        {
            _product.IsActive = false;
            return this;
        }
        
        public Product Build()
        {
            return _product;
        }
        
        public static implicit operator Product(ProductTestDataBuilder builder)
        {
            return builder.Build();
        }
    }
}
```

### 1.2 單元測試實作

#### 1.2.1 服務層單元測試

```csharp
using FluentAssertions;
using Moq;
using Xunit;
using ECommerceApp.Core.Services;
using ECommerceApp.Core.Entities;
using ECommerceApp.Core.Interfaces;
using ECommerceApp.Core.Exceptions;
using ECommerceApp.Tests.Common;

namespace ECommerceApp.Tests.UnitTests.Services
{
    public class ProductServiceTests : TestBase
    {
        private readonly Mock<IUnitOfWork> _mockUnitOfWork;
        private readonly Mock<IRepository<Product>> _mockProductRepository;
        private readonly Mock<IMemoryCache> _mockCache;
        private readonly Mock<IFileService> _mockFileService;
        private readonly ProductService _productService;
        
        public ProductServiceTests()
        {
            _mockUnitOfWork = CreateMock<IUnitOfWork>();
            _mockProductRepository = CreateMock<IRepository<Product>>();
            _mockCache = CreateMock<IMemoryCache>();
            _mockFileService = CreateMock<IFileService>();
            
            _mockUnitOfWork.Setup(u => u.Repository<Product>())
                .Returns(_mockProductRepository.Object);
            
            _productService = new ProductService(
                _mockUnitOfWork.Object,
                _mockCache.Object,
                _mockFileService.Object,
                MockLogger.Object);
        }
        
        [Fact]
        public async Task GetProductByIdAsync_ExistingProduct_ReturnsProduct()
        {
            // Arrange
            var productId = 1;
            var expectedProduct = new ProductTestDataBuilder()
                .WithId(productId)
                .WithName("Test Product")
                .Build();
            
            _mockProductRepository
                .Setup(r => r.GetByIdAsync(productId))
                .ReturnsAsync(expectedProduct);
            
            // Act
            var result = await _productService.GetProductByIdAsync(productId);
            
            // Assert
            result.Should().NotBeNull();
            result.Id.Should().Be(productId);
            result.Name.Should().Be("Test Product");
            
            _mockProductRepository.Verify(r => r.GetByIdAsync(productId), Times.Once);
        }
        
        [Fact]
        public async Task GetProductByIdAsync_NonExistingProduct_ReturnsNull()
        {
            // Arrange
            var productId = 999;
            
            _mockProductRepository
                .Setup(r => r.GetByIdAsync(productId))
                .ReturnsAsync((Product)null);
            
            // Act
            var result = await _productService.GetProductByIdAsync(productId);
            
            // Assert
            result.Should().BeNull();
        }
        
        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        public async Task GetProductByIdAsync_InvalidId_ThrowsArgumentException(int invalidId)
        {
            // Act & Assert
            await Assert.ThrowsAsync<ArgumentException>(() => 
                _productService.GetProductByIdAsync(invalidId));
        }
        
        [Fact]
        public async Task CreateProductAsync_ValidProduct_ReturnsCreatedProduct()
        {
            // Arrange
            var createDto = new CreateProductDto
            {
                Name = "New Product",
                SKU = "NEW001",
                Price = 199.99m,
                CategoryId = 1,
                StockQuantity = 50
            };
            
            var createdProduct = new ProductTestDataBuilder()
                .WithName(createDto.Name)
                .WithPrice(createDto.Price)
                .Build();
            
            _mockProductRepository
                .Setup(r => r.GetQueryable())
                .Returns(new List<Product>().AsQueryable());
            
            _mockProductRepository
                .Setup(r => r.AddAsync(It.IsAny<Product>()))
                .ReturnsAsync(createdProduct);
            
            _mockUnitOfWork
                .Setup(u => u.SaveChangesAsync())
                .ReturnsAsync(1);
            
            // Act
            var result = await _productService.CreateProductAsync(createDto, "testuser");
            
            // Assert
            result.Should().NotBeNull();
            result.Name.Should().Be(createDto.Name);
            result.Price.Should().Be(createDto.Price);
            
            _mockProductRepository.Verify(r => r.AddAsync(It.IsAny<Product>()), Times.Once);
            _mockUnitOfWork.Verify(u => u.SaveChangesAsync(), Times.Once);
        }
        
        [Fact]
        public async Task CreateProductAsync_DuplicateSKU_ThrowsBusinessException()
        {
            // Arrange
            var createDto = new CreateProductDto
            {
                Name = "New Product",
                SKU = "EXISTING001",
                Price = 199.99m,
                CategoryId = 1
            };
            
            var existingProducts = new List<Product>
            {
                new ProductTestDataBuilder().WithId(1).Build()
            };
            
            _mockProductRepository
                .Setup(r => r.GetQueryable())
                .Returns(existingProducts.AsQueryable());
            
            // Act & Assert
            var exception = await Assert.ThrowsAsync<BusinessException>(() => 
                _productService.CreateProductAsync(createDto, "testuser"));
            
            exception.Message.Should().Contain("SKU");
        }
        
        [Fact]
        public async Task UpdateStockAsync_ValidProduct_UpdatesStock()
        {
            // Arrange
            var productId = 1;
            var newStock = 100;
            var product = new ProductTestDataBuilder()
                .WithId(productId)
                .WithStock(50)
                .Build();
            
            _mockProductRepository
                .Setup(r => r.GetByIdAsync(productId))
                .ReturnsAsync(product);
            
            _mockUnitOfWork
                .Setup(u => u.SaveChangesAsync())
                .ReturnsAsync(1);
            
            // Act
            await _productService.UpdateStockAsync(productId, newStock);
            
            // Assert
            product.StockQuantity.Should().Be(newStock);
            product.UpdatedAt.Should().BeCloseTo(DateTime.UtcNow, TimeSpan.FromSeconds(1));
            
            _mockProductRepository.Verify(r => r.Update(product), Times.Once);
            _mockUnitOfWork.Verify(u => u.SaveChangesAsync(), Times.Once);
        }
        
        [Theory]
        [InlineData(-1)]
        [InlineData(-10)]
        public async Task UpdateStockAsync_NegativeStock_ThrowsArgumentException(int negativeStock)
        {
            // Arrange
            var productId = 1;
            
            // Act & Assert
            await Assert.ThrowsAsync<ArgumentException>(() => 
                _productService.UpdateStockAsync(productId, negativeStock));
        }
        
        [Fact]
        public async Task SearchProductsAsync_WithFilters_ReturnsFilteredResults()
        {
            // Arrange
            var searchCriteria = new ProductSearchCriteria
            {
                SearchTerm = "phone",
                CategoryId = 1,
                MinPrice = 100,
                MaxPrice = 1000,
                Page = 1,
                PageSize = 10
            };
            
            var products = new List<Product>
            {
                new ProductTestDataBuilder()
                    .WithName("Smartphone")
                    .WithPrice(500)
                    .Build(),
                new ProductTestDataBuilder()
                    .WithName("Phone Case")
                    .WithPrice(25)
                    .Build()
            };
            
            _mockProductRepository
                .Setup(r => r.GetPagedAsync(
                    It.IsAny<Expression<Func<Product, bool>>>(),
                    It.IsAny<Func<IQueryable<Product>, IOrderedQueryable<Product>>>(),
                    It.IsAny<string>(),
                    searchCriteria.Page,
                    searchCriteria.PageSize))
                .ReturnsAsync(new PagedResult<Product>
                {
                    Items = products.Where(p => p.Price >= searchCriteria.MinPrice && 
                                               p.Price <= searchCriteria.MaxPrice).ToList(),
                    TotalCount = 1,
                    Page = 1,
                    PageSize = 10
                });
            
            // Act
            var result = await _productService.SearchProductsAsync(searchCriteria);
            
            // Assert
            result.Should().NotBeNull();
            result.Items.Should().HaveCount(1);
            result.Items.First().Name.Should().Be("Smartphone");
            result.TotalCount.Should().Be(1);
        }
        
        [Fact]
        public async Task DeleteProductAsync_ExistingProduct_SoftDeletesProduct()
        {
            // Arrange
            var productId = 1;
            var product = new ProductTestDataBuilder()
                .WithId(productId)
                .Build();
            
            _mockProductRepository
                .Setup(r => r.GetByIdAsync(productId))
                .ReturnsAsync(product);
            
            _mockUnitOfWork
                .Setup(u => u.SaveChangesAsync())
                .ReturnsAsync(1);
            
            // Act
            await _productService.DeleteProductAsync(productId, "testuser");
            
            // Assert
            product.IsActive.Should().BeFalse();
            product.UpdatedBy.Should().Be("testuser");
            product.UpdatedAt.Should().BeCloseTo(DateTime.UtcNow, TimeSpan.FromSeconds(1));
            
            _mockUnitOfWork.Verify(u => u.SaveChangesAsync(), Times.Once);
        }
    }
    
    // 購物車服務測試
    public class CartServiceTests : TestBase
    {
        private readonly Mock<IUnitOfWork> _mockUnitOfWork;
        private readonly Mock<IRepository<ShoppingCart>> _mockCartRepository;
        private readonly Mock<IProductService> _mockProductService;
        private readonly CartService _cartService;
        
        public CartServiceTests()
        {
            _mockUnitOfWork = CreateMock<IUnitOfWork>();
            _mockCartRepository = CreateMock<IRepository<ShoppingCart>>();
            _mockProductService = CreateMock<IProductService>();
            
            _mockUnitOfWork.Setup(u => u.Repository<ShoppingCart>())
                .Returns(_mockCartRepository.Object);
            
            _cartService = new CartService(
                _mockUnitOfWork.Object,
                _mockProductService.Object,
                MockLogger.Object);
        }
        
        [Fact]
        public async Task AddToCartAsync_ValidItem_AddsToCart()
        {
            // Arrange
            var customerId = "user123";
            var productId = 1;
            var quantity = 2;
            
            var product = new ProductTestDataBuilder()
                .WithId(productId)
                .WithPrice(99.99m)
                .WithStock(10)
                .Build();
            
            var cart = new ShoppingCart
            {
                CustomerId = customerId,
                Items = new List<CartItem>()
            };
            
            _mockProductService
                .Setup(s => s.GetProductByIdAsync(productId))
                .ReturnsAsync(product);
            
            _mockCartRepository
                .Setup(r => r.GetQueryable())
                .Returns(new List<ShoppingCart> { cart }.AsQueryable());
            
            _mockUnitOfWork
                .Setup(u => u.SaveChangesAsync())
                .ReturnsAsync(1);
            
            // Act
            await _cartService.AddToCartAsync(customerId, productId, quantity);
            
            // Assert
            cart.Items.Should().HaveCount(1);
            cart.Items.First().ProductId.Should().Be(productId);
            cart.Items.First().Quantity.Should().Be(quantity);
            cart.Items.First().UnitPrice.Should().Be(product.Price);
            
            _mockUnitOfWork.Verify(u => u.SaveChangesAsync(), Times.Once);
        }
        
        [Fact]
        public async Task AddToCartAsync_InsufficientStock_ThrowsBusinessException()
        {
            // Arrange
            var customerId = "user123";
            var productId = 1;
            var quantity = 15; // 超過庫存
            
            var product = new ProductTestDataBuilder()
                .WithId(productId)
                .WithStock(10) // 只有10個庫存
                .Build();
            
            _mockProductService
                .Setup(s => s.GetProductByIdAsync(productId))
                .ReturnsAsync(product);
            
            // Act & Assert
            var exception = await Assert.ThrowsAsync<BusinessException>(() => 
                _cartService.AddToCartAsync(customerId, productId, quantity));
            
            exception.Message.Should().Contain("庫存不足");
        }
        
        [Fact]
        public async Task GetCartTotalAsync_WithItems_ReturnsCorrectTotal()
        {
            // Arrange
            var customerId = "user123";
            var cart = new ShoppingCart
            {
                CustomerId = customerId,
                Items = new List<CartItem>
                {
                    new CartItem { ProductId = 1, Quantity = 2, UnitPrice = 99.99m },
                    new CartItem { ProductId = 2, Quantity = 1, UnitPrice = 49.99m }
                }
            };
            
            _mockCartRepository
                .Setup(r => r.GetQueryable())
                .Returns(new List<ShoppingCart> { cart }.AsQueryable());
            
            // Act
            var total = await _cartService.GetCartTotalAsync(customerId);
            
            // Assert
            total.Should().Be(249.97m); // (99.99 * 2) + (49.99 * 1)
        }
    }
}
```

#### 1.2.2 控制器單元測試

```csharp
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;
using FluentAssertions;
using ECommerceApp.Web.Controllers;
using ECommerceApp.Core.Interfaces;
using ECommerceApp.Core.Entities;
using ECommerceApp.Web.ViewModels;
using ECommerceApp.Tests.Common;
using AutoMapper;

namespace ECommerceApp.Tests.UnitTests.Controllers
{
    public class ProductControllerTests : TestBase
    {
        private readonly Mock<IProductService> _mockProductService;
        private readonly Mock<ICategoryService> _mockCategoryService;
        private readonly Mock<IMapper> _mockMapper;
        private readonly Mock<ILogger<ProductController>> _mockLogger;
        private readonly ProductController _controller;
        
        public ProductControllerTests()
        {
            _mockProductService = CreateMock<IProductService>();
            _mockCategoryService = CreateMock<ICategoryService>();
            _mockMapper = CreateMock<IMapper>();
            _mockLogger = CreateMock<ILogger<ProductController>>();
            
            _controller = new ProductController(
                _mockProductService.Object,
                _mockCategoryService.Object,
                _mockMapper.Object,
                _mockLogger.Object);
        }
        
        [Fact]
        public async Task Index_ValidRequest_ReturnsViewWithProducts()
        {
            // Arrange
            var searchModel = new ProductSearchViewModel
            {
                SearchTerm = "phone",
                Page = 1,
                PageSize = 12
            };
            
            var products = CreateMany<Product>();
            var categories = CreateMany<Category>();
            
            var searchResult = new PagedResult<Product>
            {
                Items = products,
                TotalCount = products.Count,
                Page = 1,
                PageSize = 12
            };
            
            _mockProductService
                .Setup(s => s.SearchProductsAsync(It.IsAny<ProductSearchCriteria>()))
                .ReturnsAsync(searchResult);
            
            _mockCategoryService
                .Setup(s => s.GetActiveCategoriesAsync())
                .ReturnsAsync(categories);
            
            _mockMapper
                .Setup(m => m.Map<List<ProductCardViewModel>>(products))
                .Returns(CreateMany<ProductCardViewModel>());
            
            _mockMapper
                .Setup(m => m.Map<List<CategoryViewModel>>(categories))
                .Returns(CreateMany<CategoryViewModel>());
            
            // Act
            var result = await _controller.Index(searchModel);
            
            // Assert
            result.Should().BeOfType<ViewResult>();
            var viewResult = result as ViewResult;
            viewResult.Model.Should().BeOfType<ProductIndexViewModel>();
            
            var model = viewResult.Model as ProductIndexViewModel;
            model.Products.Should().NotBeEmpty();
            model.Categories.Should().NotBeEmpty();
        }
        
        [Fact]
        public async Task Details_ValidId_ReturnsViewWithProduct()
        {
            // Arrange
            var productId = 1;
            var product = new ProductTestDataBuilder()
                .WithId(productId)
                .WithName("Test Product")
                .Build();
            
            var relatedProducts = CreateMany<Product>();
            var reviews = new PagedResult<ProductReview>
            {
                Items = CreateMany<ProductReview>(),
                TotalCount = 3
            };
            
            _mockProductService
                .Setup(s => s.GetProductByIdAsync(productId))
                .ReturnsAsync(product);
            
            _mockProductService
                .Setup(s => s.GetRelatedProductsAsync(product.CategoryId, productId, 4))
                .ReturnsAsync(relatedProducts);
            
            _mockProductService
                .Setup(s => s.GetProductReviewsAsync(productId, 1, 10))
                .ReturnsAsync(reviews);
            
            _mockMapper
                .Setup(m => m.Map<ProductViewModel>(product))
                .Returns(CreateInstance<ProductViewModel>());
            
            // Act
            var result = await _controller.Details(productId);
            
            // Assert
            result.Should().BeOfType<ViewResult>();
            var viewResult = result as ViewResult;
            viewResult.Model.Should().BeOfType<ProductDetailsViewModel>();
        }
        
        [Fact]
        public async Task Details_InvalidId_ReturnsNotFound()
        {
            // Arrange
            var productId = 999;
            
            _mockProductService
                .Setup(s => s.GetProductByIdAsync(productId))
                .ReturnsAsync((Product)null);
            
            // Act
            var result = await _controller.Details(productId);
            
            // Assert
            result.Should().BeOfType<NotFoundObjectResult>();
        }
        
        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        public async Task Details_InvalidId_ReturnsBadRequest(int invalidId)
        {
            // Act
            var result = await _controller.Details(invalidId);
            
            // Assert
            result.Should().BeOfType<BadRequestObjectResult>();
        }
        
        [Fact]
        public async Task Create_ValidModel_RedirectsToDetails()
        {
            // Arrange
            var model = new ProductCreateViewModel
            {
                Name = "New Product",
                Price = 199.99m,
                CategoryId = 1,
                ProductCode = "NEW001"
            };
            
            var createdProduct = new ProductTestDataBuilder()
                .WithId(1)
                .WithName(model.Name)
                .Build();
            
            _mockProductService
                .Setup(s => s.IsProductCodeExistsAsync(model.ProductCode))
                .ReturnsAsync(false);
            
            _mockMapper
                .Setup(m => m.Map<Product>(model))
                .Returns(createdProduct);
            
            _mockProductService
                .Setup(s => s.CreateProductAsync(It.IsAny<Product>()))
                .ReturnsAsync(createdProduct);
            
            // Act
            var result = await _controller.Create(model);
            
            // Assert
            result.Should().BeOfType<RedirectToActionResult>();
            var redirectResult = result as RedirectToActionResult;
            redirectResult.ActionName.Should().Be(nameof(ProductController.Details));
            redirectResult.RouteValues["id"].Should().Be(createdProduct.Id);
        }
        
        [Fact]
        public async Task Create_InvalidModel_ReturnsViewWithErrors()
        {
            // Arrange
            var model = new ProductCreateViewModel(); // 空模型，會導致驗證錯誤
            _controller.ModelState.AddModelError("Name", "Name is required");
            
            var categories = CreateMany<Category>();
            _mockCategoryService
                .Setup(s => s.GetActiveCategoriesAsync())
                .ReturnsAsync(categories);
            
            // Act
            var result = await _controller.Create(model);
            
            // Assert
            result.Should().BeOfType<ViewResult>();
            var viewResult = result as ViewResult;
            viewResult.Model.Should().Be(model);
            _controller.ModelState.IsValid.Should().BeFalse();
        }
        
        [Fact]
        public async Task GetSearchSuggestions_ValidTerm_ReturnsJsonSuggestions()
        {
            // Arrange
            var searchTerm = "phone";
            var suggestions = new List<string> { "iPhone", "Android Phone", "Phone Case" };
            
            _mockProductService
                .Setup(s => s.GetSearchSuggestionsAsync(searchTerm, 10))
                .ReturnsAsync(suggestions);
            
            // Act
            var result = await _controller.GetSearchSuggestions(searchTerm);
            
            // Assert
            result.Should().BeOfType<JsonResult>();
            var jsonResult = result as JsonResult;
            jsonResult.Value.Should().BeEquivalentTo(suggestions);
        }
        
        [Theory]
        [InlineData("")]
        [InlineData("a")] // 太短
        [InlineData(null)]
        public async Task GetSearchSuggestions_InvalidTerm_ReturnsEmptyList(string invalidTerm)
        {
            // Act
            var result = await _controller.GetSearchSuggestions(invalidTerm);
            
            // Assert
            