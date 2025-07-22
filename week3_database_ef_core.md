# 第三週：資料庫基礎和Entity Framework Core - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 理解關聯式資料庫的基本概念和設計原則
- 掌握SQL Server的基本操作和SQL語法
- 熟練使用Entity Framework Core進行資料存取
- 實作Code First開發模式和資料庫遷移
- 建立高效的資料存取層架構
- 理解並應用Repository Pattern和Unit of Work模式

---

## 🗄️ 第一節：關聯式資料庫基礎

### 1.1 資料庫設計原理

#### 1.1.1 關聯式資料庫概念

```sql
/*
關聯式資料庫核心概念：

1. 表格 (Table) - 儲存同類型資料的結構
2. 欄位 (Column/Field) - 表格中的屬性
3. 記錄 (Row/Record) - 表格中的一筆資料
4. 主鍵 (Primary Key) - 唯一識別每筆記錄
5. 外鍵 (Foreign Key) - 建立表格間的關聯
6. 索引 (Index) - 提升查詢效能
7. 約束 (Constraint) - 確保資料完整性

資料庫正規化：
- 第一正規化 (1NF)：消除重複群組
- 第二正規化 (2NF)：消除部分相依
- 第三正規化 (3NF)：消除遞移相依
*/

-- 電商系統資料庫設計範例

-- 1. 客戶表格
CREATE TABLE Customers (
    CustomerId INT IDENTITY(1,1) PRIMARY KEY,
    FirstName NVARCHAR(50) NOT NULL,
    LastName NVARCHAR(50) NOT NULL,
    Email NVARCHAR(100) NOT NULL UNIQUE,
    Phone NVARCHAR(20),
    DateOfBirth DATE,
    RegistrationDate DATETIME2 DEFAULT GETDATE(),
    IsActive BIT DEFAULT 1,
    
    -- 地址資訊（考慮正規化，實際專案可能會分離成Address表格）
    AddressLine1 NVARCHAR(200),
    AddressLine2 NVARCHAR(200),
    City NVARCHAR(50),
    StateProvince NVARCHAR(50),
    PostalCode NVARCHAR(20),
    Country NVARCHAR(50) DEFAULT 'Taiwan',
    
    -- 稽核欄位
    CreatedAt DATETIME2 DEFAULT GETDATE(),
    UpdatedAt DATETIME2,
    CreatedBy NVARCHAR(50),
    UpdatedBy NVARCHAR(50)
);

-- 2. 產品類別表格
CREATE TABLE Categories (
    CategoryId INT IDENTITY(1,1) PRIMARY KEY,
    CategoryName NVARCHAR(100) NOT NULL,
    Description NVARCHAR(MAX),
    ParentCategoryId INT NULL, -- 支援階層式類別
    IsActive BIT DEFAULT 1,
    DisplayOrder INT DEFAULT 0,
    CreatedAt DATETIME2 DEFAULT GETDATE(),
    UpdatedAt DATETIME2,
    
    -- 自我參照外鍵
    FOREIGN KEY (ParentCategoryId) REFERENCES Categories(CategoryId)
);

-- 3. 產品表格
CREATE TABLE Products (
    ProductId INT IDENTITY(1,1) PRIMARY KEY,
    ProductName NVARCHAR(200) NOT NULL,
    ProductCode NVARCHAR(50) NOT NULL UNIQUE,
    Description NVARCHAR(MAX),
    CategoryId INT NOT NULL,
    
    -- 價格資訊
    UnitPrice DECIMAL(18,2) NOT NULL CHECK (UnitPrice >= 0),
    OriginalPrice DECIMAL(18,2),
    CostPrice DECIMAL(18,2),
    
    -- 庫存資訊
    StockQuantity INT NOT NULL DEFAULT 0 CHECK (StockQuantity >= 0),
    ReorderLevel INT DEFAULT 10,
    
    -- 產品屬性
    Weight DECIMAL(10,3),
    Dimensions NVARCHAR(50), -- 例如: "30x20x10 cm"
    Color NVARCHAR(30),
    Size NVARCHAR(20),
    
    -- 狀態管理
    IsActive BIT DEFAULT 1,
    IsDiscontinued BIT DEFAULT 0,
    
    -- SEO和行銷
    Tags NVARCHAR(500), -- 例如: "electronics,mobile,smartphone"
    MetaTitle NVARCHAR(200),
    MetaDescription NVARCHAR(500),
    
    -- 稽核欄位
    CreatedAt DATETIME2 DEFAULT GETDATE(),
    UpdatedAt DATETIME2,
    CreatedBy NVARCHAR(50),
    UpdatedBy NVARCHAR(50),
    
    -- 外鍵約束
    FOREIGN KEY (CategoryId) REFERENCES Categories(CategoryId)
);

-- 4. 訂單表格
CREATE TABLE Orders (
    OrderId INT IDENTITY(1,1) PRIMARY KEY,
    OrderNumber NVARCHAR(50) NOT NULL UNIQUE, -- 格式化的訂單編號
    CustomerId INT NOT NULL,
    
    -- 訂單日期
    OrderDate DATETIME2 DEFAULT GETDATE(),
    RequiredDate DATETIME2,
    ShippedDate DATETIME2,
    
    -- 金額資訊
    SubTotal DECIMAL(18,2) NOT NULL DEFAULT 0,
    TaxAmount DECIMAL(18,2) NOT NULL DEFAULT 0,
    ShippingCost DECIMAL(18,2) NOT NULL DEFAULT 0,
    DiscountAmount DECIMAL(18,2) NOT NULL DEFAULT 0,
    TotalAmount AS (SubTotal + TaxAmount + ShippingCost - DiscountAmount) PERSISTED,
    
    -- 訂單狀態
    OrderStatus NVARCHAR(20) NOT NULL DEFAULT 'Pending',
    PaymentStatus NVARCHAR(20) NOT NULL DEFAULT 'Pending',
    
    -- 配送資訊
    ShippingMethod NVARCHAR(50),
    TrackingNumber NVARCHAR(100),
    
    -- 地址資訊（訂單時的快照）
    ShippingAddress NVARCHAR(MAX), -- JSON格式儲存
    BillingAddress NVARCHAR(MAX),  -- JSON格式儲存
    
    -- 備註
    CustomerNotes NVARCHAR(500),
    InternalNotes NVARCHAR(500),
    
    -- 稽核欄位
    CreatedAt DATETIME2 DEFAULT GETDATE(),
    UpdatedAt DATETIME2,
    
    -- 外鍵約束
    FOREIGN KEY (CustomerId) REFERENCES Customers(CustomerId),
    
    -- 檢查約束
    CHECK (OrderStatus IN ('Pending', 'Processing', 'Shipped', 'Delivered', 'Cancelled')),
    CHECK (PaymentStatus IN ('Pending', 'Paid', 'Failed', 'Refunded'))
);

-- 5. 訂單明細表格
CREATE TABLE OrderDetails (
    OrderDetailId INT IDENTITY(1,1) PRIMARY KEY,
    OrderId INT NOT NULL,
    ProductId INT NOT NULL,
    
    -- 數量和價格（訂單時的快照）
    Quantity INT NOT NULL CHECK (Quantity > 0),
    UnitPrice DECIMAL(18,2) NOT NULL CHECK (UnitPrice >= 0),
    Discount DECIMAL(18,2) NOT NULL DEFAULT 0,
    LineTotal AS (Quantity * UnitPrice - Discount) PERSISTED,
    
    -- 產品資訊快照（避免產品資訊變更影響歷史訂單）
    ProductName NVARCHAR(200) NOT NULL,
    ProductCode NVARCHAR(50) NOT NULL,
    
    -- 稽核欄位
    CreatedAt DATETIME2 DEFAULT GETDATE(),
    
    -- 外鍵約束
    FOREIGN KEY (OrderId) REFERENCES Orders(OrderId) ON DELETE CASCADE,
    FOREIGN KEY (ProductId) REFERENCES Products(ProductId),
    
    -- 複合唯一索引（同一訂單不能有重複產品）
    UNIQUE (OrderId, ProductId)
);

-- 6. 產品評論表格
CREATE TABLE ProductReviews (
    ReviewId INT IDENTITY(1,1) PRIMARY KEY,
    ProductId INT NOT NULL,
    CustomerId INT NOT NULL,
    
    -- 評論內容
    Rating INT NOT NULL CHECK (Rating BETWEEN 1 AND 5),
    Title NVARCHAR(200),
    ReviewText NVARCHAR(MAX),
    
    -- 狀態管理
    IsApproved BIT DEFAULT 0,
    IsHelpful INT DEFAULT 0, -- 有用的投票數
    
    -- 稽核欄位
    CreatedAt DATETIME2 DEFAULT GETDATE(),
    UpdatedAt DATETIME2,
    
    -- 外鍵約束
    FOREIGN KEY (ProductId) REFERENCES Products(ProductId),
    FOREIGN KEY (CustomerId) REFERENCES Customers(CustomerId),
    
    -- 確保一個客戶對同一產品只能評論一次
    UNIQUE (ProductId, CustomerId)
);
```

#### 1.1.2 索引和效能優化

```sql
-- 索引設計策略

-- 1. 主鍵索引（自動建立）
-- 已在表格定義中包含

-- 2. 唯一索引
CREATE UNIQUE INDEX IX_Customers_Email 
ON Customers(Email);

CREATE UNIQUE INDEX IX_Products_ProductCode 
ON Products(ProductCode);

-- 3. 一般索引（提升查詢效能）
-- 客戶姓名查詢
CREATE INDEX IX_Customers_Name 
ON Customers(LastName, FirstName);

-- 產品類別查詢
CREATE INDEX IX_Products_Category 
ON Products(CategoryId) 
INCLUDE (ProductName, UnitPrice, StockQuantity);

-- 訂單日期查詢
CREATE INDEX IX_Orders_OrderDate 
ON Orders(OrderDate DESC);

-- 客戶訂單查詢
CREATE INDEX IX_Orders_Customer 
ON Orders(CustomerId, OrderDate DESC);

-- 4. 複合索引（多欄位查詢）
-- 產品搜尋（類別 + 價格範圍 + 庫存狀態）
CREATE INDEX IX_Products_Search 
ON Products(CategoryId, UnitPrice, StockQuantity) 
WHERE IsActive = 1;

-- 訂單狀態查詢
CREATE INDEX IX_Orders_Status 
ON Orders(OrderStatus, PaymentStatus, OrderDate);

-- 5. 覆蓋索引（包含所需的所有欄位）
CREATE INDEX IX_Products_Catalog 
ON Products(CategoryId, IsActive) 
INCLUDE (ProductName, UnitPrice, StockQuantity, Description);

-- 6. 篩選索引（僅索引符合條件的記錄）
CREATE INDEX IX_Products_Active 
ON Products(CategoryId, UnitPrice) 
WHERE IsActive = 1 AND IsDiscontinued = 0;

-- 查詢效能分析範例
-- 開啟執行計畫分析
SET STATISTICS IO ON;
SET STATISTICS TIME ON;

-- 效能查詢範例
-- 1. 按類別查詢產品（使用索引）
SELECT ProductId, ProductName, UnitPrice, StockQuantity
FROM Products 
WHERE CategoryId = 1 AND IsActive = 1
ORDER BY ProductName;

-- 2. 複雜查詢：客戶訂單統計
SELECT 
    c.CustomerId,
    c.FirstName + ' ' + c.LastName AS CustomerName,
    COUNT(o.OrderId) AS TotalOrders,
    SUM(o.TotalAmount) AS TotalSpent,
    AVG(o.TotalAmount) AS AverageOrderValue,
    MAX(o.OrderDate) AS LastOrderDate
FROM Customers c
LEFT JOIN Orders o ON c.CustomerId = o.CustomerId 
    AND o.OrderStatus != 'Cancelled'
WHERE c.IsActive = 1
GROUP BY c.CustomerId, c.FirstName, c.LastName
HAVING COUNT(o.OrderId) > 0
ORDER BY TotalSpent DESC;

-- 3. 產品銷售排行
WITH ProductSales AS (
    SELECT 
        p.ProductId,
        p.ProductName,
        p.CategoryId,
        SUM(od.Quantity) AS TotalQuantitySold,
        SUM(od.LineTotal) AS TotalRevenue,
        COUNT(DISTINCT od.OrderId) AS OrderCount
    FROM Products p
    INNER JOIN OrderDetails od ON p.ProductId = od.ProductId
    INNER JOIN Orders o ON od.OrderId = o.OrderId
    WHERE o.OrderStatus IN ('Shipped', 'Delivered')
        AND o.OrderDate >= DATEADD(month, -3, GETDATE()) -- 最近3個月
    GROUP BY p.ProductId, p.ProductName, p.CategoryId
)
SELECT 
    ps.*,
    c.CategoryName,
    RANK() OVER (ORDER BY ps.TotalRevenue DESC) AS RevenueRank,
    RANK() OVER (PARTITION BY ps.CategoryId ORDER BY ps.TotalQuantitySold DESC) AS CategoryRank
FROM ProductSales ps
INNER JOIN Categories c ON ps.CategoryId = c.CategoryId
ORDER BY ps.TotalRevenue DESC;

-- 關閉統計
SET STATISTICS IO OFF;
SET STATISTICS TIME OFF;
```

### 1.2 SQL基本語法

#### 1.2.1 資料查詢語言 (DQL)

```sql
-- 資料查詢語言 (Data Query Language) 範例

-- 1. 基本 SELECT 語句
-- 查詢所有客戶
SELECT * FROM Customers;

-- 查詢特定欄位
SELECT CustomerId, FirstName, LastName, Email 
FROM Customers;

-- 使用別名
SELECT 
    CustomerId AS ID,
    FirstName + ' ' + LastName AS FullName,
    Email AS EmailAddress,
    DATEDIFF(YEAR, DateOfBirth, GETDATE()) AS Age
FROM Customers;

-- 2. WHERE 子句 - 條件篩選
-- 查詢特定客戶
SELECT * FROM Customers 
WHERE CustomerId = 1;

-- 多重條件
SELECT * FROM Products 
WHERE CategoryId = 1 
    AND UnitPrice BETWEEN 1000 AND 5000 
    AND IsActive = 1;

-- 模糊查詢
SELECT * FROM Products 
WHERE ProductName LIKE '%手機%' 
    OR ProductName LIKE '%電話%';

-- 空值查詢
SELECT * FROM Customers 
WHERE Phone IS NOT NULL 
    AND AddressLine1 IS NOT NULL;

-- IN 操作符
SELECT * FROM Orders 
WHERE OrderStatus IN ('Processing', 'Shipped');

-- 日期範圍查詢
SELECT * FROM Orders 
WHERE OrderDate >= '2024-01-01' 
    AND OrderDate < '2024-02-01';

-- 3. ORDER BY - 排序
-- 單一欄位排序
SELECT * FROM Products 
ORDER BY UnitPrice DESC;

-- 多欄位排序
SELECT * FROM Customers 
ORDER BY LastName ASC, FirstName ASC;

-- 使用表達式排序
SELECT 
    ProductName,
    UnitPrice,
    StockQuantity,
    (UnitPrice * StockQuantity) AS InventoryValue
FROM Products 
ORDER BY InventoryValue DESC;

-- 4. GROUP BY 和聚合函數
-- 基本分組
SELECT 
    CategoryId,
    COUNT(*) AS ProductCount,
    AVG(UnitPrice) AS AveragePrice,
    MIN(UnitPrice) AS MinPrice,
    MAX(UnitPrice) AS MaxPrice,
    SUM(StockQuantity) AS TotalStock
FROM Products 
WHERE IsActive = 1
GROUP BY CategoryId;

-- 使用 HAVING 篩選分組結果
SELECT 
    CategoryId,
    COUNT(*) AS ProductCount,
    AVG(UnitPrice) AS AveragePrice
FROM Products 
WHERE IsActive = 1
GROUP BY CategoryId
HAVING COUNT(*) > 5 AND AVG(UnitPrice) > 1000;

-- 5. JOIN 操作
-- INNER JOIN - 內部聯結
SELECT 
    p.ProductName,
    c.CategoryName,
    p.UnitPrice,
    p.StockQuantity
FROM Products p
INNER JOIN Categories c ON p.CategoryId = c.CategoryId
WHERE p.IsActive = 1;

-- LEFT JOIN - 左外部聯結
SELECT 
    c.CategoryName,
    COUNT(p.ProductId) AS ProductCount
FROM Categories c
LEFT JOIN Products p ON c.CategoryId = p.CategoryId AND p.IsActive = 1
GROUP BY c.CategoryId, c.CategoryName
ORDER BY ProductCount DESC;

-- 複雜聯結查詢
SELECT 
    c.FirstName + ' ' + c.LastName AS CustomerName,
    o.OrderNumber,
    o.OrderDate,
    o.TotalAmount,
    p.ProductName,
    od.Quantity,
    od.UnitPrice
FROM Customers c
INNER JOIN Orders o ON c.CustomerId = o.CustomerId
INNER JOIN OrderDetails od ON o.OrderId = od.OrderId
INNER JOIN Products p ON od.ProductId = p.ProductId
WHERE o.OrderDate >= DATEADD(month, -1, GETDATE())
ORDER BY c.CustomerName, o.OrderDate DESC;

-- 6. 子查詢 (Subquery)
-- 標量子查詢
SELECT 
    ProductName,
    UnitPrice,
    (SELECT AVG(UnitPrice) FROM Products WHERE IsActive = 1) AS AvgPrice,
    UnitPrice - (SELECT AVG(UnitPrice) FROM Products WHERE IsActive = 1) AS PriceDifference
FROM Products 
WHERE IsActive = 1;

-- EXISTS 子查詢
SELECT * FROM Customers c
WHERE EXISTS (
    SELECT 1 FROM Orders o 
    WHERE o.CustomerId = c.CustomerId 
        AND o.OrderDate >= DATEADD(month, -6, GETDATE())
);

-- IN 子查詢
SELECT * FROM Products 
WHERE CategoryId IN (
    SELECT CategoryId FROM Categories 
    WHERE CategoryName LIKE '%電子%'
);

-- 7. 通用資料表運算式 (CTE)
-- 基本 CTE
WITH RecentOrders AS (
    SELECT 
        CustomerId,
        COUNT(*) AS OrderCount,
        SUM(TotalAmount) AS TotalSpent
    FROM Orders 
    WHERE OrderDate >= DATEADD(month, -3, GETDATE())
    GROUP BY CustomerId
)
SELECT 
    c.FirstName + ' ' + c.LastName AS CustomerName,
    ro.OrderCount,
    ro.TotalSpent
FROM RecentOrders ro
INNER JOIN Customers c ON ro.CustomerId = c.CustomerId
ORDER BY ro.TotalSpent DESC;

-- 遞迴 CTE（階層式類別）
WITH CategoryHierarchy AS (
    -- 基底案例：頂層類別
    SELECT 
        CategoryId,
        CategoryName,
        ParentCategoryId,
        0 AS Level,
        CAST(CategoryName AS NVARCHAR(1000)) AS FullPath
    FROM Categories 
    WHERE ParentCategoryId IS NULL
    
    UNION ALL
    
    -- 遞迴案例：子類別
    SELECT 
        c.CategoryId,
        c.CategoryName,
        c.ParentCategoryId,
        ch.Level + 1,
        CAST(ch.FullPath + ' > ' + c.CategoryName AS NVARCHAR(1000))
    FROM Categories c
    INNER JOIN CategoryHierarchy ch ON c.ParentCategoryId = ch.CategoryId
)
SELECT 
    CategoryId,
    REPLICATE('  ', Level) + CategoryName AS IndentedName,
    Level,
    FullPath
FROM CategoryHierarchy
ORDER BY FullPath;

-- 8. 視窗函數 (Window Functions)
-- ROW_NUMBER, RANK, DENSE_RANK
SELECT 
    ProductName,
    CategoryId,
    UnitPrice,
    ROW_NUMBER() OVER (PARTITION BY CategoryId ORDER BY UnitPrice DESC) AS RowNum,
    RANK() OVER (PARTITION BY CategoryId ORDER BY UnitPrice DESC) AS PriceRank,
    DENSE_RANK() OVER (PARTITION BY CategoryId ORDER BY UnitPrice DESC) AS DensePriceRank
FROM Products 
WHERE IsActive = 1;

-- LAG, LEAD - 存取前後列的值
SELECT 
    OrderDate,
    TotalAmount,
    LAG(TotalAmount, 1) OVER (ORDER BY OrderDate) AS PreviousOrderAmount,
    LEAD(TotalAmount, 1) OVER (ORDER BY OrderDate) AS NextOrderAmount,
    TotalAmount - LAG(TotalAmount, 1) OVER (ORDER BY OrderDate) AS AmountChange
FROM Orders 
WHERE CustomerId = 1
ORDER BY OrderDate;

-- 聚合視窗函數
SELECT 
    OrderDate,
    TotalAmount,
    SUM(TotalAmount) OVER (ORDER BY OrderDate ROWS UNBOUNDED PRECEDING) AS RunningTotal,
    AVG(TotalAmount) OVER (ORDER BY OrderDate ROWS 2 PRECEDING) AS MovingAverage3
FROM Orders 
WHERE CustomerId = 1
ORDER BY OrderDate;
```

#### 1.2.2 資料操作語言 (DML)

```csharp
// 在C#中使用ADO.NET執行SQL的範例
using System;
using System.Data.SqlClient;
using System.Configuration;

namespace DatabaseOperations
{
    public class SqlDmlExamples
    {
        private readonly string _connectionString;
        
        public SqlDmlExamples()
        {
            _connectionString = ConfigurationManager.ConnectionStrings["DefaultConnection"].ConnectionString;
        }
        
        // INSERT 操作
        public int InsertCustomer(string firstName, string lastName, string email, string phone)
        {
            const string sql = @"
                INSERT INTO Customers (FirstName, LastName, Email, Phone, RegistrationDate, IsActive)
                VALUES (@FirstName, @LastName, @Email, @Phone, GETDATE(), 1);
                SELECT CAST(SCOPE_IDENTITY() AS INT);";
            
            using (var connection = new SqlConnection(_connectionString))
            {
                using (var command = new SqlCommand(sql, connection))
                {
                    // 參數化查詢防止SQL注入
                    command.Parameters.AddWithValue("@FirstName", firstName);
                    command.Parameters.AddWithValue("@LastName", lastName);
                    command.Parameters.AddWithValue("@Email", email);
                    command.Parameters.AddWithValue("@Phone", (object)phone ?? DBNull.Value);
                    
                    connection.Open();
                    return (int)command.ExecuteScalar();
                }
            }
        }
        
        // UPDATE 操作
        public bool UpdateCustomerEmail(int customerId, string newEmail)
        {
            const string sql = @"
                UPDATE Customers 
                SET Email = @Email, UpdatedAt = GETDATE() 
                WHERE CustomerId = @CustomerId AND IsActive = 1";
            
            using (var connection = new SqlConnection(_connectionString))
            {
                using (var command = new SqlCommand(sql, connection))
                {
                    command.Parameters.AddWithValue("@Email", newEmail);
                    command.Parameters.AddWithValue("@CustomerId", customerId);
                    
                    connection.Open();
                    int rowsAffected = command.ExecuteNonQuery();
                    return rowsAffected > 0;
                }
            }
        }
        
        // DELETE 操作（軟刪除）
        public bool DeactivateCustomer(int customerId)
        {
            const string sql = @"
                UPDATE Customers 
                SET IsActive = 0, UpdatedAt = GETDATE() 
                WHERE CustomerId = @CustomerId";
            
            using (var connection = new SqlConnection(_connectionString))
            {
                using (var command = new SqlCommand(sql, connection))
                {
                    command.Parameters.AddWithValue("@CustomerId", customerId);
                    
                    connection.Open();
                    int rowsAffected = command.ExecuteNonQuery();
                    return rowsAffected > 0;
                }
            }
        }
        
        // 批次操作
        public void BulkInsertProducts(List<Product> products)
        {
            const string sql = @"
                INSERT INTO Products (ProductName, ProductCode, CategoryId, UnitPrice, StockQuantity, IsActive)
                VALUES (@ProductName, @ProductCode, @CategoryId, @UnitPrice, @StockQuantity, 1)";
            
            using (var connection = new SqlConnection(_connectionString))
            {
                connection.Open();
                using (var transaction = connection.BeginTransaction())
                {
                    try
                    {
                        using (var command = new SqlCommand(sql, connection, transaction))
                        {
                            // 準備參數
                            command.Parameters.Add("@ProductName", System.Data.SqlDbType.NVarChar, 200);
                            command.Parameters.Add("@ProductCode", System.Data.SqlDbType.NVarChar, 50);
                            command.Parameters.Add("@CategoryId", System.Data.SqlDbType.Int);
                            command.Parameters.Add("@UnitPrice", System.Data.SqlDbType.Decimal);
                            command.Parameters.Add("@StockQuantity", System.Data.SqlDbType.Int);
                            
                            foreach (var product in products)
                            {
                                command.Parameters["@ProductName"].Value = product.ProductName;
                                command.Parameters["@ProductCode"].Value = product.ProductCode;
                                command.Parameters["@CategoryId"].Value = product.CategoryId;
                                command.Parameters["@UnitPrice"].Value = product.UnitPrice;
                                command.Parameters["@StockQuantity"].Value = product.StockQuantity;
                                
                                command.ExecuteNonQuery();
                            }
                        }
                        
                        transaction.Commit();
                    }
                    catch
                    {
                        transaction.Rollback();
                        throw;
                    }
                }
            }
        }
        
        // 預存程序呼叫
        public List<CustomerOrderSummary> GetCustomerOrderSummary(int customerId)
        {
            const string sql = "EXEC sp_GetCustomerOrderSummary @CustomerId";
            
            var results = new List<CustomerOrderSummary>();
            
            using (var connection = new SqlConnection(_connectionString))
            {
                using (var command = new SqlCommand(sql, connection))
                {
                    command.Parameters.AddWithValue("@CustomerId", customerId);
                    
                    connection.Open();
                    using (var reader = command.ExecuteReader())
                    {
                        while (reader.Read())
                        {
                            results.Add(new CustomerOrderSummary
                            {
                                OrderId = reader.GetInt32("OrderId"),
                                OrderNumber = reader.GetString("OrderNumber"),
                                OrderDate = reader.GetDateTime("OrderDate"),
                                TotalAmount = reader.GetDecimal("TotalAmount"),
                                OrderStatus = reader.GetString("OrderStatus")
                            });
                        }
                    }
                }
            }
            
            return results;
        }
    }
    
    // 支援類別
    public class Product
    {
        public string ProductName { get; set; }
        public string ProductCode { get; set; }
        public int CategoryId { get; set; }
        public decimal UnitPrice { get; set; }
        public int StockQuantity { get; set; }
    }
    
    public class CustomerOrderSummary
    {
        public int OrderId { get; set; }
        public string OrderNumber { get; set; }
        public DateTime OrderDate { get; set; }
        public decimal TotalAmount { get; set; }
        public string OrderStatus { get; set; }
    }
}
```

---

## 🔗 第二節：Entity Framework Core 基礎

### 2.1 Entity Framework Core 簡介

#### 2.1.1 ORM概念和EF Core特色

```csharp
/*
Object-Relational Mapping (ORM) 概念：

ORM是一種程式設計技術，用於將物件導向程式語言的物件模型
與關聯式資料庫的表格結構之間進行轉換和映射。

Entity Framework Core 特色：
1. 跨平台支援 (.NET Core/.NET 5+)
2. 輕量級和高效能
3. Code First 和 Database First 支援
4. LINQ 查詢支援
5. 變更追蹤 (Change Tracking)
6. 遷移 (Migrations) 支援
7. 連接池 (Connection Pooling)
8. 異步操作支援
*/

using Microsoft.EntityFrameworkCore;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace ECommerceApp.Models
{
    // 1. 實體類別 (Entity Classes)
    public class Customer
    {
        public int CustomerId { get; set; }
        
        [Required]
        [MaxLength(50)]
        public string FirstName { get; set; }
        
        [Required]
        [MaxLength(50)]
        public string LastName { get; set; }
        
        [Required]
        [MaxLength(100)]
        [EmailAddress]
        public string Email { get; set; }
        
        [Phone]
        [MaxLength(20)]
        public string Phone { get; set; }
        
        public DateTime? DateOfBirth { get; set; }
        
        public DateTime RegistrationDate { get; set; } = DateTime.UtcNow;
        
        public bool IsActive { get; set; } = true;
        
        // 地址資訊
        [MaxLength(200)]
        public string AddressLine1 { get; set; }
        
        [MaxLength(200)]
        public string AddressLine2 { get; set; }
        
        [MaxLength(50)]
        