# 第五週：前端技術整合 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 掌握現代HTML5語義化標籤和CSS3進階特性
- 熟練使用JavaScript ES6+語法和DOM操作
- 整合Bootstrap框架建立響應式UI介面
- 實作Ajax和Fetch API進行異步資料交互
- 運用前端工具鏈優化開發流程
- 建立互動性強且用戶體驗良好的Web應用程式

---

## 🌐 第一節：HTML5和CSS3進階

### 1.1 語義化HTML5

#### 1.1.1 HTML5語義化標籤

```html
<!DOCTYPE html>
<html lang="zh-TW">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta name="description" content="電商產品頁面 - 提供最新電子產品和優質服務">
    <meta name="keywords" content="電商,產品,購物,電子產品">
    <meta name="author" content="ECommerce Team">
    
    <!-- Open Graph Meta Tags for Social Media -->
    <meta property="og:title" content="產品名稱 - 電商網站">
    <meta property="og:description" content="產品詳細描述">
    <meta property="og:image" content="/images/product-thumbnail.jpg">
    <meta property="og:url" content="https://example.com/products/123">
    <meta property="og:type" content="product">
    
    <!-- Twitter Card Meta Tags -->
    <meta name="twitter:card" content="summary_large_image">
    <meta name="twitter:title" content="產品名稱">
    <meta name="twitter:description" content="產品描述">
    <meta name="twitter:image" content="/images/product-thumbnail.jpg">
    
    <title>產品名稱 - 電商網站</title>
    
    <!-- Preload Critical Resources -->
    <link rel="preload" href="/css/critical.css" as="style">
    <link rel="preload" href="/fonts/main-font.woff2" as="font" type="font/woff2" crossorigin>
    
    <!-- Stylesheets -->
    <link href="/css/bootstrap.min.css" rel="stylesheet">
    <link href="/css/app.css" rel="stylesheet">
    
    <!-- Favicon -->
    <link rel="icon" type="image/x-icon" href="/favicon.ico">
</head>
<body>
    <!-- 頁面頂部導航 -->
    <header class="main-header" role="banner">
        <nav class="navbar navbar-expand-lg navbar-dark bg-primary" aria-label="主要導航">
            <div class="container">
                <!-- Logo -->
                <a class="navbar-brand" href="/" aria-label="回到首頁">
                    <img src="/images/logo.svg" alt="電商網站" width="120" height="40">
                </a>
                
                <!-- 手機版選單按鈕 -->
                <button class="navbar-toggler" type="button" 
                        data-bs-toggle="collapse" 
                        data-bs-target="#navbarNav" 
                        aria-controls="navbarNav" 
                        aria-expanded="false" 
                        aria-label="切換導航選單">
                    <span class="navbar-toggler-icon"></span>
                </button>
                
                <!-- 導航選單 -->
                <div class="collapse navbar-collapse" id="navbarNav">
                    <ul class="navbar-nav me-auto">
                        <li class="nav-item">
                            <a class="nav-link" href="/" aria-current="page">首頁</a>
                        </li>
                        <li class="nav-item dropdown">
                            <a class="nav-link dropdown-toggle" href="#" role="button" 
                               data-bs-toggle="dropdown" aria-expanded="false">
                                產品分類
                            </a>
                            <ul class="dropdown-menu">
                                <li><a class="dropdown-item" href="/products?category=electronics">電子產品</a></li>
                                <li><a class="dropdown-item" href="/products?category=clothing">服飾配件</a></li>
                                <li><a class="dropdown-item" href="/products?category=books">書籍文具</a></li>
                                <li><hr class="dropdown-divider"></li>
                                <li><a class="dropdown-item" href="/products">所有產品</a></li>
                            </ul>
                        </li>
                        <li class="nav-item">
                            <a class="nav-link" href="/about">關於我們</a>
                        </li>
                        <li class="nav-item">
                            <a class="nav-link" href="/contact">聯絡我們</a>
                        </li>
                    </ul>
                    
                    <!-- 搜尋表單 -->
                    <form class="d-flex me-3" role="search" action="/products" method="get">
                        <div class="input-group">
                            <input class="form-control" type="search" name="q" 
                                   placeholder="搜尋產品..." aria-label="搜尋產品" 
                                   value="@ViewBag.SearchTerm">
                            <button class="btn btn-outline-light" type="submit" aria-label="搜尋">
                                <i class="fas fa-search" aria-hidden="true"></i>
                            </button>
                        </div>
                    </form>
                    
                    <!-- 使用者選單 -->
                    <ul class="navbar-nav">
                        <li class="nav-item">
                            <a class="nav-link position-relative" href="/cart" aria-label="購物車">
                                <i class="fas fa-shopping-cart" aria-hidden="true"></i>
                                <span class="badge bg-danger position-absolute top-0 start-100 translate-middle" 
                                      id="cart-count">0</span>
                                <span class="visually-hidden">購物車</span>
                            </a>
                        </li>
                        <li class="nav-item dropdown">
                            <a class="nav-link dropdown-toggle" href="#" role="button" 
                               data-bs-toggle="dropdown" aria-expanded="false">
                                <i class="fas fa-user" aria-hidden="true"></i>
                                會員中心
                            </a>
                            <ul class="dropdown-menu">
                                <li><a class="dropdown-item" href="/account/profile">個人資料</a></li>
                                <li><a class="dropdown-item" href="/account/orders">訂單查詢</a></li>
                                <li><a class="dropdown-item" href="/account/wishlist">我的收藏</a></li>
                                <li><hr class="dropdown-divider"></li>
                                <li><a class="dropdown-item" href="/account/logout">登出</a></li>
                            </ul>
                        </li>
                    </ul>
                </div>
            </div>
        </nav>
    </header>

    <!-- 主要內容區域 -->
    <main class="main-content" role="main">
        <!-- 麵包屑導航 -->
        <section class="breadcrumb-section bg-light py-3">
            <div class="container">
                <nav aria-label="breadcrumb">
                    <ol class="breadcrumb mb-0">
                        <li class="breadcrumb-item"><a href="/">首頁</a></li>
                        <li class="breadcrumb-item"><a href="/products">產品</a></li>
                        <li class="breadcrumb-item"><a href="/products?category=electronics">電子產品</a></li>
                        <li class="breadcrumb-item active" aria-current="page">智慧型手機</li>
                    </ol>
                </nav>
            </div>
        </section>

        <!-- 產品詳情區域 -->
        <section class="product-details py-5">
            <div class="container">
                <div class="row">
                    <!-- 產品圖片 -->
                    <div class="col-lg-6">
                        <article class="product-gallery">
                            <figure class="main-image mb-3">
                                <img src="/images/products/smartphone-main.jpg" 
                                     alt="智慧型手機主圖" 
                                     class="img-fluid rounded shadow"
                                     id="main-product-image">
                            </figure>
                            
                            <!-- 縮圖畫廊 -->
                            <div class="thumbnail-gallery">
                                <div class="row g-2">
                                    <div class="col-3">
                                        <img src="/images/products/smartphone-thumb-1.jpg" 
                                             alt="智慧型手機圖片1" 
                                             class="img-fluid rounded cursor-pointer thumbnail-image active"
                                             data-main-image="/images/products/smartphone-main.jpg">
                                    </div>
                                    <div class="col-3">
                                        <img src="/images/products/smartphone-thumb-2.jpg" 
                                             alt="智慧型手機圖片2" 
                                             class="img-fluid rounded cursor-pointer thumbnail-image"
                                             data-main-image="/images/products/smartphone-side.jpg">
                                    </div>
                                    <div class="col-3">
                                        <img src="/images/products/smartphone-thumb-3.jpg" 
                                             alt="智慧型手機圖片3" 
                                             class="img-fluid rounded cursor-pointer thumbnail-image"
                                             data-main-image="/images/products/smartphone-back.jpg">
                                    </div>
                                    <div class="col-3">
                                        <img src="/images/products/smartphone-thumb-4.jpg" 
                                             alt="智慧型手機圖片4" 
                                             class="img-fluid rounded cursor-pointer thumbnail-image"
                                             data-main-image="/images/products/smartphone-box.jpg">
                                    </div>
                                </div>
                            </div>
                        </article>
                    </div>

                    <!-- 產品資訊 -->
                    <div class="col-lg-6">
                        <article class="product-info">
                            <header class="product-header mb-4">
                                <h1 class="product-title mb-2">最新智慧型手機 Pro Max 256GB</h1>
                                <div class="product-rating mb-3">
                                    <div class="star-rating" role="img" aria-label="4.5顆星評分">
                                        <i class="fas fa-star text-warning" aria-hidden="true"></i>
                                        <i class="fas fa-star text-warning" aria-hidden="true"></i>
                                        <i class="fas fa-star text-warning" aria-hidden="true"></i>
                                        <i class="fas fa-star text-warning" aria-hidden="true"></i>
                                        <i class="fas fa-star-half-alt text-warning" aria-hidden="true"></i>
                                        <span class="rating-text ms-2">(4.5/5 - 156 評論)</span>
                                    </div>
                                </div>
                            </header>

                            <!-- 價格資訊 -->
                            <div class="price-section mb-4">
                                <div class="current-price">
                                    <span class="price-label text-muted">售價：</span>
                                    <span class="price h3 text-primary fw-bold">NT$ 32,900</span>
                                </div>
                                <div class="original-price">
                                    <span class="text-muted text-decoration-line-through">原價：NT$ 35,900</span>
                                    <span class="badge bg-danger ms-2">省 NT$ 3,000</span>
                                </div>
                            </div>

                            <!-- 產品規格 -->
                            <div class="product-specs mb-4">
                                <h3 class="h5 mb-3">產品規格</h3>
                                <dl class="row">
                                    <dt class="col-sm-4">螢幕尺寸</dt>
                                    <dd class="col-sm-8">6.7吋 Super Retina XDR</dd>
                                    
                                    <dt class="col-sm-4">儲存容量</dt>
                                    <dd class="col-sm-8">256GB</dd>
                                    
                                    <dt class="col-sm-4">主相機</dt>
                                    <dd class="col-sm-8">48MP 三鏡頭系統</dd>
                                    
                                    <dt class="col-sm-4">電池</dt>
                                    <dd class="col-sm-8">最長 29 小時影片播放</dd>
                                    
                                    <dt class="col-sm-4">作業系統</dt>
                                    <dd class="col-sm-8">iOS 17</dd>
                                </dl>
                            </div>

                            <!-- 顏色選擇 -->
                            <div class="color-selection mb-4">
                                <h3 class="h6 mb-3">選擇顏色</h3>
                                <div class="color-options">
                                    <input type="radio" class="btn-check" name="color" id="color-black" value="black" checked>
                                    <label class="btn btn-outline-dark" for="color-black">太空黑</label>
                                    
                                    <input type="radio" class="btn-check" name="color" id="color-silver" value="silver">
                                    <label class="btn btn-outline-secondary" for="color-silver">銀色</label>
                                    
                                    <input type="radio" class="btn-check" name="color" id="color-gold" value="gold">
                                    <label class="btn btn-outline-warning" for="color-gold">金色</label>
                                    
                                    <input type="radio" class="btn-check" name="color" id="color-blue" value="blue">
                                    <label class="btn btn-outline-primary" for="color-blue">深藍色</label>
                                </div>
                            </div>

                            <!-- 數量選擇和購買按鈕 -->
                            <form class="add-to-cart-form" action="/cart/add" method="post">
                                <input type="hidden" name="productId" value="123">
                                <input type="hidden" name="__RequestVerificationToken" value="@Html.AntiForgeryToken()">
                                
                                <div class="quantity-section mb-4">
                                    <label for="quantity" class="form-label">數量</label>
                                    <div class="input-group" style="max-width: 150px;">
                                        <button class="btn btn-outline-secondary" type="button" 
                                                id="quantity-decrease" aria-label="減少數量">-</button>
                                        <input type="number" class="form-control text-center" 
                                               id="quantity" name="quantity" value="1" min="1" max="10" 
                                               aria-label="選擇數量">
                                        <button class="btn btn-outline-secondary" type="button" 
                                                id="quantity-increase" aria-label="增加數量">+</button>
                                    </div>
                                    <small class="text-muted">庫存：15 件</small>
                                </div>

                                <div class="action-buttons">
                                    <button type="submit" class="btn btn-primary btn-lg me-3" 
                                            id="add-to-cart-btn">
                                        <i class="fas fa-shopping-cart me-2" aria-hidden="true"></i>
                                        加入購物車
                                    </button>
                                    <button type="button" class="btn btn-outline-danger btn-lg" 
                                            id="add-to-wishlist-btn" aria-label="加入收藏">
                                        <i class="far fa-heart" aria-hidden="true"></i>
                                    </button>
                                </div>
                            </form>

                            <!-- 配送資訊 -->
                            <div class="shipping-info mt-4 p-3 bg-light rounded">
                                <h4 class="h6 mb-2">配送資訊</h4>
                                <ul class="list-unstyled mb-0">
                                    <li><i class="fas fa-truck text-success me-2" aria-hidden="true"></i>免費宅配 (訂單滿 NT$ 1,000)</li>
                                    <li><i class="fas fa-clock text-info me-2" aria-hidden="true"></i>1-3 個工作天到貨</li>
                                    <li><i class="fas fa-shield-alt text-warning me-2" aria-hidden="true"></i>1年保固服務</li>
                                </ul>
                            </div>
                        </article>
                    </div>
                </div>
            </div>
        </section>

        <!-- 產品詳細說明 -->
        <section class="product-description py-5 bg-light">
            <div class="container">
                <div class="row">
                    <div class="col-lg-8 mx-auto">
                        <nav class="description-nav mb-4">
                            <div class="nav nav-tabs" id="nav-tab" role="tablist">
                                <button class="nav-link active" id="nav-description-tab" 
                                        data-bs-toggle="tab" data-bs-target="#nav-description" 
                                        type="button" role="tab" aria-controls="nav-description" 
                                        aria-selected="true">產品描述</button>
                                <button class="nav-link" id="nav-specs-tab" 
                                        data-bs-toggle="tab" data-bs-target="#nav-specs" 
                                        type="button" role="tab" aria-controls="nav-specs" 
                                        aria-selected="false">詳細規格</button>
                                <button class="nav-link" id="nav-reviews-tab" 
                                        data-bs-toggle="tab" data-bs-target="#nav-reviews" 
                                        type="button" role="tab" aria-controls="nav-reviews" 
                                        aria-selected="false">顧客評論</button>
                            </div>
                        </nav>
                        
                        <div class="tab-content" id="nav-tabContent">
                            <!-- 產品描述 -->
                            <div class="tab-pane fade show active" id="nav-description" 
                                 role="tabpanel" aria-labelledby="nav-description-tab">
                                <div class="description-content">
                                    <h3 class="mb-3">產品特色</h3>
                                    <p class="lead">體驗前所未有的智慧型手機科技，集結頂尖效能與創新設計於一身。</p>
                                    
                                    <div class="feature-highlights">
                                        <div class="row g-4">
                                            <div class="col-md-6">
                                                <div class="feature-item">
                                                    <h4 class="h5">
                                                        <i class="fas fa-camera text-primary me-2" aria-hidden="true"></i>
                                                        專業級相機系統
                                                    </h4>
                                                    <p>48MP 主相機配備先進感光元件，無論白天黑夜都能拍出令人驚艷的照片。</p>
                                                </div>
                                            </div>
                                            <div class="col-md-6">
                                                <div class="feature-item">
                                                    <h4 class="h5">
                                                        <i class="fas fa-bolt text-warning me-2" aria-hidden="true"></i>
                                                        極速效能
                                                    </h4>
                                                    <p>搭載最新處理器，提供順暢的多工處理和遊戲體驗。</p>
                                                </div>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </div>
                            
                            <!-- 詳細規格 -->
                            <div class="tab-pane fade" id="nav-specs" 
                                 role="tabpanel" aria-labelledby="nav-specs-tab">
                                <div class="specs-table">
                                    <table class="table table-striped">
                                        <tbody>
                                            <tr>
                                                <th scope="row">尺寸</th>
                                                <td>160.7 x 77.6 x 7.85 mm</td>
                                            </tr>
                                            <tr>
                                                <th scope="row">重量</th>
                                                <td>221 g</td>
                                            </tr>
                                            <tr>
                                                <th scope="row">螢幕</th>
                                                <td>6.7吋 Super Retina XDR OLED，2796 x 1290 解析度</td>
                                            </tr>
                                            <tr>
                                                <th scope="row">處理器</th>
                                                <td>A17 Pro 仿生晶片</td>
                                            </tr>
                                            <tr>
                                                <th scope="row">儲存空間</th>
                                                <td>256GB</td>
                                            </tr>
                                            <tr>
                                                <th scope="row">主相機</th>
                                                <td>48MP f/1.78 + 12MP f/2.2 超廣角 + 12MP f/2.8 望遠</td>
                                            </tr>
                                            <tr>
                                                <th scope="row">前相機</th>
                                                <td>12MP f/1.9</td>
                                            </tr>
                                            <tr>
                                                <th scope="row">電池</th>
                                                <td>最長 29 小時影片播放</td>
                                            </tr>
                                            <tr>
                                                <th scope="row">連接</th>
                                                <td>5G, Wi-Fi 6E, 藍牙 5.3, NFC</td>
                                            </tr>
                                            <tr>
                                                <th scope="row">作業系統</th>
                                                <td>iOS 17</td>
                                            </tr>
                                        </tbody>
                                    </table>
                                </div>
                            </div>
                            
                            <!-- 顧客評論 -->
                            <div class="tab-pane fade" id="nav-reviews" 
                                 role="tabpanel" aria-labelledby="nav-reviews-tab">
                                <div class="reviews-section">
                                    <div class="reviews-summary mb-4">
                                        <div class="row align-items-center">
                                            <div class="col-md-4 text-center">
                                                <div class="average-rating">
                                                    <span class="rating-number display-4 fw-bold">4.5</span>
                                                    <div class="rating-stars mb-2">
                                                        <i class="fas fa-star text-warning" aria-hidden="true"></i>
                                                        <i class="fas fa-star text-warning" aria-hidden="true"></i>
                                                        <i class="fas fa-star text-warning" aria-hidden="true"></i>
                                                        <i class="fas fa-star text-warning" aria-hidden="true"></i>
                                                        <i class="fas fa-star-half-alt text-warning" aria-hidden="true"></i>
                                                    </div>
                                                    <p class="text-muted">共 156 則評論</p>
                                                </div>
                                            </div>
                                            <div class="col-md-8">
                                                <div class="rating-breakdown">
                                                    <div class="rating-bar-item d-flex align-items-center mb-2">
                                                        <span class="rating-label me-2">5星</span>
                                                        <div class="progress flex-grow-1 me-2">
                                                            <div class="progress-bar bg-warning" style="width: 70%"></div>
                                                        </div>
                                                        <span class="rating-count">109</span>
                                                    </div>
                                                    <div class="rating-bar-item d-flex align-items-center mb-2">
                                                        <span class="rating-label me-2">4星</span>
                                                        <div class="progress flex-grow-1 me-2">
                                                            <div class="progress-bar bg-warning" style="width: 20%"></div>
                                                        </div>
                                                        <span class="rating-count">31</span>
                                                    </div>
                                                    <div class="rating-bar-item d-flex align-items-center mb-2">
                                                        <span class="rating-label me-2">3星</span>
                                                        <div class="progress flex-grow-1 me-2">
                                                            <div class="progress-bar bg-warning" style="width: 8%"></div>
                                                        </div>
                                                        <span class="rating-count">12</span>
                                                    </div>
                                                    <div class="rating-bar-item d-flex align-items-center mb-2">
                                                        <span class="rating-label me-2">2星</span>
                                                        <div class="progress flex-grow-1 me-2">
                                                            <div class="progress-bar bg-warning" style="width: 2%"></div>
                                                        </div>
                                                        <span class="rating-count">3</span>
                                                    </div>
                                                    <div class="rating-bar-item d-flex align-items-center">
                                                        <span class="rating-label me-2">1星</span>
                                                        <div class="progress flex-grow-1 me-2">
                                                            <div class="progress-bar bg-warning" style="width: 1%"></div>
                                                        </div>
                                                        <span class="rating-count">1</span>
                                                    </div>
                                                </div>
                                            </div>
                                        </div>
                                    </div>
                                    
                                    <!-- 評論列表 -->
                                    <div class="reviews-list" id="reviews-container">
                                        <!-- 評論項目將通過JavaScript載入 -->
                                    </div>
                                    
                                    <!-- 載入更多按鈕 -->
                                    <div class="text-center mt-4">
                                        <button class="btn btn-outline-primary" id="load-more-reviews">
                                            載入更多評論
                                        </button>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </section>

        <!-- 相關產品 -->
        <section class="related-products py-5">
            <div class="container">
                <h2 class="text-center mb-5">相關產品推薦</h2>
                <div class="row" id="related-products-container">
                    <!-- 相關產品將通過JavaScript載入 -->
                </div>
            </div>
        </section>
    </main>

    <!-- 頁面底部 -->
    <footer class="main-footer bg-dark text-light py-5" role="contentinfo">
        <div class="container">
            <div class="row">
                <div class="col-lg-3 col-md-6 mb-4">
                    <h5>關於我們</h5>
                    <p class="text-muted">提供優質產品和卓越服務的電商平台，致力於為客戶創造最佳的購物體驗。</p>
                    <div class="social-links">
                        <a href="#" class="text-light me-3" aria-label="Facebook">
                            <i class="fab fa-facebook-f" aria-hidden="true"></i>
                        </a>
                        <a href="#" class="text-light me-3" aria-label="Instagram">
                            <i class="fab fa-instagram" aria-hidden="true"></i>
                        </a>
                        <a href="#" class="text-light me-3" aria-label="Line">
                            <i class="fab fa-line" aria-hidden="true"></i>
                        </a>
                    </div>
                </div>
                <div class="col-lg-3 col-md-6 mb-4">
                    <h5>客戶服務</h5>
                    <ul class="list-unstyled">
                        <li><a href="/help" class="text-muted">常見問題</a></li>
                        <li><a href="/shipping" class="text-muted">配送資訊</a></li>
                        <li><a href="/returns" class="text-muted">退換貨政策</a></li>
                        <li><a href="/contact" class="text-muted">聯絡我們</a></li>
                    </ul>
                </div>
                <div class="col-lg-3 col-md-6 mb-4">
                    <h5>會員專區</h5>
                    <ul class="list-unstyled">
                        <li><a href="/account/