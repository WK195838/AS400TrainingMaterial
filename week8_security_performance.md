# 第八週：安全性和效能優化 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 理解Web應用程式常見的安全威脅和防護措施
- 實作完整的身分驗證和授權機制
- 掌握資料保護和加密技術
- 建立安全的API和資料傳輸
- 分析和優化應用程式效能
- 實作快取策略和資料庫優化

---

## 🔒 第一節：Web應用程式安全性

### 1.1 OWASP Top 10 安全威脅

#### 1.1.1 常見安全威脅及防護

```csharp
/*
OWASP Top 10 Web應用程式安全風險 (2021版)：

1. A01:2021 – Broken Access Control (存取控制破損)
2. A02:2021 – Cryptographic Failures (加密失敗)
3. A03:2021 – Injection (注入攻擊)
4. A04:2021 – Insecure Design (不安全設計)
5. A05:2021 – Security Misconfiguration (安全性設定錯誤)
6. A06:2021 – Vulnerable and Outdated Components (易受攻擊和過時組件)
7. A07:2021 – Identification and Authentication Failures (身分識別和驗證失敗)
8. A08:2021 – Software and Data Integrity Failures (軟體和資料完整性失敗)
9. A09:2021 – Security Logging and Monitoring Failures (安全日誌和監控失敗)
10. A10:2021 – Server-Side Request Forgery (伺服器端請求偽造)
*/

using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Filters;
using Microsoft.AspNetCore.DataProtection;
using System.Security.Claims;
using System.Text.RegularExpressions;

namespace ECommerceApp.Security
{
    // 1. 防止注入攻擊 - SQL注入防護
    public class SqlInjectionProtectionService
    {
        private readonly ILogger<SqlInjectionProtectionService> _logger;
        
        public SqlInjectionProtectionService(ILogger<SqlInjectionProtectionService> logger)
        {
            _logger = logger;
        }
        
        // 危險的SQL關鍵字模式
        private static readonly string[] SqlInjectionPatterns = {
            @"(\s*(union|select|insert|delete|update|create|drop|exec|execute|sp_|xp_)\s*)",
            @"(\s*(script|javascript|vbscript|onload|onerror|onclick)\s*)",
            @"(\s*(or|and)\s+[\w\s]*\s*=\s*[\w\s]*)",
            @"(\s*;\s*(drop|delete|update|insert)\s*)",
            @"(\s*--\s*)",
            @"(\s*/\*.*\*/\s*)"
        };
        
        public bool IsPotentialSqlInjection(string input)
        {
            if (string.IsNullOrWhiteSpace(input))
                return false;
            
            var normalizedInput = input.ToLowerInvariant();
            
            foreach (var pattern in SqlInjectionPatterns)
            {
                if (Regex.IsMatch(normalizedInput, pattern, RegexOptions.IgnoreCase))
                {
                    _logger.LogWarning("Potential SQL injection detected: {Input}", input);
                    return true;
                }
            }
            
            return false;
        }
        
        public string SanitizeInput(string input)
        {
            if (string.IsNullOrWhiteSpace(input))
                return input;
            
            // 移除潛在的危險字符
            var sanitized = input
                .Replace("'", "''")  // 跳脫單引號
                .Replace("--", "")   // 移除SQL註解
                .Replace("/*", "")   // 移除註解開始
                .Replace("*/", "");  // 移除註解結束
            
            return sanitized;
        }
    }
    
    // 2. 防止XSS攻擊
    public class XssProtectionService
    {
        private readonly ILogger<XssProtectionService> _logger;
        
        public XssProtectionService(ILogger<XssProtectionService> logger)
        {
            _logger = logger;
        }
        
        // XSS攻擊模式
        private static readonly string[] XssPatterns = {
            @"<script[^>]*>.*?</script>",
            @"javascript:",
            @"vbscript:",
            @"onload\s*=",
            @"onerror\s*=",
            @"onclick\s*=",
            @"onmouseover\s*=",
            @"<iframe[^>]*>.*?</iframe>",
            @"<object[^>]*>.*?</object>",
            @"<embed[^>]*>.*?</embed>"
        };
        
        public bool IsPotentialXss(string input)
        {
            if (string.IsNullOrWhiteSpace(input))
                return false;
            
            var decodedInput = System.Net.WebUtility.HtmlDecode(input);
            
            foreach (var pattern in XssPatterns)
            {
                if (Regex.IsMatch(decodedInput, pattern, RegexOptions.IgnoreCase))
                {
                    _logger.LogWarning("Potential XSS attack detected: {Input}", input);
                    return true;
                }
            }
            
            return false;
        }
        
        public string SanitizeHtml(string input)
        {
            if (string.IsNullOrWhiteSpace(input))
                return input;
            
            // 使用HtmlEncoder進行編碼
            return System.Text.Encodings.Web.HtmlEncoder.Default.Encode(input);
        }
        
        public string StripHtml(string input)
        {
            if (string.IsNullOrWhiteSpace(input))
                return input;
            
            // 移除所有HTML標籤
            return Regex.Replace(input, "<.*?>", string.Empty);
        }
    }
    
    // 3. CSRF防護
    public class CsrfProtectionMiddleware
    {
        private readonly RequestDelegate _next;
        private readonly ILogger<CsrfProtectionMiddleware> _logger;
        
        public CsrfProtectionMiddleware(RequestDelegate next, ILogger<CsrfProtectionMiddleware> logger)
        {
            _next = next;
            _logger = logger;
        }
        
        public async Task InvokeAsync(HttpContext context)
        {
            // 檢查是否為狀態改變的HTTP方法
            if (IsStateChangingRequest(context.Request))
            {
                if (!await ValidateCsrfTokenAsync(context))
                {
                    _logger.LogWarning("CSRF token validation failed for {Method} {Path}", 
                        context.Request.Method, context.Request.Path);
                    
                    context.Response.StatusCode = 403;
                    await context.Response.WriteAsync("CSRF token validation failed");
                    return;
                }
            }
            
            await _next(context);
        }
        
        private static bool IsStateChangingRequest(HttpRequest request)
        {
            return request.Method.Equals("POST", StringComparison.OrdinalIgnoreCase) ||
                   request.Method.Equals("PUT", StringComparison.OrdinalIgnoreCase) ||
                   request.Method.Equals("DELETE", StringComparison.OrdinalIgnoreCase) ||
                   request.Method.Equals("PATCH", StringComparison.OrdinalIgnoreCase);
        }
        
        private async Task<bool> ValidateCsrfTokenAsync(HttpContext context)
        {
            // 從表單或標頭取得CSRF token
            var formToken = context.Request.Form["__RequestVerificationToken"].FirstOrDefault();
            var headerToken = context.Request.Headers["X-CSRF-TOKEN"].FirstOrDefault();
            
            var token = formToken ?? headerToken;
            
            if (string.IsNullOrEmpty(token))
                return false;
            
            // 這裡應該驗證token的有效性
            // 實際實作中會使用ASP.NET Core的內建CSRF保護
            return !string.IsNullOrEmpty(token);
        }
    }
    
    // 4. 輸入驗證中介軟體
    public class InputValidationFilter : ActionFilterAttribute
    {
        private readonly SqlInjectionProtectionService _sqlProtection;
        private readonly XssProtectionService _xssProtection;
        
        public InputValidationFilter(
            SqlInjectionProtectionService sqlProtection,
            XssProtectionService xssProtection)
        {
            _sqlProtection = sqlProtection;
            _xssProtection = xssProtection;
        }
        
        public override void OnActionExecuting(ActionExecutingContext context)
        {
            foreach (var parameter in context.ActionArguments.Values)
            {
                if (parameter is string stringValue)
                {
                    if (_sqlProtection.IsPotentialSqlInjection(stringValue) ||
                        _xssProtection.IsPotentialXss(stringValue))
                    {
                        context.Result = new BadRequestObjectResult(new
                        {
                            Error = "Invalid input detected",
                            Message = "The input contains potentially malicious content"
                        });
                        return;
                    }
                }
                else if (parameter != null)
                {
                    ValidateObjectProperties(parameter, context);
                    if (context.Result != null) return;
                }
            }
            
            base.OnActionExecuting(context);
        }
        
        private void ValidateObjectProperties(object obj, ActionExecutingContext context)
        {
            var properties = obj.GetType().GetProperties()
                .Where(p => p.PropertyType == typeof(string) && p.CanRead);
            
            foreach (var property in properties)
            {
                var value = property.GetValue(obj) as string;
                if (!string.IsNullOrEmpty(value))
                {
                    if (_sqlProtection.IsPotentialSqlInjection(value) ||
                        _xssProtection.IsPotentialXss(value))
                    {
                        context.Result = new BadRequestObjectResult(new
                        {
                            Error = "Invalid input detected",
                            Property = property.Name,
                            Message = $"The value of {property.Name} contains potentially malicious content"
                        });
                        return;
                    }
                }
            }
        }
    }
}
```

#### 1.1.2 安全標頭和HTTPS設定

```csharp
using Microsoft.AspNetCore.HttpsPolicy;

namespace ECommerceApp.Security
{
    // 安全標頭中介軟體
    public class SecurityHeadersMiddleware
    {
        private readonly RequestDelegate _next;
        
        public SecurityHeadersMiddleware(RequestDelegate next)
        {
            _next = next;
        }
        
        public async Task InvokeAsync(HttpContext context)
        {
            // X-Content-Type-Options: 防止MIME類型混亂攻擊
            context.Response.Headers.Add("X-Content-Type-Options", "nosniff");
            
            // X-Frame-Options: 防止點擊劫持攻擊
            context.Response.Headers.Add("X-Frame-Options", "DENY");
            
            // X-XSS-Protection: 啟用瀏覽器XSS保護
            context.Response.Headers.Add("X-XSS-Protection", "1; mode=block");
            
            // Referrer-Policy: 控制Referrer資訊洩露
            context.Response.Headers.Add("Referrer-Policy", "strict-origin-when-cross-origin");
            
            // Content-Security-Policy: 防止XSS和資料注入攻擊
            var csp = "default-src 'self'; " +
                     "script-src 'self' 'unsafe-inline' https://cdnjs.cloudflare.com https://cdn.jsdelivr.net; " +
                     "style-src 'self' 'unsafe-inline' https://cdnjs.cloudflare.com https://fonts.googleapis.com; " +
                     "font-src 'self' https://fonts.gstatic.com; " +
                     "img-src 'self' data: https:; " +
                     "connect-src 'self' https://api.ecommerce.com; " +
                     "frame-ancestors 'none';";
            
            context.Response.Headers.Add("Content-Security-Policy", csp);
            
            // Feature-Policy: 控制瀏覽器功能使用
            context.Response.Headers.Add("Permissions-Policy", 
                "geolocation=(), microphone=(), camera=()");
            
            // HSTS: 強制使用HTTPS
            if (context.Request.IsHttps)
            {
                context.Response.Headers.Add("Strict-Transport-Security", 
                    "max-age=31536000; includeSubDomains");
            }
            
            await _next(context);
        }
    }
    
    // HTTPS重定向和HSTS設定
    public static class SecurityExtensions
    {
        public static IServiceCollection AddSecurityServices(this IServiceCollection services)
        {
            // HSTS設定
            services.AddHsts(options =>
            {
                options.Preload = true;
                options.IncludeSubDomains = true;
                options.MaxAge = TimeSpan.FromDays(365);
            });
            
            // HTTPS重定向設定
            services.AddHttpsRedirection(options =>
            {
                options.RedirectStatusCode = StatusCodes.Status308PermanentRedirect;
                options.HttpsPort = 443;
            });
            
            // 資料保護設定
            services.AddDataProtection()
                .PersistKeysToFileSystem(new DirectoryInfo("/app/keys"))
                .SetApplicationName("ECommerceApp")
                .SetDefaultKeyLifetime(TimeSpan.FromDays(90));
            
            return services;
        }
        
        public static IApplicationBuilder UseSecurityMiddleware(this IApplicationBuilder app)
        {
            app.UseMiddleware<SecurityHeadersMiddleware>();
            app.UseMiddleware<CsrfProtectionMiddleware>();
            return app;
        }
    }
}
```

### 1.2 身分驗證和授權

#### 1.2.1 JWT Token身分驗證

```csharp
using Microsoft.AspNetCore.Authentication.JwtBearer;
using Microsoft.IdentityModel.Tokens;
using System.IdentityModel.Tokens.Jwt;
using System.Security.Claims;
using System.Text;

namespace ECommerceApp.Security.Authentication
{
    // JWT Token服務
    public interface IJwtTokenService
    {
        Task<string> GenerateTokenAsync(ApplicationUser user);
        Task<ClaimsPrincipal> ValidateTokenAsync(string token);
        Task RevokeTokenAsync(string token);
        Task<bool> IsTokenRevokedAsync(string token);
    }
    
    public class JwtTokenService : IJwtTokenService
    {
        private readonly IConfiguration _configuration;
        private readonly UserManager<ApplicationUser> _userManager;
        private readonly IMemoryCache _revokedTokensCache;
        private readonly ILogger<JwtTokenService> _logger;
        
        public JwtTokenService(
            IConfiguration configuration,
            UserManager<ApplicationUser> userManager,
            IMemoryCache revokedTokensCache,
            ILogger<JwtTokenService> logger)
        {
            _configuration = configuration;
            _userManager = userManager;
            _revokedTokensCache = revokedTokensCache;
            _logger = logger;
        }
        
        public async Task<string> GenerateTokenAsync(ApplicationUser user)
        {
            var jwtSettings = _configuration.GetSection("JwtSettings");
            var secretKey = jwtSettings["SecretKey"];
            var issuer = jwtSettings["Issuer"];
            var audience = jwtSettings["Audience"];
            var expirationMinutes = int.Parse(jwtSettings["ExpirationMinutes"]);
            
            var key = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(secretKey));
            var credentials = new SigningCredentials(key, SecurityAlgorithms.HmacSha256);
            
            // 建立Claims
            var claims = new List<Claim>
            {
                new Claim(ClaimTypes.NameIdentifier, user.Id),
                new Claim(ClaimTypes.Name, user.UserName),
                new Claim(ClaimTypes.Email, user.Email),
                new Claim("FirstName", user.FirstName ?? ""),
                new Claim("LastName", user.LastName ?? ""),
                new Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()),
                new Claim(JwtRegisteredClaimNames.Iat, 
                    new DateTimeOffset(DateTime.UtcNow).ToUnixTimeSeconds().ToString(), 
                    ClaimValueTypes.Integer64)
            };
            
            // 加入角色Claims
            var roles = await _userManager.GetRolesAsync(user);
            foreach (var role in roles)
            {
                claims.Add(new Claim(ClaimTypes.Role, role));
            }
            
            // 加入自訂Claims
            var userClaims = await _userManager.GetClaimsAsync(user);
            claims.AddRange(userClaims);
            
            var token = new JwtSecurityToken(
                issuer: issuer,
                audience: audience,
                claims: claims,
                expires: DateTime.UtcNow.AddMinutes(expirationMinutes),
                signingCredentials: credentials
            );
            
            var tokenString = new JwtSecurityTokenHandler().WriteToken(token);
            
            _logger.LogInformation("JWT token generated for user {UserId}", user.Id);
            
            return tokenString;
        }
        
        public async Task<ClaimsPrincipal> ValidateTokenAsync(string token)
        {
            try
            {
                // 檢查Token是否已被撤銷
                if (await IsTokenRevokedAsync(token))
                {
                    _logger.LogWarning("Attempted to use revoked token");
                    return null;
                }
                
                var jwtSettings = _configuration.GetSection("JwtSettings");
                var secretKey = jwtSettings["SecretKey"];
                var issuer = jwtSettings["Issuer"];
                var audience = jwtSettings["Audience"];
                
                var key = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(secretKey));
                
                var tokenHandler = new JwtSecurityTokenHandler();
                var validationParameters = new TokenValidationParameters
                {
                    ValidateIssuerSigningKey = true,
                    IssuerSigningKey = key,
                    ValidateIssuer = true,
                    ValidIssuer = issuer,
                    ValidateAudience = true,
                    ValidAudience = audience,
                    ValidateLifetime = true,
                    ClockSkew = TimeSpan.Zero
                };
                
                var principal = tokenHandler.ValidateToken(token, validationParameters, out SecurityToken validatedToken);
                return principal;
            }
            catch (Exception ex)
            {
                _logger.LogWarning(ex, "Token validation failed");
                return null;
            }
        }
        
        public async Task RevokeTokenAsync(string token)
        {
            var tokenHandler = new JwtSecurityTokenHandler();
            var jwtToken = tokenHandler.ReadJwtToken(token);
            
            // 將Token加入黑名單，直到過期時間
            var expiration = jwtToken.ValidTo;
            var cacheKey = $"revoked_token_{jwtToken.RawSignature}";
            
            _revokedTokensCache.Set(cacheKey, true, expiration);
            
            _logger.LogInformation("Token revoked: {TokenId}", jwtToken.Id);
            
            await Task.CompletedTask;
        }
        
        public async Task<bool> IsTokenRevokedAsync(string token)
        {
            try
            {
                var tokenHandler = new JwtSecurityTokenHandler();
                var jwtToken = tokenHandler.ReadJwtToken(token);
                var cacheKey = $"revoked_token_{jwtToken.RawSignature}";
                
                return _revokedTokensCache.TryGetValue(cacheKey, out _);
            }
            catch
            {
                return false;
            }
        }
    }
    
    // 登入API Controller
    [ApiController]
    [Route("api/[controller]")]
    public class AuthController : ControllerBase
    {
        private readonly UserManager<ApplicationUser> _userManager;
        private readonly SignInManager<ApplicationUser> _signInManager;
        private readonly IJwtTokenService _jwtTokenService;
        private readonly ILogger<AuthController> _logger;
        
        public AuthController(
            UserManager<ApplicationUser> userManager,
            SignInManager<ApplicationUser> signInManager,
            IJwtTokenService jwtTokenService,
            ILogger<AuthController> logger)
        {
            _userManager = userManager;
            _signInManager = signInManager;
            _jwtTokenService = jwtTokenService;
            _logger = logger;
        }
        
        [HttpPost("login")]
        public async Task<IActionResult> Login([FromBody] LoginRequest request)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }
            
            try
            {
                var user = await _userManager.FindByEmailAsync(request.Email);
                if (user == null)
                {
                    _logger.LogWarning("Login attempt with non-existent email: {Email}", request.Email);
                    return Unauthorized(new { Message = "Invalid credentials" });
                }
                
                // 檢查帳號是否被鎖定
                if (await _userManager.IsLockedOutAsync(user))
                {
                    _logger.LogWarning("Login attempt for locked account: {UserId}", user.Id);
                    return Unauthorized(new { Message = "Account is locked" });
                }
                
                var result = await _signInManager.CheckPasswordSignInAsync(user, request.Password, true);
                
                if (result.Succeeded)
                {
                    var token = await _jwtTokenService.GenerateTokenAsync(user);
                    
                    _logger.LogInformation("Successful login for user: {UserId}", user.Id);
                    
                    return Ok(new LoginResponse
                    {
                        Token = token,
                        User = new UserInfo
                        {
                            Id = user.Id,
                            Email = user.Email,
                            FirstName = user.FirstName,
                            LastName = user.LastName
                        },
                        ExpiresAt = DateTime.UtcNow.AddMinutes(
                            int.Parse(_configuration["JwtSettings:ExpirationMinutes"]))
                    });
                }
                else if (result.IsLockedOut)
                {
                    _logger.LogWarning("Account locked out for user: {UserId}", user.Id);
                    return Unauthorized(new { Message = "Account is locked due to multiple failed attempts" });
                }
                else
                {
                    _logger.LogWarning("Failed login attempt for user: {UserId}", user.Id);
                    return Unauthorized(new { Message = "Invalid credentials" });
                }
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error during login process");
                return StatusCode(500, new { Message = "An error occurred during login" });
            }
        }
        
        [HttpPost("logout")]
        [Authorize]
        public async Task<IActionResult> Logout()
        {
            try
            {
                var token = HttpContext.Request.Headers["Authorization"]
                    .FirstOrDefault()?.Split(" ").Last();
                
                if (!string.IsNullOrEmpty(token))
                {
                    await _jwtTokenService.RevokeTokenAsync(token);
                }
                
                await _signInManager.SignOutAsync();
                
                _logger.LogInformation("User logged out: {UserId}", User.FindFirst(ClaimTypes.NameIdentifier)?.Value);
                
                return Ok(new { Message = "Logged out successfully" });
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error during logout process");
                return StatusCode(500, new { Message = "An error occurred during logout" });
            }
        }
        
        [HttpPost("refresh")]
        [Authorize]
        public async Task<IActionResult> RefreshToken()
        {
            try
            {
                var userId = User.FindFirst(ClaimTypes.NameIdentifier)?.Value;
                var user = await _userManager.FindByIdAsync(userId);
                
                if (user == null)
                {
                    return Unauthorized(new { Message = "User not found" });
                }
                
                // 撤銷舊Token
                var oldToken = HttpContext.Request.Headers["Authorization"]
                    .FirstOrDefault()?.Split(" ").Last();
                
                if (!string.IsNullOrEmpty(oldToken))
                {
                    await _jwtTokenService.RevokeTokenAsync(oldToken);
                }
                
                // 生成新Token
                var newToken = await _jwtTokenService.GenerateTokenAsync(user);
                
                return Ok(new LoginResponse
                {
                    Token = newToken,
                    User = new UserInfo
                    {
                        Id = user.Id,
                        Email = user.Email,
                        FirstName = user.FirstName,
                        LastName = user.LastName
                    },
                    ExpiresAt = DateTime.UtcNow.AddMinutes(
                        int.Parse(_configuration["JwtSettings:ExpirationMinutes"]))
                });
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error during token refresh");
                return StatusCode(500, new { Message = "An error occurred during token refresh" });
            }
        }
    }
    
    // DTO類別
    public class LoginRequest
    {
        [Required]
        [EmailAddress]
        public string Email { get; set; }
        
        [Required]
        [MinLength(6)]
        public string Password { get; set; }
        
        public bool RememberMe { get; set; }
    }
    
    public class LoginResponse
    {
        public string Token { get; set; }
        public UserInfo User { get; set; }
        public DateTime ExpiresAt { get; set; }
    }
    
    public class UserInfo
    {
        public string Id { get; set; }
        public string Email { get; set; }
        public string FirstName { get; set; }
        public string LastName { get; set; }
    }
}
```

#### 1.2.2 角色和權限管理

```csharp
using Microsoft.AspNetCore.Authorization;

namespace ECommerceApp.Security.Authorization
{
    // 權限常數定義
    public static class Permissions
    {
        public static class Products
        {
            public const string View = "products.view";
            public const string Create = "products.create";
            public const string Edit = "products.edit";
            public const string Delete = "products.delete";
            public const string ManageStock = "products.manage_stock";
        }
        
        public static class Orders
        {
            public const string View = "orders.view";
            public const string ViewAll = "orders.view_all";
            public const string Create = "orders.create";
            public const string Edit = "orders.edit";
            public const string Cancel = "orders.cancel";
            public const string Fulfill = "orders.fulfill";
        }
        
        public static class Users
        {
            public const string View = "users.view";
            public const string Create = "users.create";
            public const string Edit = "users.edit";
            public const string Delete = "users.delete";
            public const string ManageRoles = "users.manage_roles";
        }
        
        public static class Reports
        {
            public const string ViewSales = "reports.view_sales";
            public const string ViewInventory = "reports.view_inventory";
            public const string ViewUsers = "reports.view_users";
            public const string Export = "reports.export";
        }
    }
    
    // 權限需求
    public class PermissionRequirement : IAuthorizationRequirement
    {
        public string Permission { get; }
        
        public PermissionRequirement(string permission)
        {
            Permission = permission;
        }
    }
    
    // 權限授權處理器
    public class PermissionAuthorizationHandler : AuthorizationHandler<PermissionRequirement>
    {
        private readonly IServiceProvider _serviceProvider;
        
        public PermissionAuthorizationHandler(IServiceProvider serviceProvider)
        {
            _serviceProvider = serviceProvider;
        }
        
        protected override async Task HandleRequirementAsync(
            AuthorizationHandlerContext context,
            PermissionRequirement requirement)
        {
            if (context.User?.Identity?.IsAuthenticated != true)
            {
                context.Fail();
                return;
            }
            
            // 檢查使用者是否有特定權限
            if (context.User.HasClaim("permission", requirement.Permission))
            {
                context.Succeed(requirement);
                return;
            }
            
            // 檢查使用者角色是否有此權限
            using var scope = _serviceProvider.CreateScope();
            var userManager = scope.ServiceProvider.GetRequiredService<UserManager<ApplicationUser>>();
            var roleManager = scope.ServiceProvider.GetRequiredService<RoleManager<IdentityRole>>();
            
            var userId = context.User.FindFirst(ClaimTypes.NameIdentifier)?.Value;
            if (string.IsNullOrEmpty(userId))
            {
                context.Fail();
                return;
            }
            
            var user = await userManager.FindByIdAsync(userId);
            if (user == null)
            {
                context.Fail();
                return;
            }
            
            var userRoles = await userManager.GetRolesAsync(user);
            
            foreach (var roleName in userRoles)
            