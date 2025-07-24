# 第五模組：程式開發與實作 (第6週)

**學習時間**：第6週 (6-8小時)  
**先修要求**：完成第四模組 - 系統設計與架構  
**本週重點**：程式開發最佳實務與品質管理

---

## 📋 學習目標

完成本週學習後，您將能夠：

✅ **掌握現代編碼標準與最佳實務**  
✅ **熟練運用Git版本控制系統**  
✅ **建立有效的程式碼審查流程**  
✅ **應用重構技巧改善程式碼品質**  
✅ **配置高效的開發環境與工具鏈**  
✅ **實踐持續整合的開發模式**

---

## 🎯 本週學習路徑

```
編碼標準實務 → Git版本控制 → 程式碼品質 → 重構技巧 → 開發環境 → 協作開發
      ↓             ↓           ↓          ↓          ↓          ↓
   風格規範       版本管理     品質保證    技術改善    工具配置   團隊協作
```

---

## 💻 第一部分：編碼標準與實務

### 1.1 程式碼風格指南

#### 📝 為什麼需要編碼標準？

**統計數據顯示**：
- 程式碼閱讀時間占開發時間的 **70%**
- 一致的編碼風格可提升 **25%** 的開發效率
- 標準化命名可減少 **40%** 的理解時間

**編碼標準的好處**：
- **提高可讀性**：程式碼更容易理解
- **降低維護成本**：減少理解和修改時間
- **促進團隊協作**：統一的開發習慣
- **減少錯誤**：避免常見的編程陷阱

#### 🎨 命名慣例

**變數命名原則**：

**好的命名範例**：
```javascript
// ✅ 清楚表達意圖
const userEmailAddress = 'user@example.com';
const calculateTotalPrice = (items) => { ... };
const isUserLoggedIn = checkAuthStatus();

// ✅ 使用動詞表示動作
function validateUserInput(input) { ... }
function sendNotificationEmail(recipient) { ... }

// ✅ 布林值使用 is/has/can 開頭
const isValidEmail = validateEmail(email);
const hasPermission = checkUserPermission(user);
const canEditProfile = user.role === 'admin';
```

**避免的命名方式**：
```javascript
// ❌ 模糊不清的命名
const data = getUserInfo();
const temp = calculate();
const flag = true;

// ❌ 使用縮寫和數字
const usrNm = 'john';
const calc1 = () => { ... };
const temp2 = process();

// ❌ 誤導性命名
const userList = {}; // 實際是物件不是陣列
const calculatePrice = 100; // 實際是值不是函數
```

**不同語言的命名慣例**：

| 語言 | 變數/函數 | 類別 | 常數 | 範例 |
|------|-----------|------|------|------|
| **JavaScript** | camelCase | PascalCase | UPPER_SNAKE_CASE | `userName`, `UserService`, `MAX_RETRY_COUNT` |
| **Python** | snake_case | PascalCase | UPPER_SNAKE_CASE | `user_name`, `UserService`, `MAX_RETRY_COUNT` |
| **Java** | camelCase | PascalCase | UPPER_SNAKE_CASE | `userName`, `UserService`, `MAX_RETRY_COUNT` |
| **C#** | camelCase | PascalCase | PascalCase | `userName`, `UserService`, `MaxRetryCount` |

#### 📏 程式碼格式規範

**縮排與空白**：
```javascript
// ✅ 一致的縮排（通常使用2或4個空格）
function processUserData(users) {
  const results = [];
  
  for (const user of users) {
    if (user.isActive) {
      const processedUser = {
        id: user.id,
        name: user.name.trim(),
        email: user.email.toLowerCase()
      };
      results.push(processedUser);
    }
  }
  
  return results;
}
```

**行長度限制**：
```javascript
// ✅ 適當的行長度（建議80-120字元）
const userConfiguration = {
  theme: 'dark',
  language: 'zh-TW',
  notifications: true,
  autoSave: false
};

// ✅ 長函數調用的換行
const result = someVeryLongFunctionName(
  firstParameter,
  secondParameter,
  thirdParameter,
  fourthParameter
);
```

**空行使用**：
```javascript
// ✅ 邏輯區塊間使用空行分隔
function UserService() {
  const users = [];

  function addUser(userData) {
    validateUserData(userData);
    
    const newUser = createUser(userData);
    users.push(newUser);
    
    return newUser;
  }

  function removeUser(userId) {
    const index = users.findIndex(user => user.id === userId);
    
    if (index !== -1) {
      return users.splice(index, 1)[0];
    }
    
    return null;
  }

  return { addUser, removeUser };
}
```

### 1.2 註解與文件

#### 💬 有效註解的原則

**好註解的特徵**：
- **解釋為什麼**，而不是做什麼
- **提供上下文**和業務邏輯
- **警告特殊情況**和副作用
- **保持簡潔**且與程式碼同步

**優秀註解範例**：
```javascript
// ✅ 解釋業務邏輯和決策原因
function calculateShippingCost(weight, distance) {
  // 使用階梯式計費，超過5公斤每公斤額外收費
  const baseRate = 50;
  const extraWeightRate = 10;
  const threshold = 5;
  
  // 距離超過100公里需要額外處理費
  const distanceFee = distance > 100 ? 25 : 0;
  
  if (weight <= threshold) {
    return baseRate + distanceFee;
  }
  
  // 計算超重費用
  const extraWeight = weight - threshold;
  return baseRate + (extraWeight * extraWeightRate) + distanceFee;
}

// ✅ 警告特殊行為
function deleteUser(userId) {
  // 警告：此操作會級聯刪除用戶的所有相關資料
  // 包括訂單、評論、收藏等，無法復原
  const user = User.findById(userId);
  
  if (!user) {
    throw new Error('用戶不存在');
  }
  
  // TODO: 考慮實現軟刪除以支援資料恢復
  return User.delete(userId);
}
```

**避免的註解**：
```javascript
// ❌ 重複程式碼內容
let counter = 0; // 設定counter為0
counter++; // counter加1

// ❌ 過時的註解
// 這個函數回傳用戶列表
function calculatePrice() { // 實際上是計算價格
  return price * quantity;
}

// ❌ 無意義的註解
const users = []; // 用戶陣列
```

#### 📚 文件化最佳實務

**JSDoc範例**：
```javascript
/**
 * 計算商品的折扣價格
 * @param {number} originalPrice - 原始價格
 * @param {number} discountPercentage - 折扣百分比 (0-100)
 * @param {boolean} [isMember=false] - 是否為會員
 * @returns {number} 折扣後的價格
 * @throws {Error} 當價格為負數或折扣百分比無效時
 * @example
 * // 計算會員八折價格
 * const finalPrice = calculateDiscountPrice(100, 20, true);
 * console.log(finalPrice); // 80
 */
function calculateDiscountPrice(originalPrice, discountPercentage, isMember = false) {
  if (originalPrice < 0) {
    throw new Error('價格不能為負數');
  }
  
  if (discountPercentage < 0 || discountPercentage > 100) {
    throw new Error('折扣百分比必須在0-100之間');
  }
  
  let discount = discountPercentage / 100;
  
  // 會員額外享有5%折扣
  if (isMember) {
    discount += 0.05;
    discount = Math.min(discount, 0.9); // 最高九折
  }
  
  return originalPrice * (1 - discount);
}
```

**README文件結構**：
```markdown
# 專案名稱

## 專案描述
簡短描述專案的目的和功能

## 安裝說明
```bash
npm install
npm start
```

## 使用方法
基本使用範例和API說明

## 開發指南
- 編碼規範
- 提交規範
- 測試要求

## 授權資訊
MIT License
```

---

## 🌿 第二部分：Git版本控制

### 2.1 Git基礎概念

#### 🔄 Git工作流程

**Git三個區域**：
```
工作目錄 (Working Directory)
    ↓ git add
暫存區 (Staging Area)
    ↓ git commit
本地倉庫 (Local Repository)
    ↓ git push
遠端倉庫 (Remote Repository)
```

**基本Git命令**：
```bash
# 初始化倉庫
git init
git clone <repository-url>

# 查看狀態
git status
git log --oneline
git diff

# 暫存和提交
git add <file>
git add .
git commit -m "提交訊息"

# 分支操作
git branch
git branch <branch-name>
git checkout <branch-name>
git checkout -b <new-branch>

# 合併和推送
git merge <branch-name>
git push origin <branch-name>
git pull origin <branch-name>
```

### 2.2 分支策略

#### 🌳 Git Flow模型

**主要分支**：
```
master/main ──────●────●────●─── (生產版本)
                  │    │    │
develop ─────●────●────●────●─── (開發主線)
             │    │    │
feature ────●────●    │    │    (功能開發)
                 │    │    │
release ─────────●────●    │    (發布準備)
                      │    │
hotfix ──────────────●────●    (緊急修復)
```

**分支說明**：
- **master/main**：穩定的生產版本
- **develop**：開發主分支，整合各種功能
- **feature**：功能開發分支
- **release**：發布準備分支
- **hotfix**：緊急修復分支

#### 🚀 GitHub Flow模型

**簡化流程**：
```
main ──●─────●─────●─────●─── (穩定主線)
       │     │     │     │
feature ●─────●     │     │    (功能分支)
             │     │     │
feature ───────●───●     │    (另一個功能)
                   │     │
feature ─────────────●───●    (第三個功能)
```

**GitHub Flow步驟**：
1. 從main分支建立功能分支
2. 開發功能並提交
3. 建立Pull Request
4. 代碼審查
5. 合併到main分支
6. 部署到生產環境

### 2.3 提交規範

#### 📝 Conventional Commits

**提交訊息格式**：
```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

**提交類型**：
- **feat**: 新功能
- **fix**: 錯誤修復
- **docs**: 文件更新
- **style**: 程式碼格式調整
- **refactor**: 重構
- **test**: 測試相關
- **chore**: 建置工具、輔助工具等

**範例提交訊息**：
```bash
# 新功能
git commit -m "feat(auth): 新增社群登入功能"

# 錯誤修復
git commit -m "fix(cart): 修復購物車數量計算錯誤"

# 文件更新
git commit -m "docs: 更新API文件範例"

# 重構
git commit -m "refactor(user): 重構使用者驗證邏輯"

# 破壞性變更
git commit -m "feat!: 變更API回應格式

BREAKING CHANGE: API回應格式從陣列改為物件"
```

#### 🏷️ 版本標籤

**語意化版本控制 (Semantic Versioning)**：
```
主版本.次版本.修訂版本
MAJOR.MINOR.PATCH

範例：v2.1.3
```

**版本號規則**：
- **MAJOR**: 不相容的API變更
- **MINOR**: 向後相容的新功能
- **PATCH**: 向後相容的錯誤修復

**Git標籤操作**：
```bash
# 建立標籤
git tag v1.0.0
git tag -a v1.0.0 -m "發布版本 1.0.0"

# 推送標籤
git push origin v1.0.0
git push origin --tags

# 查看標籤
git tag -l
git show v1.0.0
```

### 2.4 協作開發流程

#### 🤝 Pull Request流程

**完整PR流程**：
```
1. Fork/Clone 倉庫
   ↓
2. 建立功能分支
   ↓
3. 開發功能
   ↓
4. 提交變更
   ↓
5. 推送到遠端
   ↓
6. 建立Pull Request
   ↓
7. 代碼審查
   ↓
8. 修正問題
   ↓
9. 合併到主分支
   ↓
10. 刪除功能分支
```

**PR描述模板**：
```markdown
## 變更摘要
簡述此次變更的內容和目的

## 變更類型
- [ ] 新功能
- [ ] 錯誤修復
- [ ] 重構
- [ ] 文件更新
- [ ] 效能改善

## 測試
描述如何測試這些變更

## 檢查清單
- [ ] 程式碼遵循專案風格指南
- [ ] 已進行自我審查
- [ ] 已新增必要的測試
- [ ] 測試全部通過
- [ ] 已更新相關文件

## 相關問題
關閉 #123
```

#### 🔍 程式碼審查指南

**審查重點**：

**功能正確性**：
- 程式碼邏輯是否正確
- 是否處理邊界情況
- 錯誤處理是否完整

**程式碼品質**：
- 命名是否清楚
- 結構是否合理
- 是否遵循SOLID原則

**效能考量**：
- 是否有明顯的效能問題
- 資料庫查詢是否最佳化
- 記憶體使用是否合理

**安全性**：
- 輸入驗證是否完整
- 是否有SQL注入風險
- 敏感資料是否適當處理

**審查評論範例**：
```markdown
# ✅ 建設性的評論
這個函數看起來邏輯正確，建議考慮：
1. 新增輸入參數驗證
2. 將魔術數字提取為常數
3. 考慮快取查詢結果以提升效能

# ❌ 不良的評論
這段程式碼很醜。

# ✅ 具體的建議
```javascript
// 建議將這個條件提取為一個函數
if (user.age >= 18 && user.hasValidId && user.isVerified) {
  // 可以改為
  if (isEligibleUser(user)) {
```
```

---

## 🔧 第三部分：程式碼品質管理

### 3.1 程式碼品質指標

#### 📊 品質評估維度

**代碼度量指標**：
- **圈複雜度 (Cyclomatic Complexity)**：衡量程式複雜度
- **程式碼覆蓋率 (Code Coverage)**：測試覆蓋的程式碼比例
- **重複程式碼 (Code Duplication)**：重複代碼的百分比
- **技術債務 (Technical Debt)**：需要重構的程式碼量

**品質門檻範例**：
```yaml
# SonarQube品質門檻設定
quality_gates:
  coverage: ">= 80%"           # 測試覆蓋率至少80%
  duplicated_lines: "< 3%"     # 重複程式碼少於3%
  maintainability_rating: "A"  # 可維護性評級A
  reliability_rating: "A"      # 可靠性評級A
  security_rating: "A"         # 安全性評級A
  cyclomatic_complexity: "< 10" # 圈複雜度小於10
```

#### 🛠️ 靜態分析工具

**JavaScript/TypeScript工具**：
```json
// ESLint設定檔
{
  "extends": [
    "eslint:recommended",
    "@typescript-eslint/recommended",
    "prettier"
  ],
  "rules": {
    "no-console": "warn",
    "no-unused-vars": "error",
    "prefer-const": "error",
    "no-var": "error",
    "@typescript-eslint/no-explicit-any": "error"
  }
}
```

**Python工具**：
```ini
# flake8設定
[flake8]
max-line-length = 88
max-complexity = 10
ignore = E203, W503
exclude = migrations, __pycache__, .git
```

**Java工具**：
```xml
<!-- Checkstyle設定 -->
<module name="Checker">
  <module name="TreeWalker">
    <module name="NeedBraces"/>
    <module name="EmptyBlock"/>
    <module name="MethodLength">
      <property name="max" value="50"/>
    </module>
  </module>
</module>
```

### 3.2 重構技巧

#### 🔄 重構的定義與原則

**重構定義**：在不改變程式外部行為的前提下，改善程式內部結構的過程。

**重構的時機**：
- **三次法則**：第三次寫類似程式碼時就該重構
- **新增功能前**：讓新功能更容易添加
- **修復錯誤時**：理解程式碼結構
- **代碼審查時**：發現改善機會

#### 🛠️ 常見重構技法

**提取方法 (Extract Method)**：
```javascript
// ❌ 重構前：方法過長
function printOrderDetails(order) {
  console.log('訂單詳情：');
  console.log(`訂單號：${order.id}`);
  console.log(`客戶：${order.customer.name}`);
  console.log(`電話：${order.customer.phone}`);
  
  let total = 0;
  for (const item of order.items) {
    const subtotal = item.price * item.quantity;
    total += subtotal;
    console.log(`${item.name} x ${item.quantity} = $${subtotal}`);
  }
  
  console.log(`總金額：$${total}`);
}

// ✅ 重構後：提取子方法
function printOrderDetails(order) {
  printOrderHeader(order);
  const total = printOrderItems(order.items);
  printOrderTotal(total);
}

function printOrderHeader(order) {
  console.log('訂單詳情：');
  console.log(`訂單號：${order.id}`);
  console.log(`客戶：${order.customer.name}`);
  console.log(`電話：${order.customer.phone}`);
}

function printOrderItems(items) {
  let total = 0;
  for (const item of items) {
    const subtotal = item.price * item.quantity;
    total += subtotal;
    console.log(`${item.name} x ${item.quantity} = $${subtotal}`);
  }
  return total;
}

function printOrderTotal(total) {
  console.log(`總金額：$${total}`);
}
```

**提取變數 (Extract Variable)**：
```javascript
// ❌ 重構前：複雜表達式
function calculateShippingDiscount(order) {
  return order.totalAmount > 1000 && 
         order.customer.membershipLevel === 'premium' && 
         order.shippingAddress.city === 'Taipei' ? 
         order.shippingCost * 0.5 : 0;
}

// ✅ 重構後：提取有意義的變數
function calculateShippingDiscount(order) {
  const isHighValueOrder = order.totalAmount > 1000;
  const isPremiumMember = order.customer.membershipLevel === 'premium';
  const isTaipeiDelivery = order.shippingAddress.city === 'Taipei';
  
  const eligibleForDiscount = isHighValueOrder && 
                             isPremiumMember && 
                             isTaipeiDelivery;
  
  return eligibleForDiscount ? order.shippingCost * 0.5 : 0;
}
```

**替換魔術數字 (Replace Magic Numbers)**：
```javascript
// ❌ 重構前：魔術數字
function calculateMembershipDiscount(totalAmount, membershipYears) {
  if (membershipYears >= 5) {
    return totalAmount * 0.15;
  } else if (membershipYears >= 2) {
    return totalAmount * 0.1;
  } else {
    return totalAmount * 0.05;
  }
}

// ✅ 重構後：使用常數
const MEMBERSHIP_DISCOUNT = {
  SENIOR: 0.15,     // 5年以上會員
  REGULAR: 0.1,     // 2-4年會員
  NEW: 0.05         // 新會員
};

const MEMBERSHIP_THRESHOLDS = {
  SENIOR_YEARS: 5,
  REGULAR_YEARS: 2
};

function calculateMembershipDiscount(totalAmount, membershipYears) {
  if (membershipYears >= MEMBERSHIP_THRESHOLDS.SENIOR_YEARS) {
    return totalAmount * MEMBERSHIP_DISCOUNT.SENIOR;
  } else if (membershipYears >= MEMBERSHIP_THRESHOLDS.REGULAR_YEARS) {
    return totalAmount * MEMBERSHIP_DISCOUNT.REGULAR;
  } else {
    return totalAmount * MEMBERSHIP_DISCOUNT.NEW;
  }
}
```

**消除重複代碼 (Remove Duplication)**：
```javascript
// ❌ 重構前：重複的驗證邏輯
function validateEmail(email) {
  if (!email) return false;
  if (typeof email !== 'string') return false;
  if (email.length < 5) return false;
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  return emailRegex.test(email);
}

function validatePhone(phone) {
  if (!phone) return false;
  if (typeof phone !== 'string') return false;
  if (phone.length < 8) return false;
  const phoneRegex = /^\d{8,15}$/;
  return phoneRegex.test(phone);
}

// ✅ 重構後：提取共同邏輯
function validateInput(input, minLength, regex) {
  if (!input) return false;
  if (typeof input !== 'string') return false;
  if (input.length < minLength) return false;
  return regex.test(input);
}

function validateEmail(email) {
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  return validateInput(email, 5, emailRegex);
}

function validatePhone(phone) {
  const phoneRegex = /^\d{8,15}$/;
  return validateInput(phone, 8, phoneRegex);
}
```

### 3.3 測試驅動重構

#### 🔴🟢🔄 紅-綠-重構循環

**TDD重構流程**：
```
1. 🔴 紅燈：寫一個失敗的測試
   ↓
2. 🟢 綠燈：寫最少的程式碼讓測試通過
   ↓
3. 🔄 重構：改善程式碼結構但保持測試通過
   ↓
回到步驟1
```

**重構前的測試保護**：
```javascript
// 重構前先建立測試保護網
describe('OrderCalculator', () => {
  test('計算基本訂單總額', () => {
    const order = {
      items: [
        { price: 100, quantity: 2 },
        { price: 50, quantity: 1 }
      ]
    };
    
    expect(calculateOrderTotal(order)).toBe(250);
  });
  
  test('計算含折扣的訂單總額', () => {
    const order = {
      items: [{ price: 100, quantity: 3 }],
      discount: 0.1
    };
    
    expect(calculateOrderTotal(order)).toBe(270);
  });
  
  test('處理空訂單', () => {
    const order = { items: [] };
    
    expect(calculateOrderTotal(order)).toBe(0);
  });
});

// 有了測試保護後安全重構
function calculateOrderTotal(order) {
  // 重構過程中測試確保行為不變
  const subtotal = calculateSubtotal(order.items);
  const discount = calculateDiscount(subtotal, order.discount);
  return subtotal - discount;
}
```

---

## 🛠️ 第四部分：開發環境管理

### 4.1 IDE配置與優化

#### ⚙️ Visual Studio Code設定

**工作區設定檔 (.vscode/settings.json)**：
```json
{
  "editor.formatOnSave": true,
  "editor.codeActionsOnSave": {
    "source.fixAll.eslint": true,
    "source.organizeImports": true
  },
  "editor.rulers": [80, 120],
  "editor.tabSize": 2,
  "editor.insertSpaces": true,
  "files.trimTrailingWhitespace": true,
  "files.insertFinalNewline": true,
  "typescript.preferences.importModuleSpecifier": "relative",
  "eslint.validate": [
    "javascript",
    "typescript",
    "javascriptreact",
    "typescriptreact"
  ]
}
```

**推薦擴充套件**：
```json
// .vscode/extensions.json
{
  "recommendations": [
    "esbenp.prettier-vscode",        // 程式碼格式化
    "dbaeumer.vscode-eslint",        // JavaScript linting
    "ms-vscode.vscode-typescript-next", // TypeScript支援
    "bradlc.vscode-tailwindcss",     // Tailwind CSS
    "ms-python.python",              // Python支援
    "redhat.vscode-yaml",            // YAML支援
    "ms-vscode.vscode-json",         // JSON支援
    "eamodio.gitlens",              // Git增強功能
    "ms-vscode-remote.remote-containers" // 容器開發
  ]
}
```

#### 🎨 程式碼格式化

**Prettier設定檔 (.prettierrc)**：
```json
{
  "semi": true,
  "trailingComma": "es5",
  "singleQuote": true,
  "printWidth": 80,
  "tabWidth": 2,
  "useTabs": false,
  "bracketSpacing": true,
  "arrowParens": "avoid",
  "endOfLine": "lf"
}
```

**EditorConfig設定檔 (.editorconfig)**：
```ini
root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true
indent_style = space
indent_size = 2

[*.md]
trim_trailing_whitespace = false

[*.{yml,yaml}]
indent_size = 2
```

### 4.2 建置工具配置

#### 📦 Node.js專案配置

**package.json scripts**：
```json
{
  "scripts": {
    "dev": "next dev",
    "build": "next build",
    "start": "next start",
    "lint": "eslint . --ext .ts,.tsx,.js,.jsx",
    "lint:fix": "eslint . --ext .ts,.tsx,.js,.jsx --fix",
    "format": "prettier --write .",
    "format:check": "prettier --check .",
    "type-check": "tsc --noEmit",
    "test": "jest",
    "test:watch": "jest --watch",
    "test:coverage": "jest --coverage",
    "pre-commit": "lint-staged",
    "prepare": "husky install"
  }
}
```

**Git Hooks配置 (husky)**：
```json
// .husky/pre-commit
#!/usr/bin/env sh
. "$(dirname -- "$0")/_/husky.sh"

npm run pre-commit
```

**lint-staged設定**：
```json
// package.json
{
  "lint-staged": {
    "*.{js,jsx,ts,tsx}": [
      "eslint --fix",
      "prettier --write"
    ],
    "*.{json,md,yml,yaml}": [
      "prettier --write"
    ]
  }
}
```

#### 🐳 Docker開發環境

**Dockerfile**：
```dockerfile
# 開發環境
FROM node:18-alpine AS development

WORKDIR /app

# 複製package檔案
COPY package*.json ./
RUN npm ci

# 複製源代碼
COPY . .

# 開發模式啟動
CMD ["npm", "run", "dev"]

# 生產環境
FROM node:18-alpine AS production

WORKDIR /app

# 複製package檔案並安裝依賴
COPY package*.json ./
RUN npm ci --only=production

# 複製建置後的程式碼
COPY --from=build /app/.next ./.next
COPY --from=build /app/public ./public

# 生產模式啟動
CMD ["npm", "start"]
```

**docker-compose.yml**：
```yaml
version: '3.8'

services:
  app:
    build:
      context: .
      target: development
    ports:
      - "3000:3000"
    volumes:
      - .:/app
      - /app/node_modules
    environment:
      - NODE_ENV=development
    depends_on:
      - db
      - redis

  db:
    image: postgres:14
    environment:
      POSTGRES_DB: myapp
      POSTGRES_USER: developer
      POSTGRES_PASSWORD: password
    ports:
      - "5432:5432"
    volumes:
      - postgres_data:/var/lib/postgresql/data

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"

volumes:
  postgres_data:
```

### 4.3 環境變數管理

#### 🔐 環境配置策略

**環境分離原則**：
```
.env.example      # 範例檔案，提交到版本控制
.env.local        # 本地開發設定，不提交
.env.development  # 開發環境設定
.env.staging      # 測試環境設定
.env.production   # 生產環境設定
```

**.env.example**：
```bash
# 資料庫設定
DATABASE_URL="postgresql://username:password@localhost:5432/database"

# API金鑰
STRIPE_PUBLIC_KEY="pk_test_..."
STRIPE_SECRET_KEY="sk_test_..."

# 第三方服務
SENDGRID_API_KEY="SG...."
REDIS_URL="redis://localhost:6379"

# 應用程式設定
APP_URL="http://localhost:3000"
JWT_SECRET="your-secret-key"
```

**環境驗證**：
```javascript
// config/env.js
const requiredEnvVars = [
  'DATABASE_URL',
  'JWT_SECRET',
  'STRIPE_SECRET_KEY'
];

function validateEnvironment() {
  const missing = requiredEnvVars.filter(
    envVar => !process.env[envVar]
  );
  
  if (missing.length > 0) {
    throw new Error(
      `缺少必要的環境變數: ${missing.join(', ')}`
    );
  }
}

// 應用程式啟動時驗證
validateEnvironment();

module.exports = {
  database: {
    url: process.env.DATABASE_URL
  },
  jwt: {
    secret: process.env.JWT_SECRET,
    expiresIn: process.env.JWT_EXPIRES_IN || '7d'
  },
  stripe: {
    publicKey: process.env.STRIPE_PUBLIC_KEY,
    secretKey: process.env.STRIPE_SECRET_KEY
  }
};
```

---

## 🤝 第五部分：團隊協作與CI/CD

### 5.1 持續整合流程

#### 🔄 GitHub Actions工作流程

**.github/workflows/ci.yml**：
```yaml
name: CI/CD Pipeline

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest
    
    strategy:
      matrix:
        node-version: [16.x, 18.x]
    
    steps:
    - uses: actions/checkout@v3
    
    - name: 設定 Node.js
      uses: actions/setup-node@v3
      with:
        node-version: ${{ matrix.node-version }}
        cache: 'npm'
    
    - name: 安裝依賴
      run: npm ci
    
    - name: 執行 linting
      run: npm run lint
    
    - name: 執行類型檢查
      run: npm run type-check
    
    - name: 執行測試
      run: npm run test:coverage
    
    - name: 上傳測試覆蓋率
      uses: codecov/codecov-action@v3
      with:
        file: ./coverage/lcov.info

  build:
    needs: test
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: 設定 Node.js
      uses: actions/setup-node@v3
      with:
        node-version: 18.x
        cache: 'npm'
    
    - name: 安裝依賴
      run: npm ci
    
    - name: 建置專案
      run: npm run build
    
    - name: 建置 Docker 映像
      if: github.ref == 'refs/heads/main'
      run: |
        docker build -t myapp:${{ github.sha }} .
        docker tag myapp:${{ github.sha }} myapp:latest

  deploy:
    needs: build
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    
    steps:
    - name: 部署到生產環境
      run: |
        echo "部署到生產環境"
        # 實際部署指令
```

#### 🛡️ 品質門檻檢查

**SonarCloud整合**：
```yaml
- name: SonarCloud Scan
  uses: SonarSource/sonarcloud-github-action@master
  env:
    GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
    SONAR_TOKEN: ${{ secrets.SONAR_TOKEN }}
  with:
    args: >
      -Dsonar.projectKey=my-project
      -Dsonar.organization=my-org
      -Dsonar.javascript.lcov.reportPaths=coverage/lcov.info
      -Dsonar.exclusions=**/*.test.js,**/*.spec.js
```

**品質門檻設定 (sonar-project.properties)**：
```properties
sonar.projectKey=my-project
sonar.organization=my-org

# 來源檔案
sonar.sources=src
sonar.tests=src
sonar.test.inclusions=**/*.test.js,**/*.spec.js
sonar.exclusions=**/*.test.js,**/*.spec.js

# 測試覆蓋率
sonar.javascript.lcov.reportPaths=coverage/lcov.info

# 品質門檻
sonar.qualitygate.wait=true
```

### 5.2 協作開發最佳實務

#### 📋 團隊開發規範

**開發工作流程**：
```
1. 接收任務 (Jira/GitHub Issues)
   ↓
2. 建立功能分支 (feature/JIRA-123-add-user-auth)
   ↓
3. 開發功能 (TDD + 小步提交)
   ↓
4. 執行本地測試 (npm test)
   ↓
5. 推送分支 (git push origin feature/...)
   ↓
6. 建立Pull Request
   ↓
7. 自動化檢查 (CI pipeline)
   ↓
8. 代碼審查 (至少1人核准)
   ↓
9. 合併到主分支
   ↓
10. 刪除功能分支
    ↓
11. 部署到測試/生產環境
```

**程式碼審查檢查清單**：
```markdown
## 功能性
- [ ] 功能符合需求規格
- [ ] 處理邊界情況
- [ ] 錯誤處理完整
- [ ] 測試案例充足

## 程式碼品質
- [ ] 命名清楚有意義
- [ ] 函數長度適中 (< 20行)
- [ ] 複雜度合理 (< 10)
- [ ] 沒有重複程式碼

## 效能與安全
- [ ] 沒有明顯效能問題
- [ ] 輸入驗證完整
- [ ] 敏感資料保護
- [ ] SQL注入防護

## 維護性
- [ ] 程式碼容易理解
- [ ] 註解適當
- [ ] 結構清晰
- [ ] 遵循團隊規範
```

#### 🔄 持續部署策略

**部署環境規劃**：
```
開發環境 (Development)
├── 分支: develop
├── 自動部署: 每次推送
├── 用途: 開發者整合測試
└── URL: dev.myapp.com

測試環境 (Staging)
├── 分支: release/*
├── 自動部署: 發布準備
├── 用途: QA測試、使用者驗收
└── URL: staging.myapp.com

生產環境 (Production)
├── 分支: main
├── 手動部署: 審核後觸發
├── 用途: 正式服務
└── URL: myapp.com
```

**部署腳本範例**：
```bash
#!/bin/bash
# deploy.sh

set -e

ENVIRONMENT=$1
BRANCH=$2

if [ "$ENVIRONMENT" = "production" ]; then
    echo "部署到生產環境..."
    
    # 檢查分支
    if [ "$BRANCH" != "main" ]; then
        echo "錯誤：生產環境只能從main分支部署"
        exit 1
    fi
    
    # 備份當前版本
    kubectl create backup production-backup-$(date +%Y%m%d%H%M%S)
    
    # 執行部署
    kubectl apply -f k8s/production/
    
    # 健康檢查
    kubectl rollout status deployment/myapp-production
    
    echo "生產環境部署完成"
else
    echo "部署到 $ENVIRONMENT 環境..."
    kubectl apply -f k8s/$ENVIRONMENT/
    kubectl rollout status deployment/myapp-$ENVIRONMENT
    echo "$ENVIRONMENT 環境部署完成"
fi
```

---

## 🏃‍♂️ 實務練習

### 練習1：建立團隊編碼規範

**專案背景**：
為一個5人開發團隊建立完整的編碼規範和開發流程。

**任務1.1：編碼標準制定**
建立包含以下內容的編碼標準文件：

1. **命名慣例**
   - 變數、函數、類別命名規則
   - 檔案和資料夾命名規則
   - 常數和枚舉命名規則

2. **程式碼格式**
   - 縮排和空白規則
   - 行長度限制
   - 程式碼結構組織

3. **註解規範**
   - 函數文件化要求
   - 內聯註解指導原則
   - API文件撰寫標準

**任務1.2：工具配置**
設定開發工具鏈：
```json
// 建立 .eslintrc.json
{
  "extends": ["eslint:recommended"],
  "rules": {
    // 自定義規則
  }
}

// 建立 .prettierrc
{
  // 格式化規則
}

// 建立 .editorconfig
// 編輯器配置
```

**提交成果**：
- 完整的編碼規範文件
- 工具配置檔案
- 範例程式碼展示規範應用

### 練習2：Git工作流程實踐

**情境設定**：
模擬團隊協作開發一個「待辦事項管理系統」的新功能。

**任務2.1：分支策略實作**
1. **建立專案倉庫**
   ```bash
   git init todo-app
   cd todo-app
   
   # 建立初始結構
   mkdir src tests docs
   touch README.md package.json
   
   # 初始提交
   git add .
   git commit -m "feat: 初始化專案結構"
   ```

2. **實作Git Flow**
   ```bash
   # 建立develop分支
   git checkout -b develop
   
   # 建立功能分支
   git checkout -b feature/user-authentication
   
   # 開發功能（模擬多次提交）
   # 建立發布分支
   git checkout -b release/v1.0.0
   ```

**任務2.2：Pull Request流程**
1. 建立功能分支並開發功能
2. 撰寫符合規範的提交訊息
3. 建立Pull Request描述
4. 模擬程式碼審查流程

**任務2.3：衝突解決**
模擬團隊成員同時修改相同檔案的情況：
1. 建立衝突情境
2. 解決合併衝突
3. 記錄解決過程

**提交成果**：
- Git倉庫歷史記錄
- PR描述範本
- 衝突解決文件

### 練習3：程式碼重構實踐

**提供一段需要重構的程式碼**：
```javascript
// 需要重構的程式碼
function processOrder(order) {
  let total = 0;
  for (let i = 0; i < order.items.length; i++) {
    if (order.items[i].type === 'product') {
      total += order.items[i].price * order.items[i].qty;
      if (order.items[i].price > 100) {
        total *= 0.9; // 10% 折扣
      }
    } else if (order.items[i].type === 'service') {
      total += order.items[i].price;
      if (order.customer.level === 'premium') {
        total *= 0.8; // 20% 折扣
      }
    }
  }
  
  if (order.customer.age > 65) {
    total *= 0.95; // 敬老折扣
  }
  
  if (total > 1000) {
    total += 50; // 運費
  } else {
    total += 100;
  }
  
  return total;
}
```

**任務3.1：識別問題**
分析上述程式碼的問題：
- 函數職責過多
- 魔術數字
- 複雜的條件邏輯
- 重複的折扣計算

**任務3.2：重構實作**
應用重構技法改善程式碼：
1. 提取方法
2. 替換魔術數字
3. 簡化條件表達式
4. 消除重複邏輯

**任務3.3：測試保護**
為重構前後的程式碼編寫測試：
```javascript
describe('Order Processing', () => {
  test('計算基本商品訂單', () => {
    // 測試案例
  });
  
  test('應用折扣規則', () => {
    // 測試案例
  });
  
  // 更多測試案例...
});
```

**提交成果**：
- 重構前後程式碼對比
- 重構步驟說明
- 完整測試套件

### 練習4：CI/CD流程建立

**任務4.1：GitHub Actions設定**
為專案建立完整的CI/CD流程：

```yaml
# .github/workflows/main.yml
name: CI/CD Pipeline

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  # 測試工作
  test:
    # 設定測試流程
  
  # 建置工作
  build:
    # 設定建置流程
  
  # 部署工作
  deploy:
    # 設定部署流程
```

**任務4.2：品質門檻設定**
整合程式碼品質檢查工具：
- ESLint 靜態分析
- 測試覆蓋率檢查
- 安全性掃描
- 依賴漏洞檢查

**任務4.3：自動化部署**
設計多環境部署策略：
- 開發環境自動部署
- 測試環境條件部署
- 生產環境手動核准

**提交成果**：
- 完整的GitHub Actions配置
- 品質門檻設定文件
- 部署腳本和文件

---

## 📝 知識檢核測驗

### 選擇題 (每題5分，共50分)

**1. 下列哪個不是好的變數命名原則？**
A) 使用有意義的名稱
B) 避免使用縮寫
C) 使用單字母變數名
D) 保持一致的命名風格

**2. Git中的工作區、暫存區、倉庫的正確順序是？**
A) 倉庫 → 暫存區 → 工作區
B) 工作區 → 倉庫 → 暫存區
C) 工作區 → 暫存區 → 倉庫
D) 暫存區 → 工作區 → 倉庫

**3. Conventional Commits中，"feat"表示什麼？**
A) 錯誤修復
B) 新功能
C) 文件更新
D) 重構

**4. 程式碼審查主要不關注哪個方面？**
A) 功能正確性
B) 程式碼品質
C) 開發者薪資
D) 安全性

**5. 重構的定義是？**
A) 增加新功能
B) 修復錯誤
C) 在不改變外部行為下改善內部結構
D) 提升程式效能

**6. Git Flow中的主要分支不包括？**
A) master/main
B) develop
C) feature
D) test

**7. 下列哪個不是常見的重構技法？**
A) 提取方法
B) 替換魔術數字
C) 增加功能
D) 消除重複代碼

**8. ESLint主要用途是？**
A) 程式碼格式化
B) 靜態程式碼分析
C) 版本控制
D) 測試執行

**9. CI/CD中的CI指的是？**
A) Code Integration
B) Continuous Integration
C) Component Interface
D) Control Interface

**10. Semantic Versioning的格式是？**
A) 年.月.日
B) 主版本.次版本.修訂版
C) 版本.建置.修訂
D) 大版本.小版本.補丁

### 簡答題 (每題25分，共50分)

**1. 說明為什麼程式碼審查對軟體開發團隊很重要？請列舉至少4個具體好處並說明如何建立有效的程式碼審查流程。(300字)**

**2. 比較Git Flow和GitHub Flow兩種分支策略的差異，並分析在什麼情況下會選擇哪種策略？請說明各自的優缺點。(300字)**

---

## 📚 延伸學習資源

### 必讀書籍
1. **《Clean Code》** - Robert C. Martin
2. **《重構：改善既有程式的設計》** - Martin Fowler
3. **《Pro Git》** - Scott Chacon & Ben Straub
4. **《持續整合》** - Paul Duvall
5. **《軟體工藝》** - Pete McBreen

### 線上課程
1. **Git & GitHub Mastery** - Git版本控制完整教學
2. **Clean Code Principles** - 乾淨程式碼實踐
3. **DevOps Engineering** - CI/CD流程建立
4. **Code Review Best Practices** - 程式碼審查技巧

### 實用工具
1. **版本控制**：
   - Git - 分散式版本控制
   - GitHub/GitLab - 協作平台
   - Sourcetree - Git GUI工具

2. **程式碼品質**：
   - ESLint - JavaScript靜態分析
   - SonarQube - 程式碼品質平台
   - CodeClimate - 自動化程式碼審查

3. **CI/CD工具**：
   - GitHub Actions - GitHub整合CI/CD
   - Jenkins - 開源自動化伺服器
   - GitLab CI - GitLab整合CI/CD

4. **開發環境**：
   - Docker - 容器化平台
   - VS Code - 現代程式碼編輯器
   - Postman - API開發測試

### 程式碼品質資源
1. **風格指南**：
   - Airbnb JavaScript Style Guide
   - Google Style Guides
   - PEP 8 (Python)

2. **最佳實務**：
   - 12 Factor App - 現代應用程式設計原則
   - SOLID Principles - 物件導向設計原則
   - Design Patterns - 設計模式

---

## ✅ 學習檢核表

- [ ] 理解編碼標準的重要性和制定原則
- [ ] 掌握有效的命名慣例和格式規範
- [ ] 熟練使用Git版本控制系統
- [ ] 了解不同Git工作流程的應用場景
- [ ] 掌握程式碼審查的流程和技巧
- [ ] 能夠識別並應用重構技法
- [ ] 理解持續整合的概念和實踐
- [ ] 會配置開發環境和工具鏈
- [ ] 了解團隊協作的最佳實務
- [ ] 能夠建立CI/CD流程
- [ ] 完成所有實務練習
- [ ] 通過知識檢核測驗 (70分以上)

---

## 🔮 下週預告

**第六模組：軟體測試 (第7週)**

我們將學習：
- 軟體測試的基本原理與策略
- 測試案例設計技術與方法
- 單元測試、整合測試、系統測試
- 自動化測試工具與框架
- 測試驅動開發 (TDD) 實踐
- 缺陷管理與測試報告

**實務重點**：
- 設計完整的測試計畫
- 實作自動化測試套件
- 建立測試驅動開發流程
- 進行測試覆蓋率分析

---

## 🌟 學習反思

請花15分鐘思考以下問題：

1. **實務應用**：哪些編碼實務對您的開發工作最有幫助？
2. **工具掌握**：您最想深入學習哪些開發工具？為什麼？
3. **團隊協作**：如何在實際團隊中推廣這些最佳實務？
4. **持續改善**：如何建立個人的技能提升計畫？
5. **品質意識**：程式碼品質對專案成功有什麼影響？

---

**🎉 恭喜完成第五模組！**  
*您現在已經掌握了現代軟體開發的核心實務技能，能夠編寫高品質的程式碼並有效進行團隊協作。準備好進入軟體測試的專業領域了！*