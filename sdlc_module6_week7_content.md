# 第六模組：軟體測試 (第7週)

**學習時間**：第7週 (6-8小時)  
**先修要求**：完成第五模組 - 程式開發與實作  
**本週重點**：軟體測試理論與實務完整體系

---

## 📋 學習目標

完成本週學習後，您將能夠：

✅ **掌握軟體測試的基本原理與策略**  
✅ **設計有效的測試案例與測試計畫**  
✅ **實施不同層級的測試（單元、整合、系統）**  
✅ **建立自動化測試框架與流程**  
✅ **實踐測試驅動開發（TDD）方法**  
✅ **管理缺陷並撰寫測試報告**

---

## 🎯 本週學習路徑

```
測試基礎理論 → 測試設計技術 → 測試層級實務 → 自動化測試 → TDD實踐 → 缺陷管理
      ↓             ↓            ↓           ↓          ↓         ↓
   概念建立       技術方法      分層測試     工具應用   開發方法   品質控制
```

---

## 🔬 第一部分：軟體測試基礎

### 1.1 軟體測試概論

#### 📝 測試的定義與目的

**軟體測試定義**：
軟體測試是一個系統化的過程，用來評估和驗證軟體應用程式或系統是否符合指定需求，並識別實際結果與預期結果之間的差異。

**測試的主要目的**：
- **發現缺陷**：找出軟體中的錯誤和問題
- **驗證功能**：確認軟體符合需求規格
- **確保品質**：提升軟體的整體品質
- **降低風險**：減少生產環境中的問題
- **建立信心**：對軟體品質的信心

#### 📊 測試的重要性統計

**業界數據顯示**：
- 軟體缺陷修復成本隨時間**指數增長**
- 生產環境修復成本是開發階段的 **100倍**
- 有效測試可以減少 **90%** 的嚴重缺陷
- 自動化測試可以提升 **60%** 的測試效率

**測試成本效益分析**：
```
測試階段      發現成本    修復成本    ROI
需求階段        1x         1x      100x
設計階段        3x         5x       33x
編碼階段        5x        10x       20x
測試階段       10x        20x       10x
生產階段      100x       200x        2x
```

### 1.2 測試基本原則

#### 🎯 軟體測試七大原則

**原則1：測試顯示缺陷的存在**
- 測試只能證明缺陷存在，無法證明軟體完全正確
- 測試的目標是找到盡可能多的缺陷

**原則2：完全測試是不可能的**
- 無法測試所有可能的輸入組合
- 需要基於風險和優先級進行測試

**原則3：早期測試**
- 越早開始測試，發現和修復缺陷的成本越低
- 測試活動應該在SDLC早期就開始

**原則4：缺陷集群**
- 80%的問題通常來自20%的模組
- 重點關注高風險區域

**原則5：殺蟲劑悖論**
- 重複執行相同測試會失去發現新缺陷的能力
- 需要定期更新和改進測試案例

**原則6：測試依賴於背景**
- 不同類型的軟體需要不同的測試方法
- 測試策略應該根據軟體特性調整

**原則7：無錯誤謬論**
- 即使軟體無缺陷，也可能無法滿足使用者需求
- 測試必須考慮業務需求和使用者期望

### 1.3 測試分類

#### 📈 按測試層級分類

**單元測試 (Unit Testing)**
- **範圍**：個別模組或函數
- **執行者**：開發人員
- **工具**：JUnit, Jest, pytest
- **特點**：快速、獨立、可重複

**整合測試 (Integration Testing)**
- **範圍**：模組間的介面和互動
- **類型**：大爆炸、增量式、三明治
- **重點**：資料流、介面一致性

**系統測試 (System Testing)**
- **範圍**：完整系統的端到端測試
- **環境**：接近生產環境
- **驗證**：系統需求和使用案例

**驗收測試 (Acceptance Testing)**
- **範圍**：業務需求驗證
- **執行者**：使用者或業務代表
- **標準**：驗收標準和使用者需求

#### 🔍 按測試方法分類

**黑箱測試 (Black Box Testing)**
```
輸入 → [軟體系統] → 輸出
      (內部結構未知)
```

**特點**：
- 基於需求和規格
- 不考慮內部實作
- 使用者角度測試

**白箱測試 (White Box Testing)**
```
輸入 → [已知內部結構] → 輸出
      (路徑、條件、迴圈)
```

**特點**：
- 基於程式碼結構
- 考慮執行路徑
- 開發者角度測試

**灰箱測試 (Gray Box Testing)**
- 結合黑箱和白箱方法
- 部分了解內部結構
- 更全面的測試覆蓋

#### ⚡ 按執行方式分類

**手動測試 vs 自動化測試**

| 特性 | 手動測試 | 自動化測試 |
|------|----------|------------|
| **執行速度** | 慢 | 快 |
| **重複性** | 低 | 高 |
| **初期成本** | 低 | 高 |
| **長期成本** | 高 | 低 |
| **人為錯誤** | 可能 | 極少 |
| **探索性測試** | 優秀 | 有限 |
| **維護成本** | 低 | 中等 |

**自動化適用情境**：
- 重複執行的測試
- 回歸測試
- 負載和效能測試
- 資料驅動測試

**手動測試適用情境**：
- 探索性測試
- 可用性測試
- 臨時測試
- 初期測試設計

---

## 🎨 第二部分：測試設計技術

### 2.1 黑箱測試技術

#### 🔢 等價類劃分

**基本概念**：
將輸入域劃分為若干個等價類，每個等價類中的值對於測試目的而言是等價的。

**等價類劃分步驟**：
1. 識別輸入條件
2. 劃分有效等價類
3. 劃分無效等價類
4. 設計測試案例

**範例：年齡驗證系統**
```
需求：年齡必須在18-65歲之間

有效等價類：
- EC1: 18 ≤ 年齡 ≤ 65

無效等價類：
- EC2: 年齡 < 18
- EC3: 年齡 > 65
- EC4: 非數字輸入
- EC5: 空值

測試案例設計：
TC1: 年齡 = 30 (EC1, 有效)
TC2: 年齡 = 15 (EC2, 無效)
TC3: 年齡 = 70 (EC3, 無效)  
TC4: 年齡 = "abc" (EC4, 無效)
TC5: 年齡 = null (EC5, 無效)
```

#### 📏 邊界值分析

**基本概念**：
在等價類的邊界附近設計測試案例，因為錯誤經常發生在輸入域的邊界上。

**邊界值選擇原則**：
- 邊界值本身
- 邊界值的上一個值
- 邊界值的下一個值

**範例：密碼長度驗證**
```
需求：密碼長度必須在8-20個字元之間

邊界值測試案例：
TC1: 長度 = 7 (邊界下方)
TC2: 長度 = 8 (最小邊界)
TC3: 長度 = 9 (邊界上方)
TC4: 長度 = 19 (邊界下方)
TC5: 長度 = 20 (最大邊界)
TC6: 長度 = 21 (邊界上方)
```

#### 📊 決策表測試

**基本概念**：
用表格形式表示複雜業務規則的所有可能組合，確保測試覆蓋所有規則。

**決策表組成**：
- **條件部分**：輸入條件
- **動作部分**：期望結果
- **規則**：條件和動作的組合

**範例：折扣計算系統**
```
業務規則：
- 會員享有10%折扣
- 訂單金額超過1000元享有5%折扣
- 兩種折扣可以累加

決策表：
┌─────────────┬───────────────────────────┐
│   條件      │ R1 │ R2 │ R3 │ R4 │ R5 │ R6 │
├─────────────┼────┼────┼────┼────┼────┼────┤
│ 是會員      │ Y  │ Y  │ Y  │ N  │ N  │ N  │
│ 金額>1000   │ Y  │ N  │ -  │ Y  │ N  │ -  │
│ 金額有效    │ Y  │ Y  │ N  │ Y  │ Y  │ N  │
├─────────────┼────┼────┼────┼────┼────┼────┤
│ 會員折扣    │ 10%│ 10%│ 0% │ 0% │ 0% │ 0% │
│ 金額折扣    │ 5% │ 0% │ 0% │ 5% │ 0% │ 0% │
│ 總折扣      │ 15%│ 10%│錯誤│ 5% │ 0% │錯誤│
└─────────────┴────┴────┴────┴────┴────┴────┘

測試案例：
TC1: 會員，金額1500 → 15%折扣
TC2: 會員，金額800 → 10%折扣
TC3: 會員，金額-100 → 錯誤訊息
TC4: 非會員，金額1200 → 5%折扣
TC5: 非會員，金額500 → 0%折扣
TC6: 非會員，金額-50 → 錯誤訊息
```

### 2.2 白箱測試技術

#### 🛤️ 控制流程測試

**語句覆蓋 (Statement Coverage)**
```javascript
function calculateDiscount(amount, isMember) {
  let discount = 0;                    // S1
  
  if (amount > 1000) {                 // S2
    discount += 0.05;                  // S3
  }
  
  if (isMember) {                      // S4
    discount += 0.1;                   // S5
  }
  
  return amount * (1 - discount);      // S6
}

// 語句覆蓋測試案例
TC1: calculateDiscount(1500, true)
// 執行路徑: S1 → S2 → S3 → S4 → S5 → S6
// 覆蓋: 100% 語句覆蓋
```

**分支覆蓋 (Branch Coverage)**
```javascript
// 分支覆蓋測試案例
TC1: calculateDiscount(1500, true)   // T, T
TC2: calculateDiscount(800, false)   // F, F
// 覆蓋: 100% 分支覆蓋

// 完整分支覆蓋需要4個測試案例
TC1: calculateDiscount(1500, true)   // T, T
TC2: calculateDiscount(1500, false)  // T, F  
TC3: calculateDiscount(800, true)    // F, T
TC4: calculateDiscount(800, false)   // F, F
```

**路徑覆蓋 (Path Coverage)**
```javascript
function processOrder(order) {
  if (order.amount > 0) {              // 條件A
    if (order.items.length > 0) {      // 條件B
      return "valid";                  // 路徑1: A=T, B=T
    } else {
      return "no items";               // 路徑2: A=T, B=F
    }
  } else {
    return "invalid amount";           // 路徑3: A=F
  }
}

// 路徑覆蓋測試案例
TC1: { amount: 100, items: [1, 2] }   // 路徑1
TC2: { amount: 100, items: [] }       // 路徑2
TC3: { amount: -50, items: [1] }      // 路徑3
```

#### 🔍 資料流程測試

**定義-使用測試**：
- **定義 (Definition)**：變數被賦值的位置
- **使用 (Use)**：變數被讀取的位置
- **定義-使用路徑**：從定義到使用的執行路徑

```javascript
function calculateTax(income, deductions) {
  let taxableIncome = income - deductions;  // D1: taxableIncome定義
  let tax = 0;                              // D2: tax定義
  
  if (taxableIncome > 50000) {              // U1: taxableIncome使用
    tax = taxableIncome * 0.3;              // D3: tax重新定義, U2: taxableIncome使用
  } else if (taxableIncome > 20000) {       // U3: taxableIncome使用
    tax = taxableIncome * 0.2;              // D4: tax重新定義, U4: taxableIncome使用
  } else {
    tax = taxableIncome * 0.1;              // D5: tax重新定義, U5: taxableIncome使用
  }
  
  return tax;                               // U6: tax使用
}

// 定義-使用路徑測試
TC1: (60000, 5000) → D1-U1-U2-U6 路徑
TC2: (30000, 5000) → D1-U1-U3-U4-U6 路徑  
TC3: (15000, 2000) → D1-U1-U3-U5-U6 路徑
```

### 2.3 測試案例設計

#### 📋 測試案例模板

**標準測試案例格式**：
```
測試案例編號: TC_001
測試案例名稱: 使用者登入_有效憑證
所屬模組: 使用者認證
優先級: 高
前置條件: 
  - 使用者已註冊
  - 系統可正常訪問
測試步驟:
  1. 開啟登入頁面
  2. 輸入有效的email: test@example.com
  3. 輸入正確密碼: password123
  4. 點擊登入按鈕
預期結果:
  - 顯示登入成功訊息
  - 重導向到主頁面
  - 顯示使用者名稱
測試資料: 
  - Email: test@example.com
  - Password: password123
後置條件:
  - 使用者處於登入狀態
設計者: 張測試
設計日期: 2024-03-15
```

#### 🎯 測試案例優先級

**優先級分類標準**：

**P0 - 阻斷級**：
- 系統無法啟動
- 核心功能完全失效
- 資料遺失或損壞

**P1 - 嚴重級**：
- 主要功能異常
- 效能嚴重衰退
- 安全漏洞

**P2 - 一般級**：
- 次要功能問題
- 介面顯示異常
- 輕微效能問題

**P3 - 輕微級**：
- 文字錯誤
- 格式問題
- 建議改善

**測試執行順序**：
```
1. P0測試案例 (必須100%通過)
   ↓
2. P1測試案例 (必須95%以上通過)
   ↓
3. P2測試案例 (建議90%以上通過)
   ↓
4. P3測試案例 (時間允許時執行)
```

---

## 🏗️ 第三部分：測試層級實務

### 3.1 單元測試

#### 🔧 單元測試基礎

**單元測試特性**：
- **快速**：毫秒級執行
- **獨立**：不依賴外部系統
- **可重複**：結果一致
- **自動化**：可自動執行

**測試結構 (AAA模式)**：
```javascript
// Arrange - Act - Assert 模式
test('計算總價應該包含稅金', () => {
  // Arrange: 準備測試資料
  const price = 100;
  const taxRate = 0.1;
  
  // Act: 執行被測試的方法
  const total = calculateTotalPrice(price, taxRate);
  
  // Assert: 驗證結果
  expect(total).toBe(110);
});
```

#### 🧪 Jest測試框架實務

**基本測試範例**：
```javascript
// math.js - 被測試的模組
function add(a, b) {
  return a + b;
}

function divide(a, b) {
  if (b === 0) {
    throw new Error('除數不能為零');
  }
  return a / b;
}

module.exports = { add, divide };

// math.test.js - 測試檔案
const { add, divide } = require('./math');

describe('數學運算模組', () => {
  describe('加法函數', () => {
    test('應該正確計算兩個正數的和', () => {
      expect(add(2, 3)).toBe(5);
    });
    
    test('應該正確處理負數', () => {
      expect(add(-1, 1)).toBe(0);
    });
    
    test('應該正確處理小數', () => {
      expect(add(0.1, 0.2)).toBeCloseTo(0.3);
    });
  });
  
  describe('除法函數', () => {
    test('應該正確計算除法', () => {
      expect(divide(10, 2)).toBe(5);
    });
    
    test('除數為零時應該拋出錯誤', () => {
      expect(() => {
        divide(10, 0);
      }).toThrow('除數不能為零');
    });
  });
});
```

#### 🎭 Mock與Stub技術

**Mock物件使用**：
```javascript
// userService.js
class UserService {
  constructor(database) {
    this.db = database;
  }
  
  async getUserById(id) {
    const user = await this.db.findUser(id);
    if (!user) {
      throw new Error('使用者不存在');
    }
    return user;
  }
  
  async createUser(userData) {
    if (!userData.email) {
      throw new Error('Email為必填欄位');
    }
    
    const existingUser = await this.db.findUserByEmail(userData.email);
    if (existingUser) {
      throw new Error('Email已被使用');
    }
    
    return await this.db.saveUser(userData);
  }
}

// userService.test.js
const UserService = require('./userService');

describe('UserService', () => {
  let userService;
  let mockDatabase;
  
  beforeEach(() => {
    // 建立Mock資料庫
    mockDatabase = {
      findUser: jest.fn(),
      findUserByEmail: jest.fn(),
      saveUser: jest.fn()
    };
    
    userService = new UserService(mockDatabase);
  });
  
  describe('getUserById', () => {
    test('應該返回找到的使用者', async () => {
      // Arrange
      const userId = 1;
      const expectedUser = { id: 1, name: 'John', email: 'john@example.com' };
      mockDatabase.findUser.mockResolvedValue(expectedUser);
      
      // Act
      const result = await userService.getUserById(userId);
      
      // Assert
      expect(result).toEqual(expectedUser);
      expect(mockDatabase.findUser).toHaveBeenCalledWith(userId);
    });
    
    test('使用者不存在時應該拋出錯誤', async () => {
      // Arrange
      const userId = 999;
      mockDatabase.findUser.mockResolvedValue(null);
      
      // Act & Assert
      await expect(userService.getUserById(userId))
        .rejects.toThrow('使用者不存在');
    });
  });
  
  describe('createUser', () => {
    test('應該成功建立新使用者', async () => {
      // Arrange
      const userData = { name: 'Jane', email: 'jane@example.com' };
      const savedUser = { id: 2, ...userData };
      
      mockDatabase.findUserByEmail.mockResolvedValue(null);
      mockDatabase.saveUser.mockResolvedValue(savedUser);
      
      // Act
      const result = await userService.createUser(userData);
      
      // Assert
      expect(result).toEqual(savedUser);
      expect(mockDatabase.findUserByEmail).toHaveBeenCalledWith(userData.email);
      expect(mockDatabase.saveUser).toHaveBeenCalledWith(userData);
    });
    
    test('Email已存在時應該拋出錯誤', async () => {
      // Arrange
      const userData = { name: 'Jane', email: 'existing@example.com' };
      const existingUser = { id: 1, email: userData.email };
      
      mockDatabase.findUserByEmail.mockResolvedValue(existingUser);
      
      // Act & Assert
      await expect(userService.createUser(userData))
        .rejects.toThrow('Email已被使用');
      
      expect(mockDatabase.saveUser).not.toHaveBeenCalled();
    });
  });
});
```

### 3.2 整合測試

#### 🔗 整合測試策略

**大爆炸整合 (Big Bang Integration)**：
```
所有模組 → 一次性整合 → 測試整個系統
```
- **優點**：簡單直接
- **缺點**：難以定位問題、測試時間晚

**增量式整合**：

**由上而下整合 (Top-Down)**：
```
主模組
├── 模組A (測試 A + 主模組)
│   ├── 模組A1 (測試 A1 + A + 主模組)
│   └── 模組A2
└── 模組B
    ├── 模組B1
    └── 模組B2
```

**由下而上整合 (Bottom-Up)**：
```
模組A1 + 模組A2 → 模組A
模組B1 + 模組B2 → 模組B
模組A + 模組B → 主模組
```

**三明治整合 (Sandwich)**：
- 結合由上而下和由下而上方法
- 先測試上層和下層，最後測試中間層

#### 🧪 API整合測試

**Express.js API測試範例**：
```javascript
// app.js
const express = require('express');
const mongoose = require('mongoose');
const userRoutes = require('./routes/users');

const app = express();
app.use(express.json());
app.use('/api/users', userRoutes);

module.exports = app;

// routes/users.js
const express = require('express');
const User = require('../models/User');
const router = express.Router();

router.post('/', async (req, res) => {
  try {
    const user = new User(req.body);
    await user.save();
    res.status(201).json(user);
  } catch (error) {
    res.status(400).json({ error: error.message });
  }
});

router.get('/:id', async (req, res) => {
  try {
    const user = await User.findById(req.params.id);
    if (!user) {
      return res.status(404).json({ error: '使用者不存在' });
    }
    res.json(user);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

module.exports = router;

// tests/integration/users.test.js
const request = require('supertest');
const mongoose = require('mongoose');
const app = require('../../app');
const User = require('../../models/User');

describe('/api/users', () => {
  beforeAll(async () => {
    // 連接測試資料庫
    await mongoose.connect(process.env.TEST_DATABASE_URL);
  });
  
  beforeEach(async () => {
    // 清理測試資料
    await User.deleteMany({});
  });
  
  afterAll(async () => {
    // 關閉資料庫連接
    await mongoose.connection.close();
  });
  
  describe('POST /api/users', () => {
    test('應該成功建立新使用者', async () => {
      const userData = {
        name: 'John Doe',
        email: 'john@example.com',
        age: 25
      };
      
      const response = await request(app)
        .post('/api/users')
        .send(userData)
        .expect(201);
      
      expect(response.body).toMatchObject(userData);
      expect(response.body).toHaveProperty('_id');
      
      // 驗證資料庫中確實建立了使用者
      const savedUser = await User.findById(response.body._id);
      expect(savedUser).toBeTruthy();
      expect(savedUser.email).toBe(userData.email);
    });
    
    test('無效資料應該返回400錯誤', async () => {
      const invalidUserData = {
        name: 'John Doe'
        // 缺少必填的email欄位
      };
      
      const response = await request(app)
        .post('/api/users')
        .send(invalidUserData)
        .expect(400);
      
      expect(response.body).toHaveProperty('error');
    });
  });
  
  describe('GET /api/users/:id', () => {
    test('應該返回存在的使用者', async () => {
      // 先建立一個使用者
      const user = new User({
        name: 'Jane Doe',
        email: 'jane@example.com',
        age: 30
      });
      await user.save();
      
      const response = await request(app)
        .get(`/api/users/${user._id}`)
        .expect(200);
      
      expect(response.body.name).toBe(user.name);
      expect(response.body.email).toBe(user.email);
    });
    
    test('不存在的使用者應該返回404', async () => {
      const nonExistentId = new mongoose.Types.ObjectId();
      
      const response = await request(app)
        .get(`/api/users/${nonExistentId}`)
        .expect(404);
      
      expect(response.body.error).toBe('使用者不存在');
    });
  });
});
```

### 3.3 系統測試

#### 🌐 端到端測試

**Cypress E2E測試範例**：
```javascript
// cypress/e2e/user-registration.cy.js
describe('使用者註冊流程', () => {
  beforeEach(() => {
    // 訪問註冊頁面
    cy.visit('/register');
  });
  
  it('應該成功註冊新使用者', () => {
    // 填寫註冊表單
    cy.get('[data-testid="name-input"]').type('John Doe');
    cy.get('[data-testid="email-input"]').type('john@example.com');
    cy.get('[data-testid="password-input"]').type('password123');
    cy.get('[data-testid="confirm-password-input"]').type('password123');
    
    // 提交表單
    cy.get('[data-testid="submit-button"]').click();
    
    // 驗證成功註冊
    cy.url().should('include', '/welcome');
    cy.get('[data-testid="welcome-message"]')
      .should('contain', '歡迎 John Doe');
    
    // 驗證確認郵件已發送
    cy.get('[data-testid="email-sent-message"]')
      .should('be.visible')
      .and('contain', 'john@example.com');
  });
  
  it('應該驗證必填欄位', () => {
    // 嘗試提交空表單
    cy.get('[data-testid="submit-button"]').click();
    
    // 驗證錯誤訊息
    cy.get('[data-testid="name-error"]')
      .should('be.visible')
      .and('contain', '姓名為必填欄位');
    
    cy.get('[data-testid="email-error"]')
      .should('be.visible')
      .and('contain', 'Email為必填欄位');
  });
  
  it('應該驗證密碼一致性', () => {
    cy.get('[data-testid="name-input"]').type('John Doe');
    cy.get('[data-testid="email-input"]').type('john@example.com');
    cy.get('[data-testid="password-input"]').type('password123');
    cy.get('[data-testid="confirm-password-input"]').type('differentpassword');
    
    cy.get('[data-testid="submit-button"]').click();
    
    cy.get('[data-testid="password-match-error"]')
      .should('be.visible')
      .and('contain', '密碼不一致');
  });
  
  it('應該處理重複email錯誤', () => {
    // 假設john@example.com已經存在
    cy.get('[data-testid="name-input"]').type('Another John');
    cy.get('[data-testid="email-input"]').type('john@example.com');
    cy.get('[data-testid="password-input"]').type('password123');
    cy.get('[data-testid="confirm-password-input"]').type('password123');
    
    cy.get('[data-testid="submit-button"]').click();
    
    cy.get('[data-testid="error-message"]')
      .should('be.visible')
      .and('contain', 'Email已被使用');
  });
});

// cypress/e2e/shopping-cart.cy.js
describe('購物車功能', () => {
  beforeEach(() => {
    // 登入使用者
    cy.login('testuser@example.com', 'password123');
    cy.visit('/products');
  });
  
  it('應該能加入商品到購物車', () => {
    // 選擇第一個商品
    cy.get('[data-testid="product-card"]').first().within(() => {
      cy.get('[data-testid="product-name"]').should('be.visible');
      cy.get('[data-testid="add-to-cart-button"]').click();
    });
    
    // 驗證購物車圖示更新
    cy.get('[data-testid="cart-badge"]').should('contain', '1');
    
    // 前往購物車頁面
    cy.get('[data-testid="cart-icon"]').click();
    cy.url().should('include', '/cart');
    
    // 驗證商品在購物車中
    cy.get('[data-testid="cart-item"]').should('have.length', 1);
  });
  
  it('完整的購買流程', () => {
    // 加入多個商品
    cy.get('[data-testid="product-card"]').each(($product, index) => {
      if (index < 2) { // 加入前兩個商品
        cy.wrap($product).within(() => {
          cy.get('[data-testid="add-to-cart-button"]').click();
        });
        cy.wait(500); // 等待動畫完成
      }
    });
    
    // 前往購物車
    cy.get('[data-testid="cart-icon"]').click();
    
    // 驗證商品數量
    cy.get('[data-testid="cart-item"]').should('have.length', 2);
    
    // 前往結帳
    cy.get('[data-testid="checkout-button"]').click();
    
    // 填寫配送資訊
    cy.get('[data-testid="address-input"]').type('台北市信義區信義路五段7號');
    cy.get('[data-testid="phone-input"]').type('0912345678');
    
    // 選擇付款方式
    cy.get('[data-testid="payment-method-credit"]').check();
    
    // 確認訂單
    cy.get('[data-testid="place-order-button"]').click();
    
    // 驗證訂單成功
    cy.url().should('include', '/order-confirmation');
    cy.get('[data-testid="order-number"]').should('be.visible');
    cy.get('[data-testid="success-message"]')
      .should('contain', '訂單建立成功');
  });
});
```

---

## 🤖 第四部分：自動化測試

### 4.1 自動化測試策略

#### 🏗️ 測試金字塔

```
        /\
       /  \
      / UI \     (少量，慢，昂貴)
     /______\
    /        \
   /   API    \   (中等數量，中等速度)
  /____________\
 /              \
/  Unit Tests    \ (大量，快，便宜)
/__________________\
```

**各層級測試比例建議**：
- **單元測試**: 70%
- **整合測試**: 20%
- **E2E測試**: 10%

**投資回報率分析**：
```
測試類型    開發成本  維護成本  執行速度  回饋速度  故障定位
單元測試      低       低       快       快       精確
整合測試      中       中       中       中       一般
E2E測試      高       高       慢       慢       模糊
```

### 4.2 測試自動化工具

#### 🛠️ JavaScript測試工具生態

**測試框架比較**：

| 工具 | 特色 | 適用場景 | 學習曲線 |
|------|------|----------|----------|
| **Jest** | 零配置、內建Mock | React、Node.js | 容易 |
| **Mocha** | 靈活、可配置 | 任何JS專案 | 中等 |
| **Jasmine** | 行為驅動開發 | 傳統專案 | 容易 |
| **Cypress** | 現代E2E測試 | 前端應用 | 中等 |
| **Playwright** | 跨瀏覽器測試 | 複雜Web應用 | 中等 |

#### 🔧 Jest高階技巧

**測試配置檔案 (jest.config.js)**：
```javascript
module.exports = {
  // 測試環境
  testEnvironment: 'node',
  
  // 測試檔案模式
  testMatch: [
    '**/__tests__/**/*.js',
    '**/?(*.)+(spec|test).js'
  ],
  
  // 覆蓋率報告
  collectCoverage: true,
  collectCoverageFrom: [
    'src/**/*.js',
    '!src/**/*.test.js',
    '!src/index.js'
  ],
  coverageThreshold: {
    global: {
      branches: 80,
      functions: 80,
      lines: 80,
      statements: 80
    }
  },
  
  // 模組路徑對應
  moduleNameMapping: {
    '^@/(.*)$': '<rootDir>/src/$1'
  },
  
  // 設定檔案
  setupFilesAfterEnv: ['<rootDir>/tests/setup.js'],
  
  // 測試超時
  testTimeout: 10000,
  
  // 平行執行
  maxWorkers: '50%'
};
```

**進階Mock技巧**：
```javascript
// tests/setup.js
// 全域Mock設定
global.fetch = jest.fn();

// 模組Mock
jest.mock('../src/config/database', () => ({
  connect: jest.fn(),
  disconnect: jest.fn(),
  query: jest.fn()
}));

// 時間Mock
beforeEach(() => {
  jest.useFakeTimers();
  jest.setSystemTime(new Date('2024-01-01'));
});

afterEach(() => {
  jest.useRealTimers();
});

// userService.test.js
const UserService = require('../src/services/UserService');
const emailService = require('../src/services/EmailService');

// 部分Mock
jest.mock('../src/services/EmailService', () => ({
  sendWelcomeEmail: jest.fn(),
  sendPasswordResetEmail: jest.fn()
}));

describe('UserService 高階測試', () => {
  let userService;
  
  beforeEach(() => {
    userService = new UserService();
    jest.clearAllMocks();
  });
  
  test('應該在使用者註冊後發送歡迎郵件', async () => {
    // Arrange
    const userData = { email: 'test@example.com', name: 'Test User' };
    emailService.sendWelcomeEmail.mockResolvedValue(true);
    
    // Act
    await userService.registerUser(userData);
    
    // Assert
    expect(emailService.sendWelcomeEmail).toHaveBeenCalledWith(
      userData.email,
      userData.name
    );
    expect(emailService.sendWelcomeEmail).toHaveBeenCalledTimes(1);
  });
  
  test('應該重試失敗的操作', async () => {
    // Arrange
    const userData = { email: 'test@example.com' };
    emailService.sendWelcomeEmail
      .mockRejectedValueOnce(new Error('網路錯誤'))
      .mockRejectedValueOnce(new Error('伺服器錯誤'))
      .mockResolvedValue(true);
    
    // Act
    const result = await userService.registerUser(userData);
    
    // Assert
    expect(emailService.sendWelcomeEmail).toHaveBeenCalledTimes(3);
    expect(result).toBe(true);
  });
  
  test('應該正確處理定時任務', async () => {
    // Arrange
    const callback = jest.fn();
    
    // Act
    userService.scheduleCleanup(callback, 5000);
    
    // 快進時間
    jest.advanceTimersByTime(5000);
    
    // Assert
    expect(callback).toHaveBeenCalled();
  });
});
```

### 4.3 持續測試

#### 🔄 CI/CD中的測試整合

**GitHub Actions測試流程**：
```yaml
# .github/workflows/test.yml
name: 測試流程

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
        node-version: [16.x, 18.x, 20.x]
    
    services:
      postgres:
        image: postgres:13
        env:
          POSTGRES_PASSWORD: postgres
          POSTGRES_DB: test_db
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
    
    steps:
    - name: Checkout程式碼
      uses: actions/checkout@v3
    
    - name: 設定Node.js ${{ matrix.node-version }}
      uses: actions/setup-node@v3
      with:
        node-version: ${{ matrix.node-version }}
        cache: 'npm'
    
    - name: 安裝依賴
      run: npm ci
    
    - name: 執行程式碼檢查
      run: npm run lint
    
    - name: 執行單元測試
      run: npm run test:unit
      env:
        NODE_ENV: test
    
    - name: 執行整合測試
      run: npm run test:integration
      env:
        NODE_ENV: test
        DATABASE_URL: postgresql://postgres:postgres@localhost:5432/test_db
    
    - name: 執行E2E測試
      run: npm run test:e2e
      env:
        NODE_ENV: test
    
    - name: 產生測試報告
      run: npm run test:coverage
    
    - name: 上傳覆蓋率報告
      uses: codecov/codecov-action@v3
      with:
        file: ./coverage/lcov.info
        fail_ci_if_error: true
    
    - name: 發送測試結果通知
      if: failure()
      uses: 8398a7/action-slack@v3
      with:
        status: failure
        text: '測試失敗: ${{ github.ref }}'
      env:
        SLACK_WEBHOOK_URL: ${{ secrets.SLACK_WEBHOOK }}
```

**測試環境管理**：
```javascript
// tests/helpers/testDatabase.js
const { Pool } = require('pg');

class TestDatabase {
  constructor() {
    this.pool = new Pool({
      connectionString: process.env.TEST_DATABASE_URL
    });
  }
  
  async setup() {
    // 建立測試資料庫結構
    await this.pool.query(`
      CREATE TABLE IF NOT EXISTS users (
        id SERIAL PRIMARY KEY,
        email VARCHAR(255) UNIQUE NOT NULL,
        name VARCHAR(255) NOT NULL,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    `);
  }
  
  async cleanup() {
    // 清理測試資料
    await this.pool.query('TRUNCATE TABLE users RESTART IDENTITY CASCADE');
  }
  
  async teardown() {
    // 關閉連接
    await this.pool.end();
  }
  
  async seedData() {
    // 建立測試資料
    const users = [
      { email: 'user1@test.com', name: 'Test User 1' },
      { email: 'user2@test.com', name: 'Test User 2' }
    ];
    
    for (const user of users) {
      await this.pool.query(
        'INSERT INTO users (email, name) VALUES ($1, $2)',
        [user.email, user.name]
      );
    }
  }
}

module.exports = TestDatabase;

// tests/setup.js
const TestDatabase = require('./helpers/testDatabase');

let testDb;

beforeAll(async () => {
  testDb = new TestDatabase();
  await testDb.setup();
});

beforeEach(async () => {
  await testDb.cleanup();
  await testDb.seedData();
});

afterAll(async () => {
  await testDb.teardown();
});

global.testDb = testDb;
```

---

## 🔴🟢🔄 第五部分：測試驅動開發 (TDD)

### 5.1 TDD基本概念

#### 🎯 TDD週期

**紅-綠-重構循環**：
```
1. 🔴 紅燈: 寫一個失敗的測試
   ↓
2. 🟢 綠燈: 寫最少的程式碼讓測試通過
   ↓
3. 🔄 重構: 改善程式碼品質
   ↓
回到步驟1
```

**TDD的好處**：
- **設計驅動**：先思考介面設計
- **需求澄清**：測試即規格文件
- **品質保證**：高測試覆蓋率
- **重構安全**：測試提供安全網
- **快速回饋**：立即知道程式是否正確

### 5.2 TDD實戰範例

#### 💳 需求：信用卡驗證器

**第1輪：基本結構**
```javascript
// 步驟1: 🔴 寫失敗的測試
describe('CreditCardValidator', () => {
  test('應該存在', () => {
    expect(CreditCardValidator).toBeDefined();
  });
});

// 測試結果: ❌ ReferenceError: CreditCardValidator is not defined

// 步驟2: 🟢 最少程式碼讓測試通過
class CreditCardValidator {
}

module.exports = CreditCardValidator;

// 測試結果: ✅ 通過

// 步驟3: 🔄 重構 (目前沒什麼可重構的)
```

**第2輪：驗證信用卡號碼長度**
```javascript
// 步驟1: 🔴 寫失敗的測試
test('應該拒絕空的信用卡號碼', () => {
  const validator = new CreditCardValidator();
  expect(validator.isValid('')).toBe(false);
});

// 測試結果: ❌ TypeError: validator.isValid is not a function

// 步驟2: 🟢 最少程式碼讓測試通過
class CreditCardValidator {
  isValid(cardNumber) {
    return false; // 最簡單的實作
  }
}

// 測試結果: ✅ 通過

// 步驟3: 🔄 重構 (加入更明確的邏輯)
class CreditCardValidator {
  isValid(cardNumber) {
    if (!cardNumber || cardNumber.length === 0) {
      return false;
    }
    return false; // 暫時返回false
  }
}
```

**第3輪：驗證數字格式**
```javascript
// 步驟1: 🔴 寫失敗的測試
test('應該拒絕非數字字元', () => {
  const validator = new CreditCardValidator();
  expect(validator.isValid('abcd')).toBe(false);
  expect(validator.isValid('1234-5678')).toBe(false);
});

// 測試結果: ✅ 通過 (因為目前都返回false)

test('應該接受有效的信用卡號碼', () => {
  const validator = new CreditCardValidator();
  expect(validator.isValid('4111111111111111')).toBe(true);
});

// 測試結果: ❌ 期望true但得到false

// 步驟2: 🟢 最少程式碼讓測試通過
class CreditCardValidator {
  isValid(cardNumber) {
    if (!cardNumber || cardNumber.length === 0) {
      return false;
    }
    
    // 檢查是否只包含數字
    if (!/^\d+$/.test(cardNumber)) {
      return false;
    }
    
    // 檢查長度 (簡化版)
    if (cardNumber.length >= 13 && cardNumber.length <= 19) {
      return true;
    }
    
    return false;
  }
}

// 測試結果: ✅ 通過
```

**第4輪：Luhn演算法驗證**
```javascript
// 步驟1: 🔴 寫失敗的測試
test('應該使用Luhn演算法驗證', () => {
  const validator = new CreditCardValidator();
  expect(validator.isValid('4111111111111111')).toBe(true);  // 有效
  expect(validator.isValid('4111111111111112')).toBe(false); // 無效
});

// 測試結果: ❌ 第二個測試失敗

// 步驟2: 🟢 實作Luhn演算法
class CreditCardValidator {
  isValid(cardNumber) {
    if (!cardNumber || cardNumber.length === 0) {
      return false;
    }
    
    if (!/^\d+$/.test(cardNumber)) {
      return false;
    }
    
    if (cardNumber.length < 13 || cardNumber.length > 19) {
      return false;
    }
    
    return this.luhnCheck(cardNumber);
  }
  
  luhnCheck(cardNumber) {
    let sum = 0;
    let isEven = false;
    
    // 從右到左處理每一位數字
    for (let i = cardNumber.length - 1; i >= 0; i--) {
      let digit = parseInt(cardNumber[i]);
      
      if (isEven) {
        digit *= 2;
        if (digit > 9) {
          digit -= 9;
        }
      }
      
      sum += digit;
      isEven = !isEven;
    }
    
    return sum % 10 === 0;
  }
}

// 測試結果: ✅ 通過

// 步驟3: 🔄 重構
class CreditCardValidator {
  isValid(cardNumber) {
    return this.isNotEmpty(cardNumber) &&
           this.isNumeric(cardNumber) &&
           this.hasValidLength(cardNumber) &&
           this.luhnCheck(cardNumber);
  }
  
  isNotEmpty(cardNumber) {
    return cardNumber && cardNumber.length > 0;
  }
  
  isNumeric(cardNumber) {
    return /^\d+$/.test(cardNumber);
  }
  
  hasValidLength(cardNumber) {
    return cardNumber.length >= 13 && cardNumber.length <= 19;
  }
  
  luhnCheck(cardNumber) {
    let sum = 0;
    let isEven = false;
    
    for (let i = cardNumber.length - 1; i >= 0; i--) {
      let digit = parseInt(cardNumber[i]);
      
      if (isEven) {
        digit *= 2;
        if (digit > 9) {
          digit -= 9;
        }
      }
      
      sum += digit;
      isEven = !isEven;
    }
    
    return sum % 10 === 0;
  }
}
```

### 5.3 TDD最佳實務

#### 📏 測試案例設計原則

**FIRST原則**：
- **Fast (快速)**：測試應該快速執行
- **Independent (獨立)**：測試間不應相互依賴
- **Repeatable (可重複)**：任何環境都能執行
- **Self-Validating (自我驗證)**：測試結果清楚(通過/失敗)
- **Timely (及時)**：測試應在生產程式碼之前撰寫

**測試命名規範**：
```javascript
// ❌ 不好的命名
test('test1', () => { ... });
test('testLogin', () => { ... });

// ✅ 好的命名
test('空密碼應該返回錯誤訊息', () => { ... });
test('有效憑證應該允許登入', () => { ... });
test('連續三次失敗應該鎖定帳號', () => { ... });

// 使用describe組織測試
describe('UserLoginService', () => {
  describe('當提供有效憑證時', () => {
    test('應該返回認證令牌', () => { ... });
    test('應該記錄登入時間', () => { ... });
  });
  
  describe('當提供無效憑證時', () => {
    test('應該返回錯誤訊息', () => { ... });
    test('應該增加失敗計數', () => { ... });
  });
});
```

---

## 🐛 第六部分：缺陷管理

### 6.1 缺陷生命週期

#### 🔄 缺陷狀態流程

```
新建 (New) → 已指派 (Assigned) → 修復中 (In Progress) → 已修復 (Fixed)
    ↓              ↓                    ↓               ↓
  重複              已接受              需更多資訊        測試中 (Testing)
    ↓              ↓                    ↓               ↓
  關閉              修復中              已指派           通過/關閉
                    ↓                    ↓               ↓
                  已修復                修復中           重新開啟
```

**狀態定義**：
- **新建**：剛發現的缺陷
- **已指派**：分配給開發人員
- **修復中**：開發人員正在修復
- **已修復**：開發人員認為已修復
- **測試中**：測試人員驗證修復
- **關閉**：確認修復完成
- **重新開啟**：修復不完整，重新處理

### 6.2 缺陷報告

#### 📝 有效缺陷報告模板

```
缺陷編號: BUG-2024-001
發現日期: 2024-03-15
報告人: 王測試

【缺陷標題】
登入頁面 - 密碼錯誤時沒有顯示錯誤訊息

【嚴重程度】
中等 (Medium)

【優先級】
高 (High)

【環境資訊】
- 作業系統: Windows 11
- 瀏覽器: Chrome 122.0.6261.112
- 版本: v2.1.3
- 測試環境: https://staging.example.com

【重現步驟】
1. 開啟登入頁面 (https://staging.example.com/login)
2. 輸入有效的email: test@example.com
3. 輸入錯誤的密碼: wrongpassword
4. 點擊「登入」按鈕

【預期結果】
應該顯示錯誤訊息「密碼錯誤，請重新輸入」

【實際結果】
頁面沒有任何反應，沒有顯示錯誤訊息
登入按鈕保持啟用狀態

【附加資訊】
- 網路請求返回401狀態碼 (在開發者工具中可見)
- 主控台沒有JavaScript錯誤
- 相同問題在Firefox中也存在

【附件】
- 螢幕截圖: login-bug-screenshot.png
- 網路請求記錄: network-log.har
- 主控台日誌: console-log.txt

【影響範圍】
影響所有使用者的登入體驗，可能導致使用者困惑

【建議修復方案】
在密碼驗證失敗時顯示適當的錯誤訊息
```

#### 🎯 缺陷分類

**按嚴重程度分類**：
- **阻斷 (Blocker)**：系統無法使用
- **嚴重 (Critical)**：主要功能失效
- **一般 (Major)**：重要功能異常
- **輕微 (Minor)**：小問題
- **瑣碎 (Trivial)**：文字或格式問題

**按缺陷類型分類**：
- **功能性缺陷**：功能不符合需求
- **效能缺陷**：回應時間過慢
- **介面缺陷**：UI顯示問題
- **相容性缺陷**：不同環境表現不一致
- **安全性缺陷**：存在安全風險

### 6.3 缺陷追蹤工具

#### 🛠️ Jira缺陷管理範例

**缺陷工作流程配置**：
```yaml
# Jira工作流程設定
workflows:
  bug_workflow:
    statuses:
      - name: "Open"
        category: "To Do"
      - name: "In Progress"
        category: "In Progress"
      - name: "Fixed"
        category: "Done"
      - name: "Testing"
        category: "In Progress"
      - name: "Closed"
        category: "Done"
      - name: "Reopened"
        category: "To Do"
    
    transitions:
      - name: "Assign"
        from: ["Open"]
        to: "In Progress"
      - name: "Fix"
        from: ["In Progress", "Reopened"]
        to: "Fixed"
      - name: "Test"
        from: ["Fixed"]
        to: "Testing"
      - name: "Close"
        from: ["Testing"]
        to: "Closed"
      - name: "Reopen"
        from: ["Closed"]
        to: "Reopened"
```

**自動化整合**：
```javascript
// 與Git整合的缺陷追蹤
// commit訊息: "fix: 修復登入錯誤訊息顯示問題 (BUG-2024-001)"

// GitHub Actions中的Jira整合
name: 更新Jira缺陷狀態

on:
  push:
    branches: [ main ]

jobs:
  update-jira:
    runs-on: ubuntu-latest
    steps:
    - name: 提取Jira問題編號
      id: jira
      run: |
        ISSUE_KEY=$(echo "${{ github.event.head_commit.message }}" | grep -oE '(BUG-[0-9]+)')
        echo "issue_key=$ISSUE_KEY" >> $GITHUB_OUTPUT
    
    - name: 更新Jira狀態
      if: steps.jira.outputs.issue_key != ''
      uses: atlassian/gajira-transition@v2
      with:
        issue: ${{ steps.jira.outputs.issue_key }}
        transition: "Fixed"
      env:
        JIRA_BASE_URL: ${{ secrets.JIRA_BASE_URL }}
        JIRA_USER_EMAIL: ${{ secrets.JIRA_USER_EMAIL }}
        JIRA_API_TOKEN: ${{ secrets.JIRA_API_TOKEN }}
```

---

## 📊 第七部分：測試報告與度量

### 7.1 測試度量指標

#### 📈 關鍵測試指標

**測試覆蓋率指標**：
```javascript
// Jest覆蓋率報告範例
{
  "coverage": {
    "statements": {
      "total": 150,
      "covered": 135,
      "percentage": 90.0
    },
    "branches": {
      "total": 48,
      "covered": 40,
      "percentage": 83.33
    },
    "functions": {
      "total": 25,
      "covered": 23,
      "percentage": 92.0
    },
    "lines": {
      "total": 140,
      "covered": 126,
      "percentage": 90.0
    }
  }
}

// 覆蓋率目標設定
module.exports = {
  coverageThreshold: {
    global: {
      branches: 80,
      functions: 80,
      lines: 80,
      statements: 80
    },
    './src/critical/': {
      branches: 95,
      functions: 95,
      lines: 95,
      statements: 95
    }
  }
};
```

**測試效率指標**：
- **缺陷偵測率** = 測試發現的缺陷數 / 總缺陷數
- **缺陷逃逸率** = 生產環境發現的缺陷數 / 總缺陷數
- **測試執行效率** = 通過的測試案例數 / 總測試案例數
- **自動化比率** = 自動化測試案例數 / 總測試案例數

### 7.2 測試報告

#### 📄 測試報告模板

```markdown
# 測試報告

## 專案資訊
- **專案名稱**: 線上書店系統
- **版本**: v2.1.0
- **測試期間**: 2024-03-01 至 2024-03-15
- **測試負責人**: 測試團隊

## 測試摘要
### 整體測試結果
- **總測試案例數**: 324
- **執行案例數**: 320
- **通過案例數**: 298
- **失敗案例數**: 22
- **阻塞案例數**: 4
- **通過率**: 93.1%

### 測試覆蓋率
- **功能覆蓋率**: 95%
- **程式碼覆蓋率**: 88%
- **需求覆蓋率**: 100%

## 功能測試結果
| 功能模組 | 總案例 | 通過 | 失敗 | 通過率 | 狀態 |
|----------|--------|------|------|--------|------|
| 使用者管理 | 45 | 43 | 2 | 95.6% | ✅ |
| 商品管理 | 62 | 59 | 3 | 95.2% | ✅ |
| 購物車 | 38 | 35 | 3 | 92.1% | ⚠️ |
| 訂單處理 | 54 | 48 | 6 | 88.9% | ⚠️ |
| 支付系統 | 41 | 38 | 3 | 92.7% | ⚠️ |

## 缺陷分析
### 缺陷分佈
- **阻斷級**: 2個
- **嚴重級**: 5個
- **一般級**: 12個
- **輕微級**: 8個

### 主要缺陷
1. **BUG-001**: 支付處理異常導致訂單狀態錯誤 (阻斷級)
2. **BUG-002**: 大量併發時購物車資料遺失 (嚴重級)
3. **BUG-003**: 搜尋功能回應時間過長 (一般級)

## 測試環境
- **測試伺服器**: staging.bookstore.com
- **資料庫**: PostgreSQL 13.x
- **瀏覽器**: Chrome 122, Firefox 123, Safari 17
- **行動裝置**: iOS 17, Android 13

## 風險評估
### 高風險項目
1. 支付系統穩定性需要加強
2. 高負載情況下的效能表現
3. 第三方API整合的可靠性

### 建議
1. 增加支付流程的監控和錯誤處理
2. 進行壓力測試驗證系統容量
3. 建立第三方服務的備援機制

## 測試結論
系統整體品質良好，主要功能運作正常。建議修復阻斷級和嚴重級缺陷後再進行發布。

## 發布建議
✅ **建議發布** (修復高優先級缺陷後)

---
報告產生時間: 2024-03-15 10:30:00
報告版本: v1.0
```

---

## 🏃‍♂️ 實務練習

### 練習1：測試計畫設計

**專案背景**：
為「線上學習平台」設計完整的測試計畫。

**系統功能**：
- 使用者註冊/登入
- 課程瀏覽與搜尋
- 課程購買與觀看
- 作業提交與評分
- 討論區功能

**任務1.1：測試策略制定**
1. **測試範圍定義**
   - 確定測試邊界
   - 識別測試重點
   - 風險評估

2. **測試方法選擇**
   - 各功能模組的測試方法
   - 自動化vs手動測試分配
   - 測試環境規劃

3. **測試時程規劃**
   - 測試階段劃分
   - 人力資源分配
   - 里程碑設定

**任務1.2：測試案例設計**
選擇「課程購買」功能，設計完整測試案例：
- 正常流程測試
- 異常情況測試
- 邊界值測試
- 安全性測試

**提交成果**：
- 完整測試計畫文件
- 測試案例清單
- 風險評估報告

### 練習2：自動化測試實作

**任務2.1：單元測試實作**
為以下購物車類別編寫完整的單元測試：

```javascript
// ShoppingCart.js
class ShoppingCart {
  constructor() {
    this.items = [];
  }
  
  addItem(product, quantity = 1) {
    if (!product || quantity <= 0) {
      throw new Error('無效的商品或數量');
    }
    
    const existingItem = this.items.find(item => item.product.id === product.id);
    
    if (existingItem) {
      existingItem.quantity += quantity;
    } else {
      this.items.push({ product, quantity });
    }
  }
  
  removeItem(productId) {
    this.items = this.items.filter(item => item.product.id !== productId);
  }
  
  updateQuantity(productId, quantity) {
    if (quantity <= 0) {
      this.removeItem(productId);
      return;
    }
    
    const item = this.items.find(item => item.product.id === productId);
    if (item) {
      item.quantity = quantity;
    }
  }
  
  getTotalPrice() {
    return this.items.reduce((total, item) => {
      return total + (item.product.price * item.quantity);
    }, 0);
  }
  
  getItemCount() {
    return this.items.reduce((count, item) => count + item.quantity, 0);
  }
  
  clear() {
    this.items = [];
  }
}
```

**要求**：
- 100%的函數覆蓋率
- 涵蓋所有邊界情況
- 包含錯誤處理測試
- 使用AAA模式組織測試

**任務2.2：API整合測試**
為使用者註冊API建立整合測試：
```javascript
// POST /api/users/register
// 功能：註冊新使用者
// 輸入：{ name, email, password }
// 輸出：{ id, name, email, createdAt }
```

**測試場景**：
- 成功註冊新使用者
- Email已存在的錯誤處理
- 無效輸入的驗證
- 密碼強度檢查

**任務2.3：E2E測試**
使用Cypress為購物流程建立端到端測試：
1. 瀏覽商品
2. 加入購物車
3. 結帳流程
4. 訂單確認

**提交成果**：
- 完整的測試程式碼
- 測試覆蓋率報告
- CI/CD整合配置

### 練習3：TDD實作練習

**需求**：實作一個「密碼強度檢查器」

**功能規格**：
- 檢查密碼是否符合安全要求
- 返回密碼強度等級（弱、中、強）
- 提供改善建議

**安全要求**：
- 長度至少8個字元
- 包含大寫字母
- 包含小寫字母
- 包含數字
- 包含特殊字元
- 不包含常見弱密碼

**任務3.1：TDD開發流程**
使用紅-綠-重構循環開發：

1. **第1輪**：基本結構和空密碼檢查
2. **第2輪**：長度檢查
3. **第3輪**：字元類型檢查
4. **第4輪**：弱密碼字典檢查
5. **第5輪**：強度等級計算

**任務3.2：測試案例設計**
每輪開發都要先寫測試：
```javascript
// 範例測試結構
describe('PasswordValidator', () => {
  describe('基本驗證', () => {
    test('應該拒絕空密碼', () => {
      // 測試實作
    });
  });
  
  describe('長度檢查', () => {
    test('少於8字元應該為弱密碼', () => {
      // 測試實作
    });
  });
  
  // 更多測試群組...
});
```

**提交成果**：
- 完整的TDD開發過程記錄
- 每輪的測試和實作程式碼
- 重構前後的程式碼對比

### 練習4：缺陷管理流程

**任務4.1：缺陷報告撰寫**
針對以下問題撰寫標準缺陷報告：

**問題描述**：
在購物車頁面中，當使用者點擊「更新數量」按鈕時，如果輸入負數，系統會顯示錯誤訊息，但購物車中的商品並沒有被移除，仍然顯示原來的數量。

**任務4.2：缺陷追蹤流程設計**
設計缺陷從發現到關閉的完整流程：
1. 缺陷狀態定義
2. 角色權限設定
3. 通知機制
4. 升級規則

**任務4.3：缺陷度量分析**
分析以下缺陷資料並產生報告：

```
缺陷資料：
- 總缺陷數：85個
- 已修復：72個
- 進行中：8個
- 重新開啟：5個
- 嚴重級：12個
- 一般級：45個
- 輕微級：28個
```

**提交成果**：
- 標準缺陷報告
- 流程設計文件
- 缺陷分析報告

---

## 📝 知識檢核測驗

### 選擇題 (每題5分，共50分)

**1. 軟體測試的主要目的是什麼？**
A) 證明軟體沒有錯誤
B) 發現軟體中的缺陷
C) 提高開發效率
D) 減少開發成本

**2. 測試金字塔中，單元測試應該占多少比例？**
A) 10%
B) 30%
C) 50%
D) 70%

**3. 邊界值分析主要測試什麼？**
A) 輸入域的中間值
B) 輸入域的邊界附近值
C) 隨機輸入值
D) 所有可能的輸入值

**4. TDD的正確順序是？**
A) 綠-紅-重構
B) 紅-重構-綠
C) 紅-綠-重構
D) 重構-紅-綠

**5. Mock物件的主要用途是？**
A) 提高測試速度
B) 模擬外部依賴
C) 減少測試程式碼
D) 增加測試覆蓋率

**6. 下列哪個不是黑箱測試技術？**
A) 等價類劃分
B) 邊界值分析
C) 路徑覆蓋
D) 決策表測試

**7. 整合測試主要驗證什麼？**
A) 個別模組功能
B) 模組間的介面
C) 使用者需求
D) 系統效能

**8. 缺陷的生命週期中，"已修復"狀態之後通常是？**
A) 關閉
B) 測試中
C) 重新開啟
D) 已指派

**9. 測試覆蓋率中的"分支覆蓋"是指？**
A) 每個函數都被執行
B) 每行程式碼都被執行
C) 每個判斷的真假都被測試
D) 每個迴圈都被執行

**10. Cypress主要用於哪種測試？**
A) 單元測試
B) 整合測試
C) 端到端測試
D) 效能測試

### 簡答題 (每題25分，共50分)

**1. 請說明測試驅動開發(TDD)的優缺點，並描述在什麼情況下適合使用TDD，什麼情況下不適合？(300字)**

**2. 比較手動測試和自動化測試的差異，說明如何決定哪些測試案例應該自動化，哪些應該保持手動執行？請提供具體的判斷標準。(300字)**

---

## 📚 延伸學習資源

### 必讀書籍
1. **《軟體測試的藝術》** - Glenford J. Myers
2. **《測試驅動開發》** - Kent Beck
3. **《Google軟體測試之道》** - James Whittaker
4. **《敏捷測試》** - Lisa Crispin & Janet Gregory
5. **《完美軟體：對軟體測試的各種幻想》** - Gerald M. Weinberg

### 線上課程
1. **ISTQB軟體測試認證** - 國際軟體測試資格認證
2. **Test Automation University** - Applitools提供的免費課程
3. **Cypress Testing Course** - 現代E2E測試框架
4. **Jest & Testing Library** - React測試生態系統

### 測試工具
1. **單元測試框架**：
   - Jest (JavaScript)
   - JUnit (Java)
   - pytest (Python)
   - NUnit (.NET)

2. **E2E測試工具**：
   - Cypress - 現代Web應用測試
   - Playwright - 跨瀏覽器測試
   - Selenium - 傳統Web自動化
   - Puppeteer - Chrome自動化

3. **API測試工具**：
   - Postman - API開發測試
   - Insomnia - REST客戶端
   - Newman - Postman命令列工具
   - REST Assured - Java API測試

4. **效能測試工具**：
   - JMeter - 開源負載測試
   - K6 - 現代負載測試
   - Artillery - 輕量級負載測試

### 測試社群
1. **Ministry of Testing** - 全球測試社群
2. **ISTQB** - 國際軟體測試資格委員會
3. **台灣軟體測試學會** - 本地測試專業組織
4. **Test Automation Meetup** - 自動化測試聚會

---

## ✅ 學習檢核表

- [ ] 理解軟體測試的基本原理和重要性
- [ ] 掌握黑箱和白箱測試設計技術
- [ ] 能夠設計有效的測試案例
- [ ] 熟悉不同層級的測試方法
- [ ] 會使用主流的自動化測試工具
- [ ] 理解和實踐TDD開發方法
- [ ] 了解缺陷管理的完整流程
- [ ] 能夠撰寫專業的測試報告
- [ ] 掌握測試度量和分析方法
- [ ] 建立有效的測試策略
- [ ] 完成所有實務練習
- [ ] 通過知識檢核測驗 (70分以上)

---

## 🔮 下週預告

**第七模組：部署與維護 (第8週)**

我們將學習：
- 軟體部署策略與最佳實務
- 系統監控與效能調校
- 維護管理與版本控制
- 災難恢復與備份策略
- 使用者支援與文件管理

**實務重點**：
- 建立自動化部署流程
- 設計系統監控機制
- 制定維護計畫
- 建立災難恢復流程

---

## 🌟 學習反思

請花15分鐘思考以下問題：

1. **測試思維**：測試工作如何改變了您對軟體品質的認知？
2. **工具應用**：哪些測試工具對您的工作最有幫助？為什麼？
3. **TDD體驗**：實踐TDD過程中遇到了什麼挑戰？如何克服？
4. **品質意識**：如何在團隊中建立測試文化和品質意識？
5. **持續改善**：測試流程還有哪些可以優化的地方？

---

**🎉 恭喜完成第六模組！**  
*您現在已經掌握了軟體測試的完整知識體系，能夠設計和執行有效的測試策略，確保軟體品質。準備好進入部署與維護的最後階段了！*