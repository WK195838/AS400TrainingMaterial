# 第二週：COBOL語言基礎 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 理解COBOL程式的完整結構和編排規則
- 掌握四大部門的用途和語法
- 熟練使用基本資料型態和變數定義
- 進行基本的數值和文字處理運算
- 編寫符合企業標準的COBOL程式碼

---

## 📖 第一節：COBOL程式結構概論

### 1.1 COBOL的歷史背景

COBOL（Common Business-Oriented Language）是一種專為商業應用設計的程式語言，於1959年開發。它的特色是：

**設計理念：**
- 接近自然語言的語法
- 強調資料處理能力
- 適合大型企業應用
- 注重程式的可讀性和維護性

**在AS/400系統中的地位：**
- 主要的商業應用開發語言
- 與DB2資料庫深度整合
- 支援大型交易處理系統
- 具備優異的穩定性和效能

### 1.2 COBOL程式的四大部門

每個COBOL程式都由四個部門組成，必須按照固定順序排列：

```cobol
       IDENTIFICATION DIVISION.          ← 識別部門
       ENVIRONMENT DIVISION.             ← 環境部門  
       DATA DIVISION.                    ← 資料部門
       PROCEDURE DIVISION.               ← 程序部門
```

**記憶口訣：**「識環資程」- 識別、環境、資料、程序

### 1.3 程式編排規則

COBOL有嚴格的程式碼編排規則：

```
欄位位置說明：
欄位 1-6   : 序號欄位（可選用）
欄位 7     : 註解或延續指示符
欄位 8-11  : A區域（部門、節、段落名稱）
欄位 12-72 : B區域（程式敘述）
欄位 73-80 : 識別欄位（可選用）
```

**範例：**
```cobol
000100 IDENTIFICATION DIVISION.                                    PAYROLL
000200 PROGRAM-ID. SALARY-CALC.                                   PAYROLL
000300*這是註解行                                                  PAYROLL
000400 ENVIRONMENT DIVISION.                                       PAYROLL
000500 DATA DIVISION.                                              PAYROLL
000600 WORKING-STORAGE SECTION.                                    PAYROLL
000700 01  WS-EMPLOYEE-SALARY    PIC 9(7)V99.                    PAYROLL
000800 PROCEDURE DIVISION.                                         PAYROLL
000900     DISPLAY 'HELLO WORLD'.                                 PAYROLL
001000     STOP RUN.                                              PAYROLL
```

### 1.4 註解和文件化

**註解的重要性：**
- 提高程式可讀性
- 方便後續維護
- 符合企業開發標準

**註解的方式：**
```cobol
      *這是整行註解
       MOVE 0 TO WS-COUNTER.    *這是行末註解
      *
      * 程式功能：薪資計算系統
      * 作者：張三
      * 日期：2024/01/15
      * 修改記錄：
      *   2024/01/20 - 增加加班費計算 - 李四
      *
```

---

## 📊 第二節：識別部門和環境部門

### 2.1 識別部門（IDENTIFICATION DIVISION）

識別部門用來說明程式的基本資訊：

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALARY-CALC.
       AUTHOR. 張三.
       DATE-WRITTEN. 2024/01/15.
       DATE-COMPILED. 2024/01/15.
       SECURITY. 機密.
       REMARKS. 薪資計算系統主程式.
```

**各項目說明：**
- `PROGRAM-ID`：程式識別碼（必須，最多8個字元）
- `AUTHOR`：程式作者（選用）
- `DATE-WRITTEN`：撰寫日期（選用）
- `DATE-COMPILED`：編譯日期（選用）
- `SECURITY`：安全等級（選用）
- `REMARKS`：程式說明（選用）

### 2.2 環境部門（ENVIRONMENT DIVISION）

環境部門定義程式執行的環境和檔案分配：

```cobol
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE
           ASSIGN TO DATABASE-EMPMASTER
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS EMP-ID
           FILE STATUS IS WS-FILE-STATUS.
```

**主要組成：**
- `CONFIGURATION SECTION`：系統組態設定
- `INPUT-OUTPUT SECTION`：檔案控制設定

---

## 💾 第三節：資料部門 - 基礎概念

### 3.1 資料部門結構

資料部門用來定義程式中使用的所有資料：

```cobol
       DATA DIVISION.
       FILE SECTION.              ← 檔案資料定義
       WORKING-STORAGE SECTION.   ← 工作儲存區
       LOCAL-STORAGE SECTION.     ← 本地儲存區
       LINKAGE SECTION.           ← 連結區（參數傳遞）
```

### 3.2 工作儲存區（WORKING-STORAGE SECTION）

這是最常用的資料定義區域：

```cobol
       WORKING-STORAGE SECTION.
       01  WS-CONSTANTS.
           05  WS-COMPANY-NAME     PIC X(20) VALUE '台灣科技公司'.
           05  WS-TAX-RATE         PIC 9V99  VALUE 0.05.
           05  WS-MAX-EMPLOYEES    PIC 999   VALUE 500.
           
       01  WS-VARIABLES.
           05  WS-EMPLOYEE-ID      PIC X(6).
           05  WS-EMPLOYEE-NAME    PIC X(20).
           05  WS-BASIC-SALARY     PIC 9(7)V99.
           05  WS-OVERTIME-HOURS   PIC 999.
           05  WS-TOTAL-SALARY     PIC 9(8)V99.
           
       01  WS-COUNTERS.
           05  WS-RECORD-COUNT     PIC 9(5)   VALUE ZERO.
           05  WS-ERROR-COUNT      PIC 9(3)   VALUE ZERO.
           
       01  WS-FLAGS.
           05  WS-END-OF-FILE      PIC X      VALUE 'N'.
               88  EOF                        VALUE 'Y'.
           05  WS-VALID-DATA       PIC X      VALUE 'Y'.
               88  VALID-DATA                 VALUE 'Y'.
               88  INVALID-DATA               VALUE 'N'.
```

### 3.3 資料定義的階層結構

COBOL使用階層數字來組織資料：

```cobol
       01  WS-EMPLOYEE-RECORD.              ← 群組項目
           05  WS-EMP-PERSONAL.             ← 子群組
               10  WS-EMP-ID        PIC X(6).      ← 基本項目
               10  WS-EMP-NAME      PIC X(20).
               10  WS-EMP-BIRTH-DATE.
                   15  WS-BIRTH-YEAR    PIC 9(4).
                   15  WS-BIRTH-MONTH   PIC 99.
                   15  WS-BIRTH-DAY     PIC 99.
           05  WS-EMP-SALARY.
               10  WS-BASIC-PAY     PIC 9(7)V99.
               10  WS-OVERTIME-PAY  PIC 9(6)V99.
               10  WS-TOTAL-PAY     PIC 9(8)V99.
```

**階層規則：**
- 01-49：使用者定義階層
- 66：重新定義項目
- 77：獨立項目
- 88：條件名稱

---

## 🔢 第四節：基本資料型態

### 4.1 數值型態（Numeric）

#### 4.1.1 整數定義
```cobol
       01  WS-INTEGERS.
           05  WS-SMALL-NUM       PIC 9(3).        ← 3位數整數（0-999）
           05  WS-MEDIUM-NUM      PIC 9(5).        ← 5位數整數
           05  WS-LARGE-NUM       PIC 9(9).        ← 9位數整數
           05  WS-SIGNED-NUM      PIC S9(5).       ← 有號整數
```

#### 4.1.2 小數定義
```cobol
       01  WS-DECIMALS.
           05  WS-CURRENCY        PIC 9(7)V99.     ← 金額（7位整數+2位小數）
           05  WS-PERCENTAGE      PIC 9V99.        ← 百分比（1位整數+2位小數）
           05  WS-PRECISE-NUM     PIC 9(5)V9(4).   ← 高精度數值
```

#### 4.1.3 帶符號數值
```cobol
       01  WS-SIGNED-NUMBERS.
           05  WS-BALANCE         PIC S9(8)V99.    ← 帶符號的餘額
           05  WS-PROFIT-LOSS     PIC S9(10)V99.   ← 損益數值
```

### 4.2 文字型態（Alphanumeric）

```cobol
       01  WS-STRINGS.
           05  WS-EMPLOYEE-NAME   PIC X(20).       ← 20個字元的名稱
           05  WS-DEPARTMENT      PIC X(10).       ← 部門代碼
           05  WS-DESCRIPTION     PIC X(50).       ← 描述文字
           05  WS-SINGLE-CHAR     PIC X.           ← 單一字元
```

### 4.3 特殊型態

#### 4.3.1 編輯型態（Edited）
```cobol
       01  WS-EDITED-FIELDS.
           05  WS-DISPLAY-AMOUNT  PIC $$$,$$9.99.  ← 金額顯示格式
           05  WS-PHONE-NUMBER    PIC 9(3)-9(3)-9(4). ← 電話號碼格式
           05  WS-DATE-DISPLAY    PIC 99/99/9999.  ← 日期顯示格式
```

#### 4.3.2 計算型態（Computational）
```cobol
       01  WS-COMP-FIELDS.
           05  WS-BINARY-NUM      PIC 9(4) COMP.   ← 二進位整數
           05  WS-PACKED-NUM      PIC 9(7) COMP-3. ← 壓縮十進位
           05  WS-FLOAT-NUM       COMP-1.          ← 單精度浮點
           05  WS-DOUBLE-NUM      COMP-2.          ← 雙精度浮點
```

### 4.4 初始值設定

```cobol
       01  WS-INITIALIZED-DATA.
           05  WS-COUNTER         PIC 9(3)   VALUE 0.
           05  WS-COMPANY-NAME    PIC X(15)  VALUE '台灣科技公司'.
           05  WS-PI              PIC 9V9(4) VALUE 3.1416.
           05  WS-STATUS          PIC X      VALUE 'A'.
           05  WS-ARRAY           PIC X(10)  VALUE ALL '*'.
           05  WS-SPACES          PIC X(20)  VALUE SPACES.
           05  WS-ZEROS           PIC 9(5)   VALUE ZEROS.
```

---

## 🧮 第五節：基本運算操作

### 5.1 MOVE 敘述

MOVE是最基本的資料移動指令：

```cobol
       PROCEDURE DIVISION.
       MAIN-LOGIC.
      *    基本資料移動
           MOVE 1000 TO WS-SALARY.
           MOVE 'JOHN DOE' TO WS-NAME.
           MOVE WS-BASIC-PAY TO WS-TOTAL-PAY.
           
      *    群組資料移動
           MOVE WS-EMPLOYEE-RECORD TO WS-BACKUP-RECORD.
           
      *    特殊值移動
           MOVE ZERO TO WS-COUNTER.
           MOVE SPACES TO WS-DESCRIPTION.
           MOVE HIGH-VALUES TO WS-KEY-FIELD.
           
      *    條件設定
           MOVE 'Y' TO WS-END-OF-FILE.
           SET EOF TO TRUE.
```

**MOVE的規則：**
- 數值到數值：自動對齊小數點
- 文字到文字：左對齊，右邊補空格
- 數值到文字：右對齊，左邊補空格
- 文字到數值：只能是數字字元

### 5.2 算術運算

#### 5.2.1 ADD 敘述
```cobol
       PROCEDURE DIVISION.
       ARITHMETIC-OPERATIONS.
      *    基本加法
           ADD 100 TO WS-SALARY.
           ADD WS-BASIC-PAY TO WS-TOTAL-PAY.
           
      *    多項相加
           ADD WS-BASIC-PAY, WS-OVERTIME-PAY, WS-BONUS
               TO WS-TOTAL-SALARY.
           
      *    加法結果存到其他變數
           ADD WS-SALARY, WS-BONUS GIVING WS-TOTAL-INCOME.
           
      *    帶餘數的加法
           ADD WS-MONTHLY-SALARY TO WS-ANNUAL-SALARY
               ON SIZE ERROR
                   DISPLAY 'OVERFLOW ERROR'
           END-ADD.
```

#### 5.2.2 SUBTRACT 敘述
```cobol
      *    基本減法
           SUBTRACT 50 FROM WS-BALANCE.
           SUBTRACT WS-TAX FROM WS-GROSS-PAY.
           
      *    減法結果存到其他變數
           SUBTRACT WS-DEDUCTIONS FROM WS-GROSS-PAY
               GIVING WS-NET-PAY.
           
      *    多項相減
           SUBTRACT WS-TAX, WS-INSURANCE, WS-PENSION
               FROM WS-GROSS-PAY.
```

#### 5.2.3 MULTIPLY 敘述
```cobol
      *    基本乘法
           MULTIPLY 1.1 BY WS-SALARY.
           MULTIPLY WS-HOURS BY WS-RATE.
           
      *    乘法結果存到其他變數
           MULTIPLY WS-QUANTITY BY WS-UNIT-PRICE
               GIVING WS-TOTAL-AMOUNT.
           
      *    四捨五入
           MULTIPLY WS-AMOUNT BY WS-TAX-RATE
               GIVING WS-TAX-AMOUNT
               ROUNDED.
```

#### 5.2.4 DIVIDE 敘述
```cobol
      *    基本除法
           DIVIDE 12 INTO WS-ANNUAL-SALARY.
           DIVIDE WS-TOTAL-AMOUNT BY WS-QUANTITY.
           
      *    除法結果存到其他變數
           DIVIDE WS-TOTAL-SALES BY WS-SALES-DAYS
               GIVING WS-AVERAGE-DAILY-SALES.
           
      *    帶餘數的除法
           DIVIDE WS-TOTAL-AMOUNT BY 100
               GIVING WS-DOLLARS
               REMAINDER WS-CENTS.
```

### 5.3 COMPUTE 敘述

COMPUTE提供更靈活的運算方式：

```cobol
      *    複雜運算式
           COMPUTE WS-TOTAL-SALARY = 
               WS-BASIC-SALARY + 
               (WS-OVERTIME-HOURS * WS-OVERTIME-RATE) +
               WS-BONUS.
           
      *    數學函數
           COMPUTE WS-SQUARE-ROOT = SQRT(WS-NUMBER).
           COMPUTE WS-ABSOLUTE = ABS(WS-NEGATIVE-NUMBER).
           COMPUTE WS-MAX-VALUE = MAX(WS-VALUE1, WS-VALUE2).
           
      *    複雜的金融計算
           COMPUTE WS-COMPOUND-INTEREST = 
               WS-PRINCIPAL * 
               ((1 + WS-RATE) ** WS-YEARS) - 
               WS-PRINCIPAL.
```

**運算子優先順序：**
1. `**` （次方）
2. `*` `/` （乘法、除法）
3. `+` `-` （加法、減法）
4. `()` （括號改變優先順序）

---

## 💡 第六節：實例練習

### 實例6.1：員工薪資計算系統

**需求分析：**
- 計算員工的基本薪資、加班費、總薪資
- 計算應扣稅額和實領薪資
- 顯示完整的薪資明細

```cobol
      *****************************************************************
      * 程式名稱：SALARY-CALC                                        *
      * 程式功能：員工薪資計算系統                                   *
      * 作者：張三                                                   *
      * 日期：2024/01/15                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALARY-CALC.
       AUTHOR. 張三.
       DATE-WRITTEN. 2024/01/15.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *    常數定義
       01  WS-CONSTANTS.
           05  WS-COMPANY-NAME     PIC X(20) VALUE '台灣科技股份有限公司'.
           05  WS-TAX-RATE         PIC 9V99  VALUE 0.05.
           05  WS-INSURANCE-RATE   PIC 9V99  VALUE 0.02.
           05  WS-OVERTIME-RATE    PIC 9(3)  VALUE 150.
           
      *    輸入資料
       01  WS-INPUT-DATA.
           05  WS-EMP-ID           PIC X(6).
           05  WS-EMP-NAME         PIC X(20).
           05  WS-BASIC-SALARY     PIC 9(6)V99.
           05  WS-OVERTIME-HOURS   PIC 999.
           
      *    計算結果
       01  WS-CALCULATION-RESULTS.
           05  WS-OVERTIME-PAY     PIC 9(6)V99.
           05  WS-GROSS-SALARY     PIC 9(7)V99.
           05  WS-TAX-AMOUNT       PIC 9(6)V99.
           05  WS-INSURANCE-AMOUNT PIC 9(5)V99.
           05  WS-TOTAL-DEDUCTION  PIC 9(6)V99.
           05  WS-NET-SALARY       PIC 9(7)V99.
           
      *    顯示格式
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-GROSS       PIC $$$,$$9.99.
           05  WS-DISP-TAX         PIC $$$,$$9.99.
           05  WS-DISP-INSURANCE   PIC $$$,$$9.99.
           05  WS-DISP-NET         PIC $$$,$$9.99.
           
       PROCEDURE DIVISION.
       
      *****************************************************************
      * 主程式邏輯                                                   *
      *****************************************************************
       MAIN-LOGIC.
           PERFORM DISPLAY-HEADER
           PERFORM GET-INPUT-DATA
           PERFORM CALCULATE-SALARY
           PERFORM DISPLAY-RESULTS
           STOP RUN.
           
      *****************************************************************
      * 顯示標題                                                     *
      *****************************************************************
       DISPLAY-HEADER.
           DISPLAY ' '.
           DISPLAY '========================================='.
           DISPLAY '       ' WS-COMPANY-NAME.
           DISPLAY '         薪資計算系統'.
           DISPLAY '========================================='.
           DISPLAY ' '.
           
      *****************************************************************
      * 取得輸入資料                                                 *
      *****************************************************************
       GET-INPUT-DATA.
           DISPLAY '請輸入員工編號: '.
           ACCEPT WS-EMP-ID.
           
           DISPLAY '請輸入員工姓名: '.
           ACCEPT WS-EMP-NAME.
           
           DISPLAY '請輸入基本薪資: '.
           ACCEPT WS-BASIC-SALARY.
           
           DISPLAY '請輸入加班時數: '.
           ACCEPT WS-OVERTIME-HOURS.
           
      *****************************************************************
      * 薪資計算                                                     *
      *****************************************************************
       CALCULATE-SALARY.
      *    計算加班費
           COMPUTE WS-OVERTIME-PAY = 
               WS-OVERTIME-HOURS * WS-OVERTIME-RATE.
           
      *    計算總薪資
           COMPUTE WS-GROSS-SALARY = 
               WS-BASIC-SALARY + WS-OVERTIME-PAY.
           
      *    計算稅額
           COMPUTE WS-TAX-AMOUNT = 
               WS-GROSS-SALARY * WS-TAX-RATE.
           
      *    計算保險費
           COMPUTE WS-INSURANCE-AMOUNT = 
               WS-GROSS-SALARY * WS-INSURANCE-RATE.
           
      *    計算總扣除額
           COMPUTE WS-TOTAL-DEDUCTION = 
               WS-TAX-AMOUNT + WS-INSURANCE-AMOUNT.
           
      *    計算實領薪資
           COMPUTE WS-NET-SALARY = 
               WS-GROSS-SALARY - WS-TOTAL-DEDUCTION.
           
      *    準備顯示格式
           MOVE WS-GROSS-SALARY TO WS-DISP-GROSS.
           MOVE WS-TAX-AMOUNT TO WS-DISP-TAX.
           MOVE WS-INSURANCE-AMOUNT TO WS-DISP-INSURANCE.
           MOVE WS-NET-SALARY TO WS-DISP-NET.
           
      *****************************************************************
      * 顯示計算結果                                                 *
      *****************************************************************
       DISPLAY-RESULTS.
           DISPLAY ' '.
           DISPLAY '========== 薪資明細 =========='.
           DISPLAY '員工編號: ' WS-EMP-ID.
           DISPLAY '員工姓名: ' WS-EMP-NAME.
           DISPLAY ' '.
           DISPLAY '基本薪資: ' WS-BASIC-SALARY.
           DISPLAY '加班時數: ' WS-OVERTIME-HOURS ' 小時'.
           DISPLAY '加班費  : ' WS-OVERTIME-PAY.
           DISPLAY '應發薪資: ' WS-DISP-GROSS.
           DISPLAY ' '.
           DISPLAY '========== 扣除項目 =========='.
           DISPLAY '所得稅  : ' WS-DISP-TAX.
           DISPLAY '保險費  : ' WS-DISP-INSURANCE.
           DISPLAY '總扣除額: ' WS-TOTAL-DEDUCTION.
           DISPLAY ' '.
           DISPLAY '========== 實領薪資 =========='.
           DISPLAY '實領薪資: ' WS-DISP-NET.
           DISPLAY '=============================='.
```

### 實例6.2：商品庫存管理

**需求分析：**
- 記錄商品進貨、銷售情況
- 計算庫存餘額和庫存價值
- 判斷是否需要補貨

```cobol
      *****************************************************************
      * 程式名稱：INVENTORY-MGMT                                     *
      * 程式功能：商品庫存管理系統                                   *
      * 作者：李四                                                   *
      * 日期：2024/01/15                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY-MGMT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *    商品基本資料
       01  WS-PRODUCT-DATA.
           05  WS-PRODUCT-ID       PIC X(8).
           05  WS-PRODUCT-NAME     PIC X(30).
           05  WS-UNIT-PRICE       PIC 9(5)V99.
           05  WS-REORDER-LEVEL    PIC 9(4).
           
      *    庫存異動資料
       01  WS-TRANSACTION-DATA.
           05  WS-BEGINNING-QTY    PIC 9(6).
           05  WS-RECEIVED-QTY     PIC 9(5).
           05  WS-SOLD-QTY         PIC 9(5).
           05  WS-ENDING-QTY       PIC 9(6).
           
      *    計算結果
       01  WS-CALCULATIONS.
           05  WS-INVENTORY-VALUE  PIC 9(8)V99.
           05  WS-REORDER-NEEDED   PIC X.
               88  NEED-REORDER            VALUE 'Y'.
               88  NO-REORDER-NEEDED       VALUE 'N'.
           
      *    顯示格式
       01  WS-DISPLAY-FORMATS.
           05  WS-DISP-PRICE       PIC $$$,$$9.99.
           05  WS-DISP-VALUE       PIC $$$,$$9.99.
           
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           PERFORM GET-PRODUCT-DATA
           PERFORM GET-TRANSACTION-DATA
           PERFORM CALCULATE-INVENTORY
           PERFORM CHECK-REORDER-LEVEL
           PERFORM DISPLAY-INVENTORY-REPORT
           STOP RUN.
           
       GET-PRODUCT-DATA.
           DISPLAY '=== 商品基本資料輸入 ==='.
           DISPLAY '商品代碼: '.
           ACCEPT WS-PRODUCT-ID.
           DISPLAY '商品名稱: '.
           ACCEPT WS-PRODUCT-NAME.
           DISPLAY '單位價格: '.
           ACCEPT WS-UNIT-PRICE.
           DISPLAY '安全庫存量: ' WS-REORDER-LEVEL
           ELSE
               DISPLAY '庫存充足，無需補貨'
           END-IF.
           DISPLAY '=============================='.
```

### 實例6.3：學生成績統計系統

**需求分析：**
- 輸入學生各科成績
- 計算總分、平均分、等級
- 判斷是否及格

```cobol
      *****************************************************************
      * 程式名稱：STUDENT-GRADE                                      *
      * 程式功能：學生成績統計系統                                   *
      * 作者：王五                                                   *
      * 日期：2024/01/15                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-GRADE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *    學生基本資料
       01  WS-STUDENT-INFO.
           05  WS-STUDENT-ID       PIC X(8).
           05  WS-STUDENT-NAME     PIC X(20).
           05  WS-CLASS-NAME       PIC X(10).
           
      *    成績資料
       01  WS-SCORES.
           05  WS-CHINESE          PIC 999.
           05  WS-ENGLISH          PIC 999.
           05  WS-MATH             PIC 999.
           05  WS-SCIENCE          PIC 999.
           05  WS-SOCIAL           PIC 999.
           
      *    統計結果
       01  WS-STATISTICS.
           05  WS-TOTAL-SCORE      PIC 9(4).
           05  WS-AVERAGE-SCORE    PIC 999V99.
           05  WS-GRADE-LEVEL      PIC X.
           05  WS-PASS-STATUS      PIC X.
               88  PASSED                  VALUE 'Y'.
               88  FAILED                  VALUE 'N'.
           
      *    科目數量
       01  WS-SUBJECT-COUNT        PIC 9 VALUE 5.
       
      *    等級判定常數
       01  WS-GRADE-CONSTANTS.
           05  WS-GRADE-A          PIC 999 VALUE 90.
           05  WS-GRADE-B          PIC 999 VALUE 80.
           05  WS-GRADE-C          PIC 999 VALUE 70.
           05  WS-GRADE-D          PIC 999 VALUE 60.
           05  WS-PASS-MARK        PIC 999 VALUE 60.
           
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           PERFORM INPUT-STUDENT-INFO
           PERFORM INPUT-SCORES
           PERFORM CALCULATE-STATISTICS
           PERFORM DETERMINE-GRADE
           PERFORM CHECK-PASS-STATUS
           PERFORM DISPLAY-REPORT
           STOP RUN.
           
       INPUT-STUDENT-INFO.
           DISPLAY '=== 學生基本資料 ==='.
           DISPLAY '學號: '.
           ACCEPT WS-STUDENT-ID.
           DISPLAY '姓名: '.
           ACCEPT WS-STUDENT-NAME.
           DISPLAY '班級: '.
           ACCEPT WS-CLASS-NAME.
           
       INPUT-SCORES.
           DISPLAY ' '.
           DISPLAY '=== 各科成績輸入 ==='.
           DISPLAY '國文成績: '.
           ACCEPT WS-CHINESE.
           DISPLAY '英文成績: '.
           ACCEPT WS-ENGLISH.
           DISPLAY '數學成績: '.
           ACCEPT WS-MATH.
           DISPLAY '自然成績: '.
           ACCEPT WS-SCIENCE.
           DISPLAY '社會成績: '.
           ACCEPT WS-SOCIAL.
           
       CALCULATE-STATISTICS.
      *    計算總分
           COMPUTE WS-TOTAL-SCORE = 
               WS-CHINESE + WS-ENGLISH + WS-MATH + 
               WS-SCIENCE + WS-SOCIAL.
           
      *    計算平均分
           COMPUTE WS-AVERAGE-SCORE = 
               WS-TOTAL-SCORE / WS-SUBJECT-COUNT.
           
       DETERMINE-GRADE.
           IF WS-AVERAGE-SCORE >= WS-GRADE-A
               MOVE 'A' TO WS-GRADE-LEVEL
           ELSE
               IF WS-AVERAGE-SCORE >= WS-GRADE-B
                   MOVE 'B' TO WS-GRADE-LEVEL
               ELSE
                   IF WS-AVERAGE-SCORE >= WS-GRADE-C
                       MOVE 'C' TO WS-GRADE-LEVEL
                   ELSE
                       IF WS-AVERAGE-SCORE >= WS-GRADE-D
                           MOVE 'D' TO WS-GRADE-LEVEL
                       ELSE
                           MOVE 'F' TO WS-GRADE-LEVEL
                       END-IF
                   END-IF
               END-IF
           END-IF.
           
       CHECK-PASS-STATUS.
           IF WS-AVERAGE-SCORE >= WS-PASS-MARK
               SET PASSED TO TRUE
           ELSE
               SET FAILED TO TRUE
           END-IF.
           
       DISPLAY-REPORT.
           DISPLAY ' '.
           DISPLAY '=========== 成績單 ==========='.
           DISPLAY '學號: ' WS-STUDENT-ID.
           DISPLAY '姓名: ' WS-STUDENT-NAME.
           DISPLAY '班級: ' WS-CLASS-NAME.
           DISPLAY ' '.
           DISPLAY '========== 各科成績 =========='.
           DISPLAY '國文: ' WS-CHINESE.
           DISPLAY '英文: ' WS-ENGLISH.
           DISPLAY '數學: ' WS-MATH.
           DISPLAY '自然: ' WS-SCIENCE.
           DISPLAY '社會: ' WS-SOCIAL.
           DISPLAY ' '.
           DISPLAY '========== 統計結果 =========='.
           DISPLAY '總分: ' WS-TOTAL-SCORE.
           DISPLAY '平均: ' WS-AVERAGE-SCORE.
           DISPLAY '等級: ' WS-GRADE-LEVEL.
           DISPLAY ' '.
           IF PASSED
               DISPLAY '*** 恭喜！成績及格 ***'
           ELSE
               DISPLAY '*** 成績不及格，請加油 ***'
           END-IF.
           DISPLAY '=============================='.
```

---

## 🔧 第七節：編譯和執行

### 7.1 編譯過程

在AS/400系統中編譯COBOL程式的步驟：

```
1. 使用SEU編輯程式原始碼
   STRSEU SRCMBR(程式名稱) SRCFILE(QCBLLESRC)

2. 編譯程式
   CRTCBLPGM PGM(程式名稱) SRCFILE(QCBLLESRC)

3. 執行程式
   CALL PGM(程式名稱)
```

### 7.2 常見編譯錯誤

#### 7.2.1 語法錯誤
```cobol
      *錯誤示例：
       MOVE 'ABC TO WS-NAME.        ← 缺少結束引號
       ADD WS-NUM1 WS-NUM2.         ← 缺少TO或GIVING
       DISPLAY 'HELLO WORLD'        ← 缺少句號
       
      *正確寫法：
       MOVE 'ABC' TO WS-NAME.
       ADD WS-NUM1 TO WS-NUM2.
       DISPLAY 'HELLO WORLD'.
```

#### 7.2.2 資料定義錯誤
```cobol
      *錯誤示例：
       01  WS-NAME    PIC X(20.       ← 缺少右括號
       01  WS-AMOUNT  PIC 9(7)V999.   ← 小數位數過多
       
      *正確寫法：
       01  WS-NAME    PIC X(20).
       01  WS-AMOUNT  PIC 9(7)V99.
```

### 7.3 執行時錯誤處理

```cobol
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM SAFE-CALCULATION
           STOP RUN.
           
       SAFE-CALCULATION.
           COMPUTE WS-RESULT = WS-NUM1 / WS-NUM2
               ON SIZE ERROR
                   DISPLAY 'ERROR: DIVISION OVERFLOW'
                   MOVE ZERO TO WS-RESULT
           END-COMPUTE.
           
           IF WS-NUM2 = ZERO
               DISPLAY 'WARNING: DIVISION BY ZERO'
               MOVE ZERO TO WS-RESULT
           ELSE
               DIVIDE WS-NUM1 BY WS-NUM2 GIVING WS-RESULT
           END-IF.
```

---

## 📝 第八節：課後練習

### 練習8.1：基礎語法練習

**題目：**編寫一個程式，計算三角形的面積和周長。

**要求：**
- 輸入三角形三邊長度
- 計算並顯示面積和周長
- 判斷是否為有效三角形

**提示：**
- 三角形面積公式：S = √[s(s-a)(s-b)(s-c)]，其中s = (a+b+c)/2
- 三角形成立條件：任意兩邊之和大於第三邊

### 練習8.2：綜合應用練習

**題目：**銀行存款利息計算系統

**要求：**
- 輸入存款金額、年利率、存款期間
- 計算單利和複利
- 顯示本金、利息、本息合計
- 比較單利和複利的差異

**公式：**
- 單利：利息 = 本金 × 年利率 × 年數
- 複利：本息合計 = 本金 × (1 + 年利率) ^ 年數

### 練習8.3：挑戰題

**題目：**員工獎金分配系統

**要求：**
- 輸入員工績效等級（A、B、C、D）
- 根據等級計算獎金比例
- 考慮年資加成
- 顯示詳細的獎金計算明細

**規則：**
- A等級：基本獎金 × 1.5
- B等級：基本獎金 × 1.2
- C等級：基本獎金 × 1.0
- D等級：基本獎金 × 0.8
- 年資加成：每滿一年加成2%，上限20%

---

## 🎯 第九節：學習檢核

### 9.1 知識點檢核清單

完成本週學習後，請檢查是否掌握以下知識點：

**基本概念：**
- [ ] 理解COBOL四大部門的用途
- [ ] 掌握程式編排規則
- [ ] 熟悉註解和文件化方法

**資料定義：**
- [ ] 能夠正確定義各種資料型態
- [ ] 理解階層結構的概念
- [ ] 掌握初始值設定方法

**基本運算：**
- [ ] 熟練使用MOVE敘述
- [ ] 掌握四則運算指令
- [ ] 能夠使用COMPUTE進行複雜運算

**程式結構：**
- [ ] 能夠編寫結構化的程式
- [ ] 理解程式的執行流程
- [ ] 掌握錯誤處理的基本概念

### 9.2 實作能力驗證

**基礎驗證：**
- 能夠獨立編寫包含四大部門的完整程式
- 正確定義和使用各種資料型態
- 熟練進行基本的算術運算

**進階驗證：**
- 編寫中等複雜度的商業計算程式
- 使用適當的資料結構組織程式
- 加入基本的錯誤處理機制

### 9.3 常見問題與解答

**Q1：為什麼COBOL程式必須按照固定的部門順序？**
A：這是COBOL語言的設計特色，確保程式結構清晰、易於維護。編譯器需要按順序解析各部門的內容。

**Q2：PIC子句中的V和小數點有什麼區別？**
A：V表示隱含小數點，不占用儲存空間；小數點會實際儲存，占用一個字元空間。

**Q3：什麼時候使用COMPUTE，什麼時候使用ADD/SUBTRACT？**
A：簡單運算使用ADD/SUBTRACT等專用指令；複雜運算式使用COMPUTE更清晰。

**Q4：如何避免數值溢位錯誤？**
A：使用ON SIZE ERROR子句處理溢位，或在運算前檢查數值範圍。

---

## 📚 第十節：延伸學習

### 10.1 進階主題預告

下週我們將學習：
- 條件判斷和邏輯運算
- 迴圈控制結構
- 程式模組化技術
- 更複雜的資料處理

### 10.2 實務技能提升

**建議練習：**
- 多編寫小程式練習語法
- 嘗試不同的資料定義方式
- 練習閱讀和理解他人的程式碼
- 建立個人的程式碼模板庫

### 10.3 學習資源

**參考資料：**
- IBM ILE COBOL Reference Manual
- COBOL語言規範文件
- AS/400程式設計指南

**線上資源：**
- IBM Developer社群
- COBOL程式設計論壇
- 相關技術部落格

---

## 📊 本週總結

通過本週的學習，我們已經建立了COBOL程式設計的基礎知識架構。重點包括：

1. **程式結構**：掌握四大部門的組織方式
2. **資料定義**：理解各種資料型態的使用
3. **基本運算**：熟練進行數值和文字處理
4. **程式風格**：建立良好的程式撰寫習慣

下週我們將在此基礎上，學習更進階的程式邏輯控制，讓程式具備更強的處理能力。

**學習建議：**
- 多動手練習，熟悉語法細節
- 注重程式的可讀性和維護性
- 培養良好的程式設計習慣
- 為下週的進階學習做好準備

---

*本教材版權所有，僅供學習使用。如有疑問，請聯繫課程講師。*