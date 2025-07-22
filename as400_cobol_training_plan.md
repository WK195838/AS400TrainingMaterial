# AS/400 COBOL 初學者自主訓練教材規劃書

## 📋 教材總覽

### 教學目標
- 培養學員成為具備專業AS/400 COBOL開發技能的程式設計師
- 建立系統化的問題分析與解決能力
- 掌握企業級應用程式開發的實務技能

### 學習週期
**總時數：240小時（約12週，每週20小時）**

### 適用對象
- 程式設計初學者
- 具備基礎邏輯思維能力
- 希望進入大型企業系統開發領域的學員

---

## 📚 教材架構設計

### 第一階段：基礎建立（4週，80小時）

#### 第1週：AS/400系統概論
**學習目標：**
- 了解AS/400系統架構與歷史
- 掌握基本系統操作

**教學內容：**
1. **AS/400系統簡介**
   - IBM i系統演進史
   - 系統架構特色
   - 在企業中的應用場景

2. **基本系統操作**
   - 登入與基本命令
   - 程式庫概念（Library）
   - 物件管理基礎

3. **開發環境設置**
   - SEU（Source Entry Utility）使用
   - PDM（Programming Development Manager）操作
   - 編譯與執行流程

**實例練習：**
```
實例1-1：系統登入與基本命令操作
- 使用WRKOBJLCK查看物件鎖定狀態
- 使用WRKACTJOB查看活動作業

實例1-2：建立第一個程式庫
- 使用CRTLIB建立程式庫
- 使用WRKLIB查看程式庫內容
```

**驗證方式：**
- 完成系統操作檢核表
- 獨立建立開發環境

#### 第2週：COBOL語言基礎
**學習目標：**
- 理解COBOL程式結構
- 掌握基本語法規則

**教學內容：**
1. **COBOL程式結構**
   - 四大部門（DIVISION）
   - 程式碼編排規則
   - 註解與文件化

2. **資料定義**
   - WORKING-STORAGE SECTION
   - 基本資料型態
   - 數值與文字處理

3. **基本運算**
   - MOVE、ADD、SUBTRACT
   - MULTIPLY、DIVIDE
   - COMPUTE敘述

**實例練習：**
```cobol
實例2-1：薪資計算程式
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALARY-CALC.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-BASIC-SALARY    PIC 9(7)V99.
       01 WS-OVERTIME-HOURS  PIC 999.
       01 WS-TOTAL-SALARY    PIC 9(8)V99.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE 50000 TO WS-BASIC-SALARY
           MOVE 10 TO WS-OVERTIME-HOURS
           COMPUTE WS-TOTAL-SALARY = 
               WS-BASIC-SALARY + (WS-OVERTIME-HOURS * 300)
           DISPLAY 'TOTAL SALARY: ' WS-TOTAL-SALARY
           STOP RUN.
```

**驗證方式：**
- 編寫並執行5個基本運算程式
- 通過語法檢核測驗

#### 第3週：流程控制與邏輯
**學習目標：**
- 掌握條件判斷與迴圈控制
- 理解程式邏輯設計

**教學內容：**
1. **條件判斷**
   - IF-THEN-ELSE結構
   - 巢狀條件判斷
   - EVALUATE敘述

2. **迴圈控制**
   - PERFORM敘述
   - PERFORM TIMES
   - PERFORM UNTIL

3. **程式模組化**
   - SECTION與PARAGRAPH
   - 子程式呼叫
   - 參數傳遞

**實例練習：**
```cobol
實例3-1：成績等級判定程式
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM GET-SCORE
           PERFORM CALC-GRADE
           PERFORM DISPLAY-RESULT
           STOP RUN.
           
       GET-SCORE.
           DISPLAY 'ENTER SCORE: '
           ACCEPT WS-SCORE.
           
       CALC-GRADE.
           EVALUATE WS-SCORE
               WHEN 90 THRU 100
                   MOVE 'A' TO WS-GRADE
               WHEN 80 THRU 89
                   MOVE 'B' TO WS-GRADE
               WHEN 70 THRU 79
                   MOVE 'C' TO WS-GRADE
               WHEN OTHER
                   MOVE 'F' TO WS-GRADE
           END-EVALUATE.
```

**驗證方式：**
- 完成流程圖設計練習
- 編寫複合邏輯判斷程式

#### 第4週：檔案處理基礎
**學習目標：**
- 理解AS/400檔案系統
- 掌握基本檔案操作

**教學內容：**
1. **檔案系統概念**
   - 實體檔案與邏輯檔案
   - 記錄格式定義
   - 檔案描述規格

2. **檔案操作**
   - 循序讀取
   - 隨機讀取
   - 檔案更新

3. **DDS（Data Description Specifications）**
   - 基本DDS語法
   - 欄位定義
   - 關鍵字使用

**實例練習：**
```cobol
實例4-1：員工資料檔案讀取
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE
           ASSIGN TO DATABASE-EMPFILE.
           
       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-ID       PIC X(6).
           05 EMP-NAME     PIC X(20).
           05 EMP-SALARY   PIC 9(7)V99.
           
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT EMPLOYEE-FILE
           PERFORM READ-EMPLOYEE UNTIL EOF
           CLOSE EMPLOYEE-FILE
           STOP RUN.
```

**驗證方式：**
- 建立實體檔案並進行CRUD操作
- 完成檔案處理綜合練習

---

### 第二階段：進階應用（4週，80小時）

#### 第5週：資料庫整合
**學習目標：**
- 掌握DB2/400資料庫操作
- 理解SQL與COBOL整合

**教學內容：**
1. **DB2/400基礎**
   - 資料庫架構
   - 表格與索引
   - 資料完整性

2. **Embedded SQL**
   - SQL語法在COBOL中的應用
   - 游標操作
   - 異常處理

3. **效能優化**
   - 索引使用策略
   - 查詢效能調校
   - 資料存取模式

**實例練習：**
```cobol
實例5-1：客戶訂單查詢系統
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           
       01 WS-CUSTOMER-ID    PIC X(8).
       01 WS-ORDER-COUNT    PIC 9(5).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY 'ENTER CUSTOMER ID: '
           ACCEPT WS-CUSTOMER-ID
           
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-ORDER-COUNT
               FROM ORDERS
               WHERE CUSTOMER_ID = :WS-CUSTOMER-ID
           END-EXEC
           
           DISPLAY 'ORDER COUNT: ' WS-ORDER-COUNT.
```

**驗證方式：**
- 完成資料庫設計與實作
- 編寫複雜查詢程式

#### 第6週：螢幕程式設計
**學習目標：**
- 掌握互動式程式開發
- 理解使用者介面設計

**教學內容：**
1. **DSPF（Display File）**
   - 螢幕格式設計
   - 欄位屬性設定
   - 功能鍵定義

2. **互動式程式**
   - 螢幕與程式的整合
   - 輸入驗證
   - 錯誤處理

3. **使用者體驗**
   - 介面設計原則
   - 資料輸入效率
   - 錯誤訊息設計

**實例練習：**
```cobol
實例6-1：庫存管理螢幕程式
-- DDS定義
     A*%%TS SD 20240101 120000 USERID   REL-V7R3M0 5770-WDS
     A*%%EC
     A            DSPSIZ(24 80 *DS3)
     A            PRINT
     A            INDARA
     A            CF03(03 'EXIT')
     A            CF12(12 'CANCEL')
     A                                      1 30'庫存管理系統'
     A                                      3  2'商品代碼:'
     A            ITEMCODE      R  B  3 12VALUES(' ')
     A                                      4  2'商品名稱:'
     A            ITEMNAME      R  O  4 12VALUES(' ')
     A                                      5  2'目前庫存:'
     A            QUANTITY      R  O  5 12EDTCDE(1)
```

**驗證方式：**
- 設計並實作完整的螢幕程式
- 進行使用者測試

#### 第7週：報表程式設計
**學習目標：**
- 掌握報表生成技術
- 理解商業報表需求

**教學內容：**
1. **PRTF（Print File）**
   - 報表格式設計
   - 頁面控制
   - 分頁與標題

2. **報表邏輯**
   - 資料分組
   - 小計與總計
   - 條件印製

3. **進階報表**
   - 多欄位報表
   - 圖表整合
   - PDF輸出

**實例練習：**
```cobol
實例7-1：月份銷售報表
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT SALES-FILE
           OPEN OUTPUT REPORT-FILE
           PERFORM PRINT-HEADER
           PERFORM PROCESS-SALES UNTIL EOF
           PERFORM PRINT-TOTAL
           CLOSE SALES-FILE
           CLOSE REPORT-FILE
           STOP RUN.
           
       PRINT-HEADER.
           WRITE REPORT-LINE FROM HEADER-LINE1
           WRITE REPORT-LINE FROM HEADER-LINE2
           WRITE REPORT-LINE FROM HEADER-LINE3.
```

**驗證方式：**
- 完成多種格式報表設計
- 報表準確性驗證

#### 第8週：系統整合
**學習目標：**
- 理解企業系統架構
- 掌握系統間介接技術

**教學內容：**
1. **系統架構**
   - 多層式架構
   - 批次與線上處理
   - 資料流設計

2. **程式間通訊**
   - 資料佇列（Data Queue）
   - 使用者空間（User Space）
   - IPC機制

3. **外部整合**
   - FTP檔案傳輸
   - Web Services
   - JSON/XML處理

**實例練習：**
```cobol
實例8-1：資料佇列通訊程式
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-QUEUE-NAME      PIC X(10) VALUE 'MSGQUEUE'.
       01 WS-QUEUE-LIB       PIC X(10) VALUE 'QGPL'.
       01 WS-MESSAGE         PIC X(100).
       01 WS-MSG-LENGTH      PIC 9(5) COMP-5.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE 'HELLO FROM COBOL' TO WS-MESSAGE
           MOVE 17 TO WS-MSG-LENGTH
           CALL 'QSNDDTAQ' USING WS-QUEUE-NAME
                                 WS-QUEUE-LIB
                                 WS-MSG-LENGTH
                                 WS-MESSAGE.
```

**驗證方式：**
- 建立完整的系統整合方案
- 效能與穩定性測試

---

### 第三階段：專業實務（4週，80小時）

#### 第9週：錯誤處理與除錯
**學習目標：**
- 掌握專業除錯技巧
- 建立錯誤處理機制

**教學內容：**
1. **除錯技術**
   - STRDBG除錯器使用
   - 中斷點設置
   - 變數監視

2. **錯誤處理**
   - 異常捕捉機制
   - 錯誤記錄
   - 復原策略

3. **程式品質**
   - 代碼審查
   - 單元測試
   - 效能分析

**實例練習：**
```cobol
實例9-1：具備完整錯誤處理的檔案處理程式
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM OPEN-FILES
           IF FILE-ERROR = 'N'
               PERFORM PROCESS-DATA
           END-IF
           PERFORM CLOSE-FILES
           STOP RUN.
           
       OPEN-FILES.
           OPEN INPUT INPUT-FILE
           IF FILE-STATUS NOT = '00'
               MOVE 'Y' TO FILE-ERROR
               DISPLAY 'FILE OPEN ERROR: ' FILE-STATUS
           END-IF.
```

**驗證方式：**
- 完成複雜程式的除錯練習
- 建立錯誤處理標準

#### 第10週：效能優化
**學習目標：**
- 理解系統效能概念
- 掌握優化技術

**教學內容：**
1. **效能分析**
   - 系統監控工具
   - 效能瓶頸識別
   - 資源使用分析

2. **程式優化**
   - 演算法改進
   - 記憶體管理
   - I/O優化

3. **系統調校**
   - 檔案組織優化
   - 索引策略
   - 批次處理優化

**實例練習：**
```cobol
實例10-1：大量資料處理優化
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INIT-PROCESS
           PERFORM PROCESS-BATCH UNTIL END-OF-FILE
           PERFORM FINAL-PROCESS
           STOP RUN.
           
       PROCESS-BATCH.
           PERFORM READ-RECORDS 1000 TIMES
           PERFORM PROCESS-BUFFER
           PERFORM WRITE-RESULTS.
```

**驗證方式：**
- 效能測試與比較
- 優化方案實作

#### 第11週：專案管理與文件
**學習目標：**
- 掌握專案開發流程
- 理解文件化重要性

**教學內容：**
1. **開發流程**
   - 需求分析
   - 系統設計
   - 測試策略

2. **版本控制**
   - 程式碼管理
   - 變更控制
   - 發布管理

3. **文件化**
   - 技術文件撰寫
   - 使用者手冊
   - 維護文件

**實例練習：**
```
實例11-1：完整專案開發
- 需求分析文件
- 系統設計文件
- 程式規格書
- 測試計劃
- 使用者手冊
```

**驗證方式：**
- 完成小型專案開發
- 文件品質評估

#### 第12週：實戰專案
**學習目標：**
- 整合所有學習內容
- 完成專業級應用程式

**教學內容：**
1. **專案需求**
   - 企業進銷存系統
   - 包含所有學習模組
   - 真實業務場景

2. **系統功能**
   - 庫存管理
   - 訂單處理
   - 財務報表
   - 系統管理

3. **品質要求**
   - 功能完整性
   - 效能要求
   - 使用者體驗

**最終專案：企業進銷存系統**
```
系統架構：
├── 主控程式（選單系統）
├── 基本資料維護
│   ├── 商品主檔
│   ├── 客戶主檔
│   └── 供應商主檔
├── 交易處理
│   ├── 進貨管理
│   ├── 銷貨管理
│   └── 庫存調整
├── 報表系統
│   ├── 庫存報表
│   ├── 銷售報表
│   └── 財務報表
└── 系統管理
    ├── 使用者管理
    ├── 參數設定
    └── 資料備份
```

**驗證方式：**
- 功能測試
- 效能測試
- 使用者驗收
- 專案報告

---

## 📖 教材編寫原則

### 深入淺出的教學方法

#### 1. 概念導入
- **生活化比喻**：用日常生活例子解釋抽象概念
- **視覺化說明**：使用圖表、流程圖輔助理解
- **漸進式學習**：從簡單到複雜，循序漸進

#### 2. 實例設計
- **真實場景**：使用企業實際需求作為範例
- **完整案例**：從需求到實作的完整流程
- **可擴展性**：基礎例子可以延伸為複雜應用

#### 3. 實作驗證
- **動手練習**：每個概念都配有實作練習
- **段階驗證**：每個學習階段都有檢核點
- **專案整合**：最終專案整合所有學習內容

### 解決問題能力培養

#### 1. 問題分析方法
```
問題分析五步驟：
1. 問題定義 - 明確問題範圍和目標
2. 需求分析 - 分析功能和非功能需求
3. 方案設計 - 設計解決方案架構
4. 實作開發 - 編寫和測試程式
5. 驗證評估 - 確認解決方案效果
```

#### 2. 除錯思維訓練
- **系統性思考**：從整體到細節的分析方法
- **假設驗證**：提出假設並透過測試驗證
- **工具使用**：善用除錯工具和日誌分析

#### 3. 持續改進
- **反思機制**：每次練習後的反思和改進
- **同儕學習**：鼓勵討論和經驗分享
- **最佳實務**：學習業界最佳實務案例

---

## 📊 學習評估機制

### 階段性評估

#### 第一階段評估（基礎建立）
- **理論測驗**：COBOL語法和概念測驗（30%）
- **實作練習**：完成指定練習題（50%）
- **專題作業**：小型應用程式開發（20%）

#### 第二階段評估（進階應用）
- **技術測驗**：資料庫和系統整合測驗（25%）
- **程式設計**：中等複雜度程式開發（45%）
- **系統分析**：需求分析和設計文件（30%）

#### 第三階段評估（專業實務）
- **最終專案**：企業級應用系統開發（60%）
- **技術報告**：專案技術文件和報告（25%）
- **口試評估**：專案簡報和技術問答（15%）

### 能力檢核標準

#### 初級能力（完成第一階段）
- ✅ 能夠編寫基本COBOL程式
- ✅ 理解AS/400系統基本操作
- ✅ 掌握基本檔案處理技術
- ✅ 具備基本除錯能力

#### 中級能力（完成第二階段）
- ✅ 能夠開發資料庫應用程式
- ✅ 掌握螢幕和報表程式設計
- ✅ 理解系統整合概念
- ✅ 具備系統分析能力

#### 高級能力（完成第三階段）
- ✅ 能夠獨立開發企業級應用
- ✅ 掌握效能優化技術
- ✅ 具備專案管理能力
- ✅ 能夠指導其他開發人員

---

## 📚 輔助學習資源

### 參考資料
1. **IBM文件**
   - ILE COBOL Reference
   - DB2 for i SQL Reference
   - System i Programming Guide

2. **開發工具**
   - RDi（Rational Developer for i）
   - WDSC（WebSphere Development Studio Client）
   - 免費的5250模擬器

3. **線上資源**
   - IBM Developer社群
   - COBOL程式設計論壇
   - AS/400技術部落格

### 實習環境
- **雲端AS/400環境**：提供練習用的系統環境
- **範例資料庫**：包含完整的企業資料結構
- **開發工具**：完整的開發和除錯工具

### 持續學習路徑
```
基礎認證 → 進階認證 → 專業認證 → 架構師認證
    ↓           ↓           ↓           ↓
  3個月       6個月       12個月      24個月
```

---

## 🎯 成功指標

### 學習成果
- **技術能力**：能夠獨立開發維護AS/400 COBOL應用程式
- **解決問題**：具備系統性分析和解決技術問題的能力
- **職場競爭力**：達到企業初級AS/400程式設計師的技能要求

### 就業前景
- **目標職位**：AS/400程式設計師、系統分析師、技術支援工程師
- **薪資水平**：初級工程師月薪45,000-60,000元
- **發展路徑**：資深工程師 → 系統架構師 → 技術主管

### 持續成長
- **技術深化**：持續學習新技術和工具
- **領域擴展**：學習相關技術如Java、Web開發
- **管理能力**：培養專案管理和團隊領導能力

---

## 📝 結語

這份教材規劃書採用系統化的教學方法，從基礎概念到專業實務，循序漸進地培養學員的AS/400 COBOL開發能力。透過大量的實例練習和專案實作，確保學員不僅掌握技術知識，更具備解決實際問題的能力。

**成功關鍵：**
- 持續練習，熟能生巧
- 注重實務，解決真實問題
- 系統思考，建立完整知識架構
- 持續學習，跟上技術發展

透過這套完整的訓練計劃，初學者將能夠在12週內成長為具備專業技能的AS/400 COBOL程式設計師，為未來的職業發展奠定堅實基礎。