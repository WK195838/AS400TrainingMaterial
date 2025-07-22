# 第十一週：專案管理與文件 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 理解AS/400專案開發的完整生命週期
- 掌握需求分析和系統設計的方法論
- 熟練使用版本控制和變更管理工具
- 建立完整的技術文件和使用者文件
- 實作專業的測試策略和品質保證流程
- 建立可維護和可擴展的專案架構

---

## 📋 第一節：專案生命週期管理

### 1.1 AS/400專案開發流程

#### 1.1.1 軟體開發生命週期 (SDLC)

```
AS/400 專案開發生命週期：
┌─────────────────────────────────────────┐
│ 1. 需求分析階段 (Requirements Analysis) │
│ ┌─────────────────────────────────────┐ │
│ │ ◆ 業務需求收集                     │ │
│ │ ◆ 功能需求定義                     │ │
│ │ ◆ 非功能需求分析                   │ │
│ │ ◆ 可行性評估                       │ │
│ │ ◆ 需求文件編寫                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────┐
│ 2. 系統設計階段 (System Design)         │
│ ┌─────────────────────────────────────┐ │
│ │ ◆ 系統架構設計                     │ │
│ │ ◆ 資料庫設計                       │ │
│ │ ◆ 介面設計                         │ │
│ │ ◆ 程式模組設計                     │ │
│ │ ◆ 設計文件編寫                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────┐
│ 3. 程式開發階段 (Development)           │
│ ┌─────────────────────────────────────┐ │
│ │ ◆ 程式編碼實作                     │ │
│ │ ◆ 單元測試                         │ │
│ │ ◆ 程式碼審查                       │ │
│ │ ◆ 版本控制管理                     │ │
│ │ ◆ 整合測試                         │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────┐
│ 4. 系統測試階段 (Testing)               │
│ ┌─────────────────────────────────────┐ │
│ │ ◆ 系統功能測試                     │ │
│ │ ◆ 效能測試                         │ │
│ │ ◆ 使用者驗收測試                   │ │
│ │ ◆ 安全性測試                       │ │
│ │ ◆ 測試報告編寫                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────┐
│ 5. 部署上線階段 (Deployment)            │
│ ┌─────────────────────────────────────┐ │
│ │ ◆ 生產環境準備                     │ │
│ │ ◆ 資料轉換                         │ │
│ │ ◆ 系統上線                         │ │
│ │ ◆ 使用者訓練                       │ │
│ │ ◆ 監控和支援                       │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────┐
│ 6. 維護支援階段 (Maintenance)           │
│ ┌─────────────────────────────────────┐ │
│ │ ◆ 日常運維支援                     │ │
│ │ ◆ 錯誤修正                         │ │
│ │ ◆ 功能增強                         │ │
│ │ ◆ 效能調校                         │ │
│ │ ◆ 文件更新                         │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

#### 1.1.2 專案管理框架

```cobol
      *    AS/400 專案管理最佳實務：
      *
      *    1. 專案規劃 (Project Planning)
      *       - 工作分解結構 (WBS)
      *       - 時程規劃和里程碑
      *       - 資源分配和預算
      *       - 風險識別和緩解
      *
      *    2. 團隊管理 (Team Management)
      *       - 角色和職責定義
      *       - 溝通計畫
      *       - 進度追蹤
      *       - 品質保證
      *
      *    3. 變更管理 (Change Management)
      *       - 變更控制流程
      *       - 影響評估
      *       - 版本管理
      *       - 配置管理
      *
      *    4. 品質管理 (Quality Management)
      *       - 編碼標準
      *       - 測試策略
      *       - 程式碼審查
      *       - 文件標準
```

### 1.2 需求分析和系統設計

#### 1.2.1 需求分析文件範本

```markdown
# 系統需求規格書
## System Requirements Specification (SRS)

### 1. 專案概述
**專案名稱：** 員工薪資管理系統
**專案代碼：** PAYROLL-2024
**版本：** 1.0
**日期：** 2024/02/20
**負責人：** 系統分析師

### 2. 業務需求
#### 2.1 業務目標
- 自動化薪資計算流程
- 提高薪資處理準確性
- 減少人工作業時間
- 強化薪資資料安全性

#### 2.2 使用者群組
- 人力資源部門
- 財務會計部門
- 員工自助服務
- 系統管理員

### 3. 功能需求
#### 3.1 薪資計算功能
**需求編號：** FR-001
**優先級：** 高
**描述：** 系統應能根據員工基本薪資、加班時數、各項津貼和扣款項目，自動計算月薪總額。

**輸入：**
- 員工基本資料
- 出勤記錄
- 加班申請記錄
- 津貼和扣款項目

**處理：**
- 薪資計算邏輯
- 稅額計算
- 保險費計算
- 其他扣款計算

**輸出：**
- 薪資明細表
- 薪資統計報表
- 扣繳憑單資料

#### 3.2 薪資查詢功能
**需求編號：** FR-002
**優先級：** 中
**描述：** 提供員工查詢個人薪資明細的功能。

### 4. 非功能需求
#### 4.1 效能需求
- 薪資計算：5000名員工資料在30分鐘內完成
- 查詢回應：一般查詢3秒內回應
- 報表產生：月報表5分鐘內完成

#### 4.2 安全需求
- 使用者身分驗證
- 資料存取權限控制
- 敏感資料加密
- 操作日誌記錄

#### 4.3 可用性需求
- 系統可用性：99.5%
- 維護時間：每月不超過4小時
- 恢復時間：系統故障2小時內恢復

### 5. 系統限制
- 必須在AS/400平台上執行
- 與現有人事系統整合
- 符合政府法規要求
- 預算限制：500萬台幣

### 6. 驗收標準
- 所有功能需求100%實現
- 效能需求達標
- 使用者驗收測試通過
- 安全性測試通過
```

#### 1.2.2 系統設計文件範本

```cobol
      *****************************************************************
      * 文件名稱：SYSTEM-DESIGN-SPEC                                 *
      * 文件類型：系統設計規格書                                     *
      * 專案名稱：員工薪資管理系統                                   *
      * 版本：1.0                                                    *
      * 日期：2024/02/20                                             *
      * 作者：系統架構師                                             *
      *****************************************************************
      
      *    系統架構設計
      *    
      *    1. 系統層次架構
      *       ┌─────────────────────────────┐
      *       │    展示層 (Presentation)    │
      *       │  ◆ 5250螢幕程式            │
      *       │  ◆ Web介面                 │
      *       │  ◆ 報表輸出                 │
      *       └─────────────────────────────┘
      *                      ↕
      *       ┌─────────────────────────────┐
      *       │    商業邏輯層 (Business)    │
      *       │  ◆ 薪資計算引擎            │
      *       │  ◆ 業務規則驗證            │
      *       │  ◆ 工作流程控制            │
      *       └─────────────────────────────┘
      *                      ↕
      *       ┌─────────────────────────────┐
      *       │    資料存取層 (Data)        │
      *       │  ◆ 檔案存取模組            │
      *       │  ◆ SQL存取模組             │
      *       │  ◆ 外部介面模組            │
      *       └─────────────────────────────┘
      
      *    2. 模組劃分
      *    
      *       PAYROLL系統主要模組：
      *       ├── PAYMAINT - 薪資維護模組
      *       │   ├── EMPSAL001 - 員工薪資設定
      *       │   ├── EMPSAL002 - 津貼扣款維護
      *       │   └── EMPSAL003 - 薪資結構調整
      *       │
      *       ├── PAYCALC - 薪資計算模組  
      *       │   ├── SALCALC01 - 基本薪資計算
      *       │   ├── SALCALC02 - 加班費計算
      *       │   ├── SALCALC03 - 津貼計算
      *       │   └── SALCALC04 - 扣款計算
      *       │
      *       ├── PAYQUERY - 薪資查詢模組
      *       │   ├── SALINQ001 - 個人薪資查詢
      *       │   ├── SALINQ002 - 部門薪資查詢
      *       │   └── SALINQ003 - 統計資料查詢
      *       │
      *       └── PAYRPT - 薪資報表模組
      *           ├── SALRPT001 - 薪資明細表
      *           ├── SALRPT002 - 薪資統計表
      *           └── SALRPT003 - 扣繳憑單
      
      *    3. 資料庫設計
      *    
      *       主要資料表：
      *       
      *       EMPLOYEES (員工主檔)
      *       ├── EMP_ID (員工編號) - PK
      *       ├── EMP_NAME (員工姓名)
      *       ├── DEPT_CODE (部門代碼) - FK
      *       ├── POSITION (職位)
      *       ├── HIRE_DATE (到職日期)
      *       └── STATUS (狀態)
      *       
      *       SALARY_MASTER (薪資主檔)
      *       ├── EMP_ID (員工編號) - PK, FK
      *       ├── BASIC_SALARY (基本薪資)
      *       ├── POSITION_ALLOWANCE (職務津貼)
      *       ├── MEAL_ALLOWANCE (餐費津貼)
      *       └── EFFECTIVE_DATE (生效日期)
      *       
      *       PAYROLL_TRANSACTION (薪資交易檔)
      *       ├── TRANS_ID (交易編號) - PK
      *       ├── EMP_ID (員工編號) - FK
      *       ├── PAY_PERIOD (薪資期間)
      *       ├── BASIC_PAY (基本薪資)
      *       ├── OVERTIME_PAY (加班費)
      *       ├── ALLOWANCES (津貼)
      *       ├── DEDUCTIONS (扣款)
      *       └── NET_PAY (實領薪資)
      
      *    4. 介面設計
      *    
      *       螢幕介面規格：
      *       - 標準5250畫面：24行x80列
      *       - 一致的功能鍵配置
      *       - 標準化的錯誤訊息
      *       - 多語言支援能力
      *       
      *       報表介面規格：
      *       - A4頁面格式
      *       - 標準報表標頭
      *       - 分頁控制
      *       - PDF輸出支援
```

---

## 📝 第二節：版本控制和變更管理

### 2.1 版本控制策略

#### 2.1.1 AS/400環境版本控制實作

```cobol
      *****************************************************************
      * 程式名稱：VERCTRL                                            *
      * 程式功能：版本控制管理程式                                   *
      * 作者：版本控制管理員                                         *
      * 日期：2024/02/20                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VERCTRL.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *    版本控制資料結構
       01  WS-VERSION-CONTROL.
           05  WS-PROJECT-INFO.
               10  WS-PROJECT-ID       PIC X(10).
               10  WS-PROJECT-NAME     PIC X(50).
               10  WS-PROJECT-VERSION  PIC X(10).
               10  WS-PROJECT-STATUS   PIC X(15).
                   88  STATUS-DEVELOPMENT      VALUE 'DEVELOPMENT'.
                   88  STATUS-TESTING          VALUE 'TESTING'.
                   88  STATUS-PRODUCTION       VALUE 'PRODUCTION'.
                   88  STATUS-MAINTENANCE      VALUE 'MAINTENANCE'.
               
           05  WS-VERSION-INFO.
               10  WS-MAJOR-VERSION    PIC 9(2).
               10  WS-MINOR-VERSION    PIC 9(2).
               10  WS-PATCH-VERSION    PIC 9(3).
               10  WS-BUILD-NUMBER     PIC 9(5).
               10  WS-VERSION-STRING   PIC X(15).
               
           05  WS-CHANGE-INFO.
               10  WS-CHANGE-ID        PIC X(10).
               10  WS-CHANGE-TYPE      PIC X(15).
                   88  TYPE-FEATURE            VALUE 'FEATURE'.
                   88  TYPE-BUGFIX             VALUE 'BUGFIX'.
                   88  TYPE-ENHANCEMENT        VALUE 'ENHANCEMENT'.
                   88  TYPE-HOTFIX             VALUE 'HOTFIX'.
               10  WS-CHANGE-AUTHOR    PIC X(20).
               10  WS-CHANGE-DATE      PIC X(10).
               10  WS-CHANGE-DESC      PIC X(100).
               
      *    檔案版本追蹤
       01  WS-FILE-VERSION-TABLE.
           05  WS-FILE-ENTRY OCCURS 500 TIMES 
                             INDEXED BY WS-FILE-IDX.
               10  WS-FILE-NAME        PIC X(21).
               10  WS-FILE-TYPE        PIC X(10).
                   88  TYPE-COBOL              VALUE 'COBOL'.
                   88  TYPE-DDS                VALUE 'DDS'.
                   88  TYPE-SQL                VALUE 'SQL'.
                   88  TYPE-CL                 VALUE 'CL'.
               10  WS-FILE-VERSION     PIC X(10).
               10  WS-FILE-CHECKSUM    PIC X(32).
               10  WS-FILE-SIZE        PIC 9(8) COMP.
               10  WS-FILE-TIMESTAMP   PIC X(26).
               10  WS-FILE-AUTHOR      PIC X(20).
               10  WS-FILE-STATUS      PIC X(10).
                   88  FILE-ADDED              VALUE 'ADDED'.
                   88  FILE-MODIFIED           VALUE 'MODIFIED'.
                   88  FILE-DELETED            VALUE 'DELETED'.
                   88  FILE-UNCHANGED          VALUE 'UNCHANGED'.
                   
      *    分支管理
       01  WS-BRANCH-MANAGEMENT.
           05  WS-CURRENT-BRANCH       PIC X(20).
           05  WS-BRANCH-TABLE.
               10  WS-BRANCH-ENTRY OCCURS 10 TIMES
                                   INDEXED BY WS-BRANCH-IDX.
                   15  WS-BRANCH-NAME      PIC X(20).
                   15  WS-BRANCH-TYPE      PIC X(15).
                       88  BRANCH-MAIN             VALUE 'MAIN'.
                       88  BRANCH-DEVELOP          VALUE 'DEVELOP'.
                       88  BRANCH-FEATURE          VALUE 'FEATURE'.
                       88  BRANCH-RELEASE          VALUE 'RELEASE'.
                       88  BRANCH-HOTFIX           VALUE 'HOTFIX'.
                   15  WS-BRANCH-PARENT    PIC X(20).
                   15  WS-BRANCH-CREATED   PIC X(26).
                   15  WS-BRANCH-STATUS    PIC X(10).
       
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           PERFORM INITIALIZE-VERSION-CONTROL
           PERFORM DEMONSTRATE-VERSION-OPERATIONS
           PERFORM DEMONSTRATE-BRANCH-OPERATIONS
           PERFORM DEMONSTRATE-MERGE-OPERATIONS
           PERFORM GENERATE-VERSION-REPORT.
           STOP RUN.
           
       INITIALIZE-VERSION-CONTROL.
           DISPLAY 'Initializing Version Control System...'.
           
           MOVE 'PAYROLL2024' TO WS-PROJECT-ID.
           MOVE 'Employee Payroll Management System' TO WS-PROJECT-NAME.
           MOVE '1.0.0' TO WS-PROJECT-VERSION.
           SET STATUS-DEVELOPMENT TO TRUE.
           
           MOVE 1 TO WS-MAJOR-VERSION.
           MOVE 0 TO WS-MINOR-VERSION.
           MOVE 0 TO WS-PATCH-VERSION.
           MOVE 1 TO WS-BUILD-NUMBER.
           
           PERFORM BUILD-VERSION-STRING.
           
           MOVE 'main' TO WS-CURRENT-BRANCH.
           
           DISPLAY 'Project: ' WS-PROJECT-NAME.
           DISPLAY 'Version: ' WS-VERSION-STRING.
           DISPLAY 'Current Branch: ' WS-CURRENT-BRANCH.
           
       BUILD-VERSION-STRING.
           STRING WS-MAJOR-VERSION '.'
                  WS-MINOR-VERSION '.'
                  WS-PATCH-VERSION '.'
                  WS-BUILD-NUMBER
                  DELIMITED BY SIZE
                  INTO WS-VERSION-STRING.
                  
       DEMONSTRATE-VERSION-OPERATIONS.
           DISPLAY ' '.
           DISPLAY '=== Version Control Operations ==='.
           
           PERFORM ADD-FILES-TO-VERSION-CONTROL.
           PERFORM COMMIT-CHANGES.
           PERFORM CREATE-VERSION-TAG.
           
       ADD-FILES-TO-VERSION-CONTROL.
           DISPLAY 'Adding files to version control...'.
           
      *    添加COBOL程式檔案
           SET WS-FILE-IDX TO 1.
           MOVE 'EMPMAINT.COBOL' TO WS-FILE-NAME(WS-FILE-IDX).
           SET TYPE-COBOL TO TRUE.
           MOVE '1.0.0' TO WS-FILE-VERSION(WS-FILE-IDX).
           MOVE 'ABC123DEF456' TO WS-FILE-CHECKSUM(WS-FILE-IDX).
           MOVE 15432 TO WS-FILE-SIZE(WS-FILE-IDX).
           MOVE FUNCTION CURRENT-DATE TO WS-FILE-TIMESTAMP(WS-FILE-IDX).
           MOVE 'Developer A' TO WS-FILE-AUTHOR(WS-FILE-IDX).
           SET FILE-ADDED TO TRUE.
           
           SET WS-FILE-IDX UP BY 1.
           MOVE 'EMPINQ.COBOL' TO WS-FILE-NAME(WS-FILE-IDX).
           SET TYPE-COBOL TO TRUE.
           MOVE '1.0.0' TO WS-FILE-VERSION(WS-FILE-IDX).
           MOVE 'DEF456GHI789' TO WS-FILE-CHECKSUM(WS-FILE-IDX).
           MOVE 12876 TO WS-FILE-SIZE(WS-FILE-IDX).
           MOVE FUNCTION CURRENT-DATE TO WS-FILE-TIMESTAMP(WS-FILE-IDX).
           MOVE 'Developer B' TO WS-FILE-AUTHOR(WS-FILE-IDX).
           SET FILE-ADDED TO TRUE.
           
      *    添加DDS檔案
           SET WS-FILE-IDX UP BY 1.
           MOVE 'EMPFILE.DDS' TO WS-FILE-NAME(WS-FILE-IDX).
           SET TYPE-DDS TO TRUE.
           MOVE '1.0.0' TO WS-FILE-VERSION(WS-FILE-IDX).
           MOVE 'GHI789JKL012' TO WS-FILE-CHECKSUM(WS-FILE-IDX).
           MOVE 3456 TO WS-FILE-SIZE(WS-FILE-IDX).
           MOVE FUNCTION CURRENT-DATE TO WS-FILE-TIMESTAMP(WS-FILE-IDX).
           MOVE 'Designer A' TO WS-FILE-AUTHOR(WS-FILE-IDX).
           SET FILE-ADDED TO TRUE.
           
           DISPLAY 'Files added to version control successfully.'.
           
       COMMIT-CHANGES.
           DISPLAY 'Committing changes...'.
           
           MOVE 'CHG001' TO WS-CHANGE-ID.
           SET TYPE-FEATURE TO TRUE.
           MOVE 'System Architect' TO WS-CHANGE-AUTHOR.
           MOVE '2024-02-20' TO WS-CHANGE-DATE.
           MOVE 'Initial version of payroll system' TO WS-CHANGE-DESC.
           
           ADD 1 TO WS-BUILD-NUMBER.
           PERFORM BUILD-VERSION-STRING.
           
           DISPLAY 'Commit completed:'.
           DISPLAY '  Change ID: ' WS-CHANGE-ID.
           DISPLAY '  Type: ' WS-CHANGE-TYPE.
           DISPLAY '  Author: ' WS-CHANGE-AUTHOR.
           DISPLAY '  Description: ' WS-CHANGE-DESC.
           DISPLAY '  New Version: ' WS-VERSION-STRING.
           
       CREATE-VERSION-TAG.
           DISPLAY 'Creating version tag...'.
           
      *    為重要版本建立標籤
           IF WS-PATCH-VERSION = 0
               DISPLAY 'Creating release tag: v' WS-VERSION-STRING
               PERFORM ARCHIVE-RELEASE-VERSION
           END-IF.
           
       ARCHIVE-RELEASE-VERSION.
      *    將發布版本備份到檔案庫
           DISPLAY 'Archiving release version to library...'.
           
      *    這裡可以實作實際的檔案備份邏輯
           DISPLAY 'Release archived successfully.'.
           
       DEMONSTRATE-BRANCH-OPERATIONS.
           DISPLAY ' '.
           DISPLAY '=== Branch Management Operations ==='.
           
           PERFORM CREATE-DEVELOPMENT-BRANCH.
           PERFORM CREATE-FEATURE-BRANCH.
           PERFORM SWITCH-BRANCH.
           
       CREATE-DEVELOPMENT-BRANCH.
           DISPLAY 'Creating development branch...'.
           
           SET WS-BRANCH-IDX TO 1.
           MOVE 'develop' TO WS-BRANCH-NAME(WS-BRANCH-IDX).
           SET BRANCH-DEVELOP TO TRUE.
           MOVE 'main' TO WS-BRANCH-PARENT(WS-BRANCH-IDX).
           MOVE FUNCTION CURRENT-DATE TO WS-BRANCH-CREATED(WS-BRANCH-IDX).
           MOVE 'ACTIVE' TO WS-BRANCH-STATUS(WS-BRANCH-IDX).
           
           DISPLAY 'Development branch created successfully.'.
           
       CREATE-FEATURE-BRANCH.
           DISPLAY 'Creating feature branch...'.
           
           SET WS-BRANCH-IDX TO 2.
           MOVE 'feature/salary-calc' TO WS-BRANCH-NAME(WS-BRANCH-IDX).
           SET BRANCH-FEATURE TO TRUE.
           MOVE 'develop' TO WS-BRANCH-PARENT(WS-BRANCH-IDX).
           MOVE FUNCTION CURRENT-DATE TO WS-BRANCH-CREATED(WS-BRANCH-IDX).
           MOVE 'ACTIVE' TO WS-BRANCH-STATUS(WS-BRANCH-IDX).
           
           DISPLAY 'Feature branch created successfully.'.
           
       SWITCH-BRANCH.
           DISPLAY 'Switching to feature branch...'.
           
           MOVE 'feature/salary-calc' TO WS-CURRENT-BRANCH.
           
           DISPLAY 'Current branch: ' WS-CURRENT-BRANCH.
           
       DEMONSTRATE-MERGE-OPERATIONS.
           DISPLAY ' '.
           DISPLAY '=== Merge Operations ==='.
           
           PERFORM SIMULATE-FEATURE-DEVELOPMENT.
           PERFORM MERGE-FEATURE-TO-DEVELOP.
           PERFORM MERGE-DEVELOP-TO-MAIN.
           
       SIMULATE-FEATURE-DEVELOPMENT.
           DISPLAY 'Simulating feature development...'.
           
      *    模擬在功能分支上的開發工作
           PERFORM MODIFY-FILE-IN-BRANCH.
           PERFORM COMMIT-FEATURE-CHANGES.
           
       MODIFY-FILE-IN-BRANCH.
           DISPLAY 'Modifying files in feature branch...'.
           
      *    修改薪資計算程式
           SET WS-FILE-IDX TO 1.
           MOVE 'SALCALC01.COBOL' TO WS-FILE-NAME(WS-FILE-IDX).
           SET TYPE-COBOL TO TRUE.
           MOVE '1.1.0' TO WS-FILE-VERSION(WS-FILE-IDX).
           MOVE 'XYZ789ABC123' TO WS-FILE-CHECKSUM(WS-FILE-IDX).
           MOVE 16789 TO WS-FILE-SIZE(WS-FILE-IDX).
           MOVE FUNCTION CURRENT-DATE TO WS-FILE-TIMESTAMP(WS-FILE-IDX).
           MOVE 'Developer C' TO WS-FILE-AUTHOR(WS-FILE-IDX).
           SET FILE-MODIFIED TO TRUE.
           
           DISPLAY 'File modified: ' WS-FILE-NAME(WS-FILE-IDX).
           
       COMMIT-FEATURE-CHANGES.
           DISPLAY 'Committing feature changes...'.
           
           MOVE 'CHG002' TO WS-CHANGE-ID.
           SET TYPE-FEATURE TO TRUE.
           MOVE 'Developer C' TO WS-CHANGE-AUTHOR.
           MOVE '2024-02-21' TO WS-CHANGE-DATE.
           MOVE 'Enhanced salary calculation algorithm' TO WS-CHANGE-DESC.
           
           ADD 1 TO WS-MINOR-VERSION.
           MOVE 0 TO WS-PATCH-VERSION.
           ADD 1 TO WS-BUILD-NUMBER.
           PERFORM BUILD-VERSION-STRING.
           
           DISPLAY 'Feature changes committed.'.
           
       MERGE-FEATURE-TO-DEVELOP.
           DISPLAY 'Merging feature branch to develop...'.
           DISPLAY 'Resolving conflicts (if any)...'.
           DISPLAY 'Merge completed: feature/salary-calc -> develop'.

       MERGE-DEVELOP-TO-MAIN.
           DISPLAY 'Merging develop branch to main...'.
           DISPLAY 'Running integration tests...'.
           DISPLAY 'All tests passed. Merge completed: develop -> main'.

       GENERATE-VERSION-REPORT.
           DISPLAY ' '.
           DISPLAY '========== Version Control Report =========='.
           DISPLAY 'Project: ' WS-PROJECT-NAME.
           DISPLAY 'Current Version: ' WS-VERSION-STRING.
           DISPLAY 'Current Branch: ' WS-CURRENT-BRANCH.
           DISPLAY 'Recent Changes:'.
           DISPLAY '  Change ID: ' WS-CHANGE-ID.
           DISPLAY '  Type: ' WS-CHANGE-TYPE.
           DISPLAY '  Author: ' WS-CHANGE-AUTHOR.
           DISPLAY '  Description: ' WS-CHANGE-DESC.
           DISPLAY '==========================================='.

---

## 📝 本週小結

- 本週學習了AS/400專案開發生命週期、需求分析、系統設計與文件標準。
- 熟悉了版本控制、分支管理、合併與變更追蹤的實務流程。
- 掌握了技術文件、使用者文件的撰寫與維護技巧。
- 學會了測試策略、品質保證與持續整合的專案管理方法。
- 透過實例練習，能夠建立可維護、可擴展的專案架構。

---

## 📌 課後練習

1. 請設計一份完整的系統需求規格書（SRS），針對你熟悉的業務流程。
2. 修改版本控制範例，模擬多位開發者在不同分支上協作並合併。
3. 嘗試設計一個自動化測試與持續整合（CI）流程，並撰寫相關技術文件。

---