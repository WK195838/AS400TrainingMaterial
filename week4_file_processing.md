# 第四週：檔案處理基礎 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 理解AS/400檔案系統的架構和特色
- 掌握實體檔案和邏輯檔案的概念與應用
- 熟練使用DDS定義檔案結構
- 進行各種檔案操作（讀取、寫入、更新、刪除）
- 處理檔案錯誤和異常狀況
- 設計有效的檔案存取策略

---

## 🗂️ 第一節：AS/400檔案系統概論

### 1.1 檔案系統架構

AS/400檔案系統是一個高度整合的資料庫檔案系統，與其他系統不同之處：

#### 1.1.1 檔案系統特色

```
AS/400檔案系統特點：
┌─────────────────────────────────────┐
│ 1. 物件導向設計                    │
│    - 所有檔案都是物件              │
│    - 統一的物件管理機制            │
│                                     │
│ 2. 關聯式資料庫整合                │
│    - 檔案即是資料表                │
│    - 支援SQL和傳統檔案存取         │
│                                     │
│ 3. 記錄層級存取                    │
│    - 支援記錄層級的鎖定            │
│    - 高效的並行存取控制            │
│                                     │
│ 4. 多重存取路徑                    │
│    - 實體檔案和邏輯檔案分離        │
│    - 靈活的資料檢視                │
└─────────────────────────────────────┘
```

#### 1.1.2 檔案類型分類

```cobol
      *    AS/400檔案類型說明
      *
      *    1. 實體檔案 (Physical File - PF)
      *       - 實際儲存資料的檔案
      *       - 定義記錄格式和欄位結構
      *       - 每個實體檔案只有一種記錄格式
      *
      *    2. 邏輯檔案 (Logical File - LF)
      *       - 提供資料的不同檢視
      *       - 不實際儲存資料
      *       - 可以合併多個實體檔案
      *       - 可以提供不同的排序和選擇
      *
      *    3. 來源檔案 (Source File)
      *       - 儲存程式原始碼
      *       - 如QCBLLESRC, QDDSSRC等
      *
      *    4. 印表檔案 (Printer File - PRTF)
      *       - 定義報表格式
      *       - 控制列印輸出
```

### 1.2 記錄格式概念

#### 1.2.1 記錄格式定義

```cobol
      *    記錄格式是AS/400檔案的基本結構單位
      *    每個檔案至少包含一個記錄格式
      
      *    範例：員工主檔記錄格式
      *    ┌──────────────────────────────────┐
      *    │ 記錄格式名稱：EMPFMT             │
      *    ├──────────┬─────────┬─────────────┤
      *    │ 欄位名稱 │ 型態    │ 長度        │
      *    ├──────────┼─────────┼─────────────┤
      *    │ EMPID    │ CHAR    │ 6           │
      *    │ EMPNAME  │ CHAR    │ 20          │
      *    │ DEPT     │ CHAR    │ 4           │
      *    │ SALARY   │ DECIMAL │ 7,2         │
      *    │ HIREDATE │ DATE    │ 10          │
      *    └──────────┴─────────┴─────────────┘
```

#### 1.2.2 欄位屬性

```cobol
      *    欄位屬性說明：
      *
      *    資料型態：
      *    - A：字元型態 (Alphanumeric)
      *    - P：壓縮數值 (Packed Decimal)
      *    - S：區域數值 (Zoned Decimal)
      *    - B：二進位 (Binary)
      *    - F：浮點數 (Float)
      *    - L：日期 (Date)
      *    - T：時間 (Time)
      *    - Z：時間戳記 (Timestamp)
      *
      *    欄位屬性：
      *    - 必填欄位 (Required)
      *    - 預設值 (Default Value)
      *    - 檢查限制 (Check Constraint)
      *    - 參考完整性 (Referential Integrity)
```

---

## 📄 第二節：DDS (Data Description Specifications)

### 2.1 DDS基本概念

DDS是AS/400系統中定義檔案、螢幕和報表格式的語言：

#### 2.1.1 DDS語法結構

```
DDS記錄格式：
位置   內容
1-5    序號 (可選)
6      表單類型 (A=欄位, K=關鍵字, R=記錄)
7      註解指示符 (*)
8-16   名稱 (欄位名稱、記錄格式名稱等)
17     參考符號 (R=參考, 空白=定義)
18     資料型態或長度
19-29  長度和小數位數
30-44  用法 (I=輸入, O=輸出, B=兩者)
45-80  關鍵字和參數
```

#### 2.1.2 實體檔案DDS範例

```dds
      *%%TS RD 20240120 100000 USER1    REL-V7R3M0 5770-SS1
      *%%EC
      *
      * 員工主檔實體檔案定義
      *
     A*%%TS RD 20240120 100000 USER1    REL-V7R3M0 5770-SS1
     A*%%EC
     A                                      UNIQUE
     A          R EMPFMT                    TEXT('員工主檔記錄格式')
     A*%%TS RD 20240120 100000 USER1    REL-V7R3M0 5770-SS1
     A*%%EC
     A            EMPID          6A         TEXT('員工編號')
     A                                      COLHDG('員工' '編號')
     A            EMPNAME       20A         TEXT('員工姓名')
     A                                      COLHDG('員工姓名')
     A            DEPT           4A         TEXT('部門代碼')
     A                                      COLHDG('部門')
     A                                      VALUES('IT' 'HR' 'SALE' 'FIN')
     A            POSITION      15A         TEXT('職位')
     A                                      COLHDG('職位')
     A            SALARY         7P 2       TEXT('月薪')
     A                                      COLHDG('月薪')
     A                                      EDTCDE(1)
     A            HIREDATE       L          TEXT('到職日期')
     A                                      COLHDG('到職' '日期')
     A                                      DATFMT(*ISO)
     A            STATUS         1A         TEXT('狀態')
     A                                      COLHDG('狀態')
     A                                      VALUES('A' 'I' 'R')
     A                                      DFT('A')
     A          K EMPID
```

### 2.2 邏輯檔案DDS

#### 2.2.1 簡單邏輯檔案

```dds
      *
      * 按部門排序的員工邏輯檔案
      *
     A*%%TS RD 20240120 110000 USER1    REL-V7R3M0 5770-SS1
     A*%%EC
     A          R EMPFMT                    PFILE(EMPMASTER)
     A          K DEPT
     A          K EMPNAME
```

#### 2.2.2 多重關鍵字邏輯檔案

```dds
      *
      * 按薪資範圍篩選的員工邏輯檔案
      *
     A*%%TS RD 20240120 120000 USER1    REL-V7R3M0 5770-SS1
     A*%%EC
     A          R EMPFMT                    PFILE(EMPMASTER)
     A*                                     TEXT('高薪員工檢視')
     A            EMPID
     A            EMPNAME
     A            DEPT
     A            SALARY
     A          S SALARY       COMP(GT 50000.00)
     A          K SALARY
     A          K EMPID
```

#### 2.2.3 多檔案邏輯檔案 (Join)

```dds
      *
      * 員工和部門資料聯結檔案
      *
     A*%%TS RD 20240120 130000 USER1    REL-V7R3M0 5770-SS1
     A*%%EC
     A          R EMPJOIN                   JFILE(EMPMASTER DEPTMASTER)
     A*                                     TEXT('員工部門聯結檔案')
     A            EMPID
     A            EMPNAME
     A            DEPT
     A            DEPTNAME
     A            MANAGER
     A          J                          JOIN(EMPMASTER DEPTMASTER)
     A          J                          JFLD(DEPT DEPT)
     A          K DEPT
     A          K EMPNAME
```

### 2.3 DDS關鍵字

#### 2.3.1 檔案層級關鍵字

```dds
      * 常用檔案層級關鍵字
      *
      * UNIQUE          - 主鍵唯一性約束
      * REF(檔案名)     - 參考檔案定義
      * TEXT('說明')    - 檔案說明
      * ALTSEQ(表格名)  - 替代排序表格
      * FCFO            - 先進先出處理
      * LIFO            - 後進先出處理
      * FIFO            - 先進先出處理
```

#### 2.3.2 記錄層級關鍵字

```dds
      * 記錄層級關鍵字
      *
      * TEXT('說明')    - 記錄格式說明
      * PFILE(檔案名)   - 實體檔案名稱
      * FORMAT(格式名)  - 記錄格式名稱
```

#### 2.3.3 欄位層級關鍵字

```dds
      * 欄位層級關鍵字範例
      *
     A            CUSTNO         6P 0       TEXT('客戶編號')
     A                                      COLHDG('客戶' '編號')
     A                                      EDTCDE(4)
     A                                      ALIAS(CUSTOMER_NUMBER)
     A            CUSTNAME      30A         TEXT('客戶名稱')
     A                                      COLHDG('客戶名稱')
     A            CREDITLIMIT    8P 2       TEXT('信用額度')
     A                                      COLHDG('信用額度')
     A                                      DFT(10000.00)
     A                                      RANGE(0 999999.99)
     A            PHONE         15A         TEXT('電話號碼')
     A                                      COLHDG('電話')
     A                                      CMP(NN)
     A            EMAIL         50A         TEXT('電子郵件')
     A                                      COLHDG('電子郵件')
     A            REGDATE        L          TEXT('註冊日期')
     A                                      COLHDG('註冊' '日期')
     A                                      DFT(%DATE)
     A                                      DATFMT(*ISO)
```

---

## 🔄 第三節：基本檔案操作

### 3.1 檔案定義在COBOL中

#### 3.1.1 FILE-CONTROL定義

```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    員工主檔定義
           SELECT EMPLOYEE-FILE
               ASSIGN TO DATABASE-EMPMASTER
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS EMP-ID
               FILE STATUS IS WS-EMP-FILE-STATUS.
               
      *    客戶主檔定義
           SELECT CUSTOMER-FILE
               ASSIGN TO DATABASE-CUSTMASTER
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CUST-FILE-STATUS.
               
      *    交易明細檔定義
           SELECT TRANSACTION-FILE
               ASSIGN TO DATABASE-TRANSFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TRANS-FILE-STATUS.
```

#### 3.1.2 檔案描述 (FD)

```cobol
       DATA DIVISION.
       FILE SECTION.
       
      *    員工主檔檔案描述
       FD  EMPLOYEE-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 60 CHARACTERS
           DATA RECORD IS EMPLOYEE-RECORD.
       01  EMPLOYEE-RECORD.
           COPY EMPFMT OF EMPMASTER.
           
      *    客戶主檔檔案描述
       FD  CUSTOMER-FILE
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS CUSTOMER-RECORD.
       01  CUSTOMER-RECORD.
           05  CUST-ID            PIC X(6).
           05  CUST-NAME          PIC X(30).
           05  CUST-ADDRESS       PIC X(50).
           05  CUST-PHONE         PIC X(15).
           05  CUST-CREDIT-LIMIT  PIC 9(8)V99.
           
      *    交易明細檔檔案描述
       FD  TRANSACTION-FILE
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS TRANSACTION-RECORD.
       01  TRANSACTION-RECORD.
           05  TRANS-ID           PIC X(10).
           05  TRANS-DATE         PIC X(10).
           05  CUST-ID            PIC X(6).
           05  PRODUCT-ID         PIC X(8).
           05  QUANTITY           PIC 9(5).
           05  UNIT-PRICE         PIC 9(6)V99.
           05  TOTAL-AMOUNT       PIC 9(8)V99.
```

### 3.2 檔案狀態處理

#### 3.2.1 檔案狀態碼定義

```cobol
       WORKING-STORAGE SECTION.
       
      *    檔案狀態變數
       01  WS-FILE-STATUS-CODES.
           05  WS-EMP-FILE-STATUS    PIC XX.
               88  EMP-FILE-SUCCESS        VALUE '00'.
               88  EMP-FILE-EOF            VALUE '10'.
               88  EMP-FILE-NOT-FOUND      VALUE '23'.
               88  EMP-FILE-DUPLICATE      VALUE '22'.
               88  EMP-FILE-ERROR          VALUE '24' '30' '34' '35'
                                                 '37' '38' '39' '41'
                                                 '42' '43' '44' '46'
                                                 '47' '48' '49' '90'
                                                 '91' '92' '93' '94'
                                                 '95' '96' '97' '98'
                                                 '99'.
           
           05  WS-CUST-FILE-STATUS   PIC XX.
               88  CUST-FILE-SUCCESS       VALUE '00'.
               88  CUST-FILE-EOF           VALUE '10'.
               88  CUST-FILE-NOT-FOUND     VALUE '23'.
               
           05  WS-TRANS-FILE-STATUS  PIC XX.
               88  TRANS-FILE-SUCCESS      VALUE '00'.
               88  TRANS-FILE-EOF          VALUE '10'.
```

#### 3.2.2 檔案錯誤處理子程式

```cobol
       CHECK-FILE-STATUS.
           EVALUATE TRUE
               WHEN EMP-FILE-SUCCESS
                   CONTINUE
               WHEN EMP-FILE-EOF
                   DISPLAY 'INFO: 檔案結束'
               WHEN EMP-FILE-NOT-FOUND
                   DISPLAY 'ERROR: 記錄不存在'
               WHEN EMP-FILE-DUPLICATE
                   DISPLAY 'ERROR: 重複的鍵值'
               WHEN EMP-FILE-ERROR
                   DISPLAY 'ERROR: 檔案錯誤，狀態碼: ' WS-EMP-FILE-STATUS
               WHEN OTHER
                   DISPLAY 'WARNING: 未知狀態碼: ' WS-EMP-FILE-STATUS
           END-EVALUATE.
```

### 3.3 檔案開啟和關閉

#### 3.3.1 檔案開啟

```cobol
       OPEN-FILES.
      *    開啟員工主檔進行讀取和更新
           OPEN I-O EMPLOYEE-FILE.
           IF NOT EMP-FILE-SUCCESS
               DISPLAY 'ERROR: 無法開啟員工主檔'
               DISPLAY '檔案狀態: ' WS-EMP-FILE-STATUS
               PERFORM ABEND-PROGRAM
           END-IF.
           
      *    開啟客戶主檔進行讀取
           OPEN INPUT CUSTOMER-FILE.
           IF NOT CUST-FILE-SUCCESS
               DISPLAY 'ERROR: 無法開啟客戶主檔'
               DISPLAY '檔案狀態: ' WS-CUST-FILE-STATUS
               PERFORM ABEND-PROGRAM
           END-IF.
           
      *    開啟交易檔進行寫入
           OPEN OUTPUT TRANSACTION-FILE.
           IF NOT TRANS-FILE-SUCCESS
               DISPLAY 'ERROR: 無法開啟交易檔'
               DISPLAY '檔案狀態: ' WS-TRANS-FILE-STATUS
               PERFORM ABEND-PROGRAM
           END-IF.
           
           DISPLAY 'INFO: 所有檔案開啟成功'.
```

#### 3.3.2 檔案關閉

```cobol
       CLOSE-FILES.
           CLOSE EMPLOYEE-FILE.
           IF NOT EMP-FILE-SUCCESS
               DISPLAY 'WARNING: 員工主檔關閉異常'
               DISPLAY '檔案狀態: ' WS-EMP-FILE-STATUS
           END-IF.
           
           CLOSE CUSTOMER-FILE.
           IF NOT CUST-FILE-SUCCESS
               DISPLAY 'WARNING: 客戶主檔關閉異常'
               DISPLAY '檔案狀態: ' WS-CUST-FILE-STATUS
           END-IF.
           
           CLOSE TRANSACTION-FILE.
           IF NOT TRANS-FILE-SUCCESS
               DISPLAY 'WARNING: 交易檔關閉異常'
               DISPLAY '檔案狀態: ' WS-TRANS-FILE-STATUS
           END-IF.
           
           DISPLAY 'INFO: 所有檔案關閉完成'.
```

---

## 📖 第四節：循序檔案處理

### 4.1 循序讀取

#### 4.1.1 基本循序讀取

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RECORD-COUNT       PIC 9(6) VALUE 0.
       01  WS-TOTAL-SALARY       PIC 9(10)V99 VALUE 0.
       01  WS-AVERAGE-SALARY     PIC 9(8)V99.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM OPEN-FILES
           PERFORM READ-ALL-EMPLOYEES
           PERFORM CALCULATE-STATISTICS
           PERFORM CLOSE-FILES
           STOP RUN.
           
       READ-ALL-EMPLOYEES.
           PERFORM READ-EMPLOYEE-RECORD
           PERFORM UNTIL CUST-FILE-EOF
               PERFORM PROCESS-EMPLOYEE-RECORD
               PERFORM READ-EMPLOYEE-RECORD
           END-PERFORM.
           
       READ-EMPLOYEE-RECORD.
           READ EMPLOYEE-FILE
               AT END
                   SET CUST-FILE-EOF TO TRUE
               NOT AT END
                   CONTINUE
           END-READ.
           
       PROCESS-EMPLOYEE-RECORD.
           ADD 1 TO WS-RECORD-COUNT.
           ADD EMP-SALARY TO WS-TOTAL-SALARY.
           
           DISPLAY 'Employee: ' EMP-ID ' ' EMP-NAME 
                   ' Salary: ' EMP-SALARY.
           
       CALCULATE-STATISTICS.
           IF WS-RECORD-COUNT > 0
               COMPUTE WS-AVERAGE-SALARY = 
                   WS-TOTAL-SALARY / WS-RECORD-COUNT
               DISPLAY ' '
               DISPLAY '========== 統計結果 =========='
               DISPLAY '員工總數: ' WS-RECORD-COUNT
               DISPLAY '薪資總額: ' WS-TOTAL-SALARY
               DISPLAY '平均薪資: ' WS-AVERAGE-SALARY
           ELSE
               DISPLAY 'INFO: 沒有員工記錄'
           END-IF.
```

#### 4.1.2 條件篩選讀取

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-HIGH-SALARY-COUNT  PIC 9(4) VALUE 0.
       01  WS-DEPT-COUNT.
           05  WS-IT-COUNT       PIC 9(4) VALUE 0.
           05  WS-HR-COUNT       PIC 9(4) VALUE 0.
           05  WS-SALES-COUNT    PIC 9(4) VALUE 0.
           05  WS-FINANCE-COUNT  PIC 9(4) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM OPEN-FILES
           PERFORM PROCESS-FILTERED-EMPLOYEES
           PERFORM DISPLAY-SUMMARY
           PERFORM CLOSE-FILES
           STOP RUN.
           
       PROCESS-FILTERED-EMPLOYEES.
           PERFORM READ-EMPLOYEE-RECORD
           PERFORM UNTIL CUST-FILE-EOF
               PERFORM ANALYZE-EMPLOYEE-DATA
               PERFORM READ-EMPLOYEE-RECORD
           END-PERFORM.
           
       ANALYZE-EMPLOYEE-DATA.
      *    統計高薪員工
           IF EMP-SALARY > 60000
               ADD 1 TO WS-HIGH-SALARY-COUNT
               DISPLAY 'High Salary: ' EMP-NAME ' - ' EMP-SALARY
           END-IF.
           
      *    按部門統計
           EVALUATE EMP-DEPT
               WHEN 'IT'
                   ADD 1 TO WS-IT-COUNT
               WHEN 'HR'
                   ADD 1 TO WS-HR-COUNT
               WHEN 'SALE'
                   ADD 1 TO WS-SALES-COUNT
               WHEN 'FIN'
                   ADD 1 TO WS-FINANCE-COUNT
               WHEN OTHER
                   DISPLAY 'WARNING: 未知部門 ' EMP-DEPT 
                           ' for employee ' EMP-ID
           END-EVALUATE.
           
       DISPLAY-SUMMARY.
           DISPLAY ' '
           DISPLAY '========== 部門統計 =========='
           DISPLAY 'IT部門: ' WS-IT-COUNT ' 人'
           DISPLAY 'HR部門: ' WS-HR-COUNT ' 人'
           DISPLAY '業務部門: ' WS-SALES-COUNT ' 人'
           DISPLAY '財務部門: ' WS-FINANCE-COUNT ' 人'
           DISPLAY '高薪員工: ' WS-HIGH-SALARY-COUNT ' 人'
```

### 4.2 循序寫入

#### 4.2.1 建立新檔案

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NEW-EMPLOYEE.
           05  WS-NEW-EMP-ID     PIC X(6).
           05  WS-NEW-EMP-NAME   PIC X(20).
           05  WS-NEW-DEPT       PIC X(4).
           05  WS-NEW-POSITION   PIC X(15).
           05  WS-NEW-SALARY     PIC 9(7)V99.
           05  WS-NEW-HIRE-DATE  PIC X(10).
           05  WS-NEW-STATUS     PIC X.
       
       01  WS-INPUT-MORE         PIC X.
           88  MORE-INPUT              VALUE 'Y'.
           88  NO-MORE-INPUT           VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM OPEN-OUTPUT-FILE
           PERFORM CREATE-EMPLOYEE-RECORDS
           PERFORM CLOSE-FILES
           STOP RUN.
           
       OPEN-OUTPUT-FILE.
           OPEN OUTPUT EMPLOYEE-FILE.
           IF NOT EMP-FILE-SUCCESS
               DISPLAY 'ERROR: 無法開啟輸出檔案'
               STOP RUN
           END-IF.
           
       CREATE-EMPLOYEE-RECORDS.
           MOVE 'Y' TO WS-INPUT-MORE.
           PERFORM UNTIL NO-MORE-INPUT
               PERFORM GET-EMPLOYEE-INPUT
               IF MORE-INPUT
                   PERFORM WRITE-EMPLOYEE-RECORD
               END-IF
           END-PERFORM.
           
       GET-EMPLOYEE-INPUT.
           DISPLAY ' '
           DISPLAY '=== 新增員工資料 ==='.
           DISPLAY '員工編號 (6位): '.
           ACCEPT WS-NEW-EMP-ID.
           
           IF WS-NEW-EMP-ID = SPACES
               MOVE 'N' TO WS-INPUT-MORE
               EXIT PARAGRAPH
           END-IF.
           
           DISPLAY '員工姓名: '.
           ACCEPT WS-NEW-EMP-NAME.
           DISPLAY '部門代碼 (IT/HR/SALE/FIN): '.
           ACCEPT WS-NEW-DEPT.
           DISPLAY '職位: '.
           ACCEPT WS-NEW-POSITION.
           DISPLAY '月薪: '.
           ACCEPT WS-NEW-SALARY.
           DISPLAY '到職日期 (YYYY-MM-DD): '.
           ACCEPT WS-NEW-HIRE-DATE.
           MOVE 'A' TO WS-NEW-STATUS.
           
           DISPLAY '是否繼續輸入? (Y/N): '.
           ACCEPT WS-INPUT-MORE.
           
       WRITE-EMPLOYEE-RECORD.
           MOVE WS-NEW-EMP-ID TO EMP-ID.
           MOVE WS-NEW-EMP-NAME TO EMP-NAME.
           MOVE WS-NEW-DEPT TO EMP-DEPT.
           MOVE WS-NEW-POSITION TO EMP-POSITION.
           MOVE WS-NEW-SALARY TO EMP-SALARY.
           MOVE WS-NEW-HIRE-DATE TO EMP-HIRE-DATE.
           MOVE WS-NEW-STATUS TO EMP-STATUS.
           
           WRITE EMPLOYEE-RECORD
               INVALID KEY
                   DISPLAY 'ERROR: 重複的員工編號 ' EMP-ID
               NOT INVALID KEY
                   DISPLAY 'INFO: 員工記錄新增成功 ' EMP-ID
           END-WRITE.
```

---

## 🎯 第五節：隨機檔案存取

### 5.1 隨機讀取

#### 5.1.1 按鍵值查詢

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SEARCH-KEY         PIC X(6).
       01  WS-CONTINUE-SEARCH    PIC X.
           88  CONTINUE-SEARCH         VALUE 'Y'.
           88  STOP-SEARCH             VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM OPEN-FILES
           PERFORM EMPLOYEE-INQUIRY
           PERFORM CLOSE-FILES
           STOP RUN.
           
       EMPLOYEE-INQUIRY.
           MOVE 'Y' TO WS-CONTINUE-SEARCH.
           PERFORM UNTIL STOP-SEARCH
               PERFORM GET-SEARCH-KEY
               IF CONTINUE-SEARCH
                   PERFORM SEARCH-EMPLOYEE
               END-IF
           END-PERFORM.
           
       GET-SEARCH-KEY.
           DISPLAY ' '
           DISPLAY '=== 員工查詢系統 ==='.
           DISPLAY '請輸入員工編號 (空白結束): '.
           ACCEPT WS-SEARCH-KEY.
           
           IF WS-SEARCH-KEY = SPACES
               SET STOP-SEARCH TO TRUE
           END-IF.
           
       SEARCH-EMPLOYEE.
           MOVE WS-SEARCH-KEY TO EMP-ID.
           
           READ EMPLOYEE-FILE
               KEY IS EMP-ID
               INVALID KEY
                   DISPLAY 'ERROR: 員工編號 ' WS-SEARCH-KEY ' 不存在'
               NOT INVALID KEY
                   PERFORM DISPLAY-EMPLOYEE-INFO
           END-READ.
           
       DISPLAY-