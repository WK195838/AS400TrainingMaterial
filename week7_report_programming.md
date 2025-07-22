# 第七週：報表程式設計 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 理解AS/400報表系統的架構和設計原理
- 掌握PRTF（Print File）的設計和實作技術
- 熟練使用各種報表格式化和控制技巧
- 設計專業的商業報表和統計分析
- 實作複雜的分組、小計和總計邏輯
- 開發多樣化的報表輸出格式和分發機制

---

## 📊 第一節：報表程式設計概論

### 1.1 AS/400報表系統架構

#### 1.1.1 報表系統組成要素

```
AS/400 報表系統架構：
┌─────────────────────────────────────────┐
│ 輸出介面層 (Output Interface Layer)     │
│ ┌─────────────────────────────────────┐ │
│ │ 印表機輸出                         │ │
│ │ - 實體印表機列印                   │ │
│ │ - Spool檔案管理                    │ │
│ │ - 網路印表機支援                   │ │
│ └─────────────────────────────────────┘ │
│ ┌─────────────────────────────────────┐ │
│ │ 電子化輸出                         │ │
│ │ - PDF檔案產生                      │ │
│ │ - Excel格式輸出                    │ │
│ │ - 電子郵件發送                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↕ 格式控制
┌─────────────────────────────────────────┐
│ 列印檔案層 (Print File Layer - PRTF)   │
│ ┌─────────────────────────────────────┐ │
│ │ DDS定義報表格式                    │ │
│ │ - 頁面佈局控制                     │ │
│ │ - 欄位格式定義                     │ │
│ │ │ 分頁和換行控制                   │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↕ 資料處理
┌─────────────────────────────────────────┐
│ 應用程式層 (Application Program Layer) │
│ ┌─────────────────────────────────────┐ │
│ │ COBOL報表程式                      │ │
│ │ - 資料擷取和整理                   │ │
│ │ - 商業邏輯處理                     │ │
│ │ - 分組和統計計算                   │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

#### 1.1.2 報表設計原則

```cobol
      *    專業報表設計的核心原則：
      *
      *    1. 資訊呈現 (Information Presentation)
      *       - 清晰的標題和欄位標頭
      *       - 合理的資料分組
      *       - 適當的小計和總計
      *       - 一致的格式化標準
      *
      *    2. 版面配置 (Layout Design)
      *       - 充分利用頁面空間
      *       - 適當的邊界和間距
      *       - 對齊和視覺平衡
      *       - 分頁邏輯合理
      *
      *    3. 使用者需求 (User Requirements)
      *       - 符合商業需求
      *       - 支援決策制定
      *       - 易於理解和使用
      *       - 適合列印和電子檢視
      *
      *    4. 效能考量 (Performance Considerations)
      *       - 高效的資料擷取
      *       - 最佳化的排序邏輯
      *       - 適當的記憶體使用
      *       - 合理的執行時間
```

### 1.2 列印檔案基本概念

#### 1.2.1 PRTF檔案特色

```dds
      *    AS/400 列印檔案特色：
      *
      *    1. 頁面控制 (Page Control)
      *       - SPACEA/SPACEB: 換行控制
      *       - SKIPB/SKIPA: 跳行控制  
      *       - PAGNBR: 頁碼控制
      *
      *    2. 欄位格式化 (Field Formatting)
      *       - EDTCDE: 編輯碼
      *       - EDTWRD: 編輯字
      *       - DATE/TIME: 日期時間格式
      *
      *    3. 條件控制 (Conditional Control)
      *       - 指示器控制列印
      *       - 條件式換行
      *       - 動態內容顯示
      *
      *    4. 溢流處理 (Overflow Handling)
      *       - 自動分頁
      *       - 頁尾處理
      *       - 連續報表支援
```

#### 1.2.2 DDS關鍵字分類

```dds
      * 列印檔案DDS關鍵字分類
      *
      * 檔案層級關鍵字：
      * - PAGESIZE(66 132)     頁面大小設定
      * - FOLD                 摺疊列印
      * - FLTFIXDEC            浮點數處理
      * - INDTXT               指示器文字
      *
      * 記錄層級關鍵字：
      * - SPACEA(1)            列印後換行
      * - SPACEB(2)            列印前換行  
      * - SKIPB(1)             列印前跳行
      * - SKIPA(99)            列印後跳行
      * - OVERLAY              疊印
      *
      * 欄位層級關鍵字：
      * - EDTCDE(J)            編輯碼
      * - EDTWRD(' . . ')      編輯字
      * - PAGNBR               頁碼
      * - DATE                 系統日期
      * - TIME                 系統時間
```

---

## 📄 第二節：基本報表格式設計

### 2.1 簡單清單報表

#### 2.1.1 員工清單報表DDS

```dds
      *%%TS PD 20240130 100000 USER1    REL-V7R3M0 5770-WDS
      *%%EC
      *
      * 員工清單報表格式定義
      *
     A*%%TS PD 20240130 100000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                      PAGESIZE(66 132)
     A                                      FOLD
     A            R HEADING                 SPACEB(1)
     A*%%TS PD 20240130 100000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                      SKIPA(1)
     A                                 1  1'1'
     A                                 1 50'員工清單報表'
     A                                 1 50DSPATR(UL)
     A                                 1120'頁次:'
     A                                 1126PAGNBR
     A                                 1126EDTCDE(4)
     A                                 3  1'報表日期:'
     A                                 3 11DATE
     A                                 3 11EDTCDE(Y)
     A                                 3120'列印時間:'
     A                                 3130TIME
     A            COMPNAME      30A  O  5  1
     A                                 7  1'員工編號'
     A                                 7 10'員工姓名'
     A                                 7 31'部門'
     A                                 7 41'職位'
     A                                 7 57'薪資'
     A                                 7 70'到職日期'
     A                                 7 85'狀態'
     A                                 7 95'電話'
     A                                 8  1'--------'
     A                                 8 10'--------------------'
     A                                 8 31'--------'
     A                                 8 41'---------------'
     A                                 8 57'----------'
     A                                 8 70'-----------'
     A                                 8 85'----'
     A                                 8 95'---------------'
     A            R DETAIL                 SPACEA(1)
     A*%%TS PD 20240130 100000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A            EMPID          6A  O  9  1
     A            EMPNAME       20A  O  9 10
     A            DEPTNAME       8A  O  9 31
     A            POSITION      15A  O  9 41
     A            SALARY         7P 2O  9 57EDTCDE(1)
     A            HIREDATE      10A  O  9 70
     A            STATUS         1A  O  9 85
     A            PHONE         15A  O  9 95
     A            R DEPTHEAD               SPACEB(2)
     A*%%TS PD 20240130 100000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                SPACEA(1)
     A                                10  1'部門: '
     A            DEPTHEADING    25A  O 10  8
     A            R DEPTFOOT               SPACEA(1)
     A*%%TS PD 20240130 100000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                11  1'部門小計:'
     A            DEPTCOUNT       4  0O 11 15EDTCDE(4)
     A                                11 20'人'
     A            DEPTTOTAL       9P 2O 11 57EDTCDE(1)
     A            R GRANDTOTAL             SPACEB(2)
     A*%%TS PD 20240130 100000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                SPACEA(1)
     A                                12  1'總計:'
     A            GRANDCOUNT      5  0O 12 15EDTCDE(4)
     A                                12 20'人'
     A            GRANDSUM       10P 2O 12 57EDTCDE(1)
     A                                12 75'平均薪資:'
     A            AVGSAL          8P 2O 12 87EDTCDE(1)
     A            R OVERFLOW               SPACEA(1)
     A*%%TS PD 20240130 100000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                SKIPA(99)
```

#### 2.1.2 員工清單報表程式

```cobol
      *****************************************************************
      * 程式名稱：EMPLSTR                                            *
      * 程式功能：員工清單報表                                       *
      * 作者：報表設計師                                             *
      * 日期：2024/01/30                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLSTR.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLIST-REPORT
               ASSIGN TO PRINTER-QPRINT.
               
           SELECT EMPLOYEE-FILE
               ASSIGN TO DATABASE-EMPMASTER
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS EMP-ID
               FILE STATUS IS WS-EMP-FILE-STATUS.
               
           SELECT DEPARTMENT-FILE
               ASSIGN TO DATABASE-DEPTMASTER
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS DEPT-CODE
               FILE STATUS IS WS-DEPT-FILE-STATUS.
               
       DATA DIVISION.
       FILE SECTION.
       
       FD  EMPLIST-REPORT
           USAGE IS DISPLAY.
       01  EMPLIST-RECORD.
           COPY DDS-ALL-FORMATS OF EMPLISTR.
           
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           COPY EMPFMT OF EMPMASTER.
           
       FD  DEPARTMENT-FILE.
       01  DEPARTMENT-RECORD.
           COPY DEPTFMT OF DEPTMASTER.
           
       WORKING-STORAGE SECTION.
       
      *    檔案狀態
       01  WS-FILE-STATUS.
           05  WS-EMP-FILE-STATUS  PIC XX.
               88  EMP-FILE-SUCCESS        VALUE '00'.
               88  EMP-FILE-EOF            VALUE '10'.
           05  WS-DEPT-FILE-STATUS PIC XX.
               88  DEPT-FILE-SUCCESS       VALUE '00'.
               
      *    控制變數
       01  WS-CONTROL-FIELDS.
           05  WS-CURRENT-DEPT     PIC X(4).
           05  WS-PREVIOUS-DEPT    PIC X(4).
           05  WS-FIRST-RECORD     PIC X.
               88  FIRST-RECORD            VALUE 'Y'.
               88  NOT-FIRST-RECORD        VALUE 'N'.
           05  WS-PAGE-COUNT       PIC 9(3) VALUE 0.
           05  WS-LINE-COUNT       PIC 99 VALUE 99.
           05  WS-MAX-LINES        PIC 99 VALUE 55.
           
      *    統計變數
       01  WS-STATISTICS.
           05  WS-DEPT-COUNT       PIC 9(4) VALUE 0.
           05  WS-DEPT-TOTAL       PIC 9(9)V99 VALUE 0.
           05  WS-GRAND-COUNT      PIC 9(5) VALUE 0.
           05  WS-GRAND-TOTAL      PIC 9(10)V99 VALUE 0.
           05  WS-AVERAGE-SALARY   PIC 9(8)V99 VALUE 0.
           
      *    工作變數
       01  WS-WORK-FIELDS.
           05  WS-COMPANY-NAME     PIC X(30) VALUE '台灣科技股份有限公司'.
           
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           PERFORM INITIALIZE-PROGRAM
           PERFORM PROCESS-EMPLOYEES
           PERFORM PRINT-GRAND-TOTAL
           PERFORM TERMINATE-PROGRAM.
           STOP RUN.
           
       INITIALIZE-PROGRAM.
           OPEN OUTPUT EMPLIST-REPORT.
           OPEN INPUT EMPLOYEE-FILE.
           OPEN INPUT DEPARTMENT-FILE.
           
           SET FIRST-RECORD TO TRUE.
           MOVE HIGH-VALUES TO WS-PREVIOUS-DEPT.
           
       PROCESS-EMPLOYEES.
           PERFORM READ-EMPLOYEE
           PERFORM UNTIL EMP-FILE-EOF
               PERFORM CHECK-DEPARTMENT-BREAK
               PERFORM PROCESS-EMPLOYEE-RECORD
               PERFORM READ-EMPLOYEE
           END-PERFORM.
           
      *    處理最後一個部門的小計
           IF WS-GRAND-COUNT > 0
               PERFORM PRINT-DEPARTMENT-FOOTER
           END-IF.
           
       READ-EMPLOYEE.
           READ EMPLOYEE-FILE
               AT END
                   SET EMP-FILE-EOF TO TRUE
               NOT AT END
                   MOVE EMP-DEPT OF EMPLOYEE-RECORD TO WS-CURRENT-DEPT
           END-READ.
           
       CHECK-DEPARTMENT-BREAK.
           IF WS-CURRENT-DEPT NOT = WS-PREVIOUS-DEPT
               IF NOT FIRST-RECORD
                   PERFORM PRINT-DEPARTMENT-FOOTER
               END-IF
               PERFORM PRINT-DEPARTMENT-HEADER
               MOVE WS-CURRENT-DEPT TO WS-PREVIOUS-DEPT
               MOVE 0 TO WS-DEPT-COUNT
               MOVE 0 TO WS-DEPT-TOTAL
               SET NOT-FIRST-RECORD TO TRUE
           END-IF.
           
       PROCESS-EMPLOYEE-RECORD.
           PERFORM CHECK-PAGE-OVERFLOW.
           
           MOVE EMP-ID OF EMPLOYEE-RECORD TO EMPID OF DETAIL.
           MOVE EMP-NAME OF EMPLOYEE-RECORD TO EMPNAME OF DETAIL.
           MOVE EMP-DEPT OF EMPLOYEE-RECORD TO DEPTNAME OF DETAIL.
           MOVE EMP-POSITION OF EMPLOYEE-RECORD TO POSITION OF DETAIL.
           MOVE EMP-SALARY OF EMPLOYEE-RECORD TO SALARY OF DETAIL.
           MOVE EMP-HIRE-DATE OF EMPLOYEE-RECORD TO HIREDATE OF DETAIL.
           MOVE EMP-STATUS OF EMPLOYEE-RECORD TO STATUS OF DETAIL.
           MOVE EMP-PHONE OF EMPLOYEE-RECORD TO PHONE OF DETAIL.
           
           WRITE EMPLIST-RECORD FORMAT IS DETAIL.
           ADD 1 TO WS-LINE-COUNT.
           
      *    累計統計
           ADD 1 TO WS-DEPT-COUNT.
           ADD EMP-SALARY OF EMPLOYEE-RECORD TO WS-DEPT-TOTAL.
           ADD 1 TO WS-GRAND-COUNT.
           ADD EMP-SALARY OF EMPLOYEE-RECORD TO WS-GRAND-TOTAL.
           
       PRINT-DEPARTMENT-HEADER.
           PERFORM CHECK-PAGE-OVERFLOW.
           
      *    取得部門名稱
           MOVE WS-CURRENT-DEPT TO DEPT-CODE OF DEPARTMENT-RECORD.
           READ DEPARTMENT-FILE
               INVALID KEY
                   MOVE WS-CURRENT-DEPT TO DEPTHEADING OF DEPTHEAD
               NOT INVALID KEY
                   STRING DEPT-NAME OF DEPARTMENT-RECORD 
                          ' (' WS-CURRENT-DEPT ')'
                          DELIMITED BY SIZE
                          INTO DEPTHEADING OF DEPTHEAD
           END-READ.
           
           WRITE EMPLIST-RECORD FORMAT IS DEPTHEAD.
           ADD 2 TO WS-LINE-COUNT.
           
       PRINT-DEPARTMENT-FOOTER.
           MOVE WS-DEPT-COUNT TO DEPTCOUNT OF DEPTFOOT.
           MOVE WS-DEPT-TOTAL TO DEPTTOTAL OF DEPTFOOT.
           
           WRITE EMPLIST-RECORD FORMAT IS DEPTFOOT.
           ADD 1 TO WS-LINE-COUNT.
           
       CHECK-PAGE-OVERFLOW.
           IF WS-LINE-COUNT > WS-MAX-LINES
               PERFORM PRINT-PAGE-HEADER
           END-IF.
           
       PRINT-PAGE-HEADER.
           ADD 1 TO WS-PAGE-COUNT.
           MOVE WS-COMPANY-NAME TO COMPNAME OF HEADING.
           
           WRITE EMPLIST-RECORD FORMAT IS HEADING.
           MOVE 8 TO WS-LINE-COUNT.
           
       PRINT-GRAND-TOTAL.
           IF WS-GRAND-COUNT > 0
               COMPUTE WS-AVERAGE-SALARY = WS-GRAND-TOTAL / WS-GRAND-COUNT
           END-IF.
           
           MOVE WS-GRAND-COUNT TO GRANDCOUNT OF GRANDTOTAL.
           MOVE WS-GRAND-TOTAL TO GRANDSUM OF GRANDTOTAL.
           MOVE WS-AVERAGE-SALARY TO AVGSAL OF GRANDTOTAL.
           
           WRITE EMPLIST-RECORD FORMAT IS GRANDTOTAL.
           
       TERMINATE-PROGRAM.
           CLOSE EMPLIST-REPORT.
           CLOSE EMPLOYEE-FILE.
           CLOSE DEPARTMENT-FILE.
```

### 2.2 統計分析報表

#### 2.2.1 部門統計報表DDS

```dds
      *
      * 部門統計分析報表格式定義
      *
     A*%%TS PD 20240130 110000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                      PAGESIZE(66 132)
     A            R STATHEADER              SPACEB(1)
     A*%%TS PD 20240130 110000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                      SKIPA(1)
     A                                 1  1'1'
     A                                 1 45'部門統計分析報表'
     A                                 1 45DSPATR(UL)
     A                                 1120'頁次:'
     A                                 1126PAGNBR
     A                                 1126EDTCDE(4)
     A                                 3  1'報表日期:'
     A                                 3 11DATE
     A                                 3 11EDTCDE(Y)
     A                                 3120'列印時間:'
     A                                 3130TIME
     A            COMPNAME      30A  O  5  1
     A            REPORTPARAM   50A  O  6  1
     A                                 8  1'部門代碼'
     A                                 8 10'部門名稱'
     A                                 8 31'主管姓名'
     A                                 8 46'員工人數'
     A                                 8 57'薪資總額'
     A                                 8 72'平均薪資'
     A                                 8 87'最高薪資'
     A                                 8102'最低薪資'
     A                                 8117'預算餘額'
     A                                 9  1'--------'
     A                                 9 10'--------------------'
     A                                 9 31'--------------'
     A                                 9 46'--------'
     A                                 9 57'------------'
     A                                 9 72'------------'
     A                                 9 87'------------'
     A                                 9102'------------'
     A                                 9117'------------'
     A            R STATDETAIL              SPACEA(1)
     A*%%TS PD 20240130 110000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A            DEPTCODE       4A  O 10  1
     A            DEPTNAME      20A  O 10 10
     A            MANAGERNAME   14A  O 10 31
     A            EMPCOUNT       4  0O 10 46EDTCDE(4)
     A            SALARYSUM     10P 2O 10 57EDTCDE(1)
     A            SALARYAVG      8P 2O 10 72EDTCDE(1)
     A            SALARYMAX      8P 2O 10 87EDTCDE(1)
     A            SALARYMIN      8P 2O 10102EDTCDE(1)
     A            BUDGETBAL     10P 2O 10117EDTCDE(1)
     A            R STATTOTAL              SPACEB(2)
     A*%%TS PD 20240130 110000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                SPACEA(1)
     A                                11  1'統計總計:'
     A            TOTALDEPTS      3  0O 11 15EDTCDE(4)
     A                                11 19'個部門'
     A            TOTALEMPS       5  0O 11 46EDTCDE(4)
     A                                11 52'人'
     A            TOTALSAL       11P 2O 11 57EDTCDE(1)
     A            OVERALLAVG      8P 2O 11 72EDTCDE(1)
     A            TOTALBUDGET    11P 2O 11117EDTCDE(1)
     A            R ANALYSIS               SPACEB(3)
     A*%%TS PD 20240130 110000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                SPACEA(1)
     A                                12  1'分析摘要:'
     A                                13  5'1. 最大部門:'
     A            BIGGESTDEPT    20A  O 13 18
     A            BIGGESTCOUNT    4  0O 13 40EDTCDE(4)
     A                                13 45'人'
     A                                14  5'2. 薪資最高部門:'
     A            RICHESTDEPT    20A  O 14 22
     A            RICHESTSAL      8P 2O 14 44EDTCDE(1)
     A                                15  5'3. 效率最佳部門:'
     A            EFFICIENTDEPT  20A  O 15 22
     A            EFFICIENTRATIO  5P 2O 15 44EDTCDE(3)
     A                                15 51'%'
     A                                16  5'4. 預算使用率:'
     A            BUDGETUSAGE     5P 2O 16 18EDTCDE(3)
     A                                16 25'%'
```

#### 2.2.2 部門統計報表程式

```cobol
      *****************************************************************
      * 程式名稱：DEPTSTAT                                           *
      * 程式功能：部門統計分析報表                                   *
      * 作者：統計分析師                                             *
      * 日期：2024/01/30                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEPTSTAT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DEPTSTAT-REPORT
               ASSIGN TO PRINTER-QPRINT.
               
       DATA DIVISION.
       FILE SECTION.
       
       FD  DEPTSTAT-REPORT
           USAGE IS DISPLAY.
       01  DEPTSTAT-RECORD.
           COPY DDS-ALL-FORMATS OF DEPTSTATR.
           
       WORKING-STORAGE SECTION.
       
      *    SQL包含區
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           
      *    部門統計資料結構
       01  WS-DEPT-STATISTICS.
           05  WS-DEPT-DATA OCCURS 20 TIMES INDEXED BY WS-IDX.
               10  WS-DEPT-CODE        PIC X(4).
               10  WS-DEPT-NAME        PIC X(20).
               10  WS-MANAGER-NAME     PIC X(14).
               10  WS-EMP-COUNT        PIC 9(4).
               10  WS-SALARY-SUM       PIC 9(10)V99.
               10  WS-SALARY-AVG       PIC 9(8)V99.
               10  WS-SALARY-MAX       PIC 9(8)V99.
               10  WS-SALARY-MIN       PIC 9(8)V99.
               10  WS-BUDGET-BALANCE   PIC 9(10)V99.
               
       01  WS-TOTALS.
           05  WS-TOTAL-DEPTS          PIC 9(3) VALUE 0.
           05  WS-TOTAL-EMPLOYEES      PIC 9(5) VALUE 0.
           05  WS-TOTAL-SALARY         PIC 9(11)V99 VALUE 0.
           05  WS-OVERALL-AVERAGE      PIC 9(8)V99 VALUE 0.
           05  WS-TOTAL-BUDGET         PIC 9(11)V99 VALUE 0.
           
      *    分析結果
       01  WS-ANALYSIS.
           05  WS-BIGGEST-DEPT         PIC X(20).
           05  WS-BIGGEST-COUNT        PIC 9(4).
           05  WS-RICHEST-DEPT         PIC X(20).
           05  WS-RICHEST-SALARY       PIC 9(8)V99.
           05  WS-EFFICIENT-DEPT       PIC X(20).
           05  WS-EFFICIENT-RATIO      PIC 9(3)V99.
           05  WS-BUDGET-USAGE         PIC 9(3)V99.
           
      *    工作變數
       01  WS-WORK-FIELDS.
           05  WS-COMPANY-NAME         PIC X(30) VALUE '台灣科技股份有限公司'.
           05  WS-REPORT-PARAM         PIC X(50).
           05  WS-LINE-COUNT           PIC 99 VALUE 99.
           05  WS-MAX-LINES            PIC 99 VALUE 55.
           
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           PERFORM INITIALIZE-PROGRAM
           PERFORM COLLECT-STATISTICS
           PERFORM ANALYZE-DATA
           PERFORM PRINT-REPORT
           PERFORM TERMINATE-PROGRAM.
           STOP RUN.
           
       INITIALIZE-PROGRAM.
           OPEN OUTPUT DEPTSTAT-REPORT.
           MOVE 'Active Employees Only' TO WS-REPORT-PARAM.
           
       COLLECT-STATISTICS.
           PERFORM COLLECT-DEPARTMENT-DATA
           PERFORM CALCULATE-TOTALS.
           
       COLLECT-DEPARTMENT-DATA.
           SET WS-IDX TO 1.
           
           EXEC SQL
               DECLARE DEPT_CURSOR CURSOR FOR
               SELECT D.DEPT_CODE, D.DEPT_